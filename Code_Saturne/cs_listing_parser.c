/*
	CODE_SATURNE LISTING PARSE PROGRAM

	Arnau Miro, Manel Soria UPC (ESEIAAT)
	date: 21/09/2016
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/types.h>

const char *tmp_file = "tmp";
const char *out_file = "listing.parse";
const char *gnuplot_file = "gnuplot.tmp";

// Function prototypes
void reads(FILE *fin, int len, char *line);
char *trim(char *str);
void writeFile(const char *fname, float *vector, int len, int col);
void writegnuplotFile(const char *fname, const char *parsed_file, char const *order[], int num_order, int ncols);
void pipegnuplot(FILE *gnuplotPipe, const char *parsed_file, char const *order[], int num_order, int ncols);

void cs_parse_time_for_timestep(const char *fname, int *nelem, float **out);
int cs_parse_instant(const char *fname, int nelem, float **out, int start_col);
int cs_parse_c(const char *fname, int nelem, float **out, int start_col);
int cs_parse_v(const char *fname, int nelem, float **out, int start_col);
int cs_parse_bc(const char *fname, int nelem, float **out, int start_col);

int main(int argc, char const *argv[]) {
	int nelem = 0;
	int ncols = 0;
	int ii, dela, ret;
	float *out;

	char command[256],func[80];

	// Open file for writing
	FILE *gnuplotPipe = popen("gnuplot -p", "w"); assert(gnuplotPipe != NULL);

	// Parsing of input arguments
	if (argc < 3){
		printf("Wrong number of input arguments! Usage:\n");
		printf("%s <lines parsed> <delay> <VAR1> <VAR2> ...\n",argv[0]);
		printf("  lines parsed = 0 -> %s uses cat\n",argv[0]);
		printf("  lines parsed > 0 -> %s uses tail -n <lines parsed>\n",argv[0]);
		printf("  delay is in sec\n");
		printf("  variables accepted: Velocity, Pressure, CourantNb, Yplus\n");
		exit(-1);
	}

	// Display which variables are parsed
	printf("CS_PARSER variables parsed: ");
	for(ii=3;ii<argc;ii++)
		printf("<%s> ",argv[ii]);
	printf("\n");

	// Set command to be executed
	if (atoi(argv[1]) == 0)
		sprintf(func,"cat"); // Use function cat
	else
		sprintf(func,"tail -n %d",atoi(argv[1]));

	// Get delay
	dela = atoi(argv[2]);

	// Program loop
	printf("CS_PARSER Close using Ctrl+C\n");
	while (1)
	{
		ncols = 0;
		out = (float*)malloc(1*sizeof(float)); assert(out != NULL);

		// Parse calculation time and set the number of elements
		sprintf(command,"%s listing | grep 'TIME FOR THE TIME STEP' > %s",func,tmp_file);
		#ifdef VERBOSE
		printf("CS_PARSER parsing <%s>...",command); fflush(stdout);
		#endif
		ret = system(command); assert(ret != -1);
		cs_parse_time_for_timestep(tmp_file,&nelem,&out);
		ncols += 1; // Only reads 1 column
		#ifdef VERBOSE
		printf(" done!\n"); fflush(stdout);
		#endif

		if (nelem < 3) continue; // Fix for few elements

		// Command to parse instants
		sprintf(command,"%s listing | grep 'INSTANT' > %s",func,tmp_file);
		#ifdef VERBOSE
		printf("CS_PARSER parsing <%s>...",command); fflush(stdout);
		#endif
		ret = system(command); assert(ret != -1);
		ret = cs_parse_instant(tmp_file,nelem,&out,ncols); if (ret == -1) continue;
		ncols += 2; // Reads 2 columns
		#ifdef VERBOSE
		printf(" done!\n"); fflush(stdout);
		#endif

		// Parse commands
		for(ii=3;ii<argc;ii++) {
			switch (tolower(argv[ii][0])) {
				case 'v': // Command to parse velocity field
					sprintf(command,"%s listing | grep 'c  Velocity ' > %s",func,tmp_file);
					#ifdef VERBOSE
					printf("CS_PARSER parsing <%s>...",command); fflush(stdout);
					#endif
					ret = system(command); assert(ret != -1); // Execute system command
					ret = cs_parse_c(tmp_file,nelem,&out,ncols); if (ret == -1) continue; // Skip to next iteration if error
					ncols += 3; // Reads 3 columns
					#ifdef VERBOSE
					printf(" done!\n"); fflush(stdout);
					#endif
					break;
				case 'p': // Command to parse pressure field
					sprintf(command,"%s listing | grep 'c  Pressure ' > %s",func,tmp_file);
					#ifdef VERBOSE
					printf("CS_PARSER parsing <%s>...",command); fflush(stdout);
					#endif
					ret = system(command); assert(ret != -1); // Execute system command
					ret = cs_parse_c(tmp_file,nelem,&out,ncols); if (ret == -1) continue; // Skip to next iteration if error
					ncols += 3; // Reads 3 columns
					#ifdef VERBOSE
					printf(" done!\n"); fflush(stdout);
					#endif
					break;
				case 'c': // Command to parse courant no
					sprintf(command,"%s listing | grep 'v  CourantNb ' > %s",func,tmp_file);
					#ifdef VERBOSE
					printf("CS_PARSER parsing <%s>...",command); fflush(stdout);
					#endif
					ret = system(command); assert(ret != -1); // Execute system command
					ret = cs_parse_v(tmp_file,nelem,&out,ncols); if (ret == -1) continue; // Skip to next iteration if error
					ncols += 3; // Reads 3 columns
					#ifdef VERBOSE
					printf(" done!\n"); fflush(stdout);
					#endif
					break;
				case 'y':
					sprintf(command,"%s listing | grep 'yplus ' > %s",func,tmp_file);
					#ifdef VERBOSE
					printf("CS_PARSER parsing <%s>...",command); fflush(stdout);
					#endif
					ret = system(command); assert(ret != -1); // Execute system command
					ret = cs_parse_bc(tmp_file,nelem,&out,ncols); if (ret == -1) continue; // Skip to next iteration if error
					ncols += 2; // Reads 2 columns
					#ifdef VERBOSE
					printf(" done!\n"); fflush(stdout);
					#endif
					break;
				default:
					#ifdef VERBOSE
					printf("CS_PARSER option <%s> not found! Will not be parsed.\n",argv[ii]);
					#endif
					break;
			}
		}

		// Remove temporary file
		sprintf(command,"rm %s",tmp_file);
		ret = system(command); assert(ret != -1);

		// Write output to file
		#ifdef VERBOSE
		printf("CS_PARSER file %s writing %d elements in %d columns... ",out_file,nelem,ncols); fflush(stdout);
		#endif
		writeFile(out_file,out,nelem,ncols); free(out);
		#ifdef VERBOSE
		printf(" done!\n"); fflush(stdout);
		#endif

		// Write gnuplot file
		pipegnuplot(gnuplotPipe,out_file,argv,argc,ncols);

		// Free and sleep
		sleep(dela);
	}
	fclose(gnuplotPipe);
	return 0;
}

void cs_parse_time_for_timestep(const char *fname, int *nelem, float **out) {
	const int len = 256;
	char line[len];
	char *running, *token;

	int ii;

	FILE *myfile;

	// Open file for reading
	myfile = fopen(fname,"r"); assert(myfile != NULL);

	// First allocation
	//(*out) = (float*)malloc(1*sizeof(float)); assert(val != NULL);

	// Read line by line
	(*nelem) = 0;
	while( !feof(myfile) ){
		reads(myfile,len,line); // read a line from text file
		running = strdup(line);
		for(ii=0;ii<7;ii++){
			running = trim(running);
			token = strsep(&running," ");
			// Save last column
			if(ii == 6) (*out)[(*nelem)] = atof(token);
		}
		(*nelem)++;
		// Realloc
		*out = (float*)realloc(*out,((*nelem)+1)*sizeof(float)); assert(*out != NULL);
	}
	fclose(myfile);
}

int cs_parse_instant(const char *fname, int nelem, float **out, int start_col)  {
	const int len = 256;
	char line[len];
	char *running, *token;

	int ii, iii;

	FILE *myfile;

	// Open file for parsing
	myfile = fopen(fname,"r"); assert(myfile != NULL);

	// Allocate out vector
	*out = (float*)realloc(*out,(start_col+2)*nelem*sizeof(float)); assert(*out != NULL);

	// Read nelem lines
	for(ii=0; ii<nelem; ii++){
		if (feof(myfile)) break;
		reads(myfile,len,line); // Read a line from file
		running = strdup(line);
		// Parse line contents
		for(iii = 0; iii < 6; iii++) {
			running = trim(running);
			token = strsep(&running," ");
			// Save columns when needed
			if (iii == 1) (*out)[ii + (0+start_col)*nelem] = atof(token);
			if (iii == 5) (*out)[ii + (1+start_col)*nelem] = atof(token);
		}
	}
	fclose(myfile);

	if (ii != nelem) return -1;
	return 0;
}

int cs_parse_c(const char *fname, int nelem, float **out, int start_col) {
	const int len = 256;
	char line[len];
	char *running, *token;

	int ii, iii;

	FILE *myfile;

	// Open file for parsing
	myfile = fopen(fname,"r"); assert(myfile != NULL);

	// Allocate out vector
	*out = (float*)realloc(*out,(start_col+3)*nelem*sizeof(float)); assert(*out != NULL);

	// Read nelem lines
	for(ii=0; ii<nelem; ii++){
		reads(myfile,len,line); // Read a line from file
		running = strdup(line);
		// Parse line contents
		for(iii = 0; iii < 7; iii++) {
			if (feof(myfile)) break;
			running = trim(running);
			token = strsep(&running," ");
			// Save columns when needed
			if (iii == 3) (*out)[ii + (0+start_col)*nelem] = atof(token);
			if (iii == 4) (*out)[ii + (1+start_col)*nelem] = atof(token);
			if (iii == 6) (*out)[ii + (2+start_col)*nelem] = atof(token);
		}
	}
	// Deallocate and close file
	fclose(myfile);
	
	if (ii != nelem) return -1;
	return 0;
}

int cs_parse_v(const char *fname, int nelem, float **out, int start_col) {
	const int len = 256;
	char line[len];
	char *running, *token;

	int ii, iii;

	FILE *myfile;

	// Open file for parsing
	myfile = fopen(fname,"r"); assert(myfile != NULL);

	// Allocate out vector
	*out = (float*)realloc(*out,(start_col+3)*nelem*sizeof(float)); assert(*out != NULL);

	// Read nelem lines
	for(ii=0; ii<nelem; ii++){
		if (feof(myfile)) break; // Stop if end of file reached
		reads(myfile,len,line); // Read a line from file
		running = strdup(line);
		// Parse line contents
		for(iii = 0; iii < 6; iii++) {
			running = trim(running);
			token = strsep(&running," ");
			// Save columns when needed
			if (iii == 2) (*out)[ii + (0+start_col)*nelem] = atof(token);
			if (iii == 3) (*out)[ii + (1+start_col)*nelem] = atof(token);
			if (iii == 4) (*out)[ii + (2+start_col)*nelem] = atof(token);
		}
	}
	// Deallocate and close file
	fclose(myfile);

	if (ii != nelem) return -1; // Raise error if not all the lines are read
	return 0;
}

int cs_parse_bc(const char *fname, int nelem, float **out, int start_col) {
	const int len = 256;
	char line[len];
	char *running, *token;

	int ii, iii;

	FILE *myfile;

	// Open file for parsing
	myfile = fopen(fname,"r"); assert(myfile != NULL);

	// Allocate out vector
	*out = (float*)realloc(*out,(start_col+2)*nelem*sizeof(float)); assert(*out != NULL);

	// Read nelem lines
	for(ii=0; ii<nelem; ii++){
		if (feof(myfile)) break;
		reads(myfile,len,line); // Read a line from file
		running = strdup(line);
		// Parse line contents
		for(iii = 0; iii < 6; iii++) {
			running = trim(running);
			token = strsep(&running," ");
			// Save columns when needed
			if (iii == 4) (*out)[ii + (0+start_col)*nelem] = atof(token);
			if (iii == 5) (*out)[ii + (1+start_col)*nelem] = atof(token);
		}
	}
	// Deallocate and close file
	fclose(myfile);

	if (ii != nelem) return -1; // Raise error if not all the lines are read
	return 0;
}

void writeFile(const char *fname, float *vector, int len, int col) {
	int ii, iii;
	FILE *myfile;

	// Open file for writing
	myfile = fopen(fname,"w"); assert(myfile != NULL);

	// Loop the vector and write the columns
	for (ii = 0; ii < len; ii++){
		for(iii = 0;iii < col; iii++)
			fprintf(myfile,"%e\t",vector[ii+iii*len]);
		fprintf(myfile,"\n");
	}

	fclose(myfile);
}

void pipegnuplot(FILE *gnuplotPipe, const char *parsed_file, char const *order[], int num_order, int ncols) {
	const int instants = 3;
	int cols = 0;
	int niter_vel=0, niter_pre=0, cou=0, yplu=0;
	int plot_iter=0, plot_resi=0, plot_cou=0, plot_yplu=0;
	int ii;

	// Parse order and number of plots
	cols = 3; // Start with computational time and instant
	for(ii=3;ii<num_order;ii++){
		switch (tolower(order[ii][0]))
		{
			case 'v': // Velocity
				plot_iter = 1; // Plot number of iterations
				plot_resi = 1; // Plot residuals
				niter_vel = cols+1;
				cols += 3; // Velocity adds 3 columns
				break;
			case 'p': // Pressure
				plot_iter = 1; // Plot number of iterations
				plot_resi = 1; // Plot residuals
				niter_pre = cols+1;
				cols += 3; // Pressure adds 3 columns
				break;
			case 'c': // Courant Nb
				plot_cou = 1; // Plot courant nb
				cou = cols+1;
				cols += 3; // Courant adds 3 columns
				break;
			case 'y': // yplus
				plot_yplu = 1; // Plot yplus
				yplu = cols+1;
				cols += 2; // yplus adds 2 columns
				break;
		}
	}
	assert(cols == ncols);

	// Computational time plot
	fprintf(gnuplotPipe, "set term x11 0 persist\n");
	fprintf(gnuplotPipe, "set key box\n");
	fprintf(gnuplotPipe, "set grid\n");
	fprintf(gnuplotPipe, "set autoscale\n");
	fprintf(gnuplotPipe, "set ylabel 'Computational time (sec)'\n");
	fprintf(gnuplotPipe, "set xlabel 'Instant'\n");
	fprintf(gnuplotPipe, "plot '%s' using %d:1 notitle\n",parsed_file,instants);
	fprintf(gnuplotPipe,"\n");
	
	// Flush first plot
	fflush(gnuplotPipe);

	// Multiplot
	fprintf(gnuplotPipe, "set term x11 1 persist\n");
	fprintf(gnuplotPipe, "set multiplot layout 2,2\n");
	// Number of iterations plot
	if (plot_iter) {
		ii = 1;
		fprintf(gnuplotPipe, "set key box\n");
		fprintf(gnuplotPipe, "set grid\n");
		fprintf(gnuplotPipe, "set autoscale\n");
		fprintf(gnuplotPipe, "set ylabel 'N iter'\n");
		fprintf(gnuplotPipe, "set xlabel 'Instant'\n");
		fprintf(gnuplotPipe, "plot ");
		if (niter_vel > 0) {
			if (ii>1) fprintf(gnuplotPipe, ",");
			fprintf(gnuplotPipe,"'%s' using %d:%d title 'Velocity' with line lt %d",parsed_file,instants,niter_vel,ii);
			ii++;
		}
		if (niter_pre > 0) {
			if (ii>1) fprintf(gnuplotPipe, ",");
			fprintf(gnuplotPipe,"'%s' using %d:%d title 'Pressure' with line lt %d",parsed_file,instants,niter_pre,ii);
			ii++;
		}
		fprintf(gnuplotPipe, "\n");
		fprintf(gnuplotPipe, "unset key\n");
	}
	// Residuals plot
	if (plot_resi) {
		ii = 1;
		fprintf(gnuplotPipe, "set key box\n");
		fprintf(gnuplotPipe, "set grid\n");
		fprintf(gnuplotPipe, "set logscale y\n");
		fprintf(gnuplotPipe, "set format y '10^{%%T}'\n");
		fprintf(gnuplotPipe, "set autoscale\n");
		fprintf(gnuplotPipe, "set ylabel 'Residuals (norm)'\n");
		fprintf(gnuplotPipe, "set xlabel 'Instant'\n");
		fprintf(gnuplotPipe, "plot ");
		if (niter_vel > 0) {
			if (ii>1) fprintf(gnuplotPipe, ",");
			fprintf(gnuplotPipe,"'%s' using %d:%d title 'Velocity' with line lt %d",parsed_file,instants,niter_vel+1,ii);
			ii++;
		}
		if (niter_pre > 0) {
			if (ii>1) fprintf(gnuplotPipe, ",");
			fprintf(gnuplotPipe,"'%s' using %d:%d title 'Pressure' with line lt %d",parsed_file,instants,niter_pre+1,ii);
			ii++;
		}
		fprintf(gnuplotPipe, "\n");
		fprintf(gnuplotPipe, "unset logscale\n");
		fprintf(gnuplotPipe, "unset format\n");
		fprintf(gnuplotPipe, "unset key\n");
	}
	// Courant Nb plot
	if (plot_cou) {
		fprintf(gnuplotPipe, "set key box\n");
		fprintf(gnuplotPipe, "set grid\n");
		fprintf(gnuplotPipe, "set autoscale\n");
		fprintf(gnuplotPipe, "set ylabel 'CFL'\n");
		fprintf(gnuplotPipe, "set xlabel 'Time (sec)'\n");
		fprintf(gnuplotPipe, "plot '%s' using %d:%d title 'min' with line lt 1, '%s' using %d:%d title 'max' with line lt 2, '%s' using %d:%d title 'mean' with line lt 3\n",
			             parsed_file,instants-1,cou,parsed_file,instants-1,cou+1,parsed_file,instants-1,cou+2);
		fprintf(gnuplotPipe, "unset key\n");
	}
	// Yplus plot
	if (plot_yplu) {
		fprintf(gnuplotPipe, "set key box\n");
		fprintf(gnuplotPipe, "set grid\n");
		fprintf(gnuplotPipe, "set autoscale\n");
		fprintf(gnuplotPipe, "set ylabel 'y+'\n");
		fprintf(gnuplotPipe, "set xlabel 'Time (sec)'\n");
		fprintf(gnuplotPipe, "plot '%s' using %d:%d title 'min' with line lt 1, '%s' using %d:%d title 'max' with line lt 2\n",
			             parsed_file,instants-1,yplu,parsed_file,instants-1,yplu+1);
		fprintf(gnuplotPipe, "unset key\n");
	}
	fprintf(gnuplotPipe, "unset multiplot\n");

	// Flush pipe
	fflush(gnuplotPipe);
}

void reads(FILE *fin, int len, char *line) {
  int ii;

  line=fgets(line,len,fin);

	if (line != NULL)
	  for (ii = 0; line[ii] != '\0'; ii++)
	    if (line[ii] == 10) { line[ii] = '\0'; break; }
}

char *trim(char *str) {
  char *end;

  // Trim leading space
  while(isspace(*str)) str++;

  if(*str == 0)  // All spaces?
    return str;

  // Trim trailing space
  end = str + strlen(str) - 1;
  while(end > str && isspace(*end)) end--;

  // Write new null terminator
  *(end+1) = 0;

  return str;
}
