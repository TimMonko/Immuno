///Three Channel  Complete Macro - Tim Monko 10/12/2017 updated considerably
// This macro is designed to take a composite 16-bit (3 color) tiff image and normalize, then threshold each channel. Now with extra colocalization goodies

runSingleChannel16bitConversion = 0; //for grayscale 16-bit (single channel) images. These need to be converted to a Composite stack to work with the code, set to 1 if this is what you need.
	//Use the Red channel for all SingleChannel purposes.
runThresholdCode = 1; //if 1, then run the accompanying code, else skip
runExtras = 1; //this is specifically for fillholes and remove particles based on size
runColocCode = 1; //1 = run colocalization using Binary Feature Extractor, 0 = off

/////////////////////////////////
///THRESHOLDING CODE VARIABLES///
/////////////////////////////////

numchannels = 3; //use the number of 16-bit channels, if RGB set to 3

//RED /GRAY == also for 16-bit gray single channels
dored = 1;
redtograyscale = 0; 
redrbr = 2;
rthreslow = 0.25; //40 
rthreshigh = 4;
redwatererosion = 10;
redwaterconvex = 0;

//GREEN
dogreen = 1;
greentograyscale = 0;
greenrbr = 0.1; // 0.01ish for 16-bit, 10ish for 8-bit (at 10x)
gthreslow = 0.15; //25
gthreshigh = 4;
greenwatererosion = 10;
greenwaterconvex = 0;

//BLUE
doblue = 0; // 1 = will run this  channel, 0 = skip, will close the image
bluetograyscale = 0; // 1 = will not threshold image, instead just puts on grayscale LUT
bluerbr = 1; // Rolling ball radius.  At 10X, 0.01 for poor S/N. Up to 0.1 for good S/N and/or larger cells
bthreslow = 0.20; // lower threshold for this channel, this is a "percentage" of the maximum signal from normalization, which helps intuitively. ie 0.20 means that what is kept is all values 20% or higher
bthreshigh = 4; // upper threshold for green channel, keep at 4 for 16bit channels (is 400% maximum value if normalizing on 32-bit)
bluewatererosion = 10; // if a number that is not zero is put in convex, then this value is ignored. 10 = around normal watershed algorithm
bluewaterconvex = 0.95; // 0.95 might be good for dividng cells, but this is potentially threshold dependent

//////////////////////
///EXTRAS Variables///
//////////////////////

//BlackParticlesOnWhite = 0; //if you desire to have black particles on white after running the extras but not Coloc, then use a 1 here

//Fills holes, especially useful for certain Abs like PH3 which will have some holes after thresholding. Does override the original thresholded in order to
fillholesRed = 0; //Binary
fillholesGreen = 0; //Binary
fillholesBlue = 0; //Binary

//To remove small particles - this way things don't be counted as overlapping if its a noise that made it through the threshold filter. For now this will be resaved with the original which may be undesirable for counting, but I like it
removeparticlesRed = 1; particlesizeRed = 5;
removeparticlesGreen = 1; particlesizeGreen = 5;
removeparticlesBlue = 1; particlesizeBlue = 5;

/////////////////////////////////////
/// COLOCALIZATION CODE VARIABLES ///
////////////////////////////////////

//Which channels do you have? 1= run this channel for its accompanying colocalization, 0 = this channel isn't there so don't run it
haveRed = 1;
haveGreen = 1;
haveBlue = 0;
haveTriple = 0;

//Objects and Selectors. These should be relatively consistent for the marker that you are doing.
//OverlapXX = Percentages for the BFE overlap filter. Needs to be identified for every colocalization separately. To do so use Analyze Particles with %Area Output. Set the redirect in "Set Measurements". The Object image is analyzed, while the selector image is redirected against.
//Binary feature extractory is a BioVoxxel plugin that compares two images, checks for a percentage of overlap, and recombines the images with an output selecting only colocalized images

ObjectRedGreen = "Green"; //format = "Color", i.e. "Red"
SelectorRedGreen = "Red";
overlapRedGreen = 50; //this is the BFE percentage. (see above)

ObjectRedBlue = "Blue";
SelectorRedBlue = "Red";
overlapRedBlue = 40;

ObjectBlueGreen = "Green";
SelectorBlueGreen = "Blue";
overlapBlueGreen = 50;

ObjectTriple = "Blue";
SelectorTriple = ObjectRedGreen + "_On_" + SelectorRedGreen; //this is the only format that is slightly different. use the object/selector combo as such to call the properly title image. leave the "_On_" intact.
overlapTriple = 50;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
imageTitle=getTitle(); //returns a string with the image title, used later for saving 

close("\\Others"); // because of the false batch mode this will allow only the front image to remain to prevent cluttering 
setBatchMode("false");
run("Select All");

if (runSingleChannel16bitConversion == 1) {
	run("Add Slice");
}

/// CODE FOR THRESHOLDING STARTS BELOW ///

if (runThresholdCode == 1) {
	run("Make Composite");
	run("Split Channels"); //separates the 3 channels to its own color image

	/// BLUE CHANNEL

	if (doblue == 1 && bluetograyscale == 0) {
		selectWindow("C3-"+imageTitle); 
		rename("Blue"); //useful to more reliably call on the image, especially in future macros (such as coloc macro)
		run("Subtract Background...", "rolling=bluerbr sliding"); //Subtract background using slidiing parabaloid. radius =0.01 seems to be appropriate for all 16-bit counts (for Tim at least)
	
		run("Duplicate...", " "); //Duplicates image so that original is not blurred, this code will add a -1 to the title of the new image
		selectWindow("Blue-1");  
		run("Median...", "radius=2"); //Runs median blur of the image, uses pixel radius of 2, I think that radius looks ok
		//setBatchMode("false"); //This command will show a hidden image (as occurs in batch mode) allowing a bin to be drawn to get the max value
		waitForUser;
		List.setMeasurements; //saves measurements to a list
		//print(List.getList); // lists all measurements, useful for future Ref
		mxg = List.getValue("Max"); //pulls Max brightness value from List and saves as variable mxg (i.e. max green)
		close("Blue-1"); //closes the duplicate

		selectWindow("Blue"); 
		run("32-bit"); //convert from 16-bit to 32-bit (floating point scale). 
		run("Divide...", "value=mxg"); //Normalization - divides the image by the previously determined max (mxg)

		setAutoThreshold("Otsu"); //this command preferred over run("Threshold..."), because it uses Otsu's method 
		//run("Threshold..."); 
		setThreshold(bthreslow, bthreshigh); //using 32-bit float values gives a max of 3.4e38, but nothing should be above 4
		setOption("BlackBackground", true);
		run("Convert to Mask");
		selectWindow("Blue"); 
		//setBatchMode("true");
		//run("Watershed"); //splits some cells with watershed algorithm, for now this has been replaced by the Watershed Irregular Features for use with dividing cells 
		run("Watershed Irregular Features", "erosion=bluewatererosion convexity_threshold=bluewaterconvex separator_size=0-Infinity");
	} 
	if (doblue == 0 && numchannels > 2) {
		selectWindow("C3-"+imageTitle); 
		close("C3-"+imageTitle);
	}

	if (doblue == 1 && bluetograyscale == 1) {
		selectWindow("C3-"+imageTitle); 
		rename("Blue");
		run("8-bit");
	}

	/// GREEN CHANNEL, see comments from Blue Channel 
	
	if (dogreen == 0) {
		selectWindow("C2-"+imageTitle);
		close("C2-"+imageTitle);
	}
	
	if (dogreen == 1 && greentograyscale == 0) {
		selectWindow("C2-"+imageTitle);
		rename("Green"); 
		run("Subtract Background...", "rolling=greenrbr sliding"); 

		run("Duplicate...", " "); 
		selectWindow("Green-1");  
		run("Median...", "radius=2");
		//setBatchMode("false"); 
		waitForUser;
		List.setMeasurements; 
		//print(List.getList); 
		mxg = List.getValue("Max");
		close("Green-1"); 

		selectWindow("Green"); 
		run("32-bit"); 
		run("Divide...", "value=mxg"); 

		setAutoThreshold("Otsu"); 
		setThreshold(gthreslow, gthreshigh); 
		setOption("BlackBackground", true);
		run("Convert to Mask");
		selectWindow("Green"); 
		//setBatchMode("true");
		run("Watershed Irregular Features", "erosion=greenwatererosion convexity_threshold=greenwaterconvex separator_size=0-Infinity");
	} 

	if (dogreen == 1 && greentograyscale == 1) {
		selectWindow("C2-"+imageTitle);
		rename("Green"); 
		run("8-bit");
	}

	/// RED CHANNEL - See comments from Blue channel for each section of code 
		if (dored == 0) {
		selectWindow("C1-"+imageTitle);
		close("C1-"+imageTitle);
	}

	if (dored == 1 && redtograyscale == 0) {
		selectWindow("C1-"+imageTitle);
		rename("Red"); 
		run("Subtract Background...", "rolling=redrbr sliding");

		run("Duplicate...", " ");
		selectWindow("Red-1");
		run("Median...", "radius=2");
		//if (runSingleChannelConversion == 1) {
		setBatchMode("show");
		//}
		waitForUser;
		List.setMeasurements;
		mxr = List.getValue("Max"); 
		close("Red-1");
		//if (runSingleChannelConversion == 1) {
		selectWindow("Red");
		run("32-bit");
		run("Divide...", "value=mxr"); 

		setAutoThreshold("Otsu");
		setThreshold(rthreslow, rthreshigh); 
		setOption("BlackBackground", true);
		run("Convert to Mask");
		//}
		selectWindow("Red"); 
		//setBatchMode("true");
		run("Watershed Irregular Features", "erosion=redwatererosion convexity_threshold=redwaterconvex separator_size=0-Infinity");
	} 

	if (dored == 1 && redtograyscale == 1) {
		selectWindow("C1-"+imageTitle);
		rename("Red"); 
		run("8-bit");
	}

	//Final Steps, restack image and rename. The channels are still named "Red", "Green", "Blue". Only does so if colocalization is not run 
	
	if (doblue + dogreen + dored >= 2) {
		run("Images to Stack");
		run("8-bit");
	}
	if (runColocCode != 1 && runExtras != 1) {
		rename(imageTitle);
	}
}

///RUN EXTRAS CODE ///

if (runExtras == 1) {
	if (doblue + dogreen + dored >= 2) {
		run("Stack to Images");
	}

//The following chunk will a) fill holes if there are any (usually a few pixels in things like PH3) then b) remove small noise particles to make colocalizations not rely on too small of an roi
	if (dored == 1 && redtograyscale != 1) {
		selectWindow("Red");
		if (fillholesRed == 1) { run("Fill Holes", "slice"); } 
		if (removeparticlesRed == 1) { run("Particles4 ", "white show=Particles filter minimum=particlesizeRed maximum=9999999 redirect=None"); }
		//run("Duplicate...", " ");
	} 

	if (dogreen == 1 && greentograyscale != 1) {
		selectWindow("Green");
		if (fillholesGreen==1) { run("Fill Holes", "slice"); }
		if (removeparticlesGreen==1) { run("Particles4 ", "white show=Particles filter minimum=particlesizeGreen maximum=9999999 redirect=None"); } // This takes out particles with an area less than 15 (background)
		//run("Duplicate...", " ");
	}

	if (doblue==1 && bluetograyscale != 1) {
		selectWindow("Blue");
		if (fillholesBlue == 1) { run("Fill Holes", "slice"); } 
		if (removeparticlesBlue == 1) { run("Particles4 ", "white show=Particles filter minimum=particlesizeBlue maximum=9999999 redirect=None"); }
		//run("Duplicate...", " ");
	}
		if (doblue + dogreen + dored >= 2) {
		run("Images to Stack"); 
	}
}

///COLOCALIZATION CODE STARTS BELOW #######################

if(runColocCode == 1 && runExtras != 1) {
	run("Invert LUT"); //
	run("Stack to Images");
} 
if(runColocCode ==1 && runExtras ==1) {
	run("Stack to Images");
}

if(runColocCode ==1) {
//The following chunks will do colocalization for only the appropriate slices to avoid errors. 
if (haveRed==1 && haveGreen==1) {
	selectWindow(ObjectRedGreen);
	run("Duplicate...", " "); 
	rename("ObjectRG");
	selectWindow(SelectorRedGreen);
	run("Duplicate...", " "); 
	rename("SelectorRG");
	run("Binary Feature Extractor", "objects=ObjectRG selector=SelectorRG object_overlap=overlapRedGreen combine");
	rename(ObjectRedGreen + "_On_" + SelectorRedGreen);
	close("ObjectRG");
	close("SelectorRG");
}

if (haveRed==1 && haveBlue==1) {
	selectWindow(ObjectRedBlue);
	run("Duplicate...", " "); 
	rename("ObjectRB");
	selectWindow(SelectorRedBlue);
	run("Duplicate...", " "); 
	rename("SelectorRB");
	run("Binary Feature Extractor", "objects=ObjectRB selector=SelectorRB object_overlap=overlapRedBlue combine");
	rename(ObjectRedBlue + "_On_" + SelectorRedBlue);
	close("ObjectRB");
	close("SelectorRB");
}

if (haveBlue==1 && haveGreen==1) {
	selectWindow(ObjectBlueGreen);
	run("Duplicate...", " "); 
	rename("ObjectBG");
	selectWindow(SelectorBlueGreen);
	run("Duplicate...", " "); 
	rename("SelectorBG");
	run("Binary Feature Extractor", "objects=ObjectBG selector=SelectorBG object_overlap=overlapBlueGreen combine");
	rename(ObjectBlueGreen + "_On_" + SelectorBlueGreen);
	close("ObjectBG");
	close("SelectorBG");
}

if (haveTriple==1) {
	selectWindow(ObjectTriple);
	run("Duplicate...", " "); 
	rename("ObjectTriple");
	selectWindow(SelectorTriple);
	run("Duplicate...", " "); 
	rename("SelectorTriple");
	run("Binary Feature Extractor", "objects=ObjectTriple selector=SelectorTriple object_overlap=overlapTriple combine");
	rename("Triple");
	close("ObjectTriple");
	close("SelectorTriple");
}

//Close the duplicated images so the final stack is only up to 7 images 
if (haveRed==1) { close("Red-1"); }
if (haveGreen==1) { close("Green-1"); }
if (haveBlue==1) { close("Blue-1"); }

run("Images to Stack"); //Not sure why, but this does stack them all in the proper order - even after selecting them out of order 
rename('BFEcoloc_'+imageTitle); //This way the code can nicely end knowing if BFE has been run 
}