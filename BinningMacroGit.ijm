/// BINNING CODE - Tim Monko 10/31/2018
// This macro is designed to take multiple 12-bit images, stack them, and draw bins in the same way that 
// Set your line width manually by doing Image > Adjust > Line Width
// I recommend 2px, that way a 1px line doesn't get subtracted out with subtractbackground/blurring

// Variables for changing the code 
numberofchannels = 3; 
usebinningmethod = 1; //If you want pre-sized static bins, then 1 
usefreehandmethod = 0; //If you want to freehand bins, then 1 (use the hotkey 'b' to add each line to the ROI overlay)

duplicatechannel = 0; //for opening another pane of a channel of interest (i.e. DAPI)
whichchannel = 1; // Select among the series of channels, numerically, that you want duplicated 

rotate90left = 0; 
multichannelcontrast = 1; // If you want to increase the contrast of a channel for binning purposes use this - it will be reset at the end of the image
contrastchannel = 1; //The channel to get adjusted - this number is ignored if adjustbrightness = 0

numberofbins = 2; //if you want rectangular bins, use this number for the amount of bins of equal size that will be drawn 
binwidth = 316; // In pixels For E16.5,10X-Mid/Cau use 316, for Ros use 158 for 10X-P8 1350, or 316 for 400um bin at E16.5 10X
binheight = 1000; //For E16.5,10X-Mid/Cau/Ros use 1000, for 10X-P8 630, / 200 um bins for PFC/Motor 
scale = 1.575; // 4X = 0.62,  10X = 1.575, this is for setting the pixel length to be the size of the 

///////////////////////////////////////
////////CODE BELOW/////////////////////
///////////////////////////////////////

file = getDirectory("Choose a Directory");
list = getFileList(file); 
listlength = lengthOf(list);
savefolder = getDirectory("Choose a Directory");

for (i = 0; i < listlength; i) {

	open(file+list[i]);
	title = getTitle();
	if (numberofchannels > 1) {
		open(file+list[i+1]);
	}
	if (numberofchannels > 2) {
		open(file+list[i+2]);
	}
		
	if (numberofchannels >1) {
		run("Images to Stack", "name=Stack title=[] use");
		run("Make Composite", "display=Composite");
	}

	if (numberofchannels == 2) { 
		Stack.setActiveChannels("10");
		run("Magenta");
		Stack.setActiveChannels("11");
	}

	if (rotate90left == 1) {
		run("Rotate 90 Degrees Left");
	}

	if (multichannelcontrast == 1) {
		Stack.setChannel(contrastchannel);
		//setMinAndMax(0,35935);
		run("Enhance Contrast", "saturated=0.35");
	}

	run("Set Scale...", "distance=scale known=1 pixel=1 unit=µm global"); 
	setForegroundColor(255, 255, 255); //to make the box outline white use (255, 255, 255) and then for black use (0, 0, 0)
	run("Line Width...", "line=2"); // Used to edit the size of the line for the bin

	if (duplicatechannel ==1 ) {
		run("Duplicate...", "duplicate channels=whichchannel");
		run("Grays");
		run("Enhance Contrast", "saturated=0.35");
	}
	
	if (usebinningmethod == 1) {
		for (bins = 0; bins < numberofbins; bins++) {
			makeRectangle(290, 0, binwidth, binheight); // (x, y, width, height)
			waitForUser("Press OK When Finished", "(1) Use 'Selection Rotator' on toolbar \n(2) Click and drag to rotate the bin \n(3) ALT+click or SHFT+click to move the bin");
			run("Add Selection...");
		}
	}

	if (usefreehandmethod == 1) {
		waitForUser("Press OK When Finished", "(1) Use the line tool or polygon tool \n(2) Distance is shown in the FIJI toolbar at the bottom \n(3) Press 'b' to add the line or polybox to the overlay \n(4) If present, close duplicated channel before 'OK' ");
	}
	i = i + numberofchannels; 
	print(i);
	
	if (multichannelcontrast == 1) {
		resetMinAndMax();
	}
	rename(title); 
	saveAs("tif", savefolder + title);
	close();
}
