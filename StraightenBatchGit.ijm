//// TIM MONKO 05/15/2018 
//// STRAIGHTEN MACRO for batch processing a bunch of files 

setBatchMode(false)

verticalbin = 0; // if 1 then macro uses vertical bins (ie bottom = VZ, top = pia) for calculation of line width, if 0 then assumes attempt at horizontal bin 

title = getTitle;
width = getWidth;
height = getHeight;

setTool("polyline");

waitForUser("Draw A Bin", "Vertical Bins --> Draw Left to Right \n Horizontal Bins --> Draw Top to Bottom ");

if (verticalbin == 1) {
	run("Straighten...", "title=title line=height process");
} else {
	run("Straighten...", "title=title line=width process");
}

run("8-bit");
run("Canvas Size...", "width=1360 height=1036 position=Center");
