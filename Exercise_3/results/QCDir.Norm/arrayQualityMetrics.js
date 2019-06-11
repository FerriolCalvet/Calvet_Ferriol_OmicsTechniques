// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ false, false, false, true, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false ];
var arrayMetadata    = [ [ "1", "quad_ctrl_1", "quad", "control", "1", "1", "quad_ctrl_1", "quad_ctrl" ], [ "2", "quad_ctrl_2", "quad", "control", "2", "1", "quad_ctrl_2", "quad_ctrl" ], [ "3", "quad_ctrl_3", "quad", "control", "3", "1", "quad_ctrl_3", "quad_ctrl" ], [ "4", "quad_ctrl_4", "quad", "control", "4", "1", "quad_ctrl_4", "quad_ctrl" ], [ "5", "quad_ctrl_5", "quad", "control", "5", "1", "quad_ctrl_5", "quad_ctrl" ], [ "6", "quad_ctrl_6", "quad", "control", "6", "1", "quad_ctrl_6", "quad_ctrl" ], [ "7", "quad_KO_1", "quad", "cKO", "1", "2", "quad_KO_1", "quad_KO" ], [ "8", "quad_KO_2", "quad", "cKO", "2", "2", "quad_KO_2", "quad_KO" ], [ "9", "quad_KO_3", "quad", "cKO", "3", "2", "quad_KO_3", "quad_KO" ], [ "10", "quad_KO_4", "quad", "cKO", "4", "2", "quad_KO_4", "quad_KO" ], [ "11", "quad_KO_5", "quad", "cKO", "5", "2", "quad_KO_5", "quad_KO" ], [ "12", "quad_KO_6", "quad", "cKO", "6", "2", "quad_KO_6", "quad_KO" ], [ "13", "sol_ctrl_1", "soleus", "control", "1", "3", "sol_ctrl_1", "sol_ctrl" ], [ "14", "sol_ctrl_2", "soleus", "control", "2", "3", "sol_ctrl_2", "sol_ctrl" ], [ "15", "sol_ctrl_3", "soleus", "control", "3", "3", "sol_ctrl_3", "sol_ctrl" ], [ "16", "sol_ctrl_4", "soleus", "control", "4", "3", "sol_ctrl_4", "sol_ctrl" ], [ "17", "sol_ctrl_5", "soleus", "control", "5", "3", "sol_ctrl_5", "sol_ctrl" ], [ "18", "sol_ctrl_6", "soleus", "control", "6", "3", "sol_ctrl_6", "sol_ctrl" ], [ "19", "sol_KO_1", "soleus", "cKO", "1", "4", "sol_KO_1", "sol_KO" ], [ "20", "sol_KO_2", "soleus", "cKO", "2", "4", "sol_KO_2", "sol_KO" ], [ "21", "sol_KO_3", "soleus", "cKO", "3", "4", "sol_KO_3", "sol_KO" ], [ "22", "sol_KO_4", "soleus", "cKO", "4", "4", "sol_KO_4", "sol_KO" ], [ "23", "sol_KO_5", "soleus", "cKO", "5", "4", "sol_KO_5", "sol_KO" ], [ "24", "sol_KO_6", "soleus", "cKO", "6", "4", "sol_KO_6", "sol_KO" ] ];
var svgObjectNames   = [ "pca", "dens" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
    for(i=0; i<ssrules.length; i++) {
        if (ssrules[i].selectorText == (".aqm" + reportObjId)) {
		ssrules[i].style.cssText = cssText[0+status];
		break;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}
