Readme.txt

Date:  12-08-2003

This package contains the following items

1) Statewide 1:100k Routed Hydrography for California in Shapefile Format (cdfg_100k_2003_6.shp in the "Data" Folder)
2) Metadata for Hydrography (metadata.htm in the "Documentation" Folder)
3) Data Disclaimer for Hydrography (Disclaimer.htm in the "Documentation" Folder)
5) Edit request form (in PDF and Word format) for making a request to edit the hydrography (in "Documentation" folder)
6) Explanatory document including history, methods, procedures and tips for editing the hydrography 
	(Development_and_Distribution.doc in "Documentation" folder)
7) Microsoft access database which includes informational tables detailing changes since the version 2 
	(cdfg_100k_2003_2) release of Calhydro. (Hydro_edits.mdb in "Documentation" folder)
8) RouteTools Extension and documentation (in "RouteTools Extension" Folder)

Please be sure and read the metadata and disclaimer before using the hydrography dataset. 

Please utilize the Edit Request forms to document and submit any problems and/or suggested fixes.


Synopsis:

Cdfg_100k_2003_6 is a 1:100,000 scale stream based routed hydrography shapefile and ArcInfo coverage covering the
state of California developed by California Department of Fish & Game (CDFG) and Pacific States Marine Fisheries
Commission (PSMFC). The shapefile and coverage utilizes existing National Hydrography Database (NHD)
1:100,000 linework and attributes as well as Geographic Names Information System (GNIS) identifiers
as guidelines to determine the stream networks that were combined into routes. Utilizing both automatic
routines developed by CDFG/PSMFC staff as well as manual (on screen) networking techniques, a series of
line segments from the NHD were networked into a stream first by the existence of a GNIS ID. If no GNIS ID
was present for a series of segments, the NHD "Level" attribute was used to determine the network.
The resulting routes represent what are commonly referred to as streams under the aforementioned guidelines.
Streams are identified with the unique LLID identifier. Although derived from Latitude/Longitude coordinates,
the LLID should be used as a stream identifier only and not as a spatial identifier.

IMPORTANT NOTE: In the previous version (Version 2003.5, 10-29-2003), we reprojected the hydrography data from 
Teale Albers NAD27 Datum to Teale Albers NAD83 datum.  We peformed this datum conversion to be in compliance 
with other State and Federal agencies who are mandated to collect and distribute GIS data using the NAD83 Datum.  
The metadata files and prj files associated with the data reflect this datum shift. Note that this datum shift 
resulted in a slight change in stream lengths and their associated measures.This change in stream length is less 
than 1/1000% on average. Although this length change may seem neglegible, the stream length discrepancy may effect 
tabular analysis of your data vs. the hydrography. It is then suggested that if reasonably possible, to adjust the 
lengths of your hydrography-related data when associating it with the newly reprojected hydrography.

Note: In the current version (Version 2003.6, 12-08-2003), we corrected several flow issues and repaired a slight 
registration problem. If you are using Version 2003.5, you are encouraged to download and begin using the newer 
version as soon as possible.  Please contact NCNCR-ISB for more information.

Constraints:

CDFG/PSMFC decided on the GNIS/NHD Level as described above as the core methodology for the creation
of the stream networks in this hydrography. As a rule every effort was made not to stray from this methodology
in the interest of consistency and integrity. Even so, known issues remain with NHD linework that is not
included in this product as well as streams and stream segments added to this product which are not part of
the NHD linework (see metadata for more information).

Over 98% of the NHD line segments not included in this product were assigned a level value of -9998 by NHD,
meaning in effect no assigned value. The NHD level value is an assigned numerical value indicating
a stream order hierarchy. A level of -9998 indicates that NHD could not determine a level value and since the
routing methodology relied heavily on the level value determined by NHD, a -9998 level value greatly limited
the ability to determine networks accurately. This linework was not included in the product due to what would
have resulted in arbitrary network assignment. Also included in the unrouted linework are complex canal/pipeline networks
which were determined to be unroutable without further information (i.e. ground truthing). The remaining less than
2% of NHD linework not included represent streams where a valid network could not be determined by the data technician
by the time of release of this product.


Editing/Manipulating the Hydrography

A series of custom ArcView 3x scripts and ArcINFO amls were developed specifically to create and edit the hydrography.  If you
would like a copy of these tools, please feel free to contact either Tom Christy (tchristy@dfg.ca.gov) or Eric Haney (ehaney@dfg.ca.gov)
to request the hydrography editing tools package.


Corrections:

CDFG/PSMFC hopes to retain the resources to continue to develop this product which includes addressing the
known issues described above. In the meantime, we hope that you take the time to alert us to any edits or
corrections to the dataset by using the Edit Request form made available to you on this site. Also, for your
own purposes you are welcomed to make linework edits to your own copy of the hydrography utilizing the routing
tools also made available here. This may be preferable to you for the short term as edits to our base hydrography
may not be made for some time due to resource limitations and development of an edit review process.

Please email or mail your completed Edit Request form to the address below. 
Remember, positive feedback is always welcomed!

Tom Christy
PSMFC GIS/Data Technician
CDFG NCNCR-Information Services Branch
CA Dept. of Fish and Game
2440 Athens Ave.
Redding, CA  96001

email to: tchristy@dfg.ca.gov


For more information contact:

Eric M. Haney
Information Services Branch Manager
Northern California North Coast Region
CA Dept. of Fish and Game
2440 Athens Ave.
Redding, CA  96001
(530) 225-2052 
ehaney@dfg.ca.gov