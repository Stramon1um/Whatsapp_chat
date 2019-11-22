# Whatsapp chat
## R script to plot Whatsapp chat history

Thanks to this R script, it's possible to visualize in different kind of plots Whatsapp chat histories in few and very simple steps.

**Steps:**

   1. Export the Whatsapp chat history directly from the phone app and transfer it on the pc. If the phone is in 24h time format, switch it in 12h just for this step (a separation between *am* and *pm* is needed for running the script).

   2. Import the .txt file included into the Whatsapp archived exported in Excel, in order to prepare a .csv suitable for this R script.

   3. Once imported and separated in columns in Excel, delete all the data from the 5th column on, and insert a new row at the top as reported in the table below, getting a basic structure like this:

date  | time  | morning | sender | *To delete  ->*
--|--|--|--|--
16/07/2019  | 8:18:04  | am | Mauro | *To delete  ->*
16/07/2019  | 8:20:10  | am | Sara | *To delete  ->*
...  |...   |...   |...  |...

   4. Save the file in .csv format in order to be imported by the R script.

   5. Run the R script. The following packages are required:

```
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(plyr)
library(zoo)
library(lubridate)
```

   6. Get the final plot!

**Example**
![](example_plot\example_plot.png)<!-- -->

**Creator**

[Mauro Maver](https://github.com/stramon1um)
