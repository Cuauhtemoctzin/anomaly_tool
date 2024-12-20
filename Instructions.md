### Instructions

This dashboard benchmarks time series in a multivariate set by assigning an anomaly score using a nearest-neighbor approach. In brief, an anomalous observation will have large distances compared with the rest of the observations, particularly with its nearest neighbors. The anomaly score is the sum of the distances of an observation with its nearest neighbors. We extended this concept to time series by exploring different distances between time series.

The left sidebar contains the different options to compute the analysis: 

- **Choose CSV File**: Click **Browse** to upload your file. Each column of the data should represent a numerical time series. Do not include an index of observations or a time variable.
- **Header**: Check the box if the first row of your data corresponds to the names of the time series. Otherwise, the column will be named, Series_1, Series_2...
- **Download**: Download the computed scores in CSV format. 
- **Colorblind palette**: Check the box to show the result using the IBM color blind palette. Otherwise, the color code will be red =high, orange, yellow, green= low based on the scores.
- **Standardization**: The **Standard deviation scaling** checkbox triggers the scale of each time series, dividing the observations by their empirical standard deviations. The **Mean subtraction** triggers to center the time series by subtracting their mean.
- **Distance**: Select the checkbox of the distances you want to include in the analysis. The distances, Cort, Wasserstein, CortNorm, Coherence, mvLWS, and Band depth, have a low computation time. Meanwhile, the PDC, CGCI, RGPDC, and PMIME have high computational time because these distances are computed based on parametric models. Despite this inconvenience, the model-based distances could provide helpful information on the dependency structure of your data.

The **Parameters** tab allows you to change each distance's parameters. The distances that do not appear on this tab do not require further parameters for its computation. 

This tool was supported by the UK Research and Innovation (UKRI) Engineering and Physical Sciences Research Council (EPSRC) under the project titled "Reducing End Use Energy Demand in Commercial Settings Through Digital Innovation," grant number EP/T025964/1.

Visit us at [https://wp.lancs.ac.uk/net0i/]{https://wp.lancs.ac.uk/net0i/}