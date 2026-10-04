# Sex Offender Proximity to Elementary Schools in Washington, D.C. (2023)

An interactive geospatial dashboard built with R Shiny and Leaflet that maps registered Class B sex offenders, public elementary schools and police stations in Washington, D.C., and lets users measure how many offenders live within an adjustable radius of each school.

> CDS 301 · Scientific Information and Data Visualization · George Mason University · Fall 2024
> Team project (2 members) · R / Shiny / Leaflet

### This project was a collaborative work with [Giselle Rahimi](https://www.linkedin.com/in/giselle-rahimi-48454027b/)

**▶ Live app:** [heeon21y.shinyapps.io/CDS301_Final_Shiny_App_2](https://heeon21y.shinyapps.io/CDS301_Final_Shiny_App_2/)

<!-- Add a screenshot or GIF of the app here: ![App screenshot](images/app.png) -->

---

## Research Question

> How are registered sex offenders distributed around elementary schools in Washington, D.C., and how does that exposure relate to police station coverage and neighborhood income?

The goal is a tool that lets parents, school administrators and policymakers explore school-level exposure directly, instead of reading a static map.

## Data Sources

Official public datasets maintained by the District of Columbia Government, from the [Open Data DC](https://opendata.dc.gov) portal, plus Census income data:

| Dataset | Source | Records used |
|---|---|---|
| **Sex Offender Registry** (Class B offenders) | Open Data DC / Metropolitan Police Department | 391 offenders |
| **DC Public Schools** (elementary) | Open Data DC / DCPS | 72 schools |
| **Police Stations** | Open Data DC / MPD | 10 stations |
| **Median household income by ward** (2022) | U.S. Census Bureau, American Community Survey | 8 wards |

Offender classes follow [MPDC's offender classification](https://mpdc.dc.gov/service/offender-classifications) definitions.

## Features

**1. Interactive map with toggleable layers**
- Offenders, schools, police stations, offender-to-nearest-school lines and school buffers, each on its own layer
- School popups show name, address, enrollment and grade range

**2. Adjustable school buffers**
- Slider sets the buffer radius from **0.1 to 1.5 miles**
- Buffers are shaded by the number of offenders inside them (darker = more)

**3. School-level risk classification**
- Each school is assigned a risk level (**1 = High, 2 = Moderate, 3 = Low**) recalculated on the fly from offender counts within the selected radius

**4. Nearest-school linkage**
- Each of the 391 offenders is linked by a line to the closest elementary school

**5. Per-school histogram**
- Selecting a school shows the distribution of nearby offenders by distance

**6. Three map views**
- **Main Map:** points, lines and buffers
- **Heatmap:** offender density surface against school locations
- **Economic Choropleth Overlay:** offender density on top of 2022 median household income by ward

## Methods

- **Geocoding & spatial joins:** converted point data to `sf` objects and aligned coordinate systems across sources
- **Distance analysis:** computed offender-to-school distances and assigned each offender to its nearest school
- **Buffer analysis:** generated radius buffers around schools and counted offenders inside them
- **Reactive design:** Shiny reactives recompute buffers, counts and risk levels whenever the radius or selected school changes
- **Visualization:** Leaflet layer groups, density heatmap, ward-level choropleth and interactive histogram

## Limitations

- **Proximity is not risk.** Living near a school does not mean an offender poses a risk to that school; the "risk level" is a count-based label, not a measure of actual danger.
- **Residential addresses only.** Registry locations reflect where offenders are registered to live, not where they work or travel.
- **Single snapshot.** The data reflects one point in time (2023) and does not capture changes over time.
- **Straight-line distance.** Buffers use Euclidean distance rather than walking or street-network distance.
- **Ward-level income.** Income is aggregated by ward, which is too coarse to separate neighborhoods within a ward.

## Repository Contents

| File | Description |
|---|---|
| `app.R` | Shiny app: UI, server logic and map layers |
| `Sex_Offender_Registry.csv` | DC sex offender registry (Open Data DC) |
| `washington-dc-public-schools.csv` | DC public school locations (Open Data DC) |
| `Police_Stations.csv` | MPD police station locations (Open Data DC) |

## My Role

- **Nearest-school linkage:** computed distances from all 391 offenders to every elementary school, assigned each offender to its closest school and drew the connecting lines on the map
- **Density heatmap:** built the offender-density heatmap view that shows where offenders cluster relative to school locations

## Tech Stack

R · Shiny · Leaflet · leaflet.extras · sf · dplyr · ggplot2 / plotly · shinyapps.io
