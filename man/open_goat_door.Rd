% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Host Opens a Door with a Goat.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{The}{parameters included in the function are 'game' and 'a.pick'.This ensures
that the function runs 'this' particular game plus the contestant's first
choice door.}
}
\value{
The return will open a goat door.
}
\description{
The host will always open a door with a goat behind it and cannot open a door
that the contestant chose.Therefore, if the contestant chose a door with a
goat, the host will open the second goat door. However, if the contestant
chose the door with the car, then the host would have two 'goat' doors to
choose from.
}
\details{
The statements listed inside the compound statement { }, ensure that the
host has either one or two doors to open. It also ensures that the host does
not open a 'car' door.
}
