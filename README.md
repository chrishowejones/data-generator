# data-generator

Generates a CSV file that simulates a trade data file.

## Installation

Install lein.

## Usage

To run the data generator:

    $ java -jar data-generator.jar [options]

## Options

Options:

    -f, --filename FILENAME  defaults to 'trade.csv'
    -l, --lines LINES        defaults to 10 lines in output file
    -n, --nils               nils allowed in lines
    -e, --errors             errors generated randomly in lines
    -r, --rateoferrors       percentage of error rate if -e set

## Examples

    $ java -jar data-generator.jar -f newfile.csv -l 20000

Generates a trade file called 'newfile.csv' with 20,000 lines of
output plus a header line.

### Bugs


## License

Copyright © 2015

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
