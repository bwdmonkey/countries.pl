'''
Script made to reformat data from 2016 UN World Happiness Report
Parses a .csv file to export a .pl file
'''
import csv
import re


def main():
    with open('raw/world-happiness-report-2016.csv') as csvfile, \
         open('data/happiness_data.pl', 'w') as plfile:
        reader = csv.reader(csvfile)
        next(reader) # Skip header
        for row in reader:
            plfile.write(formatProlog(row)+'\n')


def snakeCase(str):
    """Convert any string into snake_case without spaces
    """
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', str)
    s2 = re.sub('\\s+', r'_', s1)
    s3 = re.sub('-', r'', s2)
    s4 = re.sub('([a-z0-9])([A-Z])', r'\1_\2', s3).lower()
    return re.sub('__', r'_', s4)


def formatProlog(row):
    """Formats a CSV row into a Prolog assertion.
    """
    # Format: country(name,region,happiness_rank,happiness_score,gdp_per_capita,family,life_expectancy,freedom,
    #                 government_corruption,generosity,dystopia_residual)
    snakeRow = [snakeCase(x) for x in row]
    return 'country({}).'.format(", ".join(snakeRow))


main()
