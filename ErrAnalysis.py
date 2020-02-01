'''
Improvements to make:
- Instead of passing in a million variables, pass in a dict or a list
- Currently, some lines are being dropped now that it's been seperating
into different functions 
- If possible, find a way to add a list esque element to dict to store
the problem lines

Issues:
- Values are staying at zero, nothing is changing
- Count isn't iterating. Am I looping nested on accident?
'''
import re


file = 'avgErrPercentageEdited.txt'
values = {'avg_error': 0, 'count': 0, 'highest': 0, 'highest_line': 0, 'problem_lines': 0}


def substring_after(s, delim):
    return s.partition(delim)[2]


# Check for single highest error percentage, store it and the line it occurs on
def check_highest(num):
    # If current highest error percentage 
    print('check highest', values['highest'])
    print('num: ', num)

    if (values['highest'] < num):
        values['highest'] = num
        values['highest_line'] = values['count']


# Pull the error percentage from each line, check run check_highest() and 
def extract_error(line):
    if line.find("ep="):
        # Pull numbers following 'ep=', then set 'num' to that number, stripped of any non-numeric values in the substring
        num = int(re.sub("[^0-9]", "", substring_after(line, "ep=")))
        # One problem line somewhere without this
        if(num != ''):
            check_highest(num)
            # Assuming absolute value is the correct thing to do
            values['avg_error'] += abs(float(num))
            print('avg_error: ', values['avg_error'])
    elif line.find("EPGG="):
        # Strip any non-numeric values off of the substring
        re.sub("[^0-9]", "", num)
        num = substring_after(line, "EPGG=")
        check_highest(num)
        # Assuming absolute value is the correct thing to do
        avg_error += abs(float(num))
        print(values['avg_error'])
    values['count'] += 1


# Average error rate for sequences whose lowest term is 1
def by_lowest(filename):
    with open(filename) as fp:
        for line in fp:
            if '1' in line:
                extract_error(line)
                # Reset shared variables to zero)
                print(values)
            else:
                continue


    print("\nTest #1 - Lowest Error Percentage when lowest degree in sequence is 1:\n")
    print_results(avg_error, count, highest, highest_line, problem_lines)
    

def print_results(avg_error, count, highest, highest_line, problem_lines):
    print("====================================================================\n")
    print("Total lines: ", count)
    print("Average Error Percentage: ", avg_error/count)
    print("Highest single error rate: ", highest, " on line: ", highest_line)

        

# Average error rate by largest term in sequence

# Average error rate by total sum of degrees

# Highest single error percentage

# Total average error percentage

# Compare with total number of realizations

# Plot data



# Main
by_lowest(file)