print('hello')

file_name = 'graduate_earnings.txt'
out_file = 'graduate_earnings.csv'
data_in = {}

with open(file_name, 'r') as input_file:
    with open(out_file, 'w') as output_file:
        for lines in input_file:
            # print()
            output_file.write(",".join(lines.split("\t")))