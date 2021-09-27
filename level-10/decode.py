def split_equal(number) -> list:
    last_idx = 0
    numbers = []
    for i in range(len(number)):
        try:
            if number[i] != number[i+1]:
                numbers.append(number[last_idx:i+1])
                last_idx = i+1
        except IndexError:
            numbers.append(number[last_idx:])
            break

    return numbers

a = ["1"]
n = 30

while n > 0:
    seq = a[-1]
    next = ""
    for d in split_equal(seq):
        next += str(len(d)) + d[0]
    a.append(next)
    n -= 1

# print(a)
print(f"len(a[30])={len(a[30])}")
