import numpy as np
from PIL import Image


def extract_info_layer() -> np.array:
    img = Image.open("oxygen.png")
    arr = np.asarray(img)
    info_layer = arr[45, 0:-21, 0]

    return info_layer


def decode_grayscale(array: np.array) -> np.array:
    chars = []
    for idx in range(0, array.size, 7):
        chars.append(array[idx])
    return chars


def encode_as_string(chars):
    return "".join(chr(c) for c in chars)


def main():
    info_layer = extract_info_layer()
    print(info_layer)
    chars = decode_grayscale(info_layer)
    print(encode_as_string(chars))
    base_cipher = np.array([105, 110, 116, 101, 103, 114, 105, 116, 121])
    print(encode_as_string(base_cipher))


if __name__ == '__main__':
    main()
