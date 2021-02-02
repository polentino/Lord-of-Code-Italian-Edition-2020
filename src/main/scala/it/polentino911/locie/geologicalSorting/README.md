### Problem description
[See here](./instructions.html) for detailed explanation.

### Solution
Nothing really difficult here:
* you transform both `vulcanic` and `nonVulcanic` arrays into maps containing the key, and its frequency in the array;
* then you find all the shared keys, and sort them;
* and finally, build the final array by replicating every key `min` times, where `min` is the minimum occurrence of the key, from the `vulcanic` and `nonVulcanic` arrays;
