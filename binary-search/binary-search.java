public static int binarySearch(int[] nums, int check, int lo, int hi) {
    if (hi < lo) {
        return -1;
    }
    int guess = (hi + lo) / 2;
    if (nums[guess] > check) {
        return binarySearch(nums, check, lo, guess - 1);
    } else if (nums[guess] < check) {
        return binarySearch(nums, check, guess + 1, hi);
    }
    return guess;
}
