//Kernel source
#pragma OPENCL EXTENSION cl_khr_fp64: enable
#pragma OPENCL EXTENSION cl_khr_int64_base_atomics: enable
__kernel void totient(__global long* g_results, const int wgs) {   

  size_t i = get_global_id(0);
  size_t x, y, t;

  //Reverse Triangle number formula to find x in the sequence
  // [0, 0, 1, 0, 1, 2, 0, 1, 2, 3 ... n] given index i
  double op = (8*(double)i)+1;
  x = floor( (sqrt(op)+1)/2 )+1;
  
  //As above to find y
  y = x-(i-((x-1)*((x-1)+1)/2));
  
  //hcf function
  while (y != 0) {
    t = x % y;
    x = y;
    y = t;
  }
  
  //increment an element in the results array
  if(x == 1) atom_inc(&g_results[i % wgs]);
}
