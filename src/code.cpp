#include <cpp11.hpp>
#include <R.h>

namespace trmt {

// wrap position to grid
int wrap(int pos, int sz) {
  if(pos < 0) pos = pos + sz;
  if(pos >= sz) pos = pos - sz;
  return pos;
}

// move the turmite in the direction it is facing
void step(cpp11::writable::integers& position, int facing, int width, int height) {
  // move
  if(facing == 0) position[1]++;
  if(facing == 1) position[0]++;
  if(facing == 2) position[1]--;
  if(facing == 3) position[0]--;

  position[0] = trmt::wrap(position[0], width);
  position[1] = trmt::wrap(position[1], height);
}


// turmite function
cpp11::writable::integers_matrix run_turmite(int width, int height, int iter, int step_size) {

  cpp11::writable::integers pos = {width/2, height/2};
  int old_state = 0;
  int new_state = 0;
  int facing = 0;
  int color;
  double u;

  cpp11::writable::integers_matrix grid(width, height); // initially zero
  cpp11::writable::integers_matrix cols(width, height); // initially zero

  for(int t = 0; t < iter; t++) {

    color = cols(pos[0], pos[1]);
    // not sure if cpp11 has an equivalent to R::runif()
    do {u = unif_rand();} while (u <= 0 || u >= 1);

    if(u < .01) {
      color = 1 - color;
    }

    if(color == 0 & old_state == 0) {
      facing = (facing + 1) % 4;             // turn clockwise
      for(int s = 0; s < step_size; s++) {
        grid(pos[0], pos[1]) = t;
        cols(pos[0], pos[1]) = 1;
        trmt::step(pos, facing, width, height); // move forward
      }
      new_state = 0;                         // stay in state 0
    }

    if(color == 0 & old_state == 1) {
      for(int s = 0; s < step_size; s++) {
        grid(pos[0], pos[1]) = t;
        cols(pos[0], pos[1]) = 0;
        trmt::step(pos, facing, width, height); // move forward
      }
      new_state = 0;                         // swap to state 0
    }

    if(color > 0 & old_state == 0) {
      facing = (facing + 1) % 4;             // turn clockwise
      for(int s = 0; s < step_size; s++) {
        grid(pos[0], pos[1]) = t;
        cols(pos[0], pos[1]) = 1;
        trmt::step(pos, facing, width, height); // move forward
      }
      new_state = 1;                         // swap to state 1
    }

    if(color > 0 & old_state == 1) {
      for(int s = 0; s < step_size; s++) {
        grid(pos[0], pos[1]) = t;
        cols(pos[0], pos[1]) = 0;
        trmt::step(pos, facing, width, height); // move forward
      }
      new_state = 1;                         // stay in state 1
    }

    old_state = new_state;
  }

  return grid;
}

}


// turmite function to be called from R
[[cpp11::register]]
cpp11::writable::integers_matrix turmite(int width, int height, int iter, int step_size) {
  return trmt::run_turmite(width, height, iter, step_size);
}


