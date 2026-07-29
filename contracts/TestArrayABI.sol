// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.28;

contract TestArrayABI {

  function idStatic(uint[4] memory arr) public pure
   returns (uint[4] memory) {
    return arr;
  }

  function idDynamic(uint[] memory arr) public pure
   returns (uint[] memory) {
    return arr;
  }
 function idNested(uint[][] memory arr) public pure
   returns (uint[][] memory) {
    return arr;
  }

 function sumOfSums(uint[][] memory arr) public pure
   returns (uint) {
   uint accum = 0;
   for(uint i = 0; i < arr.length; i++) {
    uint[] memory subarr = arr[i];
    for(uint j = 0; j < subarr.length; j++) {
     accum += subarr[j];
    }
   }
  return accum;
  }
}

