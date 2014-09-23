/**
 * y-combinator via javascript
 */

function Y(le){
	return function(f){
		return f(f);
	}(function(f){
		return le(function(x){
			return f(f)(x);
		})
	})
};

// the base function receive the destination function and the remain param ,
// applying which to Y will result the destionation function  
var len = function(length){
	return function(x){
		return x == "" ? 0 : 1 + length(x.replace(/^./,""));
	}
}

console.log(Y(len)("abcdefg"));


