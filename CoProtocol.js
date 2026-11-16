//The interface for CoProtocol.evmc; handles saving and resending the execution
//state.

var u

//Pass u.js (which contains the simulated blockchain connection)
function init(u_) {u = u_}

//Create a new CoProtocol instance and return its interface object.
async function newCoProtocol() {
    //Valid path for evmc-hardhat repo
    var addr = await u.deploy("CoProtocol.evm"),
	//The initial exec state is ignored, but the protocol message must
	//still be at the right offset. execState is 745 bytes.
	execState = "0x" + [...Array(745).keys()]
	.map(_ => "00").reduce((a,b)=>a+b)
    return {propose, contest, timeout, getArray //Methods
	    , addr, execState                   //State
	   }
}

//Interface methods
//undefined = await iface.propose(number)
async function propose(prime) {
    await callWith(this,u.integer(2,prime))
}
//undefined = await iface.contest(number)
async function contest(factor) {
    await callWith(this,"01" + u.integer(2,factor))
}
//undefined = await iface.timeout()
async function timeout() {
    await callWith(this,"")
}
//A helper for the shared logic
//Catches and throws on failing calls; u.call returns an {error: _} on
//failure which would corrupt execState if written to it.
//Race condition: in propose, the timestamp becomes part of the execution
//state. If the call and send are executed in different 60-second epochs, es and
//storage[0] will disagree.
//That could be solved in a real system by tracing the EVM.
async function callWith(iface,hex) {
    //Nasty trick: send a trivial transaction to advance the timestamp
    //call sees to within a second of send's.
    var ethers = u.getEthers(),
	signer = (await ethers.getSigners())[0]
    await u.send({from: signer, to: signer})
    
    var tx = {to: iface.addr,
	      data: iface.execState + hex
	     }
    var es = await u.call(tx)
    if (typeof es == "string") {
	iface.execState = es
	await u.send(tx)
    }
    else throw es.error
    //Invariant: the hash in storage[0] matches es.
    var expected = u.getEthers().keccak256(es),
	actual = await u.getStorageAt(iface.addr,"0x00")
    if (actual != expected)
	console.log("Mismatch: \nExpected: " + expected +"\nActual: " + actual)
}

//numbers = await iface.getArray()
//Since this method makes an async call for each element, it'll get slow as
//the array grows.
async function getArray() {
    var arr = []
    var ix = 1
    while (true) {
	var n = Number(await u.getStorageAt(this.addr,u.integer(4,ix++)))
	if (n) arr.push(n)
	else break
    }
    return arr
}

export {init,newCoProtocol}
