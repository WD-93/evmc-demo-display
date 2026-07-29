//The new util module for Hardhat 3
/*
  TODO:
  Raw calls
*/
//import * as fs from "fs"
const fs = await import("fs")
//Ethers import:
//import * as hardhat from "hardhat"
const hardhat = await import("hardhat")
const network = hardhat.network
//Don't do this: it creates a new in-process blockchain for each u instance,
//very confusing and not very useful. Separate init is unavoidable...
//const connection = await network.connect()
//const ethers = connection.ethers

var connection
var ethers
//Creates a new simulated blockchain
async function newConnection() {
    connection = await network.connect()
    ethers = connection.ethers
}
function getConnection() {
    return connection
}
function getEthers() {
    return ethers
}
function setConnection(conn) {
    connection = conn
    ethers = connection.ethers
}

//Reload module, bypassing the node cache.
//The suffix trick is recommended by ChatGPT; it seems really nasty but is
//supposedly the standard trick.
async function reload (str) {
    return await import(str + `?t=${Date.now()}`)
}
//Takes a module name such as "Foo.evm", and sends a create transaction using
//the bytecode it contains.
//Params is a throwaway object containing fields for sendTransaction, e.g.
//value, gas. Its data field is mutated.
//Returns the address of the deployed contract.
async function deploy(filepath,params) {
    var bytecode = fs.readFileSync(filepath,{encoding:"utf-8"})
    return await deployBytecode(bytecode,params)
}
//Deploys a contract with a given interface
async function deployContract(filepath,str_arr,params) {
    var addr = await deploy(filepath,params),
	iface = getInterface(str_arr)
    return await giveInterface(addr,iface)
}
async function deployBytecode(bytecode,params = {}) {
    var signers = await ethers.getSigners(),
	signer = signers[0]
    params.data = bytecode
    var tx = await signer.sendTransaction({data: bytecode}),
	receipt = await tx.wait()
    return receipt.contractAddress
}
//Given a Solidity contract name such as "Foo", looks up its interface.
//Given an array of method strings such as
//"function foo(uint256 x) returns (uint256)", creates an interface from them.
async function getInterface(string_or_arr) {
    if (typeof string_or_arr == "string") {
	const artifact =
	      await hardhat.artifacts.readArtifact(string_or_arr)
	return new ethers.Interface(artifact.abi)
    }
    else return new ethers.Interface(string_or_arr)
}
//iface :: string | [string] | ethers.Interface
async function giveInterface(addr,iface) {
    if(typeof iface == "string" || Array.isArray(iface))
	iface = await getInterface(iface)
    var signers = await ethers.getSigners()
    return new ethers.Contract(addr,iface,signers[0])
}
//Sends a raw call transaction from the provider address (signers[0]?)
//If .call throws an error, it's returned as {error: err}
async function call (tx) {
    try {
	return await ethers.provider.call(tx)
    }
    catch (e) {
	return {error: e}
    }
}
//Return the bytecode at a given address
async function getCode(addr) {
    return await ethers.provider.getCode(addr)
}

export {
    hardhat,network
    ,newConnection
    ,getConnection
    ,getEthers
    ,setConnection
    ,reload
    ,deploy
    ,deployBytecode
    ,deployContract
    ,getInterface
    ,giveInterface
    ,call
    ,getCode
}
