# Amazon Elastic Load Balancing SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.3`


## Description

Elastic Load Balancing

A load balancer distributes incoming traffic across your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered instances and ensures that it routes traffic only to healthy instances. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer and a protocol and port number for connections from the load balancer to the instances.

Elastic Load Balancing supports two types of load balancers: Classic load balancers and Application load balancers (new). A Classic load balancer makes routing and load balancing decisions either at the transport layer (TCP\/SSL) or the application layer (HTTP\/HTTPS), and supports either EC2-Classic or a VPC. An Application load balancer makes routing and load balancing decisions at the application layer (HTTP\/HTTPS), supports path-based routing, and can route requests to one or more ports on each EC2 instance or container instance in your virtual private cloud (VPC). For more information, see the .

This reference covers the 2012-06-01 API, which supports Classic load balancers. The 2015-12-01 API supports Application load balancers.

To get started, create a load balancer with one or more listeners using < CreateLoadBalancer>. Register your instances with the load balancer using < RegisterInstancesWithLoadBalancer>.

All Elastic Load Balancing operations are /idempotent/, which means that they complete at most one time. If you repeat an operation, it succeeds with a 200 OK response code.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-elb)
and the [AWS API Reference](https://aws.amazon.com/documentation/).

The types from this library are intended to be used with [amazonka](http://hackage.haskell.org/package/amazonka),
which provides mechanisms for specifying AuthN/AuthZ information and sending requests.

Use of lenses is required for constructing and manipulating types.
This is due to the amount of nesting of AWS types and transparency regarding
de/serialisation into more palatable Haskell values.
The provided lenses should be compatible with any of the major lens libraries
[lens](http://hackage.haskell.org/package/lens) or [lens-family-core](http://hackage.haskell.org/package/lens-family-core).

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-elb` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
