# Amazon IoT Data Plane SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.1`


## Description

AWS IoT

AWS IoT-Data enables secure, bi-directional communication between
Internet-connected things (such as sensors, actuators, embedded devices,
or smart appliances) and the AWS cloud. It implements a broker for
applications and things to publish messages over HTTP (Publish) and
retrieve, update, and delete thing shadows. A thing shadow is a
persistent representation of your things and their state in the AWS
cloud.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-iot-dataplane)
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

`amazonka-iot-dataplane` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
