# Amazon Relational Database Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.3`


## Description

Amazon Relational Database Service

Amazon Relational Database Service (Amazon RDS) is a web service that makes it easier to set up, operate, and scale a relational database in the cloud. It provides cost-efficient, resizeable capacity for an industry-standard relational database and manages common database administration tasks, freeing up developers to focus on what makes their applications and businesses unique.

Amazon RDS gives you access to the capabilities of a MySQL, MariaDB, PostgreSQL, Microsoft SQL Server, Oracle, or Amazon Aurora database server. These capabilities mean that the code, applications, and tools you already use today with your existing databases work with Amazon RDS without modification. Amazon RDS automatically backs up your database and maintains the database software that powers your DB instance. Amazon RDS is flexible: you can scale your database instance\'s compute resources and storage capacity to meet your application\'s demand. As with all Amazon Web Services, there are no up-front investments, and you pay only for the resources you use.

This interface reference for Amazon RDS contains documentation for a programming or command line interface you can use to manage Amazon RDS. Note that Amazon RDS is asynchronous, which means that some interfaces might require techniques such as polling or callback functions to determine when a command has been applied. In this reference, the parameter descriptions indicate whether a command is applied immediately, on the next instance reboot, or during the maintenance window. The reference structure is as follows, and we list following some related topics from the user guide.

__Amazon RDS API Reference__

-   For the alphabetical list of API actions, see <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_Operations.html API Actions>.

-   For the alphabetical list of data types, see <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_Types.html Data Types>.

-   For a list of common query parameters, see <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/CommonParameters.html Common Parameters>.

-   For descriptions of the error codes, see <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/CommonErrors.html Common Errors>.

__Amazon RDS User Guide__

-   For a summary of the Amazon RDS interfaces, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Welcome.html#Welcome.Interfaces Available RDS Interfaces>.

-   For more information about how to use the Query API, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Using_the_Query_API.html Using the Query API>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-rds)
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

`amazonka-rds` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
