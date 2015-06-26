# Amazon Glacier SDK

> _Warning:_ This is an experimental preview release which is still under heavy development and not intended for public consumption, _caveat emptor_!

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.0.0`


## Description

Amazon Glacier is a storage solution for \"cold data.\"

Amazon Glacier is an extremely low-cost storage service that provides
secure, durable, and easy-to-use storage for data backup and archival.
With Amazon Glacier, customers can store their data cost effectively for
months, years, or decades. Amazon Glacier also enables customers to
offload the administrative burdens of operating and scaling storage to
AWS, so they don\'t have to worry about capacity planning, hardware
provisioning, data replication, hardware failure and recovery, or
time-consuming hardware migrations.

Amazon Glacier is a great storage choice when low storage cost is
paramount, your data is rarely retrieved, and retrieval latency of
several hours is acceptable. If your application requires fast or
frequent access to your data, consider using Amazon S3. For more
information, go to
<http://aws.amazon.com/s3/ Amazon Simple Storage Service (Amazon S3)>.

You can store any kind of data in any format. There is no maximum limit
on the total amount of data you can store in Amazon Glacier.

If you are a first-time user of Amazon Glacier, we recommend that you
begin by reading the following sections in the /Amazon Glacier Developer
Guide/:

-   <http://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html What is Amazon Glacier>
    - This section of the Developer Guide describes the underlying data
    model, the operations it supports, and the AWS SDKs that you can use
    to interact with the service.

-   <http://docs.aws.amazon.com/amazonglacier/latest/dev/amazon-glacier-getting-started.html Getting Started with Amazon Glacier>
    - The Getting Started section walks you through the process of
    creating a vault, uploading archives, creating jobs to download
    archives, retrieving the job output, and deleting archives.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-glacier)
and the [AWS API Reference](http://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html).


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-glacier` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
