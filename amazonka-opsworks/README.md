# Amazon OpsWorks SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

AWS OpsWorks

Welcome to the /AWS OpsWorks API Reference/. This guide provides
descriptions, syntax, and usage examples about AWS OpsWorks actions and
data types, including common parameters and error codes.

AWS OpsWorks is an application management service that provides an
integrated experience for overseeing the complete application lifecycle.
For information about this product, go to the
<http://aws.amazon.com/opsworks/ AWS OpsWorks> details page.

__SDKs and CLI__

The most common way to use the AWS OpsWorks API is by using the AWS
Command Line Interface (CLI) or by using one of the AWS SDKs to
implement applications in your preferred language. For more information,
see:

-   <http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html AWS CLI>
-   <http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html AWS SDK for Java>
-   <http://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm AWS SDK for .NET>
-   <http://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html AWS SDK for PHP 2>
-   <http://docs.aws.amazon.com/AWSRubySDK/latest/AWS/OpsWorks/Client.html AWS SDK for Ruby>
-   <http://aws.amazon.com/documentation/sdkforjavascript/ AWS SDK for Node.js>
-   <http://docs.pythonboto.org/en/latest/ref/opsworks.html AWS SDK for Python(Boto)>

__Endpoints__

AWS OpsWorks supports only one endpoint,
opsworks.us-east-1.amazonaws.com (HTTPS), so you must connect to that
endpoint. You can then use the API to direct AWS OpsWorks to create
stacks in any AWS Region.

__Chef Versions__

When you call CreateStack, CloneStack, or UpdateStack we recommend you
use the @ConfigurationManager@ parameter to specify the Chef version.
The recommended value for Linux stacks, which is also the default value,
is currently 11.10. Windows stacks use Chef 12.2. For more information,
see
<http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html Chef Versions>.

You can also specify Chef 11.4 or Chef 0.9 for your Linux stack.
However, Chef 0.9 has been deprecated. We do not recommend using Chef
0.9 for new stacks, and we recommend migrating your existing Chef 0.9
stacks to Chef 11.10 as soon as possible.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-opsworks)
and the [AWS API Reference](http://docs.aws.amazon.com/opsworks/latest/APIReference/Welcome.html).

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

`amazonka-opsworks` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
