# Amazon Support SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

AWS Support

The AWS Support API reference is intended for programmers who need
detailed information about the AWS Support operations and data types.
This service enables you to manage your AWS Support cases
programmatically. It uses HTTP methods that return results in JSON
format.

The AWS Support service also exposes a set of
<https://aws.amazon.com/premiumsupport/trustedadvisor/ Trusted Advisor>
features. You can retrieve a list of checks and their descriptions, get
check results, specify checks to refresh, and get the refresh status of
checks.

The following list describes the AWS Support case management operations:

-   __Service names, issue categories, and available severity levels.__
    The DescribeServices and DescribeSeverityLevels operations return
    AWS service names, service codes, service categories, and problem
    severity levels. You use these values when you call the CreateCase
    operation.
-   __Case creation, case details, and case resolution.__ The
    CreateCase, DescribeCases, DescribeAttachment, and ResolveCase
    operations create AWS Support cases, retrieve information about
    cases, and resolve cases.
-   __Case communication.__ The DescribeCommunications,
    AddCommunicationToCase, and AddAttachmentsToSet operations retrieve
    and add communications and attachments to AWS Support cases.

The following list describes the operations available from the AWS
Support service for Trusted Advisor:

-   DescribeTrustedAdvisorChecks returns the list of checks that run
    against your AWS resources.
-   Using the @CheckId@ for a specific check returned by
    DescribeTrustedAdvisorChecks, you can call
    DescribeTrustedAdvisorCheckResult to obtain the results for the
    check you specified.
-   DescribeTrustedAdvisorCheckSummaries returns summarized results for
    one or more Trusted Advisor checks.
-   RefreshTrustedAdvisorCheck requests that Trusted Advisor rerun a
    specified check.
-   DescribeTrustedAdvisorCheckRefreshStatuses reports the refresh
    status of one or more checks.

For authentication of requests, AWS Support uses
<http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.

See
<http://docs.aws.amazon.com/awssupport/latest/user/Welcome.html About the AWS Support API>
in the /AWS Support User Guide/ for information about how to use this
service to create and manage your support cases, and how to call Trusted
Advisor for results of checks on your resources.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-support)
and the [AWS API Reference](http://docs.aws.amazon.com/awssupport/latest/APIReference/Welcome.html).

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

`amazonka-support` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
