# Amazon CodeCommit SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.2`


## Description

AWS CodeCommit

This is the /AWS CodeCommit API Reference/. This reference provides
descriptions of the operations and data types for AWS CodeCommit API.

You can use the AWS CodeCommit API to work with the following objects:

-   Repositories, by calling the following:
    -   < BatchGetRepositories>, which returns information about one or
        more repositories associated with your AWS account
    -   < CreateRepository>, which creates an AWS CodeCommit repository
    -   < DeleteRepository>, which deletes an AWS CodeCommit repository
    -   < GetRepository>, which returns information about a specified
        repository
    -   < ListRepositories>, which lists all AWS CodeCommit repositories
        associated with your AWS account
    -   < UpdateRepositoryDescription>, which sets or updates the
        description of the repository
    -   < UpdateRepositoryName>, which changes the name of the
        repository. If you change the name of a repository, no other
        users of that repository will be able to access it until you
        send them the new HTTPS or SSH URL to use.
-   Branches, by calling the following:
    -   < CreateBranch>, which creates a new branch in a specified
        repository
    -   < GetBranch>, which returns information about a specified branch
    -   < ListBranches>, which lists all branches for a specified
        repository
    -   < UpdateDefaultBranch>, which changes the default branch for a
        repository
-   Information about committed code in a repository, by calling the
    following:
    -   < GetCommit>, which returns information about a commit,
        including commit messages and committer information.
-   Triggers, by calling the following:
    -   < GetRepositoryTriggers>, which returns information about
        triggers configured for a repository
    -   < PutRepositoryTriggers>, which replaces all triggers for a
        repository and can be used to create or delete triggers
    -   < TestRepositoryTriggers>, which tests the functionality of a
        repository trigger by sending data to the trigger target

For information about how to use AWS CodeCommit, see the
<http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-codecommit)
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

`amazonka-codecommit` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
