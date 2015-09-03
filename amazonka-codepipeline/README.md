# Amazon CodePipeline SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.2.0.2`


## Description

AWS CodePipeline __Overview__

This is the AWS CodePipeline API Reference. This guide provides
descriptions of the actions and data types for AWS CodePipeline. Some
functionality for your pipeline is only configurable through the API.
For additional information, see the
<http://docs.aws.amazon.com/pipelines/latest/userguide/welcome.html AWS CodePipeline User Guide>.

You can use the AWS CodePipeline API to work with pipelines, stages,
actions, gates, and transitions, as described below.

/Pipelines/ are models of automated release processes. Each pipeline is
uniquely named, and consists of actions, gates, and stages.

You can work with pipelines by calling:

-   CreatePipeline, which creates a uniquely-named pipeline.
-   DeletePipeline, which deletes the specified pipeline.
-   GetPipeline, which returns information about a pipeline structure.
-   GetPipelineState, which returns information about the current state
    of the stages and actions of a pipeline.
-   ListPipelines, which gets a summary of all of the pipelines
    associated with your account.
-   StartPipelineExecution, which runs the the most recent revision of
    an artifact through the pipeline.
-   UpdatePipeline, which updates a pipeline with edits or changes to
    the structure of the pipeline.

Pipelines include /stages/, which are which are logical groupings of
gates and actions. Each stage contains one or more actions that must
complete before the next stage begins. A stage will result in success or
failure. If a stage fails, then the pipeline stops at that stage and
will remain stopped until either a new version of an artifact appears in
the source location, or a user takes action to re-run the most recent
artifact through the pipeline. You can call GetPipelineState, which
displays the status of a pipeline, including the status of stages in the
pipeline, or GetPipeline, which returns the entire structure of the
pipeline, including the stages of that pipeline. For more information
about the structure of stages and actions, also refer to the AWS
CodePipeline Pipeline Structure Reference.

Pipeline stages include /actions/, which are categorized into categories
such as source or build actions performed within a stage of a pipeline.
For example, you can use a source action to import artifacts into a
pipeline from a source such as Amazon S3. Like stages, you do not work
with actions directly in most cases, but you do define and interact with
actions when working with pipeline operations such as CreatePipeline and
GetPipelineState.

Pipelines also include /transitions/, which allow the transition of
artifacts from one stage to the next in a pipeline after the actions in
one stage complete.

You can work with transitions by calling:

-   DisableStageTransition, which prevents artifacts from transitioning
    to the next stage in a pipeline.
-   EnableStageTransition, which enables transition of artifacts between
    stages in a pipeline.

__Using the API to integrate with AWS CodePipeline__

For third-party integrators or developers who want to create their own
integrations with AWS CodePipeline, the expected sequence varies from
the standard API user. In order to integrate with AWS CodePipeline,
developers will need to work with the following items:

-   Jobs, which are instances of an action. For example, a job for a
    source action might import a revision of an artifact from a source.

    You can work with jobs by calling:

    -   AcknowledgeJob, which confirms whether a job worker has received
        the specified job,
    -   GetJobDetails, which returns the details of a job,
    -   PollForJobs, which determines whether there are any jobs to act
        upon,
    -   PutJobFailureResult, which provides details of a job failure,
        and
    -   PutJobSuccessResult, which provides details of a job success.
-   Third party jobs, which are instances of an action created by a
    partner action and integrated into AWS CodePipeline. Partner actions
    are created by members of the AWS Partner Network.

    You can work with third party jobs by calling:

    -   AcknowledgeThirdPartyJob, which confirms whether a job worker
        has received the specified job,
    -   GetThirdPartyJobDetails, which requests the details of a job for
        a partner action,
    -   PollForThirdPartyJobs, which determines whether there are any
        jobs to act upon,
    -   PutThirdPartyJobFailureResult, which provides details of a job
        failure, and
    -   PutThirdPartyJobSuccessResult, which provides details of a job
        success.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-codepipeline)
and the [AWS API Reference](http://docs.aws.amazon.com/codepipeline/latest/APIReference/Welcome.html).

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

`amazonka-codepipeline` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
