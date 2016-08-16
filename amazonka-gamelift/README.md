# Amazon GameLift SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.3`


## Description

Amazon GameLift Service

Welcome to the /Amazon GameLift API Reference/. Amazon GameLift is a managed Amazon Web Services (AWS) service for developers who need a scalable, server-based solution for multiplayer games. Amazon GameLift provides setup and deployment of game servers, and handles infrastructure scaling and session management.

This reference describes the low-level service API for GameLift. You can call this API directly or use the <https://aws.amazon.com/tools/ AWS SDK> for your preferred language. The AWS SDK includes a set of high-level GameLift actions multiplayer game sessions. Alternatively, you can use the <https://aws.amazon.com/cli/ AWS command-line interface> (CLI) tool, which includes commands for GameLift. For administrative actions, you can also use the Amazon GameLift console.

__More Resources__

-   <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide>: Learn more about GameLift features and how to use them
-   <https://gamedev.amazon.com/forums/tutorials Lumberyard and GameLift Tutorials>: Get started fast with walkthroughs and sample projects
-   <https://aws.amazon.com/blogs/gamedev/ GameDev Blog>: Stay up to date with new features and techniques
-   <https://gamedev.amazon.com/forums/spaces/123/gamelift-discussion.html GameDev Forums>: Connect with the GameDev community

__Manage Games and Players Through GameLift__

Call these actions from your game clients and\/or services to create and manage multiplayer game sessions and player sessions.

-   __Game sessions__
    -   < CreateGameSession>
    -   < DescribeGameSessions>
    -   < DescribeGameSessionDetails>
    -   < UpdateGameSession>
    -   < SearchGameSessions>
-   __Player sessions__
    -   < CreatePlayerSession>
    -   < CreatePlayerSessions>
    -   < DescribePlayerSessions>
-   __Other actions:__
    -   < GetGameSessionLogUrl>

__Set Up and Manage Game Servers__

Use these administrative actions to configure GameLift to host your game servers. When setting up GameLift, you\'ll need to (1) configure a build for your game and upload build files, and (2) set up one or more fleets to host game sessions. Once you\'ve created and activated a fleet, you can assign aliases to it, scale capacity, track performance and utilization, etc.

-   __Manage your builds:__
    -   < ListBuilds>
    -   < CreateBuild>
    -   < DescribeBuild>
    -   < UpdateBuild>
    -   < DeleteBuild>
    -   < RequestUploadCredentials>
-   __Manage your fleets:__
    -   < ListFleets>
    -   < CreateFleet>
    -   Describe fleets:
        -   < DescribeFleetAttributes>
        -   < DescribeFleetCapacity>
        -   < DescribeFleetPortSettings>
        -   < DescribeFleetUtilization>
        -   < DescribeEC2InstanceLimits>
        -   < DescribeFleetEvents>
        -   < DescribeRuntimeConfiguration>
    -   Update fleets:
        -   < UpdateFleetAttributes>
        -   < UpdateFleetCapacity>
        -   < UpdateFleetPortSettings>
        -   < UpdateRuntimeConfiguration>
    -   < DeleteFleet>
-   __Manage fleet aliases:__
    -   < ListAliases>
    -   < CreateAlias>
    -   < DescribeAlias>
    -   < UpdateAlias>
    -   < DeleteAlias>
    -   < ResolveAlias>
-   __Manage autoscaling:__
    -   < PutScalingPolicy>
    -   < DescribeScalingPolicies>
    -   < DeleteScalingPolicy>

To view changes to the API, see the GameLift <http://docs.aws.amazon.com/gamelift/latest/developerguide/doc-history.html Document History> page.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-gamelift)
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

`amazonka-gamelift` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
