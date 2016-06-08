{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon GameLift Service
--
-- Welcome to the /Amazon GameLift API Reference/. Amazon GameLift is a managed Amazon Web Services (AWS) service for developers who need a scalable, server-based solution for multiplayer games. Amazon GameLift provides setup and deployment of game servers, and handles infrastructure scaling and session management. For more information about the GameLift service, including a feature overview, getting started guide, and tutorial, see the accompanying <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide>.
--
-- This reference describes the low-level service API for GameLift. You can call this API directly or use the <https://aws.amazon.com/tools/ AWS SDK> for your preferred language. The AWS SDK includes a set of high-level GameLift actions multiplayer game sessions. Alternatively, you can use the <https://aws.amazon.com/cli/ AWS command-line interface> (CLI) tool, which includes commands for GameLift. For administrative actions, you can use the Amazon GameLift console.
--
-- __Managing Game and Player Sessions Through GameLift__
--
-- Call these actions from your game clients and\/or services to create and manage multiplayer game sessions.
--
-- -   __Game sessions__
--     -   < CreateGameSession>
--     -   < DescribeGameSessions>
--     -   < DescribeGameSessionDetails>
--     -   < UpdateGameSession>
-- -   __Player sessions__
--     -   < CreatePlayerSession>
--     -   < CreatePlayerSessions>
--     -   < DescribePlayerSessions>
-- -   __Other actions:__
--     -   < GetGameSessionLogUrl>
--
-- __Setting Up Game Servers__
--
-- Use these administrative actions to configure GameLift to host your game servers. When configuring GameLift, you\'ll need to (1) configure a build for your game and provide build files, and (2) set up one or more fleets to host game sessions.
--
-- -   __Build actions:__
--     -   < ListBuilds>
--     -   < CreateBuild>
--     -   < DescribeBuild>
--     -   < UpdateBuild>
--     -   < DeleteBuild>
--     -   < RequestUploadCredentials>
-- -   __Fleet actions:__
--     -   < ListFleets>
--     -   < CreateFleet>
--     -   Describe fleet actions:
--         -   < DescribeFleetAttributes>
--         -   < DescribeFleetCapacity>
--         -   < DescribeFleetPortSettings>
--         -   < DescribeFleetUtilization>
--         -   < DescribeEC2InstanceLimits>
--         -   < DescribeFleetEvents>
--     -   Update fleet actions:
--         -   < UpdateFleetAttributes>
--         -   < UpdateFleetCapacity>
--         -   < UpdateFleetPortSettings>
--     -   < DeleteFleet>
-- -   __Alias actions:__
--     -   < ListAliases>
--     -   < CreateAlias>
--     -   < DescribeAlias>
--     -   < UpdateAlias>
--     -   < DeleteAlias>
--     -   < ResolveAlias>
-- -   __Scaling policy actions:__
--     -   < PutScalingPolicy>
--     -   < DescribeScalingPolicies>
--     -   < DeleteScalingPolicy>
module Network.AWS.GameLift
    (
    -- * Service Configuration
      gameLift

    -- * Errors
    -- $errors

    -- ** InvalidFleetStatusException
    , _InvalidFleetStatusException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** ConflictException
    , _ConflictException

    -- ** TerminalRoutingStrategyException
    , _TerminalRoutingStrategyException

    -- ** NotFoundException
    , _NotFoundException

    -- ** GameSessionFullException
    , _GameSessionFullException

    -- ** InvalidGameSessionStatusException
    , _InvalidGameSessionStatusException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** FleetCapacityExceededException
    , _FleetCapacityExceededException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateGameSession
    , module Network.AWS.GameLift.CreateGameSession

    -- ** DeleteScalingPolicy
    , module Network.AWS.GameLift.DeleteScalingPolicy

    -- ** PutScalingPolicy
    , module Network.AWS.GameLift.PutScalingPolicy

    -- ** ListBuilds
    , module Network.AWS.GameLift.ListBuilds

    -- ** DeleteFleet
    , module Network.AWS.GameLift.DeleteFleet

    -- ** CreateBuild
    , module Network.AWS.GameLift.CreateBuild

    -- ** RequestUploadCredentials
    , module Network.AWS.GameLift.RequestUploadCredentials

    -- ** CreateAlias
    , module Network.AWS.GameLift.CreateAlias

    -- ** ResolveAlias
    , module Network.AWS.GameLift.ResolveAlias

    -- ** ListAliases
    , module Network.AWS.GameLift.ListAliases

    -- ** DescribeScalingPolicies
    , module Network.AWS.GameLift.DescribeScalingPolicies

    -- ** DescribeGameSessions
    , module Network.AWS.GameLift.DescribeGameSessions

    -- ** DescribeFleetUtilization
    , module Network.AWS.GameLift.DescribeFleetUtilization

    -- ** GetGameSessionLogURL
    , module Network.AWS.GameLift.GetGameSessionLogURL

    -- ** DescribeFleetAttributes
    , module Network.AWS.GameLift.DescribeFleetAttributes

    -- ** DescribeFleetEvents
    , module Network.AWS.GameLift.DescribeFleetEvents

    -- ** DescribeFleetCapacity
    , module Network.AWS.GameLift.DescribeFleetCapacity

    -- ** DeleteBuild
    , module Network.AWS.GameLift.DeleteBuild

    -- ** UpdateBuild
    , module Network.AWS.GameLift.UpdateBuild

    -- ** ListFleets
    , module Network.AWS.GameLift.ListFleets

    -- ** DeleteAlias
    , module Network.AWS.GameLift.DeleteAlias

    -- ** UpdateAlias
    , module Network.AWS.GameLift.UpdateAlias

    -- ** DescribeGameSessionDetails
    , module Network.AWS.GameLift.DescribeGameSessionDetails

    -- ** DescribeFleetPortSettings
    , module Network.AWS.GameLift.DescribeFleetPortSettings

    -- ** CreatePlayerSessions
    , module Network.AWS.GameLift.CreatePlayerSessions

    -- ** CreateFleet
    , module Network.AWS.GameLift.CreateFleet

    -- ** UpdateFleetAttributes
    , module Network.AWS.GameLift.UpdateFleetAttributes

    -- ** DescribePlayerSessions
    , module Network.AWS.GameLift.DescribePlayerSessions

    -- ** DescribeBuild
    , module Network.AWS.GameLift.DescribeBuild

    -- ** UpdateFleetPortSettings
    , module Network.AWS.GameLift.UpdateFleetPortSettings

    -- ** UpdateFleetCapacity
    , module Network.AWS.GameLift.UpdateFleetCapacity

    -- ** DescribeAlias
    , module Network.AWS.GameLift.DescribeAlias

    -- ** DescribeEC2InstanceLimits
    , module Network.AWS.GameLift.DescribeEC2InstanceLimits

    -- ** UpdateGameSession
    , module Network.AWS.GameLift.UpdateGameSession

    -- ** CreatePlayerSession
    , module Network.AWS.GameLift.CreatePlayerSession

    -- * Types

    -- ** BuildStatus
    , BuildStatus (..)

    -- ** ComparisonOperatorType
    , ComparisonOperatorType (..)

    -- ** EC2InstanceType
    , EC2InstanceType (..)

    -- ** EventCode
    , EventCode (..)

    -- ** FleetStatus
    , FleetStatus (..)

    -- ** GameSessionStatus
    , GameSessionStatus (..)

    -- ** IPProtocol
    , IPProtocol (..)

    -- ** MetricName
    , MetricName (..)

    -- ** PlayerSessionCreationPolicy
    , PlayerSessionCreationPolicy (..)

    -- ** PlayerSessionStatus
    , PlayerSessionStatus (..)

    -- ** ProtectionPolicy
    , ProtectionPolicy (..)

    -- ** RoutingStrategyType
    , RoutingStrategyType (..)

    -- ** ScalingAdjustmentType
    , ScalingAdjustmentType (..)

    -- ** ScalingStatusType
    , ScalingStatusType (..)

    -- ** AWSCredentials
    , AWSCredentials
    , awsCredentials
    , acSecretAccessKey
    , acSessionToken
    , acAccessKeyId

    -- ** Alias
    , Alias
    , alias
    , aCreationTime
    , aLastUpdatedTime
    , aAliasId
    , aRoutingStrategy
    , aName
    , aDescription

    -- ** Build
    , Build
    , build
    , bCreationTime
    , bStatus
    , bBuildId
    , bName
    , bVersion
    , bSizeOnDisk

    -- ** EC2InstanceCounts
    , EC2InstanceCounts
    , ec2InstanceCounts
    , eicIdLE
    , eicTERMINATING
    , eicPENDING
    , eicMAXIMUM
    , eicDESIRED
    , eicMINIMUM
    , eicACTIVE

    -- ** EC2InstanceLimit
    , EC2InstanceLimit
    , ec2InstanceLimit
    , eilEC2InstanceType
    , eilCurrentInstances
    , eilInstanceLimit

    -- ** Event
    , Event
    , event
    , eResourceId
    , eEventTime
    , eMessage
    , eEventCode
    , eEventId

    -- ** FleetAttributes
    , FleetAttributes
    , fleetAttributes
    , faCreationTime
    , faStatus
    , faServerLaunchParameters
    , faLogPaths
    , faBuildId
    , faTerminationTime
    , faNewGameSessionProtectionPolicy
    , faName
    , faServerLaunchPath
    , faFleetId
    , faDescription

    -- ** FleetCapacity
    , FleetCapacity
    , fleetCapacity
    , fcInstanceType
    , fcFleetId
    , fcInstanceCounts

    -- ** FleetUtilization
    , FleetUtilization
    , fleetUtilization
    , fuActiveGameSessionCount
    , fuMaximumPlayerSessionCount
    , fuCurrentPlayerSessionCount
    , fuFleetId

    -- ** GameProperty
    , GameProperty
    , gameProperty
    , gpKey
    , gpValue

    -- ** GameSession
    , GameSession
    , gameSession
    , gsCreationTime
    , gsStatus
    , gsGameProperties
    , gsIPAddress
    , gsGameSessionId
    , gsMaximumPlayerSessionCount
    , gsTerminationTime
    , gsPlayerSessionCreationPolicy
    , gsName
    , gsCurrentPlayerSessionCount
    , gsFleetId

    -- ** GameSessionDetail
    , GameSessionDetail
    , gameSessionDetail
    , gsdGameSession
    , gsdProtectionPolicy

    -- ** IPPermission
    , IPPermission
    , ipPermission
    , ipFromPort
    , ipToPort
    , ipIPRange
    , ipProtocol

    -- ** PlayerSession
    , PlayerSession
    , playerSession
    , psCreationTime
    , psStatus
    , psIPAddress
    , psGameSessionId
    , psTerminationTime
    , psPlayerSessionId
    , psFleetId
    , psPlayerId

    -- ** RoutingStrategy
    , RoutingStrategy
    , routingStrategy
    , rsType
    , rsMessage
    , rsFleetId

    -- ** S3Location
    , S3Location
    , s3Location
    , slBucket
    , slKey
    , slRoleARN

    -- ** ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , spStatus
    , spScalingAdjustmentType
    , spEvaluationPeriods
    , spMetricName
    , spComparisonOperator
    , spName
    , spThreshold
    , spScalingAdjustment
    , spFleetId
    ) where

import           Network.AWS.GameLift.CreateAlias
import           Network.AWS.GameLift.CreateBuild
import           Network.AWS.GameLift.CreateFleet
import           Network.AWS.GameLift.CreateGameSession
import           Network.AWS.GameLift.CreatePlayerSession
import           Network.AWS.GameLift.CreatePlayerSessions
import           Network.AWS.GameLift.DeleteAlias
import           Network.AWS.GameLift.DeleteBuild
import           Network.AWS.GameLift.DeleteFleet
import           Network.AWS.GameLift.DeleteScalingPolicy
import           Network.AWS.GameLift.DescribeAlias
import           Network.AWS.GameLift.DescribeBuild
import           Network.AWS.GameLift.DescribeEC2InstanceLimits
import           Network.AWS.GameLift.DescribeFleetAttributes
import           Network.AWS.GameLift.DescribeFleetCapacity
import           Network.AWS.GameLift.DescribeFleetEvents
import           Network.AWS.GameLift.DescribeFleetPortSettings
import           Network.AWS.GameLift.DescribeFleetUtilization
import           Network.AWS.GameLift.DescribeGameSessionDetails
import           Network.AWS.GameLift.DescribeGameSessions
import           Network.AWS.GameLift.DescribePlayerSessions
import           Network.AWS.GameLift.DescribeScalingPolicies
import           Network.AWS.GameLift.GetGameSessionLogURL
import           Network.AWS.GameLift.ListAliases
import           Network.AWS.GameLift.ListBuilds
import           Network.AWS.GameLift.ListFleets
import           Network.AWS.GameLift.PutScalingPolicy
import           Network.AWS.GameLift.RequestUploadCredentials
import           Network.AWS.GameLift.ResolveAlias
import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.UpdateAlias
import           Network.AWS.GameLift.UpdateBuild
import           Network.AWS.GameLift.UpdateFleetAttributes
import           Network.AWS.GameLift.UpdateFleetCapacity
import           Network.AWS.GameLift.UpdateFleetPortSettings
import           Network.AWS.GameLift.UpdateGameSession
import           Network.AWS.GameLift.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'GameLift'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
