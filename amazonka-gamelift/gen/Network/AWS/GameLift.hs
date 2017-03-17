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
-- __Amazon GameLift Service__
--
-- Amazon GameLift is a managed service for developers who need a scalable, dedicated server solution for their multiplayer games. Amazon GameLift provides tools to acquire computing resources and deploy game servers, scale game server capacity to meed player demand, and track in-depth metrics on player usage and server performance.
--
-- The Amazon GameLift service API includes important functionality to:
--
--     * Find game sessions and match players to games – Retrieve information on available game sessions; create new game sessions; send player requests to join a game session.
--
--     * Configure and manage game server resources – Manage builds, fleets, queues, and aliases; set autoscaling policies; retrieve logs and metrics.
--
--
--
-- This reference guide describes the low-level service API for Amazon GameLift. We recommend using either the Amazon Web Services software development kit (<http://aws.amazon.com/tools/#sdk AWS SDK> ), available in multiple languages, or the <http://aws.amazon.com/cli/ AWS command-line interface> (CLI) tool. Both of these align with the low-level service API. In addition, you can use the <https://console.aws.amazon.com/gamelift/home AWS Management Console> for Amazon GameLift for many administrative actions.
--
-- __MORE RESOURCES__
--
--     * <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide> – Learn more about Amazon GameLift features and how to use them.
--
--     * <https://gamedev.amazon.com/forums/tutorials Lumberyard and Amazon GameLift Tutorials> – Get started fast with walkthroughs and sample projects.
--
--     * <http://aws.amazon.com/blogs/gamedev/ GameDev Blog> – Stay up to date with new features and techniques.
--
--     * <https://gamedev.amazon.com/forums/spaces/123/gamelift-discussion.html GameDev Forums> – Connect with the GameDev community.
--
--     * <http://docs.aws.amazon.com/gamelift/latest/developerguide/doc-history.html Amazon GameLift Document History> – See changes to the Amazon GameLift service, SDKs, and documentation, as well as links to release notes.
--
--
--
-- __API SUMMARY__
--
-- This list offers a functional overview of the Amazon GameLift service API.
--
-- __Finding Games and Joining Players__
--
-- You can enable players to connect to game servers on Amazon GameLift from a game client or through a game service (such as a matchmaking service). You can use these operations to discover actively running game or start new games. You can also match players to games, either singly or as a group.
--
--     * __Discover existing game sessions__
--
--     * 'SearchGameSessions' – Get all available game sessions or search for game sessions that match a set of criteria.
--
--
--
--     * __Start a new game session__
--
--     * Game session placement – Use a queue to process new game session requests and create game sessions on fleets designated for the queue.
--
--     * 'StartGameSessionPlacement' – Request a new game session placement and add one or more players to it.
--
--     * 'DescribeGameSessionPlacement' – Get details on a placement request, including status.
--
--     * 'StopGameSessionPlacement' – Cancel a placement request.
--
--
--
--     * 'CreateGameSession' – Start a new game session on a specific fleet.
--
--
--
--     * __Manage game session objects__
--
--     * 'DescribeGameSessionDetails' – Retrieve metadata and protection policies associated with one or more game sessions, including length of time active and current player count.
--
--     * 'UpdateGameSession' – Change game session settings, such as maximum player count and join policy.
--
--     * 'GetGameSessionLogUrl' – Get the location of saved logs for a game session.
--
--
--
--     * __Manage player sessions objects__
--
--     * 'CreatePlayerSession' – Send a request for a player to join a game session.
--
--     * 'CreatePlayerSessions' – Send a request for multiple players to join a game session.
--
--     * 'DescribePlayerSessions' – Get details on player activity, including status, playing time, and player data.
--
--
--
--
--
-- __Setting Up and Managing Game Servers__
--
-- When setting up Amazon GameLift, first create a game build and upload the files to Amazon GameLift. Then use these operations to set up a fleet of resources to run your game servers. Manage games to scale capacity, adjust configuration settings, access raw utilization data, and more.
--
--     * __Manage game builds__
--
--     * 'CreateBuild' – Create a new build by uploading files stored in an Amazon S3 bucket. (To create a build stored at a local file location, use the AWS CLI command @upload-build@ .)
--
--     * 'ListBuilds' – Get a list of all builds uploaded to a Amazon GameLift region.
--
--     * 'DescribeBuild' – Retrieve information associated with a build.
--
--     * 'UpdateBuild' – Change build metadata, including build name and version.
--
--     * 'DeleteBuild' – Remove a build from Amazon GameLift.
--
--
--
--     * __Manage fleets__
--
--     * 'CreateFleet' – Configure and activate a new fleet to run a build's game servers.
--
--     * 'DeleteFleet' – Terminate a fleet that is no longer running game servers or hosting players.
--
--     * View / update fleet configurations.
--
--     * 'ListFleets' – Get a list of all fleet IDs in a Amazon GameLift region (all statuses).
--
--     * 'DescribeFleetAttributes' / 'UpdateFleetAttributes' – View or change a fleet's metadata and settings for game session protection and resource creation limits.
--
--     * 'DescribeFleetPortSettings' / 'UpdateFleetPortSettings' – View or change the inbound permissions (IP address and port setting ranges) allowed for a fleet.
--
--     * 'DescribeRuntimeConfiguration' / 'UpdateRuntimeConfiguration' – View or change what server processes (and how many) to run on each instance in a fleet.
--
--     * 'DescribeInstances' – Get information on each instance in a fleet, including instance ID, IP address, and status.
--
--
--
--
--
--     * __Control fleet capacity__
--
--     * 'DescribeEC2InstanceLimits' – Retrieve maximum number of instances allowed for the current AWS account and the current usage level.
--
--     * 'DescribeFleetCapacity' / 'UpdateFleetCapacity' – Retrieve the capacity settings and the current number of instances in a fleet; adjust fleet capacity settings to scale up or down.
--
--     * Autoscale – Manage autoscaling rules and apply them to a fleet.
--
--     * 'PutScalingPolicy' – Create a new autoscaling policy, or update an existing one.
--
--     * 'DescribeScalingPolicies' – Retrieve an existing autoscaling policy.
--
--     * 'DeleteScalingPolicy' – Delete an autoscaling policy and stop it from affecting a fleet's capacity.
--
--
--
--
--
--     * __Access fleet activity statistics__
--
--     * 'DescribeFleetUtilization' – Get current data on the number of server processes, game sessions, and players currently active on a fleet.
--
--     * 'DescribeFleetEvents' – Get a fleet's logged events for a specified time span.
--
--     * 'DescribeGameSessions' – Retrieve metadata associated with one or more game sessions, including length of time active and current player count.
--
--
--
--     * __Remotely access an instance__
--
--     * 'GetInstanceAccess' – Request access credentials needed to remotely connect to a specified instance on a fleet.
--
--
--
--     * __Manage fleet aliases__
--
--     * 'CreateAlias' – Define a new alias and optionally assign it to a fleet.
--
--     * 'ListAliases' – Get all fleet aliases defined in a Amazon GameLift region.
--
--     * 'DescribeAlias' – Retrieve information on an existing alias.
--
--     * 'UpdateAlias' – Change settings for a alias, such as redirecting it from one fleet to another.
--
--     * 'DeleteAlias' – Remove an alias from the region.
--
--     * 'ResolveAlias' – Get the fleet ID that a specified alias points to.
--
--
--
--     * __Manage game session queues__
--
--     * 'CreateGameSessionQueue' – Create a queue for processing requests for new game sessions.
--
--     * 'DescribeGameSessionQueues' – Get data on all game session queues defined in a Amazon GameLift region.
--
--     * 'UpdateGameSessionQueue' – Change the configuration of a game session queue.
--
--     * 'DeleteGameSessionQueue' – Remove a game session queue from the region.
--
--
--
--
--
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

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

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

    -- ** UpdateRuntimeConfiguration
    , module Network.AWS.GameLift.UpdateRuntimeConfiguration

    -- ** CreateGameSessionQueue
    , module Network.AWS.GameLift.CreateGameSessionQueue

    -- ** SearchGameSessions
    , module Network.AWS.GameLift.SearchGameSessions

    -- ** UpdateGameSessionQueue
    , module Network.AWS.GameLift.UpdateGameSessionQueue

    -- ** DeleteGameSessionQueue
    , module Network.AWS.GameLift.DeleteGameSessionQueue

    -- ** GetInstanceAccess
    , module Network.AWS.GameLift.GetInstanceAccess

    -- ** DescribeScalingPolicies
    , module Network.AWS.GameLift.DescribeScalingPolicies

    -- ** DescribeGameSessions
    , module Network.AWS.GameLift.DescribeGameSessions

    -- ** StartGameSessionPlacement
    , module Network.AWS.GameLift.StartGameSessionPlacement

    -- ** DescribeFleetUtilization
    , module Network.AWS.GameLift.DescribeFleetUtilization

    -- ** DescribeRuntimeConfiguration
    , module Network.AWS.GameLift.DescribeRuntimeConfiguration

    -- ** GetGameSessionLogURL
    , module Network.AWS.GameLift.GetGameSessionLogURL

    -- ** DescribeFleetAttributes
    , module Network.AWS.GameLift.DescribeFleetAttributes

    -- ** DescribeGameSessionPlacement
    , module Network.AWS.GameLift.DescribeGameSessionPlacement

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

    -- ** DescribeInstances
    , module Network.AWS.GameLift.DescribeInstances

    -- ** DescribeGameSessionDetails
    , module Network.AWS.GameLift.DescribeGameSessionDetails

    -- ** DescribeFleetPortSettings
    , module Network.AWS.GameLift.DescribeFleetPortSettings

    -- ** DescribeGameSessionQueues
    , module Network.AWS.GameLift.DescribeGameSessionQueues

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

    -- ** StopGameSessionPlacement
    , module Network.AWS.GameLift.StopGameSessionPlacement

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

    -- ** GameSessionPlacementState
    , GameSessionPlacementState (..)

    -- ** GameSessionStatus
    , GameSessionStatus (..)

    -- ** IPProtocol
    , IPProtocol (..)

    -- ** InstanceStatus
    , InstanceStatus (..)

    -- ** MetricName
    , MetricName (..)

    -- ** OperatingSystem
    , OperatingSystem (..)

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
    , aAliasARN
    , aDescription

    -- ** Build
    , Build
    , build
    , bCreationTime
    , bStatus
    , bOperatingSystem
    , bBuildId
    , bName
    , bVersion
    , bSizeOnDisk

    -- ** DesiredPlayerSession
    , DesiredPlayerSession
    , desiredPlayerSession
    , dpsPlayerData
    , dpsPlayerId

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
    , faOperatingSystem
    , faBuildId
    , faFleetARN
    , faTerminationTime
    , faNewGameSessionProtectionPolicy
    , faName
    , faServerLaunchPath
    , faFleetId
    , faDescription
    , faResourceCreationLimitPolicy

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
    , fuActiveServerProcessCount

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
    , gsCreatorId
    , gsPort

    -- ** GameSessionDetail
    , GameSessionDetail
    , gameSessionDetail
    , gsdGameSession
    , gsdProtectionPolicy

    -- ** GameSessionPlacement
    , GameSessionPlacement
    , gameSessionPlacement
    , gspStatus
    , gspPlacementId
    , gspGameProperties
    , gspGameSessionName
    , gspStartTime
    , gspGameSessionRegion
    , gspMaximumPlayerSessionCount
    , gspEndTime
    , gspGameSessionARN
    , gspPlayerLatencies
    , gspGameSessionQueueName

    -- ** GameSessionQueue
    , GameSessionQueue
    , gameSessionQueue
    , gsqTimeoutInSeconds
    , gsqDestinations
    , gsqName

    -- ** GameSessionQueueDestination
    , GameSessionQueueDestination
    , gameSessionQueueDestination
    , gsqdDestinationARN

    -- ** IPPermission
    , IPPermission
    , ipPermission
    , ipFromPort
    , ipToPort
    , ipIPRange
    , ipProtocol

    -- ** Instance
    , Instance
    , instance'
    , iCreationTime
    , iInstanceId
    , iStatus
    , iIPAddress
    , iOperatingSystem
    , iType
    , iFleetId

    -- ** InstanceAccess
    , InstanceAccess
    , instanceAccess
    , iaInstanceId
    , iaIPAddress
    , iaOperatingSystem
    , iaCredentials
    , iaFleetId

    -- ** InstanceCredentials
    , InstanceCredentials
    , instanceCredentials
    , icUserName
    , icSecret

    -- ** PlayerLatency
    , PlayerLatency
    , playerLatency
    , plLatencyInMilliseconds
    , plRegionIdentifier
    , plPlayerId

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
    , psPlayerData
    , psPlayerId
    , psPort

    -- ** ResourceCreationLimitPolicy
    , ResourceCreationLimitPolicy
    , resourceCreationLimitPolicy
    , rclpNewGameSessionsPerCreator
    , rclpPolicyPeriodInMinutes

    -- ** RoutingStrategy
    , RoutingStrategy
    , routingStrategy
    , rsType
    , rsMessage
    , rsFleetId

    -- ** RuntimeConfiguration
    , RuntimeConfiguration
    , runtimeConfiguration
    , rcServerProcesses

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

    -- ** ServerProcess
    , ServerProcess
    , serverProcess
    , spParameters
    , spLaunchPath
    , spConcurrentExecutions
    ) where

import           Network.AWS.GameLift.CreateAlias
import           Network.AWS.GameLift.CreateBuild
import           Network.AWS.GameLift.CreateFleet
import           Network.AWS.GameLift.CreateGameSession
import           Network.AWS.GameLift.CreateGameSessionQueue
import           Network.AWS.GameLift.CreatePlayerSession
import           Network.AWS.GameLift.CreatePlayerSessions
import           Network.AWS.GameLift.DeleteAlias
import           Network.AWS.GameLift.DeleteBuild
import           Network.AWS.GameLift.DeleteFleet
import           Network.AWS.GameLift.DeleteGameSessionQueue
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
import           Network.AWS.GameLift.DescribeGameSessionPlacement
import           Network.AWS.GameLift.DescribeGameSessionQueues
import           Network.AWS.GameLift.DescribeGameSessions
import           Network.AWS.GameLift.DescribeInstances
import           Network.AWS.GameLift.DescribePlayerSessions
import           Network.AWS.GameLift.DescribeRuntimeConfiguration
import           Network.AWS.GameLift.DescribeScalingPolicies
import           Network.AWS.GameLift.GetGameSessionLogURL
import           Network.AWS.GameLift.GetInstanceAccess
import           Network.AWS.GameLift.ListAliases
import           Network.AWS.GameLift.ListBuilds
import           Network.AWS.GameLift.ListFleets
import           Network.AWS.GameLift.PutScalingPolicy
import           Network.AWS.GameLift.RequestUploadCredentials
import           Network.AWS.GameLift.ResolveAlias
import           Network.AWS.GameLift.SearchGameSessions
import           Network.AWS.GameLift.StartGameSessionPlacement
import           Network.AWS.GameLift.StopGameSessionPlacement
import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.UpdateAlias
import           Network.AWS.GameLift.UpdateBuild
import           Network.AWS.GameLift.UpdateFleetAttributes
import           Network.AWS.GameLift.UpdateFleetCapacity
import           Network.AWS.GameLift.UpdateFleetPortSettings
import           Network.AWS.GameLift.UpdateGameSession
import           Network.AWS.GameLift.UpdateGameSessionQueue
import           Network.AWS.GameLift.UpdateRuntimeConfiguration
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
