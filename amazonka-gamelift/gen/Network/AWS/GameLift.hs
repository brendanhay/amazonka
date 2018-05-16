{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon GameLift Service__
--
-- Amazon GameLift is a managed service for developers who need a scalable, dedicated server solution for their multiplayer games. Use Amazon GameLift for these tasks: (1) set up computing resources and deploy your game servers, (2) run game sessions and get players into games, (3) automatically scale your resources to meet player demand and manage costs, and (4) track in-depth metrics on game server performance and player usage.
--
-- The Amazon GameLift service API includes two important function sets:
--
--     * __Manage game sessions and player access__ -- Retrieve information on available game sessions; create new game sessions; send player requests to join a game session.
--
--     * __Configure and manage game server resources__ -- Manage builds, fleets, queues, and aliases; set auto-scaling policies; retrieve logs and metrics.
--
--
--
-- This reference guide describes the low-level service API for Amazon GameLift. You can use the API functionality with these tools:
--
--     * The Amazon Web Services software development kit (<http://aws.amazon.com/tools/#sdk AWS SDK> ) is available in <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-supported.html#gamelift-supported-clients multiple languages> including C++ and C#. Use the SDK to access the API programmatically from an application, such as a game client.
--
--     * The <http://aws.amazon.com/cli/ AWS command-line interface> (CLI) tool is primarily useful for handling administrative actions, such as setting up and managing Amazon GameLift settings and resources. You can use the AWS CLI to manage all of your AWS services.
--
--     * The <https://console.aws.amazon.com/gamelift/home AWS Management Console> for Amazon GameLift provides a web interface to manage your Amazon GameLift settings and resources. The console includes a dashboard for tracking key resources, including builds and fleets, and displays usage and performance metrics for your games as customizable graphs.
--
--     * Amazon GameLift Local is a tool for testing your game's integration with Amazon GameLift before deploying it on the service. This tools supports a subset of key API actions, which can be called from either the AWS CLI or programmatically. See <http://docs.aws.amazon.com/gamelift/latest/developerguide/integration-testing-local.html Testing an Integration> .
--
--
--
-- __Learn more__
--
--     * <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Developer Guide> -- Read about Amazon GameLift features and how to use them.
--
--     * <https://gamedev.amazon.com/forums/tutorials Tutorials> -- Get started fast with walkthroughs and sample projects.
--
--     * <http://aws.amazon.com/blogs/gamedev/ GameDev Blog> -- Stay up to date with new features and techniques.
--
--     * <https://gamedev.amazon.com/forums/spaces/123/gamelift-discussion.html GameDev Forums> -- Connect with the GameDev community.
--
--     * <http://aws.amazon.com/releasenotes/Amazon-GameLift/ Release notes> and <http://docs.aws.amazon.com/gamelift/latest/developerguide/doc-history.html document history> -- Stay current with updates to the Amazon GameLift service, SDKs, and documentation.
--
--
--
-- __API SUMMARY__
--
-- This list offers a functional overview of the Amazon GameLift service API.
--
-- __Managing Games and Players__
--
-- Use these actions to start new game sessions, find existing game sessions, track game session status and other information, and enable player access to game sessions.
--
--     * __Discover existing game sessions__
--
--     * 'SearchGameSessions' -- Retrieve all available game sessions or search for game sessions that match a set of criteria.
--
--
--
--     * __Start new game sessions__
--
--     * Start new games with Queues to find the best available hosting resources across multiple regions, minimize player latency, and balance game session activity for efficiency and cost effectiveness.
--
--     * 'StartGameSessionPlacement' -- Request a new game session placement and add one or more players to it.
--
--     * 'DescribeGameSessionPlacement' -- Get details on a placement request, including status.
--
--     * 'StopGameSessionPlacement' -- Cancel a placement request.
--
--
--
--     * 'CreateGameSession' -- Start a new game session on a specific fleet. /Available in Amazon GameLift Local./
--
--
--
--     * __Match players to game sessions with FlexMatch matchmaking__
--
--     * 'StartMatchmaking' -- Request matchmaking for one players or a group who want to play together.
--
--     * 'StartMatchBackfill' - Request additional player matches to fill empty slots in an existing game session.
--
--     * 'DescribeMatchmaking' -- Get details on a matchmaking request, including status.
--
--     * 'AcceptMatch' -- Register that a player accepts a proposed match, for matches that require player acceptance.
--
--     * 'StopMatchmaking' -- Cancel a matchmaking request.
--
--
--
--     * __Manage game session data__
--
--     * 'DescribeGameSessions' -- Retrieve metadata for one or more game sessions, including length of time active and current player count. /Available in Amazon GameLift Local./
--
--     * 'DescribeGameSessionDetails' -- Retrieve metadata and the game session protection setting for one or more game sessions.
--
--     * 'UpdateGameSession' -- Change game session settings, such as maximum player count and join policy.
--
--     * 'GetGameSessionLogUrl' -- Get the location of saved logs for a game session.
--
--
--
--     * __Manage player sessions__
--
--     * 'CreatePlayerSession' -- Send a request for a player to join a game session. /Available in Amazon GameLift Local./
--
--     * 'CreatePlayerSessions' -- Send a request for multiple players to join a game session. /Available in Amazon GameLift Local./
--
--     * 'DescribePlayerSessions' -- Get details on player activity, including status, playing time, and player data. /Available in Amazon GameLift Local./
--
--
--
--
--
-- __Setting Up and Managing Game Servers__
--
-- When setting up Amazon GameLift resources for your game, you first <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html create a game build> and upload it to Amazon GameLift. You can then use these actions to configure and manage a fleet of resources to run your game servers, scale capacity to meet player demand, access performance and utilization metrics, and more.
--
--     * __Manage game builds__
--
--     * 'CreateBuild' -- Create a new build using files stored in an Amazon S3 bucket. To create a build and upload files from a local path, use the AWS CLI command @upload-build@ .
--
--     * 'ListBuilds' -- Get a list of all builds uploaded to a Amazon GameLift region.
--
--     * 'DescribeBuild' -- Retrieve information associated with a build.
--
--     * 'UpdateBuild' -- Change build metadata, including build name and version.
--
--     * 'DeleteBuild' -- Remove a build from Amazon GameLift.
--
--
--
--     * __Manage fleets__
--
--     * 'CreateFleet' -- Configure and activate a new fleet to run a build's game servers.
--
--     * 'ListFleets' -- Get a list of all fleet IDs in a Amazon GameLift region (all statuses).
--
--     * 'DeleteFleet' -- Terminate a fleet that is no longer running game servers or hosting players.
--
--     * View / update fleet configurations.
--
--     * 'DescribeFleetAttributes' / 'UpdateFleetAttributes' -- View or change a fleet's metadata and settings for game session protection and resource creation limits.
--
--     * 'DescribeFleetPortSettings' / 'UpdateFleetPortSettings' -- View or change the inbound permissions (IP address and port setting ranges) allowed for a fleet.
--
--     * 'DescribeRuntimeConfiguration' / 'UpdateRuntimeConfiguration' -- View or change what server processes (and how many) to run on each instance in a fleet.
--
--
--
--
--
--     * __Control fleet capacity__
--
--     * 'DescribeEC2InstanceLimits' -- Retrieve maximum number of instances allowed for the current AWS account and the current usage level.
--
--     * 'DescribeFleetCapacity' / 'UpdateFleetCapacity' -- Retrieve the capacity settings and the current number of instances in a fleet; adjust fleet capacity settings to scale up or down.
--
--     * Autoscale -- Manage auto-scaling rules and apply them to a fleet.
--
--     * 'PutScalingPolicy' -- Create a new auto-scaling policy, or update an existing one.
--
--     * 'DescribeScalingPolicies' -- Retrieve an existing auto-scaling policy.
--
--     * 'DeleteScalingPolicy' -- Delete an auto-scaling policy and stop it from affecting a fleet's capacity.
--
--     * 'StartFleetActions' -- Restart a fleet's auto-scaling policies.
--
--     * 'StopFleetActions' -- Suspend a fleet's auto-scaling policies.
--
--
--
--
--
--     * __Manage VPC peering connections for fleets__
--
--     * 'CreateVpcPeeringAuthorization' -- Authorize a peering connection to one of your VPCs.
--
--     * 'DescribeVpcPeeringAuthorizations' -- Retrieve valid peering connection authorizations.
--
--     * 'DeleteVpcPeeringAuthorization' -- Delete a peering connection authorization.
--
--     * 'CreateVpcPeeringConnection' -- Establish a peering connection between the VPC for a Amazon GameLift fleet and one of your VPCs.
--
--     * 'DescribeVpcPeeringConnections' -- Retrieve information on active or pending VPC peering connections with a Amazon GameLift fleet.
--
--     * 'DeleteVpcPeeringConnection' -- Delete a VPC peering connection with a Amazon GameLift fleet.
--
--
--
--     * __Access fleet activity statistics__
--
--     * 'DescribeFleetUtilization' -- Get current data on the number of server processes, game sessions, and players currently active on a fleet.
--
--     * 'DescribeFleetEvents' -- Get a fleet's logged events for a specified time span.
--
--     * 'DescribeGameSessions' -- Retrieve metadata associated with one or more game sessions, including length of time active and current player count.
--
--
--
--     * __Remotely access an instance__
--
--     * 'DescribeInstances' -- Get information on each instance in a fleet, including instance ID, IP address, and status.
--
--     * 'GetInstanceAccess' -- Request access credentials needed to remotely connect to a specified instance in a fleet.
--
--
--
--     * __Manage fleet aliases__
--
--     * 'CreateAlias' -- Define a new alias and optionally assign it to a fleet.
--
--     * 'ListAliases' -- Get all fleet aliases defined in a Amazon GameLift region.
--
--     * 'DescribeAlias' -- Retrieve information on an existing alias.
--
--     * 'UpdateAlias' -- Change settings for a alias, such as redirecting it from one fleet to another.
--
--     * 'DeleteAlias' -- Remove an alias from the region.
--
--     * 'ResolveAlias' -- Get the fleet ID that a specified alias points to.
--
--
--
--     * __Manage game session queues__
--
--     * 'CreateGameSessionQueue' -- Create a queue for processing requests for new game sessions.
--
--     * 'DescribeGameSessionQueues' -- Retrieve game session queues defined in a Amazon GameLift region.
--
--     * 'UpdateGameSessionQueue' -- Change the configuration of a game session queue.
--
--     * 'DeleteGameSessionQueue' -- Remove a game session queue from the region.
--
--
--
--     * __Manage FlexMatch resources__
--
--     * 'CreateMatchmakingConfiguration' -- Create a matchmaking configuration with instructions for building a player group and placing in a new game session.
--
--     * 'DescribeMatchmakingConfigurations' -- Retrieve matchmaking configurations defined a Amazon GameLift region.
--
--     * 'UpdateMatchmakingConfiguration' -- Change settings for matchmaking configuration. queue.
--
--     * 'DeleteMatchmakingConfiguration' -- Remove a matchmaking configuration from the region.
--
--     * 'CreateMatchmakingRuleSet' -- Create a set of rules to use when searching for player matches.
--
--     * 'DescribeMatchmakingRuleSets' -- Retrieve matchmaking rule sets defined in a Amazon GameLift region.
--
--     * 'ValidateMatchmakingRuleSet' -- Verify syntax for a set of matchmaking rules.
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

    -- ** UnsupportedRegionException
    , _UnsupportedRegionException

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

    -- ** StopMatchmaking
    , module Network.AWS.GameLift.StopMatchmaking

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

    -- ** CreateVPCPeeringConnection
    , module Network.AWS.GameLift.CreateVPCPeeringConnection

    -- ** CreateGameSessionQueue
    , module Network.AWS.GameLift.CreateGameSessionQueue

    -- ** SearchGameSessions
    , module Network.AWS.GameLift.SearchGameSessions

    -- ** CreateVPCPeeringAuthorization
    , module Network.AWS.GameLift.CreateVPCPeeringAuthorization

    -- ** UpdateGameSessionQueue
    , module Network.AWS.GameLift.UpdateGameSessionQueue

    -- ** DeleteGameSessionQueue
    , module Network.AWS.GameLift.DeleteGameSessionQueue

    -- ** DeleteVPCPeeringConnection
    , module Network.AWS.GameLift.DeleteVPCPeeringConnection

    -- ** StartFleetActions
    , module Network.AWS.GameLift.StartFleetActions

    -- ** GetInstanceAccess
    , module Network.AWS.GameLift.GetInstanceAccess

    -- ** DescribeScalingPolicies
    , module Network.AWS.GameLift.DescribeScalingPolicies

    -- ** DescribeMatchmakingRuleSets
    , module Network.AWS.GameLift.DescribeMatchmakingRuleSets

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

    -- ** StartMatchmaking
    , module Network.AWS.GameLift.StartMatchmaking

    -- ** CreateMatchmakingRuleSet
    , module Network.AWS.GameLift.CreateMatchmakingRuleSet

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

    -- ** StartMatchBackfill
    , module Network.AWS.GameLift.StartMatchBackfill

    -- ** DescribeInstances
    , module Network.AWS.GameLift.DescribeInstances

    -- ** DescribeGameSessionDetails
    , module Network.AWS.GameLift.DescribeGameSessionDetails

    -- ** DescribeFleetPortSettings
    , module Network.AWS.GameLift.DescribeFleetPortSettings

    -- ** DescribeGameSessionQueues
    , module Network.AWS.GameLift.DescribeGameSessionQueues

    -- ** DescribeVPCPeeringConnections
    , module Network.AWS.GameLift.DescribeVPCPeeringConnections

    -- ** CreatePlayerSessions
    , module Network.AWS.GameLift.CreatePlayerSessions

    -- ** DescribeMatchmakingConfigurations
    , module Network.AWS.GameLift.DescribeMatchmakingConfigurations

    -- ** DescribeVPCPeeringAuthorizations
    , module Network.AWS.GameLift.DescribeVPCPeeringAuthorizations

    -- ** CreateFleet
    , module Network.AWS.GameLift.CreateFleet

    -- ** DeleteMatchmakingConfiguration
    , module Network.AWS.GameLift.DeleteMatchmakingConfiguration

    -- ** UpdateMatchmakingConfiguration
    , module Network.AWS.GameLift.UpdateMatchmakingConfiguration

    -- ** DeleteVPCPeeringAuthorization
    , module Network.AWS.GameLift.DeleteVPCPeeringAuthorization

    -- ** UpdateFleetAttributes
    , module Network.AWS.GameLift.UpdateFleetAttributes

    -- ** CreateMatchmakingConfiguration
    , module Network.AWS.GameLift.CreateMatchmakingConfiguration

    -- ** DescribePlayerSessions
    , module Network.AWS.GameLift.DescribePlayerSessions

    -- ** StopFleetActions
    , module Network.AWS.GameLift.StopFleetActions

    -- ** DescribeBuild
    , module Network.AWS.GameLift.DescribeBuild

    -- ** UpdateFleetPortSettings
    , module Network.AWS.GameLift.UpdateFleetPortSettings

    -- ** UpdateFleetCapacity
    , module Network.AWS.GameLift.UpdateFleetCapacity

    -- ** AcceptMatch
    , module Network.AWS.GameLift.AcceptMatch

    -- ** DescribeAlias
    , module Network.AWS.GameLift.DescribeAlias

    -- ** ValidateMatchmakingRuleSet
    , module Network.AWS.GameLift.ValidateMatchmakingRuleSet

    -- ** DescribeEC2InstanceLimits
    , module Network.AWS.GameLift.DescribeEC2InstanceLimits

    -- ** StopGameSessionPlacement
    , module Network.AWS.GameLift.StopGameSessionPlacement

    -- ** UpdateGameSession
    , module Network.AWS.GameLift.UpdateGameSession

    -- ** DescribeMatchmaking
    , module Network.AWS.GameLift.DescribeMatchmaking

    -- ** CreatePlayerSession
    , module Network.AWS.GameLift.CreatePlayerSession

    -- * Types

    -- ** AcceptanceType
    , AcceptanceType (..)

    -- ** BuildStatus
    , BuildStatus (..)

    -- ** ComparisonOperatorType
    , ComparisonOperatorType (..)

    -- ** EC2InstanceType
    , EC2InstanceType (..)

    -- ** EventCode
    , EventCode (..)

    -- ** FleetAction
    , FleetAction (..)

    -- ** FleetStatus
    , FleetStatus (..)

    -- ** FleetType
    , FleetType (..)

    -- ** GameSessionPlacementState
    , GameSessionPlacementState (..)

    -- ** GameSessionStatus
    , GameSessionStatus (..)

    -- ** GameSessionStatusReason
    , GameSessionStatusReason (..)

    -- ** IPProtocol
    , IPProtocol (..)

    -- ** InstanceStatus
    , InstanceStatus (..)

    -- ** MatchmakingConfigurationStatus
    , MatchmakingConfigurationStatus (..)

    -- ** MetricName
    , MetricName (..)

    -- ** OperatingSystem
    , OperatingSystem (..)

    -- ** PlayerSessionCreationPolicy
    , PlayerSessionCreationPolicy (..)

    -- ** PlayerSessionStatus
    , PlayerSessionStatus (..)

    -- ** PolicyType
    , PolicyType (..)

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

    -- ** AttributeValue
    , AttributeValue
    , attributeValue
    , avSL
    , avSDM
    , avN
    , avS

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
    , ePreSignedLogURL
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
    , faFleetType
    , faTerminationTime
    , faInstanceType
    , faStoppedActions
    , faNewGameSessionProtectionPolicy
    , faName
    , faServerLaunchPath
    , faMetricGroups
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
    , gsMatchmakerData
    , gsMaximumPlayerSessionCount
    , gsTerminationTime
    , gsPlayerSessionCreationPolicy
    , gsName
    , gsCurrentPlayerSessionCount
    , gsStatusReason
    , gsGameSessionData
    , gsFleetId
    , gsCreatorId
    , gsPort

    -- ** GameSessionConnectionInfo
    , GameSessionConnectionInfo
    , gameSessionConnectionInfo
    , gsciMatchedPlayerSessions
    , gsciIPAddress
    , gsciGameSessionARN
    , gsciPort

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
    , gspIPAddress
    , gspGameSessionName
    , gspStartTime
    , gspGameSessionId
    , gspGameSessionRegion
    , gspMatchmakerData
    , gspMaximumPlayerSessionCount
    , gspEndTime
    , gspGameSessionARN
    , gspPlayerLatencies
    , gspGameSessionData
    , gspGameSessionQueueName
    , gspPlacedPlayerSessions
    , gspPort

    -- ** GameSessionQueue
    , GameSessionQueue
    , gameSessionQueue
    , gsqGameSessionQueueARN
    , gsqPlayerLatencyPolicies
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

    -- ** MatchedPlayerSession
    , MatchedPlayerSession
    , matchedPlayerSession
    , mpsPlayerSessionId
    , mpsPlayerId

    -- ** MatchmakingConfiguration
    , MatchmakingConfiguration
    , matchmakingConfiguration
    , mcCreationTime
    , mcGameProperties
    , mcRuleSetName
    , mcAcceptanceTimeoutSeconds
    , mcRequestTimeoutSeconds
    , mcNotificationTarget
    , mcGameSessionQueueARNs
    , mcName
    , mcCustomEventData
    , mcAcceptanceRequired
    , mcGameSessionData
    , mcDescription
    , mcAdditionalPlayerCount

    -- ** MatchmakingRuleSet
    , MatchmakingRuleSet
    , matchmakingRuleSet
    , mrsCreationTime
    , mrsRuleSetName
    , mrsRuleSetBody

    -- ** MatchmakingTicket
    , MatchmakingTicket
    , matchmakingTicket
    , mtStatus
    , mtConfigurationName
    , mtStartTime
    , mtGameSessionConnectionInfo
    , mtTicketId
    , mtEstimatedWaitTime
    , mtStatusMessage
    , mtEndTime
    , mtStatusReason
    , mtPlayers

    -- ** PlacedPlayerSession
    , PlacedPlayerSession
    , placedPlayerSession
    , ppsPlayerSessionId
    , ppsPlayerId

    -- ** Player
    , Player
    , player
    , pPlayerAttributes
    , pTeam
    , pPlayerId
    , pLatencyInMs

    -- ** PlayerLatency
    , PlayerLatency
    , playerLatency
    , plLatencyInMilliseconds
    , plRegionIdentifier
    , plPlayerId

    -- ** PlayerLatencyPolicy
    , PlayerLatencyPolicy
    , playerLatencyPolicy
    , plpPolicyDurationSeconds
    , plpMaximumIndividualPlayerLatencyMilliseconds

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
    , rcGameSessionActivationTimeoutSeconds
    , rcServerProcesses
    , rcMaxConcurrentGameSessionActivations

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
    , spPolicyType
    , spMetricName
    , spComparisonOperator
    , spName
    , spThreshold
    , spScalingAdjustment
    , spFleetId
    , spTargetConfiguration

    -- ** ServerProcess
    , ServerProcess
    , serverProcess
    , spParameters
    , spLaunchPath
    , spConcurrentExecutions

    -- ** TargetConfiguration
    , TargetConfiguration
    , targetConfiguration
    , tcTargetValue

    -- ** VPCPeeringAuthorization
    , VPCPeeringAuthorization
    , vpcPeeringAuthorization
    , vpaCreationTime
    , vpaPeerVPCId
    , vpaPeerVPCAWSAccountId
    , vpaGameLiftAWSAccountId
    , vpaExpirationTime

    -- ** VPCPeeringConnection
    , VPCPeeringConnection
    , vpcPeeringConnection
    , vpcVPCPeeringConnectionId
    , vpcStatus
    , vpcPeerVPCId
    , vpcIPV4CidrBlock
    , vpcGameLiftVPCId
    , vpcFleetId

    -- ** VPCPeeringConnectionStatus
    , VPCPeeringConnectionStatus
    , vpcPeeringConnectionStatus
    , vpcsCode
    , vpcsMessage
    ) where

import Network.AWS.GameLift.AcceptMatch
import Network.AWS.GameLift.CreateAlias
import Network.AWS.GameLift.CreateBuild
import Network.AWS.GameLift.CreateFleet
import Network.AWS.GameLift.CreateGameSession
import Network.AWS.GameLift.CreateGameSessionQueue
import Network.AWS.GameLift.CreateMatchmakingConfiguration
import Network.AWS.GameLift.CreateMatchmakingRuleSet
import Network.AWS.GameLift.CreatePlayerSession
import Network.AWS.GameLift.CreatePlayerSessions
import Network.AWS.GameLift.CreateVPCPeeringAuthorization
import Network.AWS.GameLift.CreateVPCPeeringConnection
import Network.AWS.GameLift.DeleteAlias
import Network.AWS.GameLift.DeleteBuild
import Network.AWS.GameLift.DeleteFleet
import Network.AWS.GameLift.DeleteGameSessionQueue
import Network.AWS.GameLift.DeleteMatchmakingConfiguration
import Network.AWS.GameLift.DeleteScalingPolicy
import Network.AWS.GameLift.DeleteVPCPeeringAuthorization
import Network.AWS.GameLift.DeleteVPCPeeringConnection
import Network.AWS.GameLift.DescribeAlias
import Network.AWS.GameLift.DescribeBuild
import Network.AWS.GameLift.DescribeEC2InstanceLimits
import Network.AWS.GameLift.DescribeFleetAttributes
import Network.AWS.GameLift.DescribeFleetCapacity
import Network.AWS.GameLift.DescribeFleetEvents
import Network.AWS.GameLift.DescribeFleetPortSettings
import Network.AWS.GameLift.DescribeFleetUtilization
import Network.AWS.GameLift.DescribeGameSessionDetails
import Network.AWS.GameLift.DescribeGameSessionPlacement
import Network.AWS.GameLift.DescribeGameSessionQueues
import Network.AWS.GameLift.DescribeGameSessions
import Network.AWS.GameLift.DescribeInstances
import Network.AWS.GameLift.DescribeMatchmaking
import Network.AWS.GameLift.DescribeMatchmakingConfigurations
import Network.AWS.GameLift.DescribeMatchmakingRuleSets
import Network.AWS.GameLift.DescribePlayerSessions
import Network.AWS.GameLift.DescribeRuntimeConfiguration
import Network.AWS.GameLift.DescribeScalingPolicies
import Network.AWS.GameLift.DescribeVPCPeeringAuthorizations
import Network.AWS.GameLift.DescribeVPCPeeringConnections
import Network.AWS.GameLift.GetGameSessionLogURL
import Network.AWS.GameLift.GetInstanceAccess
import Network.AWS.GameLift.ListAliases
import Network.AWS.GameLift.ListBuilds
import Network.AWS.GameLift.ListFleets
import Network.AWS.GameLift.PutScalingPolicy
import Network.AWS.GameLift.RequestUploadCredentials
import Network.AWS.GameLift.ResolveAlias
import Network.AWS.GameLift.SearchGameSessions
import Network.AWS.GameLift.StartFleetActions
import Network.AWS.GameLift.StartGameSessionPlacement
import Network.AWS.GameLift.StartMatchBackfill
import Network.AWS.GameLift.StartMatchmaking
import Network.AWS.GameLift.StopFleetActions
import Network.AWS.GameLift.StopGameSessionPlacement
import Network.AWS.GameLift.StopMatchmaking
import Network.AWS.GameLift.Types
import Network.AWS.GameLift.UpdateAlias
import Network.AWS.GameLift.UpdateBuild
import Network.AWS.GameLift.UpdateFleetAttributes
import Network.AWS.GameLift.UpdateFleetCapacity
import Network.AWS.GameLift.UpdateFleetPortSettings
import Network.AWS.GameLift.UpdateGameSession
import Network.AWS.GameLift.UpdateGameSessionQueue
import Network.AWS.GameLift.UpdateMatchmakingConfiguration
import Network.AWS.GameLift.UpdateRuntimeConfiguration
import Network.AWS.GameLift.ValidateMatchmakingRuleSet
import Network.AWS.GameLift.Waiters

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
