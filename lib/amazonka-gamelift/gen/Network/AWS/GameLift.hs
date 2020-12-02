{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon GameLift Service__
--
-- GameLift provides solutions for hosting session-based multiplayer game servers in the cloud, including tools for deploying, operating, and scaling game servers. Built on AWS global computing infrastructure, GameLift helps you deliver high-performance, high-reliability, low-cost game servers while dynamically scaling your resource usage to meet player demand.
--
-- __About GameLift solutions__
--
-- Get more information on these GameLift solutions in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide> .
--
--     * Managed GameLift -- GameLift offers a fully managed service to set up and maintain computing machines for hosting, manage game session and player session life cycle, and handle security, storage, and performance tracking. You can use automatic scaling tools to balance hosting costs against meeting player demand., configure your game session management to minimize player latency, or add FlexMatch for matchmaking.
--
--     * Managed GameLift with Realtime Servers – With GameLift Realtime Servers, you can quickly configure and set up game servers for your game. Realtime Servers provides a game server framework with core Amazon GameLift infrastructure already built in.
--
--     * GameLift FleetIQ – Use GameLift FleetIQ as a standalone feature while managing your own EC2 instances and Auto Scaling groups for game hosting. GameLift FleetIQ provides optimizations that make low-cost Spot Instances viable for game hosting.
--
--
--
-- __About this API Reference__
--
-- This reference guide describes the low-level service API for Amazon GameLift. You can find links to language-specific SDK guides and the AWS CLI reference with each operation and data type topic. Useful links:
--
--     * <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html GameLift API operations listed by tasks>
--
--     * <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-components.html GameLift tools and resources>
module Network.AWS.GameLift
  ( -- * Service Configuration
    gameLift,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StopMatchmaking
    module Network.AWS.GameLift.StopMatchmaking,

    -- ** DescribeGameServerInstances (Paginated)
    module Network.AWS.GameLift.DescribeGameServerInstances,

    -- ** CreateGameSession
    module Network.AWS.GameLift.CreateGameSession,

    -- ** DeleteScalingPolicy
    module Network.AWS.GameLift.DeleteScalingPolicy,

    -- ** PutScalingPolicy
    module Network.AWS.GameLift.PutScalingPolicy,

    -- ** ListBuilds (Paginated)
    module Network.AWS.GameLift.ListBuilds,

    -- ** DeleteFleet
    module Network.AWS.GameLift.DeleteFleet,

    -- ** CreateBuild
    module Network.AWS.GameLift.CreateBuild,

    -- ** RequestUploadCredentials
    module Network.AWS.GameLift.RequestUploadCredentials,

    -- ** CreateAlias
    module Network.AWS.GameLift.CreateAlias,

    -- ** ListGameServers (Paginated)
    module Network.AWS.GameLift.ListGameServers,

    -- ** ResolveAlias
    module Network.AWS.GameLift.ResolveAlias,

    -- ** ListTagsForResource
    module Network.AWS.GameLift.ListTagsForResource,

    -- ** RegisterGameServer
    module Network.AWS.GameLift.RegisterGameServer,

    -- ** ListAliases (Paginated)
    module Network.AWS.GameLift.ListAliases,

    -- ** UpdateRuntimeConfiguration
    module Network.AWS.GameLift.UpdateRuntimeConfiguration,

    -- ** CreateVPCPeeringConnection
    module Network.AWS.GameLift.CreateVPCPeeringConnection,

    -- ** ListGameServerGroups (Paginated)
    module Network.AWS.GameLift.ListGameServerGroups,

    -- ** CreateGameSessionQueue
    module Network.AWS.GameLift.CreateGameSessionQueue,

    -- ** SearchGameSessions (Paginated)
    module Network.AWS.GameLift.SearchGameSessions,

    -- ** CreateVPCPeeringAuthorization
    module Network.AWS.GameLift.CreateVPCPeeringAuthorization,

    -- ** UpdateGameSessionQueue
    module Network.AWS.GameLift.UpdateGameSessionQueue,

    -- ** DeleteGameSessionQueue
    module Network.AWS.GameLift.DeleteGameSessionQueue,

    -- ** CreateGameServerGroup
    module Network.AWS.GameLift.CreateGameServerGroup,

    -- ** DeleteVPCPeeringConnection
    module Network.AWS.GameLift.DeleteVPCPeeringConnection,

    -- ** StartFleetActions
    module Network.AWS.GameLift.StartFleetActions,

    -- ** DeregisterGameServer
    module Network.AWS.GameLift.DeregisterGameServer,

    -- ** GetInstanceAccess
    module Network.AWS.GameLift.GetInstanceAccess,

    -- ** DescribeScalingPolicies (Paginated)
    module Network.AWS.GameLift.DescribeScalingPolicies,

    -- ** DescribeMatchmakingRuleSets (Paginated)
    module Network.AWS.GameLift.DescribeMatchmakingRuleSets,

    -- ** DescribeGameSessions (Paginated)
    module Network.AWS.GameLift.DescribeGameSessions,

    -- ** DescribeGameServer
    module Network.AWS.GameLift.DescribeGameServer,

    -- ** UpdateScript
    module Network.AWS.GameLift.UpdateScript,

    -- ** DeleteScript
    module Network.AWS.GameLift.DeleteScript,

    -- ** StartGameSessionPlacement
    module Network.AWS.GameLift.StartGameSessionPlacement,

    -- ** DescribeFleetUtilization (Paginated)
    module Network.AWS.GameLift.DescribeFleetUtilization,

    -- ** DescribeRuntimeConfiguration
    module Network.AWS.GameLift.DescribeRuntimeConfiguration,

    -- ** GetGameSessionLogURL
    module Network.AWS.GameLift.GetGameSessionLogURL,

    -- ** DescribeFleetAttributes (Paginated)
    module Network.AWS.GameLift.DescribeFleetAttributes,

    -- ** DescribeGameSessionPlacement
    module Network.AWS.GameLift.DescribeGameSessionPlacement,

    -- ** DescribeFleetEvents (Paginated)
    module Network.AWS.GameLift.DescribeFleetEvents,

    -- ** StartMatchmaking
    module Network.AWS.GameLift.StartMatchmaking,

    -- ** CreateMatchmakingRuleSet
    module Network.AWS.GameLift.CreateMatchmakingRuleSet,

    -- ** DescribeFleetCapacity (Paginated)
    module Network.AWS.GameLift.DescribeFleetCapacity,

    -- ** DeleteBuild
    module Network.AWS.GameLift.DeleteBuild,

    -- ** UpdateBuild
    module Network.AWS.GameLift.UpdateBuild,

    -- ** ListFleets (Paginated)
    module Network.AWS.GameLift.ListFleets,

    -- ** DeleteAlias
    module Network.AWS.GameLift.DeleteAlias,

    -- ** UpdateAlias
    module Network.AWS.GameLift.UpdateAlias,

    -- ** StartMatchBackfill
    module Network.AWS.GameLift.StartMatchBackfill,

    -- ** DescribeInstances (Paginated)
    module Network.AWS.GameLift.DescribeInstances,

    -- ** DescribeGameSessionDetails (Paginated)
    module Network.AWS.GameLift.DescribeGameSessionDetails,

    -- ** DescribeFleetPortSettings
    module Network.AWS.GameLift.DescribeFleetPortSettings,

    -- ** DescribeGameSessionQueues (Paginated)
    module Network.AWS.GameLift.DescribeGameSessionQueues,

    -- ** DescribeVPCPeeringConnections
    module Network.AWS.GameLift.DescribeVPCPeeringConnections,

    -- ** DescribeScript
    module Network.AWS.GameLift.DescribeScript,

    -- ** CreatePlayerSessions
    module Network.AWS.GameLift.CreatePlayerSessions,

    -- ** DescribeMatchmakingConfigurations (Paginated)
    module Network.AWS.GameLift.DescribeMatchmakingConfigurations,

    -- ** DescribeVPCPeeringAuthorizations
    module Network.AWS.GameLift.DescribeVPCPeeringAuthorizations,

    -- ** UpdateGameServer
    module Network.AWS.GameLift.UpdateGameServer,

    -- ** CreateFleet
    module Network.AWS.GameLift.CreateFleet,

    -- ** DeleteMatchmakingConfiguration
    module Network.AWS.GameLift.DeleteMatchmakingConfiguration,

    -- ** UpdateMatchmakingConfiguration
    module Network.AWS.GameLift.UpdateMatchmakingConfiguration,

    -- ** DeleteGameServerGroup
    module Network.AWS.GameLift.DeleteGameServerGroup,

    -- ** UpdateGameServerGroup
    module Network.AWS.GameLift.UpdateGameServerGroup,

    -- ** ResumeGameServerGroup
    module Network.AWS.GameLift.ResumeGameServerGroup,

    -- ** DeleteVPCPeeringAuthorization
    module Network.AWS.GameLift.DeleteVPCPeeringAuthorization,

    -- ** UpdateFleetAttributes
    module Network.AWS.GameLift.UpdateFleetAttributes,

    -- ** TagResource
    module Network.AWS.GameLift.TagResource,

    -- ** CreateMatchmakingConfiguration
    module Network.AWS.GameLift.CreateMatchmakingConfiguration,

    -- ** DescribePlayerSessions (Paginated)
    module Network.AWS.GameLift.DescribePlayerSessions,

    -- ** StopFleetActions
    module Network.AWS.GameLift.StopFleetActions,

    -- ** DescribeBuild
    module Network.AWS.GameLift.DescribeBuild,

    -- ** UpdateFleetPortSettings
    module Network.AWS.GameLift.UpdateFleetPortSettings,

    -- ** UpdateFleetCapacity
    module Network.AWS.GameLift.UpdateFleetCapacity,

    -- ** CreateScript
    module Network.AWS.GameLift.CreateScript,

    -- ** AcceptMatch
    module Network.AWS.GameLift.AcceptMatch,

    -- ** UntagResource
    module Network.AWS.GameLift.UntagResource,

    -- ** DescribeAlias
    module Network.AWS.GameLift.DescribeAlias,

    -- ** ValidateMatchmakingRuleSet
    module Network.AWS.GameLift.ValidateMatchmakingRuleSet,

    -- ** ListScripts (Paginated)
    module Network.AWS.GameLift.ListScripts,

    -- ** DescribeEC2InstanceLimits
    module Network.AWS.GameLift.DescribeEC2InstanceLimits,

    -- ** SuspendGameServerGroup
    module Network.AWS.GameLift.SuspendGameServerGroup,

    -- ** DeleteMatchmakingRuleSet
    module Network.AWS.GameLift.DeleteMatchmakingRuleSet,

    -- ** StopGameSessionPlacement
    module Network.AWS.GameLift.StopGameSessionPlacement,

    -- ** ClaimGameServer
    module Network.AWS.GameLift.ClaimGameServer,

    -- ** UpdateGameSession
    module Network.AWS.GameLift.UpdateGameSession,

    -- ** DescribeMatchmaking
    module Network.AWS.GameLift.DescribeMatchmaking,

    -- ** CreatePlayerSession
    module Network.AWS.GameLift.CreatePlayerSession,

    -- ** DescribeGameServerGroup
    module Network.AWS.GameLift.DescribeGameServerGroup,

    -- * Types

    -- ** AcceptanceType
    AcceptanceType (..),

    -- ** BackfillMode
    BackfillMode (..),

    -- ** BalancingStrategy
    BalancingStrategy (..),

    -- ** BuildStatus
    BuildStatus (..),

    -- ** CertificateType
    CertificateType (..),

    -- ** ComparisonOperatorType
    ComparisonOperatorType (..),

    -- ** EC2InstanceType
    EC2InstanceType (..),

    -- ** EventCode
    EventCode (..),

    -- ** FleetAction
    FleetAction (..),

    -- ** FleetStatus
    FleetStatus (..),

    -- ** FleetType
    FleetType (..),

    -- ** FlexMatchMode
    FlexMatchMode (..),

    -- ** GameServerClaimStatus
    GameServerClaimStatus (..),

    -- ** GameServerGroupAction
    GameServerGroupAction (..),

    -- ** GameServerGroupDeleteOption
    GameServerGroupDeleteOption (..),

    -- ** GameServerGroupInstanceType
    GameServerGroupInstanceType (..),

    -- ** GameServerGroupStatus
    GameServerGroupStatus (..),

    -- ** GameServerHealthCheck
    GameServerHealthCheck (..),

    -- ** GameServerInstanceStatus
    GameServerInstanceStatus (..),

    -- ** GameServerProtectionPolicy
    GameServerProtectionPolicy (..),

    -- ** GameServerUtilizationStatus
    GameServerUtilizationStatus (..),

    -- ** GameSessionPlacementState
    GameSessionPlacementState (..),

    -- ** GameSessionStatus
    GameSessionStatus (..),

    -- ** GameSessionStatusReason
    GameSessionStatusReason (..),

    -- ** IPProtocol
    IPProtocol (..),

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** MatchmakingConfigurationStatus
    MatchmakingConfigurationStatus (..),

    -- ** MetricName
    MetricName (..),

    -- ** OperatingSystem
    OperatingSystem (..),

    -- ** PlayerSessionCreationPolicy
    PlayerSessionCreationPolicy (..),

    -- ** PlayerSessionStatus
    PlayerSessionStatus (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** ProtectionPolicy
    ProtectionPolicy (..),

    -- ** RoutingStrategyType
    RoutingStrategyType (..),

    -- ** ScalingAdjustmentType
    ScalingAdjustmentType (..),

    -- ** ScalingStatusType
    ScalingStatusType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** AWSCredentials
    AWSCredentials,
    awsCredentials,
    acSecretAccessKey,
    acSessionToken,
    acAccessKeyId,

    -- ** Alias
    Alias,
    alias,
    aCreationTime,
    aLastUpdatedTime,
    aAliasId,
    aRoutingStrategy,
    aName,
    aAliasARN,
    aDescription,

    -- ** AttributeValue
    AttributeValue,
    attributeValue,
    avSL,
    avSDM,
    avN,
    avS,

    -- ** Build
    Build,
    build,
    bCreationTime,
    bStatus,
    bOperatingSystem,
    bBuildId,
    bName,
    bVersion,
    bBuildARN,
    bSizeOnDisk,

    -- ** CertificateConfiguration
    CertificateConfiguration,
    certificateConfiguration,
    ccCertificateType,

    -- ** DesiredPlayerSession
    DesiredPlayerSession,
    desiredPlayerSession,
    dpsPlayerData,
    dpsPlayerId,

    -- ** EC2InstanceCounts
    EC2InstanceCounts,
    ec2InstanceCounts,
    eicIdLE,
    eicTERMINATING,
    eicPENDING,
    eicMAXIMUM,
    eicDESIRED,
    eicMINIMUM,
    eicACTIVE,

    -- ** EC2InstanceLimit
    EC2InstanceLimit,
    ec2InstanceLimit,
    eilEC2InstanceType,
    eilCurrentInstances,
    eilInstanceLimit,

    -- ** Event
    Event,
    event,
    eResourceId,
    ePreSignedLogURL,
    eEventTime,
    eMessage,
    eEventCode,
    eEventId,

    -- ** FleetAttributes
    FleetAttributes,
    fleetAttributes,
    faCreationTime,
    faStatus,
    faServerLaunchParameters,
    faLogPaths,
    faOperatingSystem,
    faBuildId,
    faFleetARN,
    faFleetType,
    faTerminationTime,
    faInstanceType,
    faStoppedActions,
    faNewGameSessionProtectionPolicy,
    faName,
    faScriptId,
    faScriptARN,
    faCertificateConfiguration,
    faServerLaunchPath,
    faInstanceRoleARN,
    faMetricGroups,
    faBuildARN,
    faFleetId,
    faDescription,
    faResourceCreationLimitPolicy,

    -- ** FleetCapacity
    FleetCapacity,
    fleetCapacity,
    fcInstanceType,
    fcFleetId,
    fcInstanceCounts,

    -- ** FleetUtilization
    FleetUtilization,
    fleetUtilization,
    fuActiveGameSessionCount,
    fuMaximumPlayerSessionCount,
    fuCurrentPlayerSessionCount,
    fuFleetId,
    fuActiveServerProcessCount,

    -- ** GameProperty
    GameProperty,
    gameProperty,
    gpKey,
    gpValue,

    -- ** GameServer
    GameServer,
    gameServer,
    gsInstanceId,
    gsLastClaimTime,
    gsGameServerGroupName,
    gsGameServerData,
    gsClaimStatus,
    gsGameServerId,
    gsUtilizationStatus,
    gsRegistrationTime,
    gsLastHealthCheckTime,
    gsConnectionInfo,
    gsGameServerGroupARN,

    -- ** GameServerGroup
    GameServerGroup,
    gameServerGroup,
    gsgCreationTime,
    gsgStatus,
    gsgInstanceDefinitions,
    gsgLastUpdatedTime,
    gsgBalancingStrategy,
    gsgGameServerGroupName,
    gsgSuspendedActions,
    gsgAutoScalingGroupARN,
    gsgStatusReason,
    gsgGameServerProtectionPolicy,
    gsgGameServerGroupARN,
    gsgRoleARN,

    -- ** GameServerGroupAutoScalingPolicy
    GameServerGroupAutoScalingPolicy,
    gameServerGroupAutoScalingPolicy,
    gsgaspEstimatedInstanceWarmup,
    gsgaspTargetTrackingConfiguration,

    -- ** GameServerInstance
    GameServerInstance,
    gameServerInstance,
    gsiInstanceId,
    gsiGameServerGroupName,
    gsiInstanceStatus,
    gsiGameServerGroupARN,

    -- ** GameSession
    GameSession,
    gameSession,
    gsCreationTime,
    gsStatus,
    gsGameProperties,
    gsIPAddress,
    gsGameSessionId,
    gsMatchmakerData,
    gsFleetARN,
    gsMaximumPlayerSessionCount,
    gsTerminationTime,
    gsPlayerSessionCreationPolicy,
    gsName,
    gsCurrentPlayerSessionCount,
    gsStatusReason,
    gsGameSessionData,
    gsFleetId,
    gsDNSName,
    gsCreatorId,
    gsPort,

    -- ** GameSessionConnectionInfo
    GameSessionConnectionInfo,
    gameSessionConnectionInfo,
    gsciMatchedPlayerSessions,
    gsciIPAddress,
    gsciGameSessionARN,
    gsciDNSName,
    gsciPort,

    -- ** GameSessionDetail
    GameSessionDetail,
    gameSessionDetail,
    gsdGameSession,
    gsdProtectionPolicy,

    -- ** GameSessionPlacement
    GameSessionPlacement,
    gameSessionPlacement,
    gspStatus,
    gspPlacementId,
    gspGameProperties,
    gspIPAddress,
    gspGameSessionName,
    gspStartTime,
    gspGameSessionId,
    gspGameSessionRegion,
    gspMatchmakerData,
    gspMaximumPlayerSessionCount,
    gspEndTime,
    gspGameSessionARN,
    gspPlayerLatencies,
    gspGameSessionData,
    gspDNSName,
    gspGameSessionQueueName,
    gspPlacedPlayerSessions,
    gspPort,

    -- ** GameSessionQueue
    GameSessionQueue,
    gameSessionQueue,
    gsqGameSessionQueueARN,
    gsqPlayerLatencyPolicies,
    gsqTimeoutInSeconds,
    gsqDestinations,
    gsqName,

    -- ** GameSessionQueueDestination
    GameSessionQueueDestination,
    gameSessionQueueDestination,
    gsqdDestinationARN,

    -- ** IPPermission
    IPPermission,
    ipPermission,
    ipFromPort,
    ipToPort,
    ipIPRange,
    ipProtocol,

    -- ** Instance
    Instance,
    instance',
    iCreationTime,
    iInstanceId,
    iStatus,
    iIPAddress,
    iOperatingSystem,
    iType,
    iFleetId,
    iDNSName,

    -- ** InstanceAccess
    InstanceAccess,
    instanceAccess,
    iaInstanceId,
    iaIPAddress,
    iaOperatingSystem,
    iaCredentials,
    iaFleetId,

    -- ** InstanceCredentials
    InstanceCredentials,
    instanceCredentials,
    icUserName,
    icSecret,

    -- ** InstanceDefinition
    InstanceDefinition,
    instanceDefinition,
    idWeightedCapacity,
    idInstanceType,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification,
    launchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- ** MatchedPlayerSession
    MatchedPlayerSession,
    matchedPlayerSession,
    mpsPlayerSessionId,
    mpsPlayerId,

    -- ** MatchmakingConfiguration
    MatchmakingConfiguration,
    matchmakingConfiguration,
    mcCreationTime,
    mcBackfillMode,
    mcGameProperties,
    mcRuleSetName,
    mcAcceptanceTimeoutSeconds,
    mcRequestTimeoutSeconds,
    mcNotificationTarget,
    mcFlexMatchMode,
    mcGameSessionQueueARNs,
    mcName,
    mcCustomEventData,
    mcConfigurationARN,
    mcAcceptanceRequired,
    mcGameSessionData,
    mcDescription,
    mcAdditionalPlayerCount,
    mcRuleSetARN,

    -- ** MatchmakingRuleSet
    MatchmakingRuleSet,
    matchmakingRuleSet,
    mrsCreationTime,
    mrsRuleSetName,
    mrsRuleSetARN,
    mrsRuleSetBody,

    -- ** MatchmakingTicket
    MatchmakingTicket,
    matchmakingTicket,
    mtStatus,
    mtConfigurationName,
    mtStartTime,
    mtGameSessionConnectionInfo,
    mtTicketId,
    mtEstimatedWaitTime,
    mtStatusMessage,
    mtEndTime,
    mtConfigurationARN,
    mtStatusReason,
    mtPlayers,

    -- ** PlacedPlayerSession
    PlacedPlayerSession,
    placedPlayerSession,
    ppsPlayerSessionId,
    ppsPlayerId,

    -- ** Player
    Player,
    player,
    pPlayerAttributes,
    pTeam,
    pPlayerId,
    pLatencyInMs,

    -- ** PlayerLatency
    PlayerLatency,
    playerLatency,
    plLatencyInMilliseconds,
    plRegionIdentifier,
    plPlayerId,

    -- ** PlayerLatencyPolicy
    PlayerLatencyPolicy,
    playerLatencyPolicy,
    plpPolicyDurationSeconds,
    plpMaximumIndividualPlayerLatencyMilliseconds,

    -- ** PlayerSession
    PlayerSession,
    playerSession,
    psCreationTime,
    psStatus,
    psIPAddress,
    psGameSessionId,
    psFleetARN,
    psTerminationTime,
    psPlayerSessionId,
    psFleetId,
    psPlayerData,
    psPlayerId,
    psDNSName,
    psPort,

    -- ** ResourceCreationLimitPolicy
    ResourceCreationLimitPolicy,
    resourceCreationLimitPolicy,
    rclpNewGameSessionsPerCreator,
    rclpPolicyPeriodInMinutes,

    -- ** RoutingStrategy
    RoutingStrategy,
    routingStrategy,
    rsType,
    rsMessage,
    rsFleetId,

    -- ** RuntimeConfiguration
    RuntimeConfiguration,
    runtimeConfiguration,
    rcGameSessionActivationTimeoutSeconds,
    rcServerProcesses,
    rcMaxConcurrentGameSessionActivations,

    -- ** S3Location
    S3Location,
    s3Location,
    slBucket,
    slKey,
    slObjectVersion,
    slRoleARN,

    -- ** ScalingPolicy
    ScalingPolicy,
    scalingPolicy,
    spStatus,
    spScalingAdjustmentType,
    spEvaluationPeriods,
    spPolicyType,
    spMetricName,
    spComparisonOperator,
    spName,
    spThreshold,
    spScalingAdjustment,
    spFleetId,
    spTargetConfiguration,

    -- ** Script
    Script,
    script,
    sCreationTime,
    sStorageLocation,
    sName,
    sScriptId,
    sVersion,
    sScriptARN,
    sSizeOnDisk,

    -- ** ServerProcess
    ServerProcess,
    serverProcess,
    spParameters,
    spLaunchPath,
    spConcurrentExecutions,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** TargetConfiguration
    TargetConfiguration,
    targetConfiguration,
    tcTargetValue,

    -- ** TargetTrackingConfiguration
    TargetTrackingConfiguration,
    targetTrackingConfiguration,
    ttcTargetValue,

    -- ** VPCPeeringAuthorization
    VPCPeeringAuthorization,
    vpcPeeringAuthorization,
    vpaCreationTime,
    vpaPeerVPCId,
    vpaPeerVPCAWSAccountId,
    vpaGameLiftAWSAccountId,
    vpaExpirationTime,

    -- ** VPCPeeringConnection
    VPCPeeringConnection,
    vpcPeeringConnection,
    vpcVPCPeeringConnectionId,
    vpcStatus,
    vpcPeerVPCId,
    vpcFleetARN,
    vpcIPV4CidrBlock,
    vpcGameLiftVPCId,
    vpcFleetId,

    -- ** VPCPeeringConnectionStatus
    VPCPeeringConnectionStatus,
    vpcPeeringConnectionStatus,
    vpcsCode,
    vpcsMessage,
  )
where

import Network.AWS.GameLift.AcceptMatch
import Network.AWS.GameLift.ClaimGameServer
import Network.AWS.GameLift.CreateAlias
import Network.AWS.GameLift.CreateBuild
import Network.AWS.GameLift.CreateFleet
import Network.AWS.GameLift.CreateGameServerGroup
import Network.AWS.GameLift.CreateGameSession
import Network.AWS.GameLift.CreateGameSessionQueue
import Network.AWS.GameLift.CreateMatchmakingConfiguration
import Network.AWS.GameLift.CreateMatchmakingRuleSet
import Network.AWS.GameLift.CreatePlayerSession
import Network.AWS.GameLift.CreatePlayerSessions
import Network.AWS.GameLift.CreateScript
import Network.AWS.GameLift.CreateVPCPeeringAuthorization
import Network.AWS.GameLift.CreateVPCPeeringConnection
import Network.AWS.GameLift.DeleteAlias
import Network.AWS.GameLift.DeleteBuild
import Network.AWS.GameLift.DeleteFleet
import Network.AWS.GameLift.DeleteGameServerGroup
import Network.AWS.GameLift.DeleteGameSessionQueue
import Network.AWS.GameLift.DeleteMatchmakingConfiguration
import Network.AWS.GameLift.DeleteMatchmakingRuleSet
import Network.AWS.GameLift.DeleteScalingPolicy
import Network.AWS.GameLift.DeleteScript
import Network.AWS.GameLift.DeleteVPCPeeringAuthorization
import Network.AWS.GameLift.DeleteVPCPeeringConnection
import Network.AWS.GameLift.DeregisterGameServer
import Network.AWS.GameLift.DescribeAlias
import Network.AWS.GameLift.DescribeBuild
import Network.AWS.GameLift.DescribeEC2InstanceLimits
import Network.AWS.GameLift.DescribeFleetAttributes
import Network.AWS.GameLift.DescribeFleetCapacity
import Network.AWS.GameLift.DescribeFleetEvents
import Network.AWS.GameLift.DescribeFleetPortSettings
import Network.AWS.GameLift.DescribeFleetUtilization
import Network.AWS.GameLift.DescribeGameServer
import Network.AWS.GameLift.DescribeGameServerGroup
import Network.AWS.GameLift.DescribeGameServerInstances
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
import Network.AWS.GameLift.DescribeScript
import Network.AWS.GameLift.DescribeVPCPeeringAuthorizations
import Network.AWS.GameLift.DescribeVPCPeeringConnections
import Network.AWS.GameLift.GetGameSessionLogURL
import Network.AWS.GameLift.GetInstanceAccess
import Network.AWS.GameLift.ListAliases
import Network.AWS.GameLift.ListBuilds
import Network.AWS.GameLift.ListFleets
import Network.AWS.GameLift.ListGameServerGroups
import Network.AWS.GameLift.ListGameServers
import Network.AWS.GameLift.ListScripts
import Network.AWS.GameLift.ListTagsForResource
import Network.AWS.GameLift.PutScalingPolicy
import Network.AWS.GameLift.RegisterGameServer
import Network.AWS.GameLift.RequestUploadCredentials
import Network.AWS.GameLift.ResolveAlias
import Network.AWS.GameLift.ResumeGameServerGroup
import Network.AWS.GameLift.SearchGameSessions
import Network.AWS.GameLift.StartFleetActions
import Network.AWS.GameLift.StartGameSessionPlacement
import Network.AWS.GameLift.StartMatchBackfill
import Network.AWS.GameLift.StartMatchmaking
import Network.AWS.GameLift.StopFleetActions
import Network.AWS.GameLift.StopGameSessionPlacement
import Network.AWS.GameLift.StopMatchmaking
import Network.AWS.GameLift.SuspendGameServerGroup
import Network.AWS.GameLift.TagResource
import Network.AWS.GameLift.Types
import Network.AWS.GameLift.UntagResource
import Network.AWS.GameLift.UpdateAlias
import Network.AWS.GameLift.UpdateBuild
import Network.AWS.GameLift.UpdateFleetAttributes
import Network.AWS.GameLift.UpdateFleetCapacity
import Network.AWS.GameLift.UpdateFleetPortSettings
import Network.AWS.GameLift.UpdateGameServer
import Network.AWS.GameLift.UpdateGameServerGroup
import Network.AWS.GameLift.UpdateGameSession
import Network.AWS.GameLift.UpdateGameSessionQueue
import Network.AWS.GameLift.UpdateMatchmakingConfiguration
import Network.AWS.GameLift.UpdateRuntimeConfiguration
import Network.AWS.GameLift.UpdateScript
import Network.AWS.GameLift.ValidateMatchmakingRuleSet
import Network.AWS.GameLift.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'GameLift'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
