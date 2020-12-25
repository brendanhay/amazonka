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
-- __About GameLift solutions__
-- Get more information on these GameLift solutions in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide> .
--
--     * Managed GameLift -- GameLift offers a fully managed service to set up and maintain computing machines for hosting, manage game session and player session life cycle, and handle security, storage, and performance tracking. You can use automatic scaling tools to balance hosting costs against meeting player demand., configure your game session management to minimize player latency, or add FlexMatch for matchmaking.
--
--
--     * Managed GameLift with Realtime Servers – With GameLift Realtime Servers, you can quickly configure and set up game servers for your game. Realtime Servers provides a game server framework with core Amazon GameLift infrastructure already built in.
--
--
--     * GameLift FleetIQ – Use GameLift FleetIQ as a standalone feature while managing your own EC2 instances and Auto Scaling groups for game hosting. GameLift FleetIQ provides optimizations that make low-cost Spot Instances viable for game hosting.
--
--
-- __About this API Reference__
-- This reference guide describes the low-level service API for Amazon GameLift. You can find links to language-specific SDK guides and the AWS CLI reference with each operation and data type topic. Useful links:
--
--     * <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html GameLift API operations listed by tasks>
--
--
--     * <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-components.html GameLift tools and resources>
module Network.AWS.GameLift
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** OutOfCapacityException
    _OutOfCapacityException,

    -- ** InvalidFleetStatusException
    _InvalidFleetStatusException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** TaggingFailedException
    _TaggingFailedException,

    -- ** TerminalRoutingStrategyException
    _TerminalRoutingStrategyException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** GameSessionFullException
    _GameSessionFullException,

    -- ** UnsupportedRegionException
    _UnsupportedRegionException,

    -- ** InvalidGameSessionStatusException
    _InvalidGameSessionStatusException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** FleetCapacityExceededException
    _FleetCapacityExceededException,

    -- ** LimitExceededException
    _LimitExceededException,

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

    -- ** CreateVpcPeeringConnection
    module Network.AWS.GameLift.CreateVpcPeeringConnection,

    -- ** ListGameServerGroups (Paginated)
    module Network.AWS.GameLift.ListGameServerGroups,

    -- ** CreateGameSessionQueue
    module Network.AWS.GameLift.CreateGameSessionQueue,

    -- ** SearchGameSessions (Paginated)
    module Network.AWS.GameLift.SearchGameSessions,

    -- ** CreateVpcPeeringAuthorization
    module Network.AWS.GameLift.CreateVpcPeeringAuthorization,

    -- ** UpdateGameSessionQueue
    module Network.AWS.GameLift.UpdateGameSessionQueue,

    -- ** DeleteGameSessionQueue
    module Network.AWS.GameLift.DeleteGameSessionQueue,

    -- ** CreateGameServerGroup
    module Network.AWS.GameLift.CreateGameServerGroup,

    -- ** DeleteVpcPeeringConnection
    module Network.AWS.GameLift.DeleteVpcPeeringConnection,

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

    -- ** GetGameSessionLogUrl
    module Network.AWS.GameLift.GetGameSessionLogUrl,

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

    -- ** DescribeVpcPeeringConnections
    module Network.AWS.GameLift.DescribeVpcPeeringConnections,

    -- ** DescribeScript
    module Network.AWS.GameLift.DescribeScript,

    -- ** CreatePlayerSessions
    module Network.AWS.GameLift.CreatePlayerSessions,

    -- ** DescribeMatchmakingConfigurations (Paginated)
    module Network.AWS.GameLift.DescribeMatchmakingConfigurations,

    -- ** DescribeVpcPeeringAuthorizations
    module Network.AWS.GameLift.DescribeVpcPeeringAuthorizations,

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

    -- ** DeleteVpcPeeringAuthorization
    module Network.AWS.GameLift.DeleteVpcPeeringAuthorization,

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

    -- ** GameSessionQueueArn
    GameSessionQueueArn (..),

    -- ** InstanceId
    InstanceId (..),

    -- ** MatchmakingConfiguration
    MatchmakingConfiguration (..),
    mkMatchmakingConfiguration,
    mcAcceptanceRequired,
    mcAcceptanceTimeoutSeconds,
    mcAdditionalPlayerCount,
    mcBackfillMode,
    mcConfigurationArn,
    mcCreationTime,
    mcCustomEventData,
    mcDescription,
    mcFlexMatchMode,
    mcGameProperties,
    mcGameSessionData,
    mcGameSessionQueueArns,
    mcName,
    mcNotificationTarget,
    mcRequestTimeoutSeconds,
    mcRuleSetArn,
    mcRuleSetName,

    -- ** IamRoleArn
    IamRoleArn (..),

    -- ** Event
    Event (..),
    mkEvent,
    eEventCode,
    eEventId,
    eEventTime,
    eMessage,
    ePreSignedLogUrl,
    eResourceId,

    -- ** BackfillMode
    BackfillMode (..),

    -- ** RoutingStrategyType
    RoutingStrategyType (..),

    -- ** Script
    Script (..),
    mkScript,
    sCreationTime,
    sName,
    sScriptArn,
    sScriptId,
    sSizeOnDisk,
    sStorageLocation,
    sVersion,

    -- ** IdStringModel
    IdStringModel (..),

    -- ** IpAddress
    IpAddress (..),

    -- ** NonZeroAndMaxString
    NonZeroAndMaxString (..),

    -- ** AttributeValue
    AttributeValue (..),
    mkAttributeValue,
    avN,
    avS,
    avSDM,
    avSL,

    -- ** MatchedPlayerSession
    MatchedPlayerSession (..),
    mkMatchedPlayerSession,
    mpsPlayerId,
    mpsPlayerSessionId,

    -- ** GameServerInstanceId
    GameServerInstanceId (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** PlacedPlayerSession
    PlacedPlayerSession (..),
    mkPlacedPlayerSession,
    ppsPlayerId,
    ppsPlayerSessionId,

    -- ** LaunchTemplateName
    LaunchTemplateName (..),

    -- ** FleetStatus
    FleetStatus (..),

    -- ** GamePropertyValue
    GamePropertyValue (..),

    -- ** GameServerGroupAction
    GameServerGroupAction (..),

    -- ** ScalingAdjustmentType
    ScalingAdjustmentType (..),

    -- ** StringModel
    StringModel (..),

    -- ** PlayerSession
    PlayerSession (..),
    mkPlayerSession,
    psCreationTime,
    psDnsName,
    psFleetArn,
    psFleetId,
    psGameSessionId,
    psIpAddress,
    psPlayerData,
    psPlayerId,
    psPlayerSessionId,
    psPort,
    psStatus,
    psTerminationTime,

    -- ** GameSessionQueueDestination
    GameSessionQueueDestination (..),
    mkGameSessionQueueDestination,
    gsqdDestinationArn,

    -- ** GameServerClaimStatus
    GameServerClaimStatus (..),

    -- ** WeightedCapacity
    WeightedCapacity (..),

    -- ** ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spComparisonOperator,
    spEvaluationPeriods,
    spFleetId,
    spMetricName,
    spName,
    spPolicyType,
    spScalingAdjustment,
    spScalingAdjustmentType,
    spStatus,
    spTargetConfiguration,
    spThreshold,

    -- ** GameServerUtilizationStatus
    GameServerUtilizationStatus (..),

    -- ** CertificateType
    CertificateType (..),

    -- ** DesiredPlayerSession
    DesiredPlayerSession (..),
    mkDesiredPlayerSession,
    dpsPlayerData,
    dpsPlayerId,

    -- ** MatchmakingRuleSetArn
    MatchmakingRuleSetArn (..),

    -- ** EC2InstanceType
    EC2InstanceType (..),

    -- ** OperatingSystem
    OperatingSystem (..),

    -- ** GameSessionConnectionInfo
    GameSessionConnectionInfo (..),
    mkGameSessionConnectionInfo,
    gsciDnsName,
    gsciGameSessionArn,
    gsciIpAddress,
    gsciMatchedPlayerSessions,
    gsciPort,

    -- ** MatchmakingConfigurationStatus
    MatchmakingConfigurationStatus (..),

    -- ** LaunchTemplateId
    LaunchTemplateId (..),

    -- ** BalancingStrategy
    BalancingStrategy (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** MatchmakingRuleSet
    MatchmakingRuleSet (..),
    mkMatchmakingRuleSet,
    mrsRuleSetBody,
    mrsCreationTime,
    mrsRuleSetArn,
    mrsRuleSetName,

    -- ** MatchmakingTicket
    MatchmakingTicket (..),
    mkMatchmakingTicket,
    mtConfigurationArn,
    mtConfigurationName,
    mtEndTime,
    mtEstimatedWaitTime,
    mtGameSessionConnectionInfo,
    mtPlayers,
    mtStartTime,
    mtStatus,
    mtStatusMessage,
    mtStatusReason,
    mtTicketId,

    -- ** GameSessionStatusReason
    GameSessionStatusReason (..),

    -- ** GameServer
    GameServer (..),
    mkGameServer,
    gsClaimStatus,
    gsConnectionInfo,
    gsGameServerData,
    gsGameServerGroupArn,
    gsGameServerGroupName,
    gsGameServerId,
    gsInstanceId,
    gsLastClaimTime,
    gsLastHealthCheckTime,
    gsRegistrationTime,
    gsUtilizationStatus,

    -- ** MetricName
    MetricName (..),

    -- ** GameServerGroupName
    GameServerGroupName (..),

    -- ** BuildId
    BuildId (..),

    -- ** PlayerLatency
    PlayerLatency (..),
    mkPlayerLatency,
    plLatencyInMilliseconds,
    plPlayerId,
    plRegionIdentifier,

    -- ** FleetIdOrArn
    FleetIdOrArn (..),

    -- ** GameServerData
    GameServerData (..),

    -- ** AliasId
    AliasId (..),

    -- ** MatchmakerData
    MatchmakerData (..),

    -- ** FleetArn
    FleetArn (..),

    -- ** GameServerGroupDeleteOption
    GameServerGroupDeleteOption (..),

    -- ** FleetType
    FleetType (..),

    -- ** Alias
    Alias (..),
    mkAlias,
    aAliasArn,
    aAliasId,
    aCreationTime,
    aDescription,
    aLastUpdatedTime,
    aName,
    aRoutingStrategy,

    -- ** GameSessionPlacementState
    GameSessionPlacementState (..),

    -- ** VpcSubnet
    VpcSubnet (..),

    -- ** GameServerInstanceStatus
    GameServerInstanceStatus (..),

    -- ** TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcTargetValue,

    -- ** Build
    Build (..),
    mkBuild,
    bBuildArn,
    bBuildId,
    bCreationTime,
    bName,
    bOperatingSystem,
    bSizeOnDisk,
    bStatus,
    bVersion,

    -- ** GameServerId
    GameServerId (..),

    -- ** GamePropertyKey
    GamePropertyKey (..),

    -- ** SnsArnStringModel
    SnsArnStringModel (..),

    -- ** ScalingStatusType
    ScalingStatusType (..),

    -- ** PlayerSessionStatus
    PlayerSessionStatus (..),

    -- ** InstanceCredentials
    InstanceCredentials (..),
    mkInstanceCredentials,
    icSecret,
    icUserName,

    -- ** AcceptanceType
    AcceptanceType (..),

    -- ** GameSessionPlacement
    GameSessionPlacement (..),
    mkGameSessionPlacement,
    gspDnsName,
    gspEndTime,
    gspGameProperties,
    gspGameSessionArn,
    gspGameSessionData,
    gspGameSessionId,
    gspGameSessionName,
    gspGameSessionQueueName,
    gspGameSessionRegion,
    gspIpAddress,
    gspMatchmakerData,
    gspMaximumPlayerSessionCount,
    gspPlacedPlayerSessions,
    gspPlacementId,
    gspPlayerLatencies,
    gspPort,
    gspStartTime,
    gspStatus,

    -- ** FreeText
    FreeText (..),

    -- ** GameSessionDetail
    GameSessionDetail (..),
    mkGameSessionDetail,
    gsdGameSession,
    gsdProtectionPolicy,

    -- ** RuntimeConfiguration
    RuntimeConfiguration (..),
    mkRuntimeConfiguration,
    rcGameSessionActivationTimeoutSeconds,
    rcMaxConcurrentGameSessionActivations,
    rcServerProcesses,

    -- ** PlayerSessionCreationPolicy
    PlayerSessionCreationPolicy (..),

    -- ** GameServerGroup
    GameServerGroup (..),
    mkGameServerGroup,
    gsgAutoScalingGroupArn,
    gsgBalancingStrategy,
    gsgCreationTime,
    gsgGameServerGroupArn,
    gsgGameServerGroupName,
    gsgGameServerProtectionPolicy,
    gsgInstanceDefinitions,
    gsgLastUpdatedTime,
    gsgRoleArn,
    gsgStatus,
    gsgStatusReason,
    gsgSuspendedActions,

    -- ** FleetUtilization
    FleetUtilization (..),
    mkFleetUtilization,
    fuActiveGameSessionCount,
    fuActiveServerProcessCount,
    fuCurrentPlayerSessionCount,
    fuFleetId,
    fuMaximumPlayerSessionCount,

    -- ** RuleSetBody
    RuleSetBody (..),

    -- ** InstanceDefinition
    InstanceDefinition (..),
    mkInstanceDefinition,
    idInstanceType,
    idWeightedCapacity,

    -- ** VpcPeeringAuthorization
    VpcPeeringAuthorization (..),
    mkVpcPeeringAuthorization,
    vpaCreationTime,
    vpaExpirationTime,
    vpaGameLiftAwsAccountId,
    vpaPeerVpcAwsAccountId,
    vpaPeerVpcId,

    -- ** SortOrder
    SortOrder (..),

    -- ** GameSessionStatus
    GameSessionStatus (..),

    -- ** NonEmptyString
    NonEmptyString (..),

    -- ** FlexMatchMode
    FlexMatchMode (..),

    -- ** ArnStringModel
    ArnStringModel (..),

    -- ** LaunchTemplateVersion
    LaunchTemplateVersion (..),

    -- ** MatchmakingRuleSetName
    MatchmakingRuleSetName (..),

    -- ** RoutingStrategy
    RoutingStrategy (..),
    mkRoutingStrategy,
    rsFleetId,
    rsMessage,
    rsType,

    -- ** IpProtocol
    IpProtocol (..),

    -- ** CustomEventData
    CustomEventData (..),

    -- ** ScriptId
    ScriptId (..),

    -- ** ScriptIdOrArn
    ScriptIdOrArn (..),

    -- ** IpPermission
    IpPermission (..),
    mkIpPermission,
    ipFromPort,
    ipToPort,
    ipIpRange,
    ipProtocol,

    -- ** FleetCapacity
    FleetCapacity (..),
    mkFleetCapacity,
    fcFleetId,
    fcInstanceCounts,
    fcInstanceType,

    -- ** BuildStatus
    BuildStatus (..),

    -- ** ScriptArn
    ScriptArn (..),

    -- ** GameServerInstance
    GameServerInstance (..),
    mkGameServerInstance,
    gsiGameServerGroupArn,
    gsiGameServerGroupName,
    gsiInstanceId,
    gsiInstanceStatus,

    -- ** NonBlankAndLengthConstraintString
    NonBlankAndLengthConstraintString (..),

    -- ** EC2InstanceCounts
    EC2InstanceCounts (..),
    mkEC2InstanceCounts,
    ecicACTIVE,
    ecicDESIRED,
    ecicIDLE,
    ecicMAXIMUM,
    ecicMINIMUM,
    ecicPENDING,
    ecicTERMINATING,

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** CertificateConfiguration
    CertificateConfiguration (..),
    mkCertificateConfiguration,
    ccCertificateType,

    -- ** AwsCredentials
    AwsCredentials (..),
    mkAwsCredentials,
    acAccessKeyId,
    acSecretAccessKey,
    acSessionToken,

    -- ** InstanceAccess
    InstanceAccess (..),
    mkInstanceAccess,
    iaCredentials,
    iaFleetId,
    iaInstanceId,
    iaIpAddress,
    iaOperatingSystem,

    -- ** VpcPeeringConnectionStatus
    VpcPeeringConnectionStatus (..),
    mkVpcPeeringConnectionStatus,
    vpcsCode,
    vpcsMessage,

    -- ** TagKey
    TagKey (..),

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,
    slObjectVersion,
    slRoleArn,

    -- ** GameServerGroupInstanceType
    GameServerGroupInstanceType (..),

    -- ** AutoScalingGroupArn
    AutoScalingGroupArn (..),

    -- ** EC2InstanceLimit
    EC2InstanceLimit (..),
    mkEC2InstanceLimit,
    ecilCurrentInstances,
    ecilEC2InstanceType,
    ecilInstanceLimit,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateId,
    ltsLaunchTemplateName,
    ltsVersion,

    -- ** GameSession
    GameSession (..),
    mkGameSession,
    gsCreationTime,
    gsCreatorId,
    gsCurrentPlayerSessionCount,
    gsDnsName,
    gsFleetArn,
    gsFleetId,
    gsGameProperties,
    gsGameSessionData,
    gsGameSessionId,
    gsIpAddress,
    gsMatchmakerData,
    gsMaximumPlayerSessionCount,
    gsName,
    gsPlayerSessionCreationPolicy,
    gsPort,
    gsStatus,
    gsStatusReason,
    gsTerminationTime,

    -- ** PlayerSessionId
    PlayerSessionId (..),

    -- ** FleetAction
    FleetAction (..),

    -- ** GameSessionData
    GameSessionData (..),

    -- ** GameServerProtectionPolicy
    GameServerProtectionPolicy (..),

    -- ** GameServerGroupStatus
    GameServerGroupStatus (..),

    -- ** BuildArn
    BuildArn (..),

    -- ** ProtectionPolicy
    ProtectionPolicy (..),

    -- ** BuildIdOrArn
    BuildIdOrArn (..),

    -- ** MatchmakingIdStringModel
    MatchmakingIdStringModel (..),

    -- ** AmazonResourceName
    AmazonResourceName (..),

    -- ** Player
    Player (..),
    mkPlayer,
    pLatencyInMs,
    pPlayerAttributes,
    pPlayerId,
    pTeam,

    -- ** AliasArn
    AliasArn (..),

    -- ** MetricGroup
    MetricGroup (..),

    -- ** FleetId
    FleetId (..),

    -- ** PlayerLatencyPolicy
    PlayerLatencyPolicy (..),
    mkPlayerLatencyPolicy,
    plpMaximumIndividualPlayerLatencyMilliseconds,
    plpPolicyDurationSeconds,

    -- ** AliasIdOrArn
    AliasIdOrArn (..),

    -- ** PlayerData
    PlayerData (..),

    -- ** MatchmakingConfigurationName
    MatchmakingConfigurationName (..),

    -- ** GameServerGroupNameOrArn
    GameServerGroupNameOrArn (..),

    -- ** DnsName
    DnsName (..),

    -- ** ServerProcess
    ServerProcess (..),
    mkServerProcess,
    spLaunchPath,
    spConcurrentExecutions,
    spParameters,

    -- ** GameServerGroupAutoScalingPolicy
    GameServerGroupAutoScalingPolicy (..),
    mkGameServerGroupAutoScalingPolicy,
    gsgaspTargetTrackingConfiguration,
    gsgaspEstimatedInstanceWarmup,

    -- ** GameSessionQueueNameOrArn
    GameSessionQueueNameOrArn (..),

    -- ** GameSessionQueueName
    GameSessionQueueName (..),

    -- ** ResourceCreationLimitPolicy
    ResourceCreationLimitPolicy (..),
    mkResourceCreationLimitPolicy,
    rclpNewGameSessionsPerCreator,
    rclpPolicyPeriodInMinutes,

    -- ** EventCode
    EventCode (..),

    -- ** VpcPeeringConnection
    VpcPeeringConnection (..),
    mkVpcPeeringConnection,
    vpcFleetArn,
    vpcFleetId,
    vpcGameLiftVpcId,
    vpcIpV4CidrBlock,
    vpcPeerVpcId,
    vpcStatus,
    vpcVpcPeeringConnectionId,

    -- ** FleetAttributes
    FleetAttributes (..),
    mkFleetAttributes,
    faBuildArn,
    faBuildId,
    faCertificateConfiguration,
    faCreationTime,
    faDescription,
    faFleetArn,
    faFleetId,
    faFleetType,
    faInstanceRoleArn,
    faInstanceType,
    faLogPaths,
    faMetricGroups,
    faName,
    faNewGameSessionProtectionPolicy,
    faOperatingSystem,
    faResourceCreationLimitPolicy,
    faScriptArn,
    faScriptId,
    faServerLaunchParameters,
    faServerLaunchPath,
    faStatus,
    faStoppedActions,
    faTerminationTime,

    -- ** GameServerGroupArn
    GameServerGroupArn (..),

    -- ** GameSessionQueue
    GameSessionQueue (..),
    mkGameSessionQueue,
    gsqDestinations,
    gsqGameSessionQueueArn,
    gsqName,
    gsqPlayerLatencyPolicies,
    gsqTimeoutInSeconds,

    -- ** GameServerHealthCheck
    GameServerHealthCheck (..),

    -- ** Instance
    Instance (..),
    mkInstance,
    iCreationTime,
    iDnsName,
    iFleetId,
    iInstanceId,
    iIpAddress,
    iOperatingSystem,
    iStatus,
    iType,

    -- ** TargetConfiguration
    TargetConfiguration (..),
    mkTargetConfiguration,
    tcTargetValue,

    -- ** ComparisonOperatorType
    ComparisonOperatorType (..),

    -- ** GameProperty
    GameProperty (..),
    mkGameProperty,
    gpKey,
    gpValue,

    -- ** Message
    Message (..),

    -- ** Name
    Name (..),

    -- ** Description
    Description (..),

    -- ** InstanceRoleArn
    InstanceRoleArn (..),

    -- ** PeerVpcAwsAccountId
    PeerVpcAwsAccountId (..),

    -- ** PeerVpcId
    PeerVpcId (..),

    -- ** ServerLaunchParameters
    ServerLaunchParameters (..),

    -- ** ServerLaunchPath
    ServerLaunchPath (..),

    -- ** NextToken
    NextToken (..),

    -- ** RuleSetName
    RuleSetName (..),

    -- ** ConfigurationArn
    ConfigurationArn (..),

    -- ** NotificationTarget
    NotificationTarget (..),

    -- ** RuleSetArn
    RuleSetArn (..),

    -- ** GameSessionId
    GameSessionId (..),

    -- ** EventId
    EventId (..),

    -- ** PreSignedLogUrl
    PreSignedLogUrl (..),

    -- ** ResourceId
    ResourceId (..),

    -- ** Version
    Version (..),

    -- ** GameSessionName
    GameSessionName (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** ConfigurationName
    ConfigurationName (..),

    -- ** TicketId
    TicketId (..),

    -- ** DestinationArn
    DestinationArn (..),

    -- ** GameSessionArn
    GameSessionArn (..),

    -- ** ConnectionInfo
    ConnectionInfo (..),

    -- ** ResourceARN
    ResourceARN (..),

    -- ** Secret
    Secret (..),

    -- ** UserName
    UserName (..),

    -- ** IpRange
    IpRange (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import Network.AWS.GameLift.CreateVpcPeeringAuthorization
import Network.AWS.GameLift.CreateVpcPeeringConnection
import Network.AWS.GameLift.DeleteAlias
import Network.AWS.GameLift.DeleteBuild
import Network.AWS.GameLift.DeleteFleet
import Network.AWS.GameLift.DeleteGameServerGroup
import Network.AWS.GameLift.DeleteGameSessionQueue
import Network.AWS.GameLift.DeleteMatchmakingConfiguration
import Network.AWS.GameLift.DeleteMatchmakingRuleSet
import Network.AWS.GameLift.DeleteScalingPolicy
import Network.AWS.GameLift.DeleteScript
import Network.AWS.GameLift.DeleteVpcPeeringAuthorization
import Network.AWS.GameLift.DeleteVpcPeeringConnection
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
import Network.AWS.GameLift.DescribeVpcPeeringAuthorizations
import Network.AWS.GameLift.DescribeVpcPeeringConnections
import Network.AWS.GameLift.GetGameSessionLogUrl
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
import qualified Network.AWS.Prelude as Lude

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
