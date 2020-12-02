{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types
  ( -- * Service Configuration
    gameLift,

    -- * Errors

    -- * AcceptanceType
    AcceptanceType (..),

    -- * BackfillMode
    BackfillMode (..),

    -- * BalancingStrategy
    BalancingStrategy (..),

    -- * BuildStatus
    BuildStatus (..),

    -- * CertificateType
    CertificateType (..),

    -- * ComparisonOperatorType
    ComparisonOperatorType (..),

    -- * EC2InstanceType
    EC2InstanceType (..),

    -- * EventCode
    EventCode (..),

    -- * FleetAction
    FleetAction (..),

    -- * FleetStatus
    FleetStatus (..),

    -- * FleetType
    FleetType (..),

    -- * FlexMatchMode
    FlexMatchMode (..),

    -- * GameServerClaimStatus
    GameServerClaimStatus (..),

    -- * GameServerGroupAction
    GameServerGroupAction (..),

    -- * GameServerGroupDeleteOption
    GameServerGroupDeleteOption (..),

    -- * GameServerGroupInstanceType
    GameServerGroupInstanceType (..),

    -- * GameServerGroupStatus
    GameServerGroupStatus (..),

    -- * GameServerHealthCheck
    GameServerHealthCheck (..),

    -- * GameServerInstanceStatus
    GameServerInstanceStatus (..),

    -- * GameServerProtectionPolicy
    GameServerProtectionPolicy (..),

    -- * GameServerUtilizationStatus
    GameServerUtilizationStatus (..),

    -- * GameSessionPlacementState
    GameSessionPlacementState (..),

    -- * GameSessionStatus
    GameSessionStatus (..),

    -- * GameSessionStatusReason
    GameSessionStatusReason (..),

    -- * IPProtocol
    IPProtocol (..),

    -- * InstanceStatus
    InstanceStatus (..),

    -- * MatchmakingConfigurationStatus
    MatchmakingConfigurationStatus (..),

    -- * MetricName
    MetricName (..),

    -- * OperatingSystem
    OperatingSystem (..),

    -- * PlayerSessionCreationPolicy
    PlayerSessionCreationPolicy (..),

    -- * PlayerSessionStatus
    PlayerSessionStatus (..),

    -- * PolicyType
    PolicyType (..),

    -- * ProtectionPolicy
    ProtectionPolicy (..),

    -- * RoutingStrategyType
    RoutingStrategyType (..),

    -- * ScalingAdjustmentType
    ScalingAdjustmentType (..),

    -- * ScalingStatusType
    ScalingStatusType (..),

    -- * SortOrder
    SortOrder (..),

    -- * AWSCredentials
    AWSCredentials,
    awsCredentials,
    acSecretAccessKey,
    acSessionToken,
    acAccessKeyId,

    -- * Alias
    Alias,
    alias,
    aCreationTime,
    aLastUpdatedTime,
    aAliasId,
    aRoutingStrategy,
    aName,
    aAliasARN,
    aDescription,

    -- * AttributeValue
    AttributeValue,
    attributeValue,
    avSL,
    avSDM,
    avN,
    avS,

    -- * Build
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

    -- * CertificateConfiguration
    CertificateConfiguration,
    certificateConfiguration,
    ccCertificateType,

    -- * DesiredPlayerSession
    DesiredPlayerSession,
    desiredPlayerSession,
    dpsPlayerData,
    dpsPlayerId,

    -- * EC2InstanceCounts
    EC2InstanceCounts,
    ec2InstanceCounts,
    eicIdLE,
    eicTERMINATING,
    eicPENDING,
    eicMAXIMUM,
    eicDESIRED,
    eicMINIMUM,
    eicACTIVE,

    -- * EC2InstanceLimit
    EC2InstanceLimit,
    ec2InstanceLimit,
    eilEC2InstanceType,
    eilCurrentInstances,
    eilInstanceLimit,

    -- * Event
    Event,
    event,
    eResourceId,
    ePreSignedLogURL,
    eEventTime,
    eMessage,
    eEventCode,
    eEventId,

    -- * FleetAttributes
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

    -- * FleetCapacity
    FleetCapacity,
    fleetCapacity,
    fcInstanceType,
    fcFleetId,
    fcInstanceCounts,

    -- * FleetUtilization
    FleetUtilization,
    fleetUtilization,
    fuActiveGameSessionCount,
    fuMaximumPlayerSessionCount,
    fuCurrentPlayerSessionCount,
    fuFleetId,
    fuActiveServerProcessCount,

    -- * GameProperty
    GameProperty,
    gameProperty,
    gpKey,
    gpValue,

    -- * GameServer
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

    -- * GameServerGroup
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

    -- * GameServerGroupAutoScalingPolicy
    GameServerGroupAutoScalingPolicy,
    gameServerGroupAutoScalingPolicy,
    gsgaspEstimatedInstanceWarmup,
    gsgaspTargetTrackingConfiguration,

    -- * GameServerInstance
    GameServerInstance,
    gameServerInstance,
    gsiInstanceId,
    gsiGameServerGroupName,
    gsiInstanceStatus,
    gsiGameServerGroupARN,

    -- * GameSession
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

    -- * GameSessionConnectionInfo
    GameSessionConnectionInfo,
    gameSessionConnectionInfo,
    gsciMatchedPlayerSessions,
    gsciIPAddress,
    gsciGameSessionARN,
    gsciDNSName,
    gsciPort,

    -- * GameSessionDetail
    GameSessionDetail,
    gameSessionDetail,
    gsdGameSession,
    gsdProtectionPolicy,

    -- * GameSessionPlacement
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

    -- * GameSessionQueue
    GameSessionQueue,
    gameSessionQueue,
    gsqGameSessionQueueARN,
    gsqPlayerLatencyPolicies,
    gsqTimeoutInSeconds,
    gsqDestinations,
    gsqName,

    -- * GameSessionQueueDestination
    GameSessionQueueDestination,
    gameSessionQueueDestination,
    gsqdDestinationARN,

    -- * IPPermission
    IPPermission,
    ipPermission,
    ipFromPort,
    ipToPort,
    ipIPRange,
    ipProtocol,

    -- * Instance
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

    -- * InstanceAccess
    InstanceAccess,
    instanceAccess,
    iaInstanceId,
    iaIPAddress,
    iaOperatingSystem,
    iaCredentials,
    iaFleetId,

    -- * InstanceCredentials
    InstanceCredentials,
    instanceCredentials,
    icUserName,
    icSecret,

    -- * InstanceDefinition
    InstanceDefinition,
    instanceDefinition,
    idWeightedCapacity,
    idInstanceType,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification,
    launchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- * MatchedPlayerSession
    MatchedPlayerSession,
    matchedPlayerSession,
    mpsPlayerSessionId,
    mpsPlayerId,

    -- * MatchmakingConfiguration
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

    -- * MatchmakingRuleSet
    MatchmakingRuleSet,
    matchmakingRuleSet,
    mrsCreationTime,
    mrsRuleSetName,
    mrsRuleSetARN,
    mrsRuleSetBody,

    -- * MatchmakingTicket
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

    -- * PlacedPlayerSession
    PlacedPlayerSession,
    placedPlayerSession,
    ppsPlayerSessionId,
    ppsPlayerId,

    -- * Player
    Player,
    player,
    pPlayerAttributes,
    pTeam,
    pPlayerId,
    pLatencyInMs,

    -- * PlayerLatency
    PlayerLatency,
    playerLatency,
    plLatencyInMilliseconds,
    plRegionIdentifier,
    plPlayerId,

    -- * PlayerLatencyPolicy
    PlayerLatencyPolicy,
    playerLatencyPolicy,
    plpPolicyDurationSeconds,
    plpMaximumIndividualPlayerLatencyMilliseconds,

    -- * PlayerSession
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

    -- * ResourceCreationLimitPolicy
    ResourceCreationLimitPolicy,
    resourceCreationLimitPolicy,
    rclpNewGameSessionsPerCreator,
    rclpPolicyPeriodInMinutes,

    -- * RoutingStrategy
    RoutingStrategy,
    routingStrategy,
    rsType,
    rsMessage,
    rsFleetId,

    -- * RuntimeConfiguration
    RuntimeConfiguration,
    runtimeConfiguration,
    rcGameSessionActivationTimeoutSeconds,
    rcServerProcesses,
    rcMaxConcurrentGameSessionActivations,

    -- * S3Location
    S3Location,
    s3Location,
    slBucket,
    slKey,
    slObjectVersion,
    slRoleARN,

    -- * ScalingPolicy
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

    -- * Script
    Script,
    script,
    sCreationTime,
    sStorageLocation,
    sName,
    sScriptId,
    sVersion,
    sScriptARN,
    sSizeOnDisk,

    -- * ServerProcess
    ServerProcess,
    serverProcess,
    spParameters,
    spLaunchPath,
    spConcurrentExecutions,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TargetConfiguration
    TargetConfiguration,
    targetConfiguration,
    tcTargetValue,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration,
    targetTrackingConfiguration,
    ttcTargetValue,

    -- * VPCPeeringAuthorization
    VPCPeeringAuthorization,
    vpcPeeringAuthorization,
    vpaCreationTime,
    vpaPeerVPCId,
    vpaPeerVPCAWSAccountId,
    vpaGameLiftAWSAccountId,
    vpaExpirationTime,

    -- * VPCPeeringConnection
    VPCPeeringConnection,
    vpcPeeringConnection,
    vpcVPCPeeringConnectionId,
    vpcStatus,
    vpcPeerVPCId,
    vpcFleetARN,
    vpcIPV4CidrBlock,
    vpcGameLiftVPCId,
    vpcFleetId,

    -- * VPCPeeringConnectionStatus
    VPCPeeringConnectionStatus,
    vpcPeeringConnectionStatus,
    vpcsCode,
    vpcsMessage,
  )
where

import Network.AWS.GameLift.Types.AWSCredentials
import Network.AWS.GameLift.Types.AcceptanceType
import Network.AWS.GameLift.Types.Alias
import Network.AWS.GameLift.Types.AttributeValue
import Network.AWS.GameLift.Types.BackfillMode
import Network.AWS.GameLift.Types.BalancingStrategy
import Network.AWS.GameLift.Types.Build
import Network.AWS.GameLift.Types.BuildStatus
import Network.AWS.GameLift.Types.CertificateConfiguration
import Network.AWS.GameLift.Types.CertificateType
import Network.AWS.GameLift.Types.ComparisonOperatorType
import Network.AWS.GameLift.Types.DesiredPlayerSession
import Network.AWS.GameLift.Types.EC2InstanceCounts
import Network.AWS.GameLift.Types.EC2InstanceLimit
import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.GameLift.Types.Event
import Network.AWS.GameLift.Types.EventCode
import Network.AWS.GameLift.Types.FleetAction
import Network.AWS.GameLift.Types.FleetAttributes
import Network.AWS.GameLift.Types.FleetCapacity
import Network.AWS.GameLift.Types.FleetStatus
import Network.AWS.GameLift.Types.FleetType
import Network.AWS.GameLift.Types.FleetUtilization
import Network.AWS.GameLift.Types.FlexMatchMode
import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameServer
import Network.AWS.GameLift.Types.GameServerClaimStatus
import Network.AWS.GameLift.Types.GameServerGroup
import Network.AWS.GameLift.Types.GameServerGroupAction
import Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
import Network.AWS.GameLift.Types.GameServerGroupDeleteOption
import Network.AWS.GameLift.Types.GameServerGroupInstanceType
import Network.AWS.GameLift.Types.GameServerGroupStatus
import Network.AWS.GameLift.Types.GameServerHealthCheck
import Network.AWS.GameLift.Types.GameServerInstance
import Network.AWS.GameLift.Types.GameServerInstanceStatus
import Network.AWS.GameLift.Types.GameServerProtectionPolicy
import Network.AWS.GameLift.Types.GameServerUtilizationStatus
import Network.AWS.GameLift.Types.GameSession
import Network.AWS.GameLift.Types.GameSessionConnectionInfo
import Network.AWS.GameLift.Types.GameSessionDetail
import Network.AWS.GameLift.Types.GameSessionPlacement
import Network.AWS.GameLift.Types.GameSessionPlacementState
import Network.AWS.GameLift.Types.GameSessionQueue
import Network.AWS.GameLift.Types.GameSessionQueueDestination
import Network.AWS.GameLift.Types.GameSessionStatus
import Network.AWS.GameLift.Types.GameSessionStatusReason
import Network.AWS.GameLift.Types.IPPermission
import Network.AWS.GameLift.Types.IPProtocol
import Network.AWS.GameLift.Types.Instance
import Network.AWS.GameLift.Types.InstanceAccess
import Network.AWS.GameLift.Types.InstanceCredentials
import Network.AWS.GameLift.Types.InstanceDefinition
import Network.AWS.GameLift.Types.InstanceStatus
import Network.AWS.GameLift.Types.LaunchTemplateSpecification
import Network.AWS.GameLift.Types.MatchedPlayerSession
import Network.AWS.GameLift.Types.MatchmakingConfiguration
import Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
import Network.AWS.GameLift.Types.MatchmakingRuleSet
import Network.AWS.GameLift.Types.MatchmakingTicket
import Network.AWS.GameLift.Types.MetricName
import Network.AWS.GameLift.Types.OperatingSystem
import Network.AWS.GameLift.Types.PlacedPlayerSession
import Network.AWS.GameLift.Types.Player
import Network.AWS.GameLift.Types.PlayerLatency
import Network.AWS.GameLift.Types.PlayerLatencyPolicy
import Network.AWS.GameLift.Types.PlayerSession
import Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
import Network.AWS.GameLift.Types.PlayerSessionStatus
import Network.AWS.GameLift.Types.PolicyType
import Network.AWS.GameLift.Types.ProtectionPolicy
import Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
import Network.AWS.GameLift.Types.RoutingStrategy
import Network.AWS.GameLift.Types.RoutingStrategyType
import Network.AWS.GameLift.Types.RuntimeConfiguration
import Network.AWS.GameLift.Types.S3Location
import Network.AWS.GameLift.Types.ScalingAdjustmentType
import Network.AWS.GameLift.Types.ScalingPolicy
import Network.AWS.GameLift.Types.ScalingStatusType
import Network.AWS.GameLift.Types.Script
import Network.AWS.GameLift.Types.ServerProcess
import Network.AWS.GameLift.Types.SortOrder
import Network.AWS.GameLift.Types.Tag
import Network.AWS.GameLift.Types.TargetConfiguration
import Network.AWS.GameLift.Types.TargetTrackingConfiguration
import Network.AWS.GameLift.Types.VPCPeeringAuthorization
import Network.AWS.GameLift.Types.VPCPeeringConnection
import Network.AWS.GameLift.Types.VPCPeeringConnectionStatus
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-10-01@ of the Amazon GameLift SDK configuration.
gameLift :: Service
gameLift =
  Service
    { _svcAbbrev = "GameLift",
      _svcSigner = v4,
      _svcPrefix = "gamelift",
      _svcVersion = "2015-10-01",
      _svcEndpoint = defaultEndpoint gameLift,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "GameLift",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
