-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types
  ( -- * Service configuration
    gameLiftService,

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
    AWSCredentials (..),
    mkAWSCredentials,
    acSecretAccessKey,
    acSessionToken,
    acAccessKeyId,

    -- * Alias
    Alias (..),
    mkAlias,
    aCreationTime,
    aLastUpdatedTime,
    aAliasId,
    aRoutingStrategy,
    aName,
    aAliasARN,
    aDescription,

    -- * AttributeValue
    AttributeValue (..),
    mkAttributeValue,
    avSL,
    avSDM,
    avN,
    avS,

    -- * Build
    Build (..),
    mkBuild,
    bCreationTime,
    bStatus,
    bOperatingSystem,
    bBuildId,
    bName,
    bVersion,
    bBuildARN,
    bSizeOnDisk,

    -- * CertificateConfiguration
    CertificateConfiguration (..),
    mkCertificateConfiguration,
    ccCertificateType,

    -- * DesiredPlayerSession
    DesiredPlayerSession (..),
    mkDesiredPlayerSession,
    dpsPlayerData,
    dpsPlayerId,

    -- * EC2InstanceCounts
    EC2InstanceCounts (..),
    mkEC2InstanceCounts,
    eicIdLE,
    eicTERMINATING,
    eicPENDING,
    eicMAXIMUM,
    eicDESIRED,
    eicMINIMUM,
    eicACTIVE,

    -- * EC2InstanceLimit
    EC2InstanceLimit (..),
    mkEC2InstanceLimit,
    eilEC2InstanceType,
    eilCurrentInstances,
    eilInstanceLimit,

    -- * Event
    Event (..),
    mkEvent,
    eResourceId,
    ePreSignedLogURL,
    eEventTime,
    eMessage,
    eEventCode,
    eEventId,

    -- * FleetAttributes
    FleetAttributes (..),
    mkFleetAttributes,
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
    FleetCapacity (..),
    mkFleetCapacity,
    fcInstanceType,
    fcFleetId,
    fcInstanceCounts,

    -- * FleetUtilization
    FleetUtilization (..),
    mkFleetUtilization,
    fuActiveGameSessionCount,
    fuMaximumPlayerSessionCount,
    fuCurrentPlayerSessionCount,
    fuFleetId,
    fuActiveServerProcessCount,

    -- * GameProperty
    GameProperty (..),
    mkGameProperty,
    gpKey,
    gpValue,

    -- * GameServer
    GameServer (..),
    mkGameServer,
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
    GameServerGroup (..),
    mkGameServerGroup,
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
    GameServerGroupAutoScalingPolicy (..),
    mkGameServerGroupAutoScalingPolicy,
    gsgaspEstimatedInstanceWarmup,
    gsgaspTargetTrackingConfiguration,

    -- * GameServerInstance
    GameServerInstance (..),
    mkGameServerInstance,
    gsiInstanceId,
    gsiGameServerGroupName,
    gsiInstanceStatus,
    gsiGameServerGroupARN,

    -- * GameSession
    GameSession (..),
    mkGameSession,
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
    GameSessionConnectionInfo (..),
    mkGameSessionConnectionInfo,
    gsciMatchedPlayerSessions,
    gsciIPAddress,
    gsciGameSessionARN,
    gsciDNSName,
    gsciPort,

    -- * GameSessionDetail
    GameSessionDetail (..),
    mkGameSessionDetail,
    gsdGameSession,
    gsdProtectionPolicy,

    -- * GameSessionPlacement
    GameSessionPlacement (..),
    mkGameSessionPlacement,
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
    GameSessionQueue (..),
    mkGameSessionQueue,
    gsqGameSessionQueueARN,
    gsqPlayerLatencyPolicies,
    gsqTimeoutInSeconds,
    gsqDestinations,
    gsqName,

    -- * GameSessionQueueDestination
    GameSessionQueueDestination (..),
    mkGameSessionQueueDestination,
    gsqdDestinationARN,

    -- * IPPermission
    IPPermission (..),
    mkIPPermission,
    ipFromPort,
    ipToPort,
    ipIPRange,
    ipProtocol,

    -- * Instance
    Instance (..),
    mkInstance,
    iCreationTime,
    iInstanceId,
    iStatus,
    iIPAddress,
    iOperatingSystem,
    iType,
    iFleetId,
    iDNSName,

    -- * InstanceAccess
    InstanceAccess (..),
    mkInstanceAccess,
    iaInstanceId,
    iaIPAddress,
    iaOperatingSystem,
    iaCredentials,
    iaFleetId,

    -- * InstanceCredentials
    InstanceCredentials (..),
    mkInstanceCredentials,
    icUserName,
    icSecret,

    -- * InstanceDefinition
    InstanceDefinition (..),
    mkInstanceDefinition,
    idWeightedCapacity,
    idInstanceType,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- * MatchedPlayerSession
    MatchedPlayerSession (..),
    mkMatchedPlayerSession,
    mpsPlayerSessionId,
    mpsPlayerId,

    -- * MatchmakingConfiguration
    MatchmakingConfiguration (..),
    mkMatchmakingConfiguration,
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
    MatchmakingRuleSet (..),
    mkMatchmakingRuleSet,
    mrsCreationTime,
    mrsRuleSetName,
    mrsRuleSetARN,
    mrsRuleSetBody,

    -- * MatchmakingTicket
    MatchmakingTicket (..),
    mkMatchmakingTicket,
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
    PlacedPlayerSession (..),
    mkPlacedPlayerSession,
    ppsPlayerSessionId,
    ppsPlayerId,

    -- * Player
    Player (..),
    mkPlayer,
    pPlayerAttributes,
    pTeam,
    pPlayerId,
    pLatencyInMs,

    -- * PlayerLatency
    PlayerLatency (..),
    mkPlayerLatency,
    plLatencyInMilliseconds,
    plRegionIdentifier,
    plPlayerId,

    -- * PlayerLatencyPolicy
    PlayerLatencyPolicy (..),
    mkPlayerLatencyPolicy,
    plpPolicyDurationSeconds,
    plpMaximumIndividualPlayerLatencyMilliseconds,

    -- * PlayerSession
    PlayerSession (..),
    mkPlayerSession,
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
    ResourceCreationLimitPolicy (..),
    mkResourceCreationLimitPolicy,
    rclpNewGameSessionsPerCreator,
    rclpPolicyPeriodInMinutes,

    -- * RoutingStrategy
    RoutingStrategy (..),
    mkRoutingStrategy,
    rsType,
    rsMessage,
    rsFleetId,

    -- * RuntimeConfiguration
    RuntimeConfiguration (..),
    mkRuntimeConfiguration,
    rcGameSessionActivationTimeoutSeconds,
    rcServerProcesses,
    rcMaxConcurrentGameSessionActivations,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,
    slObjectVersion,
    slRoleARN,

    -- * ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
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
    Script (..),
    mkScript,
    sCreationTime,
    sStorageLocation,
    sName,
    sScriptId,
    sVersion,
    sScriptARN,
    sSizeOnDisk,

    -- * ServerProcess
    ServerProcess (..),
    mkServerProcess,
    spParameters,
    spLaunchPath,
    spConcurrentExecutions,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TargetConfiguration
    TargetConfiguration (..),
    mkTargetConfiguration,
    tcTargetValue,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcTargetValue,

    -- * VPCPeeringAuthorization
    VPCPeeringAuthorization (..),
    mkVPCPeeringAuthorization,
    vpaCreationTime,
    vpaPeerVPCId,
    vpaPeerVPCAWSAccountId,
    vpaGameLiftAWSAccountId,
    vpaExpirationTime,

    -- * VPCPeeringConnection
    VPCPeeringConnection (..),
    mkVPCPeeringConnection,
    vpcVPCPeeringConnectionId,
    vpcStatus,
    vpcPeerVPCId,
    vpcFleetARN,
    vpcIPV4CidrBlock,
    vpcGameLiftVPCId,
    vpcFleetId,

    -- * VPCPeeringConnectionStatus
    VPCPeeringConnectionStatus (..),
    mkVPCPeeringConnectionStatus,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-10-01@ of the Amazon GameLift SDK configuration.
gameLiftService :: Lude.Service
gameLiftService =
  Lude.Service
    { Lude._svcAbbrev = "GameLift",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "gamelift",
      Lude._svcVersion = "2015-10-01",
      Lude._svcEndpoint = Lude.defaultEndpoint gameLiftService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "GameLift",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
