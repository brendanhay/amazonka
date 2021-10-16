{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _OutOfCapacityException,
    _NotFoundException,
    _TaggingFailedException,
    _FleetCapacityExceededException,
    _UnauthorizedException,
    _InternalServiceException,
    _InvalidGameSessionStatusException,
    _InvalidRequestException,
    _TerminalRoutingStrategyException,
    _GameSessionFullException,
    _InvalidFleetStatusException,
    _ConflictException,
    _LimitExceededException,
    _IdempotentParameterMismatchException,
    _UnsupportedRegionException,

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

    -- * InstanceStatus
    InstanceStatus (..),

    -- * IpProtocol
    IpProtocol (..),

    -- * LocationUpdateStatus
    LocationUpdateStatus (..),

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

    -- * PriorityType
    PriorityType (..),

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

    -- * Alias
    Alias (..),
    newAlias,
    alias_routingStrategy,
    alias_creationTime,
    alias_name,
    alias_description,
    alias_aliasArn,
    alias_aliasId,
    alias_lastUpdatedTime,

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_sl,
    attributeValue_n,
    attributeValue_s,
    attributeValue_sdm,

    -- * AwsCredentials
    AwsCredentials (..),
    newAwsCredentials,
    awsCredentials_secretAccessKey,
    awsCredentials_accessKeyId,
    awsCredentials_sessionToken,

    -- * Build
    Build (..),
    newBuild,
    build_creationTime,
    build_status,
    build_version,
    build_name,
    build_sizeOnDisk,
    build_buildId,
    build_buildArn,
    build_operatingSystem,

    -- * CertificateConfiguration
    CertificateConfiguration (..),
    newCertificateConfiguration,
    certificateConfiguration_certificateType,

    -- * DesiredPlayerSession
    DesiredPlayerSession (..),
    newDesiredPlayerSession,
    desiredPlayerSession_playerId,
    desiredPlayerSession_playerData,

    -- * EC2InstanceCounts
    EC2InstanceCounts (..),
    newEC2InstanceCounts,
    eC2InstanceCounts_idle,
    eC2InstanceCounts_minimum,
    eC2InstanceCounts_pending,
    eC2InstanceCounts_terminating,
    eC2InstanceCounts_active,
    eC2InstanceCounts_maximum,
    eC2InstanceCounts_desired,

    -- * EC2InstanceLimit
    EC2InstanceLimit (..),
    newEC2InstanceLimit,
    eC2InstanceLimit_instanceLimit,
    eC2InstanceLimit_currentInstances,
    eC2InstanceLimit_eC2InstanceType,
    eC2InstanceLimit_location,

    -- * Event
    Event (..),
    newEvent,
    event_resourceId,
    event_eventCode,
    event_eventId,
    event_message,
    event_eventTime,
    event_preSignedLogUrl,

    -- * FilterConfiguration
    FilterConfiguration (..),
    newFilterConfiguration,
    filterConfiguration_allowedLocations,

    -- * FleetAttributes
    FleetAttributes (..),
    newFleetAttributes,
    fleetAttributes_creationTime,
    fleetAttributes_status,
    fleetAttributes_instanceType,
    fleetAttributes_fleetType,
    fleetAttributes_fleetId,
    fleetAttributes_fleetArn,
    fleetAttributes_instanceRoleArn,
    fleetAttributes_certificateConfiguration,
    fleetAttributes_serverLaunchPath,
    fleetAttributes_logPaths,
    fleetAttributes_serverLaunchParameters,
    fleetAttributes_scriptArn,
    fleetAttributes_name,
    fleetAttributes_newGameSessionProtectionPolicy,
    fleetAttributes_stoppedActions,
    fleetAttributes_terminationTime,
    fleetAttributes_resourceCreationLimitPolicy,
    fleetAttributes_description,
    fleetAttributes_buildId,
    fleetAttributes_buildArn,
    fleetAttributes_metricGroups,
    fleetAttributes_operatingSystem,
    fleetAttributes_scriptId,

    -- * FleetCapacity
    FleetCapacity (..),
    newFleetCapacity,
    fleetCapacity_instanceType,
    fleetCapacity_fleetId,
    fleetCapacity_fleetArn,
    fleetCapacity_instanceCounts,
    fleetCapacity_location,

    -- * FleetUtilization
    FleetUtilization (..),
    newFleetUtilization,
    fleetUtilization_activeGameSessionCount,
    fleetUtilization_currentPlayerSessionCount,
    fleetUtilization_maximumPlayerSessionCount,
    fleetUtilization_fleetId,
    fleetUtilization_fleetArn,
    fleetUtilization_activeServerProcessCount,
    fleetUtilization_location,

    -- * GameProperty
    GameProperty (..),
    newGameProperty,
    gameProperty_key,
    gameProperty_value,

    -- * GameServer
    GameServer (..),
    newGameServer,
    gameServer_instanceId,
    gameServer_utilizationStatus,
    gameServer_claimStatus,
    gameServer_gameServerData,
    gameServer_lastClaimTime,
    gameServer_registrationTime,
    gameServer_gameServerGroupArn,
    gameServer_gameServerId,
    gameServer_gameServerGroupName,
    gameServer_connectionInfo,
    gameServer_lastHealthCheckTime,

    -- * GameServerGroup
    GameServerGroup (..),
    newGameServerGroup,
    gameServerGroup_creationTime,
    gameServerGroup_status,
    gameServerGroup_roleArn,
    gameServerGroup_autoScalingGroupArn,
    gameServerGroup_instanceDefinitions,
    gameServerGroup_gameServerGroupArn,
    gameServerGroup_suspendedActions,
    gameServerGroup_gameServerGroupName,
    gameServerGroup_balancingStrategy,
    gameServerGroup_statusReason,
    gameServerGroup_gameServerProtectionPolicy,
    gameServerGroup_lastUpdatedTime,

    -- * GameServerGroupAutoScalingPolicy
    GameServerGroupAutoScalingPolicy (..),
    newGameServerGroupAutoScalingPolicy,
    gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup,
    gameServerGroupAutoScalingPolicy_targetTrackingConfiguration,

    -- * GameServerInstance
    GameServerInstance (..),
    newGameServerInstance,
    gameServerInstance_instanceId,
    gameServerInstance_instanceStatus,
    gameServerInstance_gameServerGroupArn,
    gameServerInstance_gameServerGroupName,

    -- * GameSession
    GameSession (..),
    newGameSession,
    gameSession_currentPlayerSessionCount,
    gameSession_gameProperties,
    gameSession_creationTime,
    gameSession_status,
    gameSession_playerSessionCreationPolicy,
    gameSession_creatorId,
    gameSession_maximumPlayerSessionCount,
    gameSession_matchmakerData,
    gameSession_fleetId,
    gameSession_fleetArn,
    gameSession_gameSessionData,
    gameSession_gameSessionId,
    gameSession_name,
    gameSession_ipAddress,
    gameSession_terminationTime,
    gameSession_port,
    gameSession_dnsName,
    gameSession_statusReason,
    gameSession_location,

    -- * GameSessionConnectionInfo
    GameSessionConnectionInfo (..),
    newGameSessionConnectionInfo,
    gameSessionConnectionInfo_gameSessionArn,
    gameSessionConnectionInfo_ipAddress,
    gameSessionConnectionInfo_matchedPlayerSessions,
    gameSessionConnectionInfo_port,
    gameSessionConnectionInfo_dnsName,

    -- * GameSessionDetail
    GameSessionDetail (..),
    newGameSessionDetail,
    gameSessionDetail_gameSession,
    gameSessionDetail_protectionPolicy,

    -- * GameSessionPlacement
    GameSessionPlacement (..),
    newGameSessionPlacement,
    gameSessionPlacement_gameProperties,
    gameSessionPlacement_status,
    gameSessionPlacement_gameSessionQueueName,
    gameSessionPlacement_maximumPlayerSessionCount,
    gameSessionPlacement_matchmakerData,
    gameSessionPlacement_gameSessionData,
    gameSessionPlacement_gameSessionId,
    gameSessionPlacement_startTime,
    gameSessionPlacement_gameSessionArn,
    gameSessionPlacement_endTime,
    gameSessionPlacement_gameSessionName,
    gameSessionPlacement_ipAddress,
    gameSessionPlacement_placementId,
    gameSessionPlacement_placedPlayerSessions,
    gameSessionPlacement_port,
    gameSessionPlacement_dnsName,
    gameSessionPlacement_gameSessionRegion,
    gameSessionPlacement_playerLatencies,

    -- * GameSessionQueue
    GameSessionQueue (..),
    newGameSessionQueue,
    gameSessionQueue_customEventData,
    gameSessionQueue_playerLatencyPolicies,
    gameSessionQueue_priorityConfiguration,
    gameSessionQueue_name,
    gameSessionQueue_destinations,
    gameSessionQueue_timeoutInSeconds,
    gameSessionQueue_gameSessionQueueArn,
    gameSessionQueue_notificationTarget,
    gameSessionQueue_filterConfiguration,

    -- * GameSessionQueueDestination
    GameSessionQueueDestination (..),
    newGameSessionQueueDestination,
    gameSessionQueueDestination_destinationArn,

    -- * Instance
    Instance (..),
    newInstance,
    instance_creationTime,
    instance_status,
    instance_instanceId,
    instance_fleetId,
    instance_fleetArn,
    instance_ipAddress,
    instance_dnsName,
    instance_operatingSystem,
    instance_type,
    instance_location,

    -- * InstanceAccess
    InstanceAccess (..),
    newInstanceAccess,
    instanceAccess_instanceId,
    instanceAccess_fleetId,
    instanceAccess_ipAddress,
    instanceAccess_operatingSystem,
    instanceAccess_credentials,

    -- * InstanceCredentials
    InstanceCredentials (..),
    newInstanceCredentials,
    instanceCredentials_userName,
    instanceCredentials_secret,

    -- * InstanceDefinition
    InstanceDefinition (..),
    newInstanceDefinition,
    instanceDefinition_weightedCapacity,
    instanceDefinition_instanceType,

    -- * IpPermission
    IpPermission (..),
    newIpPermission,
    ipPermission_fromPort,
    ipPermission_toPort,
    ipPermission_ipRange,
    ipPermission_protocol,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,
    launchTemplateSpecification_launchTemplateName,

    -- * LocationAttributes
    LocationAttributes (..),
    newLocationAttributes,
    locationAttributes_updateStatus,
    locationAttributes_locationState,
    locationAttributes_stoppedActions,

    -- * LocationConfiguration
    LocationConfiguration (..),
    newLocationConfiguration,
    locationConfiguration_location,

    -- * LocationState
    LocationState (..),
    newLocationState,
    locationState_status,
    locationState_location,

    -- * MatchedPlayerSession
    MatchedPlayerSession (..),
    newMatchedPlayerSession,
    matchedPlayerSession_playerId,
    matchedPlayerSession_playerSessionId,

    -- * MatchmakingConfiguration
    MatchmakingConfiguration (..),
    newMatchmakingConfiguration,
    matchmakingConfiguration_flexMatchMode,
    matchmakingConfiguration_customEventData,
    matchmakingConfiguration_backfillMode,
    matchmakingConfiguration_gameProperties,
    matchmakingConfiguration_creationTime,
    matchmakingConfiguration_acceptanceTimeoutSeconds,
    matchmakingConfiguration_additionalPlayerCount,
    matchmakingConfiguration_gameSessionData,
    matchmakingConfiguration_configurationArn,
    matchmakingConfiguration_name,
    matchmakingConfiguration_gameSessionQueueArns,
    matchmakingConfiguration_notificationTarget,
    matchmakingConfiguration_ruleSetArn,
    matchmakingConfiguration_requestTimeoutSeconds,
    matchmakingConfiguration_description,
    matchmakingConfiguration_ruleSetName,
    matchmakingConfiguration_acceptanceRequired,

    -- * MatchmakingRuleSet
    MatchmakingRuleSet (..),
    newMatchmakingRuleSet,
    matchmakingRuleSet_creationTime,
    matchmakingRuleSet_ruleSetArn,
    matchmakingRuleSet_ruleSetName,
    matchmakingRuleSet_ruleSetBody,

    -- * MatchmakingTicket
    MatchmakingTicket (..),
    newMatchmakingTicket,
    matchmakingTicket_statusMessage,
    matchmakingTicket_status,
    matchmakingTicket_estimatedWaitTime,
    matchmakingTicket_ticketId,
    matchmakingTicket_players,
    matchmakingTicket_configurationArn,
    matchmakingTicket_startTime,
    matchmakingTicket_endTime,
    matchmakingTicket_configurationName,
    matchmakingTicket_gameSessionConnectionInfo,
    matchmakingTicket_statusReason,

    -- * PlacedPlayerSession
    PlacedPlayerSession (..),
    newPlacedPlayerSession,
    placedPlayerSession_playerId,
    placedPlayerSession_playerSessionId,

    -- * Player
    Player (..),
    newPlayer,
    player_playerAttributes,
    player_latencyInMs,
    player_playerId,
    player_team,

    -- * PlayerLatency
    PlayerLatency (..),
    newPlayerLatency,
    playerLatency_playerId,
    playerLatency_latencyInMilliseconds,
    playerLatency_regionIdentifier,

    -- * PlayerLatencyPolicy
    PlayerLatencyPolicy (..),
    newPlayerLatencyPolicy,
    playerLatencyPolicy_policyDurationSeconds,
    playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds,

    -- * PlayerSession
    PlayerSession (..),
    newPlayerSession,
    playerSession_creationTime,
    playerSession_status,
    playerSession_playerId,
    playerSession_fleetId,
    playerSession_fleetArn,
    playerSession_playerSessionId,
    playerSession_gameSessionId,
    playerSession_ipAddress,
    playerSession_terminationTime,
    playerSession_port,
    playerSession_dnsName,
    playerSession_playerData,

    -- * PriorityConfiguration
    PriorityConfiguration (..),
    newPriorityConfiguration,
    priorityConfiguration_locationOrder,
    priorityConfiguration_priorityOrder,

    -- * ResourceCreationLimitPolicy
    ResourceCreationLimitPolicy (..),
    newResourceCreationLimitPolicy,
    resourceCreationLimitPolicy_policyPeriodInMinutes,
    resourceCreationLimitPolicy_newGameSessionsPerCreator,

    -- * RoutingStrategy
    RoutingStrategy (..),
    newRoutingStrategy,
    routingStrategy_fleetId,
    routingStrategy_message,
    routingStrategy_type,

    -- * RuntimeConfiguration
    RuntimeConfiguration (..),
    newRuntimeConfiguration,
    runtimeConfiguration_gameSessionActivationTimeoutSeconds,
    runtimeConfiguration_maxConcurrentGameSessionActivations,
    runtimeConfiguration_serverProcesses,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_key,
    s3Location_objectVersion,
    s3Location_roleArn,
    s3Location_bucket,

    -- * ScalingPolicy
    ScalingPolicy (..),
    newScalingPolicy,
    scalingPolicy_threshold,
    scalingPolicy_status,
    scalingPolicy_targetConfiguration,
    scalingPolicy_comparisonOperator,
    scalingPolicy_metricName,
    scalingPolicy_fleetId,
    scalingPolicy_fleetArn,
    scalingPolicy_policyType,
    scalingPolicy_updateStatus,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_name,
    scalingPolicy_evaluationPeriods,
    scalingPolicy_location,
    scalingPolicy_scalingAdjustmentType,

    -- * Script
    Script (..),
    newScript,
    script_creationTime,
    script_version,
    script_scriptArn,
    script_name,
    script_storageLocation,
    script_sizeOnDisk,
    script_scriptId,

    -- * ServerProcess
    ServerProcess (..),
    newServerProcess,
    serverProcess_parameters,
    serverProcess_launchPath,
    serverProcess_concurrentExecutions,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TargetConfiguration
    TargetConfiguration (..),
    newTargetConfiguration,
    targetConfiguration_targetValue,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    newTargetTrackingConfiguration,
    targetTrackingConfiguration_targetValue,

    -- * VpcPeeringAuthorization
    VpcPeeringAuthorization (..),
    newVpcPeeringAuthorization,
    vpcPeeringAuthorization_creationTime,
    vpcPeeringAuthorization_peerVpcAwsAccountId,
    vpcPeeringAuthorization_expirationTime,
    vpcPeeringAuthorization_gameLiftAwsAccountId,
    vpcPeeringAuthorization_peerVpcId,

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    newVpcPeeringConnection,
    vpcPeeringConnection_status,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_ipV4CidrBlock,
    vpcPeeringConnection_fleetId,
    vpcPeeringConnection_fleetArn,
    vpcPeeringConnection_gameLiftVpcId,
    vpcPeeringConnection_peerVpcId,

    -- * VpcPeeringConnectionStatus
    VpcPeeringConnectionStatus (..),
    newVpcPeeringConnectionStatus,
    vpcPeeringConnectionStatus_message,
    vpcPeeringConnectionStatus_code,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.AcceptanceType
import Network.AWS.GameLift.Types.Alias
import Network.AWS.GameLift.Types.AttributeValue
import Network.AWS.GameLift.Types.AwsCredentials
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
import Network.AWS.GameLift.Types.FilterConfiguration
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
import Network.AWS.GameLift.Types.Instance
import Network.AWS.GameLift.Types.InstanceAccess
import Network.AWS.GameLift.Types.InstanceCredentials
import Network.AWS.GameLift.Types.InstanceDefinition
import Network.AWS.GameLift.Types.InstanceStatus
import Network.AWS.GameLift.Types.IpPermission
import Network.AWS.GameLift.Types.IpProtocol
import Network.AWS.GameLift.Types.LaunchTemplateSpecification
import Network.AWS.GameLift.Types.LocationAttributes
import Network.AWS.GameLift.Types.LocationConfiguration
import Network.AWS.GameLift.Types.LocationState
import Network.AWS.GameLift.Types.LocationUpdateStatus
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
import Network.AWS.GameLift.Types.PriorityConfiguration
import Network.AWS.GameLift.Types.PriorityType
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
import Network.AWS.GameLift.Types.VpcPeeringAuthorization
import Network.AWS.GameLift.Types.VpcPeeringConnection
import Network.AWS.GameLift.Types.VpcPeeringConnectionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-10-01@ of the Amazon GameLift SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "GameLift",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "gamelift",
      Core._serviceSigningName = "gamelift",
      Core._serviceVersion = "2015-10-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "GameLift",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified game server group has no available game servers to fulfill
-- a @ClaimGameServer@ request. Clients can retry such requests immediately
-- or after a waiting period.
_OutOfCapacityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OutOfCapacityException =
  Core._MatchServiceError
    defaultService
    "OutOfCapacityException"

-- | A service resource associated with the request could not be found.
-- Clients should not retry such requests.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The requested tagging operation did not succeed. This may be due to
-- invalid tag format or the maximum tag limit may have been exceeded.
-- Resolve the issue before retrying.
_TaggingFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaggingFailedException =
  Core._MatchServiceError
    defaultService
    "TaggingFailedException"

-- | The specified fleet has no available instances to fulfill a
-- @CreateGameSession@ request. Clients can retry such requests immediately
-- or after a waiting period.
_FleetCapacityExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FleetCapacityExceededException =
  Core._MatchServiceError
    defaultService
    "FleetCapacityExceededException"

-- | The client failed authentication. Clients should not retry such
-- requests.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | The service encountered an unrecoverable internal failure while
-- processing the request. Clients can retry such requests immediately or
-- after a waiting period.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The requested operation would cause a conflict with the current state of
-- a resource associated with the request and\/or the game instance.
-- Resolve the conflict before retrying.
_InvalidGameSessionStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGameSessionStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidGameSessionStatusException"

-- | One or more parameter values in the request are invalid. Correct the
-- invalid parameter values before retrying.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The service is unable to resolve the routing for a particular alias
-- because it has a terminal RoutingStrategy associated with it. The
-- message returned in this exception is the message defined in the routing
-- strategy itself. Such requests should only be retried if the routing
-- strategy for the specified alias is modified.
_TerminalRoutingStrategyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TerminalRoutingStrategyException =
  Core._MatchServiceError
    defaultService
    "TerminalRoutingStrategyException"

-- | The game instance is currently full and cannot allow the requested
-- player(s) to join. Clients can retry such requests immediately or after
-- a waiting period.
_GameSessionFullException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GameSessionFullException =
  Core._MatchServiceError
    defaultService
    "GameSessionFullException"

-- | The requested operation would cause a conflict with the current state of
-- a resource associated with the request and\/or the fleet. Resolve the
-- conflict before retrying.
_InvalidFleetStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFleetStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidFleetStatusException"

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The requested operation would cause the resource to exceed the allowed
-- service limit. Resolve the issue before retrying.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | A game session with this custom ID string already exists in this fleet.
-- Resolve this conflict before retrying this request.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The requested operation is not supported in the Region specified.
_UnsupportedRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedRegionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedRegionException"
