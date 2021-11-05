{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _OutOfCapacityException,
    _InvalidFleetStatusException,
    _InvalidRequestException,
    _ConflictException,
    _TaggingFailedException,
    _TerminalRoutingStrategyException,
    _NotFoundException,
    _GameSessionFullException,
    _UnsupportedRegionException,
    _InvalidGameSessionStatusException,
    _InternalServiceException,
    _IdempotentParameterMismatchException,
    _UnauthorizedException,
    _FleetCapacityExceededException,
    _LimitExceededException,

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
    alias_creationTime,
    alias_lastUpdatedTime,
    alias_aliasId,
    alias_routingStrategy,
    alias_name,
    alias_aliasArn,
    alias_description,

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_sl,
    attributeValue_sdm,
    attributeValue_n,
    attributeValue_s,

    -- * AwsCredentials
    AwsCredentials (..),
    newAwsCredentials,
    awsCredentials_secretAccessKey,
    awsCredentials_sessionToken,
    awsCredentials_accessKeyId,

    -- * Build
    Build (..),
    newBuild,
    build_creationTime,
    build_status,
    build_operatingSystem,
    build_buildId,
    build_name,
    build_version,
    build_buildArn,
    build_sizeOnDisk,

    -- * CertificateConfiguration
    CertificateConfiguration (..),
    newCertificateConfiguration,
    certificateConfiguration_certificateType,

    -- * DesiredPlayerSession
    DesiredPlayerSession (..),
    newDesiredPlayerSession,
    desiredPlayerSession_playerData,
    desiredPlayerSession_playerId,

    -- * EC2InstanceCounts
    EC2InstanceCounts (..),
    newEC2InstanceCounts,
    eC2InstanceCounts_idle,
    eC2InstanceCounts_terminating,
    eC2InstanceCounts_pending,
    eC2InstanceCounts_maximum,
    eC2InstanceCounts_desired,
    eC2InstanceCounts_minimum,
    eC2InstanceCounts_active,

    -- * EC2InstanceLimit
    EC2InstanceLimit (..),
    newEC2InstanceLimit,
    eC2InstanceLimit_location,
    eC2InstanceLimit_eC2InstanceType,
    eC2InstanceLimit_currentInstances,
    eC2InstanceLimit_instanceLimit,

    -- * Event
    Event (..),
    newEvent,
    event_resourceId,
    event_preSignedLogUrl,
    event_eventTime,
    event_message,
    event_eventCode,
    event_eventId,

    -- * FilterConfiguration
    FilterConfiguration (..),
    newFilterConfiguration,
    filterConfiguration_allowedLocations,

    -- * FleetAttributes
    FleetAttributes (..),
    newFleetAttributes,
    fleetAttributes_creationTime,
    fleetAttributes_status,
    fleetAttributes_serverLaunchParameters,
    fleetAttributes_logPaths,
    fleetAttributes_operatingSystem,
    fleetAttributes_buildId,
    fleetAttributes_fleetArn,
    fleetAttributes_fleetType,
    fleetAttributes_terminationTime,
    fleetAttributes_instanceType,
    fleetAttributes_stoppedActions,
    fleetAttributes_newGameSessionProtectionPolicy,
    fleetAttributes_name,
    fleetAttributes_scriptId,
    fleetAttributes_scriptArn,
    fleetAttributes_certificateConfiguration,
    fleetAttributes_serverLaunchPath,
    fleetAttributes_instanceRoleArn,
    fleetAttributes_metricGroups,
    fleetAttributes_buildArn,
    fleetAttributes_fleetId,
    fleetAttributes_description,
    fleetAttributes_resourceCreationLimitPolicy,

    -- * FleetCapacity
    FleetCapacity (..),
    newFleetCapacity,
    fleetCapacity_location,
    fleetCapacity_fleetArn,
    fleetCapacity_instanceType,
    fleetCapacity_fleetId,
    fleetCapacity_instanceCounts,

    -- * FleetUtilization
    FleetUtilization (..),
    newFleetUtilization,
    fleetUtilization_activeGameSessionCount,
    fleetUtilization_location,
    fleetUtilization_fleetArn,
    fleetUtilization_maximumPlayerSessionCount,
    fleetUtilization_currentPlayerSessionCount,
    fleetUtilization_fleetId,
    fleetUtilization_activeServerProcessCount,

    -- * GameProperty
    GameProperty (..),
    newGameProperty,
    gameProperty_key,
    gameProperty_value,

    -- * GameServer
    GameServer (..),
    newGameServer,
    gameServer_instanceId,
    gameServer_lastClaimTime,
    gameServer_gameServerGroupName,
    gameServer_gameServerData,
    gameServer_claimStatus,
    gameServer_gameServerId,
    gameServer_utilizationStatus,
    gameServer_registrationTime,
    gameServer_lastHealthCheckTime,
    gameServer_connectionInfo,
    gameServer_gameServerGroupArn,

    -- * GameServerGroup
    GameServerGroup (..),
    newGameServerGroup,
    gameServerGroup_creationTime,
    gameServerGroup_status,
    gameServerGroup_instanceDefinitions,
    gameServerGroup_lastUpdatedTime,
    gameServerGroup_balancingStrategy,
    gameServerGroup_gameServerGroupName,
    gameServerGroup_suspendedActions,
    gameServerGroup_autoScalingGroupArn,
    gameServerGroup_statusReason,
    gameServerGroup_gameServerProtectionPolicy,
    gameServerGroup_gameServerGroupArn,
    gameServerGroup_roleArn,

    -- * GameServerGroupAutoScalingPolicy
    GameServerGroupAutoScalingPolicy (..),
    newGameServerGroupAutoScalingPolicy,
    gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup,
    gameServerGroupAutoScalingPolicy_targetTrackingConfiguration,

    -- * GameServerInstance
    GameServerInstance (..),
    newGameServerInstance,
    gameServerInstance_instanceId,
    gameServerInstance_gameServerGroupName,
    gameServerInstance_instanceStatus,
    gameServerInstance_gameServerGroupArn,

    -- * GameSession
    GameSession (..),
    newGameSession,
    gameSession_creationTime,
    gameSession_status,
    gameSession_gameProperties,
    gameSession_ipAddress,
    gameSession_location,
    gameSession_gameSessionId,
    gameSession_matchmakerData,
    gameSession_fleetArn,
    gameSession_maximumPlayerSessionCount,
    gameSession_terminationTime,
    gameSession_playerSessionCreationPolicy,
    gameSession_name,
    gameSession_currentPlayerSessionCount,
    gameSession_statusReason,
    gameSession_gameSessionData,
    gameSession_fleetId,
    gameSession_dnsName,
    gameSession_creatorId,
    gameSession_port,

    -- * GameSessionConnectionInfo
    GameSessionConnectionInfo (..),
    newGameSessionConnectionInfo,
    gameSessionConnectionInfo_matchedPlayerSessions,
    gameSessionConnectionInfo_ipAddress,
    gameSessionConnectionInfo_gameSessionArn,
    gameSessionConnectionInfo_dnsName,
    gameSessionConnectionInfo_port,

    -- * GameSessionDetail
    GameSessionDetail (..),
    newGameSessionDetail,
    gameSessionDetail_gameSession,
    gameSessionDetail_protectionPolicy,

    -- * GameSessionPlacement
    GameSessionPlacement (..),
    newGameSessionPlacement,
    gameSessionPlacement_status,
    gameSessionPlacement_placementId,
    gameSessionPlacement_gameProperties,
    gameSessionPlacement_ipAddress,
    gameSessionPlacement_gameSessionName,
    gameSessionPlacement_startTime,
    gameSessionPlacement_gameSessionId,
    gameSessionPlacement_gameSessionRegion,
    gameSessionPlacement_matchmakerData,
    gameSessionPlacement_maximumPlayerSessionCount,
    gameSessionPlacement_endTime,
    gameSessionPlacement_gameSessionArn,
    gameSessionPlacement_playerLatencies,
    gameSessionPlacement_gameSessionData,
    gameSessionPlacement_dnsName,
    gameSessionPlacement_gameSessionQueueName,
    gameSessionPlacement_placedPlayerSessions,
    gameSessionPlacement_port,

    -- * GameSessionQueue
    GameSessionQueue (..),
    newGameSessionQueue,
    gameSessionQueue_gameSessionQueueArn,
    gameSessionQueue_playerLatencyPolicies,
    gameSessionQueue_filterConfiguration,
    gameSessionQueue_notificationTarget,
    gameSessionQueue_timeoutInSeconds,
    gameSessionQueue_destinations,
    gameSessionQueue_name,
    gameSessionQueue_customEventData,
    gameSessionQueue_priorityConfiguration,

    -- * GameSessionQueueDestination
    GameSessionQueueDestination (..),
    newGameSessionQueueDestination,
    gameSessionQueueDestination_destinationArn,

    -- * Instance
    Instance (..),
    newInstance,
    instance_creationTime,
    instance_instanceId,
    instance_status,
    instance_ipAddress,
    instance_location,
    instance_operatingSystem,
    instance_fleetArn,
    instance_type,
    instance_fleetId,
    instance_dnsName,

    -- * InstanceAccess
    InstanceAccess (..),
    newInstanceAccess,
    instanceAccess_instanceId,
    instanceAccess_ipAddress,
    instanceAccess_operatingSystem,
    instanceAccess_credentials,
    instanceAccess_fleetId,

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
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,

    -- * LocationAttributes
    LocationAttributes (..),
    newLocationAttributes,
    locationAttributes_stoppedActions,
    locationAttributes_locationState,
    locationAttributes_updateStatus,

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
    matchedPlayerSession_playerSessionId,
    matchedPlayerSession_playerId,

    -- * MatchmakingConfiguration
    MatchmakingConfiguration (..),
    newMatchmakingConfiguration,
    matchmakingConfiguration_creationTime,
    matchmakingConfiguration_backfillMode,
    matchmakingConfiguration_gameProperties,
    matchmakingConfiguration_ruleSetName,
    matchmakingConfiguration_acceptanceTimeoutSeconds,
    matchmakingConfiguration_requestTimeoutSeconds,
    matchmakingConfiguration_notificationTarget,
    matchmakingConfiguration_flexMatchMode,
    matchmakingConfiguration_gameSessionQueueArns,
    matchmakingConfiguration_name,
    matchmakingConfiguration_customEventData,
    matchmakingConfiguration_configurationArn,
    matchmakingConfiguration_acceptanceRequired,
    matchmakingConfiguration_gameSessionData,
    matchmakingConfiguration_description,
    matchmakingConfiguration_additionalPlayerCount,
    matchmakingConfiguration_ruleSetArn,

    -- * MatchmakingRuleSet
    MatchmakingRuleSet (..),
    newMatchmakingRuleSet,
    matchmakingRuleSet_creationTime,
    matchmakingRuleSet_ruleSetName,
    matchmakingRuleSet_ruleSetArn,
    matchmakingRuleSet_ruleSetBody,

    -- * MatchmakingTicket
    MatchmakingTicket (..),
    newMatchmakingTicket,
    matchmakingTicket_status,
    matchmakingTicket_configurationName,
    matchmakingTicket_startTime,
    matchmakingTicket_gameSessionConnectionInfo,
    matchmakingTicket_ticketId,
    matchmakingTicket_estimatedWaitTime,
    matchmakingTicket_statusMessage,
    matchmakingTicket_endTime,
    matchmakingTicket_configurationArn,
    matchmakingTicket_statusReason,
    matchmakingTicket_players,

    -- * PlacedPlayerSession
    PlacedPlayerSession (..),
    newPlacedPlayerSession,
    placedPlayerSession_playerSessionId,
    placedPlayerSession_playerId,

    -- * Player
    Player (..),
    newPlayer,
    player_playerAttributes,
    player_team,
    player_playerId,
    player_latencyInMs,

    -- * PlayerLatency
    PlayerLatency (..),
    newPlayerLatency,
    playerLatency_latencyInMilliseconds,
    playerLatency_regionIdentifier,
    playerLatency_playerId,

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
    playerSession_ipAddress,
    playerSession_gameSessionId,
    playerSession_fleetArn,
    playerSession_terminationTime,
    playerSession_playerSessionId,
    playerSession_fleetId,
    playerSession_playerData,
    playerSession_playerId,
    playerSession_dnsName,
    playerSession_port,

    -- * PriorityConfiguration
    PriorityConfiguration (..),
    newPriorityConfiguration,
    priorityConfiguration_priorityOrder,
    priorityConfiguration_locationOrder,

    -- * ResourceCreationLimitPolicy
    ResourceCreationLimitPolicy (..),
    newResourceCreationLimitPolicy,
    resourceCreationLimitPolicy_newGameSessionsPerCreator,
    resourceCreationLimitPolicy_policyPeriodInMinutes,

    -- * RoutingStrategy
    RoutingStrategy (..),
    newRoutingStrategy,
    routingStrategy_type,
    routingStrategy_message,
    routingStrategy_fleetId,

    -- * RuntimeConfiguration
    RuntimeConfiguration (..),
    newRuntimeConfiguration,
    runtimeConfiguration_gameSessionActivationTimeoutSeconds,
    runtimeConfiguration_serverProcesses,
    runtimeConfiguration_maxConcurrentGameSessionActivations,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucket,
    s3Location_key,
    s3Location_objectVersion,
    s3Location_roleArn,

    -- * ScalingPolicy
    ScalingPolicy (..),
    newScalingPolicy,
    scalingPolicy_status,
    scalingPolicy_scalingAdjustmentType,
    scalingPolicy_location,
    scalingPolicy_evaluationPeriods,
    scalingPolicy_policyType,
    scalingPolicy_metricName,
    scalingPolicy_fleetArn,
    scalingPolicy_comparisonOperator,
    scalingPolicy_name,
    scalingPolicy_threshold,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_updateStatus,
    scalingPolicy_fleetId,
    scalingPolicy_targetConfiguration,

    -- * Script
    Script (..),
    newScript,
    script_creationTime,
    script_storageLocation,
    script_name,
    script_scriptId,
    script_version,
    script_scriptArn,
    script_sizeOnDisk,

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
    vpcPeeringAuthorization_peerVpcId,
    vpcPeeringAuthorization_peerVpcAwsAccountId,
    vpcPeeringAuthorization_gameLiftAwsAccountId,
    vpcPeeringAuthorization_expirationTime,

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    newVpcPeeringConnection,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_status,
    vpcPeeringConnection_peerVpcId,
    vpcPeeringConnection_fleetArn,
    vpcPeeringConnection_ipV4CidrBlock,
    vpcPeeringConnection_gameLiftVpcId,
    vpcPeeringConnection_fleetId,

    -- * VpcPeeringConnectionStatus
    VpcPeeringConnectionStatus (..),
    newVpcPeeringConnectionStatus,
    vpcPeeringConnectionStatus_code,
    vpcPeeringConnectionStatus_message,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types.AcceptanceType
import Amazonka.GameLift.Types.Alias
import Amazonka.GameLift.Types.AttributeValue
import Amazonka.GameLift.Types.AwsCredentials
import Amazonka.GameLift.Types.BackfillMode
import Amazonka.GameLift.Types.BalancingStrategy
import Amazonka.GameLift.Types.Build
import Amazonka.GameLift.Types.BuildStatus
import Amazonka.GameLift.Types.CertificateConfiguration
import Amazonka.GameLift.Types.CertificateType
import Amazonka.GameLift.Types.ComparisonOperatorType
import Amazonka.GameLift.Types.DesiredPlayerSession
import Amazonka.GameLift.Types.EC2InstanceCounts
import Amazonka.GameLift.Types.EC2InstanceLimit
import Amazonka.GameLift.Types.EC2InstanceType
import Amazonka.GameLift.Types.Event
import Amazonka.GameLift.Types.EventCode
import Amazonka.GameLift.Types.FilterConfiguration
import Amazonka.GameLift.Types.FleetAction
import Amazonka.GameLift.Types.FleetAttributes
import Amazonka.GameLift.Types.FleetCapacity
import Amazonka.GameLift.Types.FleetStatus
import Amazonka.GameLift.Types.FleetType
import Amazonka.GameLift.Types.FleetUtilization
import Amazonka.GameLift.Types.FlexMatchMode
import Amazonka.GameLift.Types.GameProperty
import Amazonka.GameLift.Types.GameServer
import Amazonka.GameLift.Types.GameServerClaimStatus
import Amazonka.GameLift.Types.GameServerGroup
import Amazonka.GameLift.Types.GameServerGroupAction
import Amazonka.GameLift.Types.GameServerGroupAutoScalingPolicy
import Amazonka.GameLift.Types.GameServerGroupDeleteOption
import Amazonka.GameLift.Types.GameServerGroupInstanceType
import Amazonka.GameLift.Types.GameServerGroupStatus
import Amazonka.GameLift.Types.GameServerHealthCheck
import Amazonka.GameLift.Types.GameServerInstance
import Amazonka.GameLift.Types.GameServerInstanceStatus
import Amazonka.GameLift.Types.GameServerProtectionPolicy
import Amazonka.GameLift.Types.GameServerUtilizationStatus
import Amazonka.GameLift.Types.GameSession
import Amazonka.GameLift.Types.GameSessionConnectionInfo
import Amazonka.GameLift.Types.GameSessionDetail
import Amazonka.GameLift.Types.GameSessionPlacement
import Amazonka.GameLift.Types.GameSessionPlacementState
import Amazonka.GameLift.Types.GameSessionQueue
import Amazonka.GameLift.Types.GameSessionQueueDestination
import Amazonka.GameLift.Types.GameSessionStatus
import Amazonka.GameLift.Types.GameSessionStatusReason
import Amazonka.GameLift.Types.Instance
import Amazonka.GameLift.Types.InstanceAccess
import Amazonka.GameLift.Types.InstanceCredentials
import Amazonka.GameLift.Types.InstanceDefinition
import Amazonka.GameLift.Types.InstanceStatus
import Amazonka.GameLift.Types.IpPermission
import Amazonka.GameLift.Types.IpProtocol
import Amazonka.GameLift.Types.LaunchTemplateSpecification
import Amazonka.GameLift.Types.LocationAttributes
import Amazonka.GameLift.Types.LocationConfiguration
import Amazonka.GameLift.Types.LocationState
import Amazonka.GameLift.Types.LocationUpdateStatus
import Amazonka.GameLift.Types.MatchedPlayerSession
import Amazonka.GameLift.Types.MatchmakingConfiguration
import Amazonka.GameLift.Types.MatchmakingConfigurationStatus
import Amazonka.GameLift.Types.MatchmakingRuleSet
import Amazonka.GameLift.Types.MatchmakingTicket
import Amazonka.GameLift.Types.MetricName
import Amazonka.GameLift.Types.OperatingSystem
import Amazonka.GameLift.Types.PlacedPlayerSession
import Amazonka.GameLift.Types.Player
import Amazonka.GameLift.Types.PlayerLatency
import Amazonka.GameLift.Types.PlayerLatencyPolicy
import Amazonka.GameLift.Types.PlayerSession
import Amazonka.GameLift.Types.PlayerSessionCreationPolicy
import Amazonka.GameLift.Types.PlayerSessionStatus
import Amazonka.GameLift.Types.PolicyType
import Amazonka.GameLift.Types.PriorityConfiguration
import Amazonka.GameLift.Types.PriorityType
import Amazonka.GameLift.Types.ProtectionPolicy
import Amazonka.GameLift.Types.ResourceCreationLimitPolicy
import Amazonka.GameLift.Types.RoutingStrategy
import Amazonka.GameLift.Types.RoutingStrategyType
import Amazonka.GameLift.Types.RuntimeConfiguration
import Amazonka.GameLift.Types.S3Location
import Amazonka.GameLift.Types.ScalingAdjustmentType
import Amazonka.GameLift.Types.ScalingPolicy
import Amazonka.GameLift.Types.ScalingStatusType
import Amazonka.GameLift.Types.Script
import Amazonka.GameLift.Types.ServerProcess
import Amazonka.GameLift.Types.SortOrder
import Amazonka.GameLift.Types.Tag
import Amazonka.GameLift.Types.TargetConfiguration
import Amazonka.GameLift.Types.TargetTrackingConfiguration
import Amazonka.GameLift.Types.VpcPeeringAuthorization
import Amazonka.GameLift.Types.VpcPeeringConnection
import Amazonka.GameLift.Types.VpcPeeringConnectionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified game server group has no available game servers to fulfill
-- a @ClaimGameServer@ request. Clients can retry such requests immediately
-- or after a waiting period.
_OutOfCapacityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OutOfCapacityException =
  Core._MatchServiceError
    defaultService
    "OutOfCapacityException"

-- | The requested operation would cause a conflict with the current state of
-- a resource associated with the request and\/or the fleet. Resolve the
-- conflict before retrying.
_InvalidFleetStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFleetStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidFleetStatusException"

-- | One or more parameter values in the request are invalid. Correct the
-- invalid parameter values before retrying.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The requested tagging operation did not succeed. This may be due to
-- invalid tag format or the maximum tag limit may have been exceeded.
-- Resolve the issue before retrying.
_TaggingFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaggingFailedException =
  Core._MatchServiceError
    defaultService
    "TaggingFailedException"

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

-- | A service resource associated with the request could not be found.
-- Clients should not retry such requests.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The game instance is currently full and cannot allow the requested
-- player(s) to join. Clients can retry such requests immediately or after
-- a waiting period.
_GameSessionFullException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GameSessionFullException =
  Core._MatchServiceError
    defaultService
    "GameSessionFullException"

-- | The requested operation is not supported in the Region specified.
_UnsupportedRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedRegionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedRegionException"

-- | The requested operation would cause a conflict with the current state of
-- a resource associated with the request and\/or the game instance.
-- Resolve the conflict before retrying.
_InvalidGameSessionStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGameSessionStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidGameSessionStatusException"

-- | The service encountered an unrecoverable internal failure while
-- processing the request. Clients can retry such requests immediately or
-- after a waiting period.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | A game session with this custom ID string already exists in this fleet.
-- Resolve this conflict before retrying this request.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The client failed authentication. Clients should not retry such
-- requests.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | The specified fleet has no available instances to fulfill a
-- @CreateGameSession@ request. Clients can retry such requests immediately
-- or after a waiting period.
_FleetCapacityExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FleetCapacityExceededException =
  Core._MatchServiceError
    defaultService
    "FleetCapacityExceededException"

-- | The requested operation would cause the resource to exceed the allowed
-- service limit. Resolve the issue before retrying.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
