{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _FleetCapacityExceededException,
    _GameSessionFullException,
    _IdempotentParameterMismatchException,
    _InternalServiceException,
    _InvalidFleetStatusException,
    _InvalidGameSessionStatusException,
    _InvalidRequestException,
    _LimitExceededException,
    _NotFoundException,
    _OutOfCapacityException,
    _TaggingFailedException,
    _TerminalRoutingStrategyException,
    _UnauthorizedException,
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

    -- * ComputeStatus
    ComputeStatus (..),

    -- * ComputeType
    ComputeType (..),

    -- * EC2InstanceType
    EC2InstanceType (..),

    -- * EventCode
    EventCode (..),

    -- * FilterInstanceStatus
    FilterInstanceStatus (..),

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

    -- * LocationFilter
    LocationFilter (..),

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
    alias_aliasArn,
    alias_aliasId,
    alias_creationTime,
    alias_description,
    alias_lastUpdatedTime,
    alias_name,
    alias_routingStrategy,

    -- * AnywhereConfiguration
    AnywhereConfiguration (..),
    newAnywhereConfiguration,
    anywhereConfiguration_cost,

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_n,
    attributeValue_s,
    attributeValue_sdm,
    attributeValue_sl,

    -- * AwsCredentials
    AwsCredentials (..),
    newAwsCredentials,
    awsCredentials_accessKeyId,
    awsCredentials_secretAccessKey,
    awsCredentials_sessionToken,

    -- * Build
    Build (..),
    newBuild,
    build_buildArn,
    build_buildId,
    build_creationTime,
    build_name,
    build_operatingSystem,
    build_serverSdkVersion,
    build_sizeOnDisk,
    build_status,
    build_version,

    -- * CertificateConfiguration
    CertificateConfiguration (..),
    newCertificateConfiguration,
    certificateConfiguration_certificateType,

    -- * ClaimFilterOption
    ClaimFilterOption (..),
    newClaimFilterOption,
    claimFilterOption_instanceStatuses,

    -- * Compute
    Compute (..),
    newCompute,
    compute_computeArn,
    compute_computeName,
    compute_computeStatus,
    compute_creationTime,
    compute_dnsName,
    compute_fleetArn,
    compute_fleetId,
    compute_gameLiftServiceSdkEndpoint,
    compute_ipAddress,
    compute_location,
    compute_operatingSystem,
    compute_type,

    -- * DesiredPlayerSession
    DesiredPlayerSession (..),
    newDesiredPlayerSession,
    desiredPlayerSession_playerData,
    desiredPlayerSession_playerId,

    -- * EC2InstanceCounts
    EC2InstanceCounts (..),
    newEC2InstanceCounts,
    eC2InstanceCounts_active,
    eC2InstanceCounts_desired,
    eC2InstanceCounts_idle,
    eC2InstanceCounts_maximum,
    eC2InstanceCounts_minimum,
    eC2InstanceCounts_pending,
    eC2InstanceCounts_terminating,

    -- * EC2InstanceLimit
    EC2InstanceLimit (..),
    newEC2InstanceLimit,
    eC2InstanceLimit_currentInstances,
    eC2InstanceLimit_eC2InstanceType,
    eC2InstanceLimit_instanceLimit,
    eC2InstanceLimit_location,

    -- * Event
    Event (..),
    newEvent,
    event_eventCode,
    event_eventId,
    event_eventTime,
    event_message,
    event_preSignedLogUrl,
    event_resourceId,

    -- * FilterConfiguration
    FilterConfiguration (..),
    newFilterConfiguration,
    filterConfiguration_allowedLocations,

    -- * FleetAttributes
    FleetAttributes (..),
    newFleetAttributes,
    fleetAttributes_anywhereConfiguration,
    fleetAttributes_buildArn,
    fleetAttributes_buildId,
    fleetAttributes_certificateConfiguration,
    fleetAttributes_computeType,
    fleetAttributes_creationTime,
    fleetAttributes_description,
    fleetAttributes_fleetArn,
    fleetAttributes_fleetId,
    fleetAttributes_fleetType,
    fleetAttributes_instanceRoleArn,
    fleetAttributes_instanceType,
    fleetAttributes_logPaths,
    fleetAttributes_metricGroups,
    fleetAttributes_name,
    fleetAttributes_newGameSessionProtectionPolicy,
    fleetAttributes_operatingSystem,
    fleetAttributes_resourceCreationLimitPolicy,
    fleetAttributes_scriptArn,
    fleetAttributes_scriptId,
    fleetAttributes_serverLaunchParameters,
    fleetAttributes_serverLaunchPath,
    fleetAttributes_status,
    fleetAttributes_stoppedActions,
    fleetAttributes_terminationTime,

    -- * FleetCapacity
    FleetCapacity (..),
    newFleetCapacity,
    fleetCapacity_fleetArn,
    fleetCapacity_fleetId,
    fleetCapacity_instanceCounts,
    fleetCapacity_instanceType,
    fleetCapacity_location,

    -- * FleetUtilization
    FleetUtilization (..),
    newFleetUtilization,
    fleetUtilization_activeGameSessionCount,
    fleetUtilization_activeServerProcessCount,
    fleetUtilization_currentPlayerSessionCount,
    fleetUtilization_fleetArn,
    fleetUtilization_fleetId,
    fleetUtilization_location,
    fleetUtilization_maximumPlayerSessionCount,

    -- * GameProperty
    GameProperty (..),
    newGameProperty,
    gameProperty_key,
    gameProperty_value,

    -- * GameServer
    GameServer (..),
    newGameServer,
    gameServer_claimStatus,
    gameServer_connectionInfo,
    gameServer_gameServerData,
    gameServer_gameServerGroupArn,
    gameServer_gameServerGroupName,
    gameServer_gameServerId,
    gameServer_instanceId,
    gameServer_lastClaimTime,
    gameServer_lastHealthCheckTime,
    gameServer_registrationTime,
    gameServer_utilizationStatus,

    -- * GameServerGroup
    GameServerGroup (..),
    newGameServerGroup,
    gameServerGroup_autoScalingGroupArn,
    gameServerGroup_balancingStrategy,
    gameServerGroup_creationTime,
    gameServerGroup_gameServerGroupArn,
    gameServerGroup_gameServerGroupName,
    gameServerGroup_gameServerProtectionPolicy,
    gameServerGroup_instanceDefinitions,
    gameServerGroup_lastUpdatedTime,
    gameServerGroup_roleArn,
    gameServerGroup_status,
    gameServerGroup_statusReason,
    gameServerGroup_suspendedActions,

    -- * GameServerGroupAutoScalingPolicy
    GameServerGroupAutoScalingPolicy (..),
    newGameServerGroupAutoScalingPolicy,
    gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup,
    gameServerGroupAutoScalingPolicy_targetTrackingConfiguration,

    -- * GameServerInstance
    GameServerInstance (..),
    newGameServerInstance,
    gameServerInstance_gameServerGroupArn,
    gameServerInstance_gameServerGroupName,
    gameServerInstance_instanceId,
    gameServerInstance_instanceStatus,

    -- * GameSession
    GameSession (..),
    newGameSession,
    gameSession_creationTime,
    gameSession_creatorId,
    gameSession_currentPlayerSessionCount,
    gameSession_dnsName,
    gameSession_fleetArn,
    gameSession_fleetId,
    gameSession_gameProperties,
    gameSession_gameSessionData,
    gameSession_gameSessionId,
    gameSession_ipAddress,
    gameSession_location,
    gameSession_matchmakerData,
    gameSession_maximumPlayerSessionCount,
    gameSession_name,
    gameSession_playerSessionCreationPolicy,
    gameSession_port,
    gameSession_status,
    gameSession_statusReason,
    gameSession_terminationTime,

    -- * GameSessionConnectionInfo
    GameSessionConnectionInfo (..),
    newGameSessionConnectionInfo,
    gameSessionConnectionInfo_dnsName,
    gameSessionConnectionInfo_gameSessionArn,
    gameSessionConnectionInfo_ipAddress,
    gameSessionConnectionInfo_matchedPlayerSessions,
    gameSessionConnectionInfo_port,

    -- * GameSessionDetail
    GameSessionDetail (..),
    newGameSessionDetail,
    gameSessionDetail_gameSession,
    gameSessionDetail_protectionPolicy,

    -- * GameSessionPlacement
    GameSessionPlacement (..),
    newGameSessionPlacement,
    gameSessionPlacement_dnsName,
    gameSessionPlacement_endTime,
    gameSessionPlacement_gameProperties,
    gameSessionPlacement_gameSessionArn,
    gameSessionPlacement_gameSessionData,
    gameSessionPlacement_gameSessionId,
    gameSessionPlacement_gameSessionName,
    gameSessionPlacement_gameSessionQueueName,
    gameSessionPlacement_gameSessionRegion,
    gameSessionPlacement_ipAddress,
    gameSessionPlacement_matchmakerData,
    gameSessionPlacement_maximumPlayerSessionCount,
    gameSessionPlacement_placedPlayerSessions,
    gameSessionPlacement_placementId,
    gameSessionPlacement_playerLatencies,
    gameSessionPlacement_port,
    gameSessionPlacement_startTime,
    gameSessionPlacement_status,

    -- * GameSessionQueue
    GameSessionQueue (..),
    newGameSessionQueue,
    gameSessionQueue_customEventData,
    gameSessionQueue_destinations,
    gameSessionQueue_filterConfiguration,
    gameSessionQueue_gameSessionQueueArn,
    gameSessionQueue_name,
    gameSessionQueue_notificationTarget,
    gameSessionQueue_playerLatencyPolicies,
    gameSessionQueue_priorityConfiguration,
    gameSessionQueue_timeoutInSeconds,

    -- * GameSessionQueueDestination
    GameSessionQueueDestination (..),
    newGameSessionQueueDestination,
    gameSessionQueueDestination_destinationArn,

    -- * Instance
    Instance (..),
    newInstance,
    instance_creationTime,
    instance_dnsName,
    instance_fleetArn,
    instance_fleetId,
    instance_instanceId,
    instance_ipAddress,
    instance_location,
    instance_operatingSystem,
    instance_status,
    instance_type,

    -- * InstanceAccess
    InstanceAccess (..),
    newInstanceAccess,
    instanceAccess_credentials,
    instanceAccess_fleetId,
    instanceAccess_instanceId,
    instanceAccess_ipAddress,
    instanceAccess_operatingSystem,

    -- * InstanceCredentials
    InstanceCredentials (..),
    newInstanceCredentials,
    instanceCredentials_secret,
    instanceCredentials_userName,

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
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- * LocationAttributes
    LocationAttributes (..),
    newLocationAttributes,
    locationAttributes_locationState,
    locationAttributes_stoppedActions,
    locationAttributes_updateStatus,

    -- * LocationConfiguration
    LocationConfiguration (..),
    newLocationConfiguration,
    locationConfiguration_location,

    -- * LocationModel
    LocationModel (..),
    newLocationModel,
    locationModel_locationArn,
    locationModel_locationName,

    -- * LocationState
    LocationState (..),
    newLocationState,
    locationState_location,
    locationState_status,

    -- * MatchedPlayerSession
    MatchedPlayerSession (..),
    newMatchedPlayerSession,
    matchedPlayerSession_playerId,
    matchedPlayerSession_playerSessionId,

    -- * MatchmakingConfiguration
    MatchmakingConfiguration (..),
    newMatchmakingConfiguration,
    matchmakingConfiguration_acceptanceRequired,
    matchmakingConfiguration_acceptanceTimeoutSeconds,
    matchmakingConfiguration_additionalPlayerCount,
    matchmakingConfiguration_backfillMode,
    matchmakingConfiguration_configurationArn,
    matchmakingConfiguration_creationTime,
    matchmakingConfiguration_customEventData,
    matchmakingConfiguration_description,
    matchmakingConfiguration_flexMatchMode,
    matchmakingConfiguration_gameProperties,
    matchmakingConfiguration_gameSessionData,
    matchmakingConfiguration_gameSessionQueueArns,
    matchmakingConfiguration_name,
    matchmakingConfiguration_notificationTarget,
    matchmakingConfiguration_requestTimeoutSeconds,
    matchmakingConfiguration_ruleSetArn,
    matchmakingConfiguration_ruleSetName,

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
    matchmakingTicket_configurationArn,
    matchmakingTicket_configurationName,
    matchmakingTicket_endTime,
    matchmakingTicket_estimatedWaitTime,
    matchmakingTicket_gameSessionConnectionInfo,
    matchmakingTicket_players,
    matchmakingTicket_startTime,
    matchmakingTicket_status,
    matchmakingTicket_statusMessage,
    matchmakingTicket_statusReason,
    matchmakingTicket_ticketId,

    -- * PlacedPlayerSession
    PlacedPlayerSession (..),
    newPlacedPlayerSession,
    placedPlayerSession_playerId,
    placedPlayerSession_playerSessionId,

    -- * Player
    Player (..),
    newPlayer,
    player_latencyInMs,
    player_playerAttributes,
    player_playerId,
    player_team,

    -- * PlayerLatency
    PlayerLatency (..),
    newPlayerLatency,
    playerLatency_latencyInMilliseconds,
    playerLatency_playerId,
    playerLatency_regionIdentifier,

    -- * PlayerLatencyPolicy
    PlayerLatencyPolicy (..),
    newPlayerLatencyPolicy,
    playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds,
    playerLatencyPolicy_policyDurationSeconds,

    -- * PlayerSession
    PlayerSession (..),
    newPlayerSession,
    playerSession_creationTime,
    playerSession_dnsName,
    playerSession_fleetArn,
    playerSession_fleetId,
    playerSession_gameSessionId,
    playerSession_ipAddress,
    playerSession_playerData,
    playerSession_playerId,
    playerSession_playerSessionId,
    playerSession_port,
    playerSession_status,
    playerSession_terminationTime,

    -- * PriorityConfiguration
    PriorityConfiguration (..),
    newPriorityConfiguration,
    priorityConfiguration_locationOrder,
    priorityConfiguration_priorityOrder,

    -- * ResourceCreationLimitPolicy
    ResourceCreationLimitPolicy (..),
    newResourceCreationLimitPolicy,
    resourceCreationLimitPolicy_newGameSessionsPerCreator,
    resourceCreationLimitPolicy_policyPeriodInMinutes,

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
    s3Location_bucket,
    s3Location_key,
    s3Location_objectVersion,
    s3Location_roleArn,

    -- * ScalingPolicy
    ScalingPolicy (..),
    newScalingPolicy,
    scalingPolicy_comparisonOperator,
    scalingPolicy_evaluationPeriods,
    scalingPolicy_fleetArn,
    scalingPolicy_fleetId,
    scalingPolicy_location,
    scalingPolicy_metricName,
    scalingPolicy_name,
    scalingPolicy_policyType,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_scalingAdjustmentType,
    scalingPolicy_status,
    scalingPolicy_targetConfiguration,
    scalingPolicy_threshold,
    scalingPolicy_updateStatus,

    -- * Script
    Script (..),
    newScript,
    script_creationTime,
    script_name,
    script_scriptArn,
    script_scriptId,
    script_sizeOnDisk,
    script_storageLocation,
    script_version,

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
    vpcPeeringAuthorization_expirationTime,
    vpcPeeringAuthorization_gameLiftAwsAccountId,
    vpcPeeringAuthorization_peerVpcAwsAccountId,
    vpcPeeringAuthorization_peerVpcId,

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    newVpcPeeringConnection,
    vpcPeeringConnection_fleetArn,
    vpcPeeringConnection_fleetId,
    vpcPeeringConnection_gameLiftVpcId,
    vpcPeeringConnection_ipV4CidrBlock,
    vpcPeeringConnection_peerVpcId,
    vpcPeeringConnection_status,
    vpcPeeringConnection_vpcPeeringConnectionId,

    -- * VpcPeeringConnectionStatus
    VpcPeeringConnectionStatus (..),
    newVpcPeeringConnectionStatus,
    vpcPeeringConnectionStatus_code,
    vpcPeeringConnectionStatus_message,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types.AcceptanceType
import Amazonka.GameLift.Types.Alias
import Amazonka.GameLift.Types.AnywhereConfiguration
import Amazonka.GameLift.Types.AttributeValue
import Amazonka.GameLift.Types.AwsCredentials
import Amazonka.GameLift.Types.BackfillMode
import Amazonka.GameLift.Types.BalancingStrategy
import Amazonka.GameLift.Types.Build
import Amazonka.GameLift.Types.BuildStatus
import Amazonka.GameLift.Types.CertificateConfiguration
import Amazonka.GameLift.Types.CertificateType
import Amazonka.GameLift.Types.ClaimFilterOption
import Amazonka.GameLift.Types.ComparisonOperatorType
import Amazonka.GameLift.Types.Compute
import Amazonka.GameLift.Types.ComputeStatus
import Amazonka.GameLift.Types.ComputeType
import Amazonka.GameLift.Types.DesiredPlayerSession
import Amazonka.GameLift.Types.EC2InstanceCounts
import Amazonka.GameLift.Types.EC2InstanceLimit
import Amazonka.GameLift.Types.EC2InstanceType
import Amazonka.GameLift.Types.Event
import Amazonka.GameLift.Types.EventCode
import Amazonka.GameLift.Types.FilterConfiguration
import Amazonka.GameLift.Types.FilterInstanceStatus
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
import Amazonka.GameLift.Types.LocationFilter
import Amazonka.GameLift.Types.LocationModel
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-10-01@ of the Amazon GameLift SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "GameLift",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "gamelift",
      Core.signingName = "gamelift",
      Core.version = "2015-10-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "GameLift",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The specified fleet has no available instances to fulfill a
-- @CreateGameSession@ request. Clients can retry such requests immediately
-- or after a waiting period.
_FleetCapacityExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FleetCapacityExceededException =
  Core._MatchServiceError
    defaultService
    "FleetCapacityExceededException"

-- | The game instance is currently full and cannot allow the requested
-- player(s) to join. Clients can retry such requests immediately or after
-- a waiting period.
_GameSessionFullException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GameSessionFullException =
  Core._MatchServiceError
    defaultService
    "GameSessionFullException"

-- | A game session with this custom ID string already exists in this fleet.
-- Resolve this conflict before retrying this request.
_IdempotentParameterMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The service encountered an unrecoverable internal failure while
-- processing the request. Clients can retry such requests immediately or
-- after a waiting period.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The requested operation would cause a conflict with the current state of
-- a resource associated with the request and\/or the fleet. Resolve the
-- conflict before retrying.
_InvalidFleetStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidFleetStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidFleetStatusException"

-- | The requested operation would cause a conflict with the current state of
-- a resource associated with the request and\/or the game instance.
-- Resolve the conflict before retrying.
_InvalidGameSessionStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGameSessionStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidGameSessionStatusException"

-- | One or more parameter values in the request are invalid. Correct the
-- invalid parameter values before retrying.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The requested operation would cause the resource to exceed the allowed
-- service limit. Resolve the issue before retrying.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | THe requested resources was not found. The resource was either not
-- created yet or deleted.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The specified game server group has no available game servers to fulfill
-- a @ClaimGameServer@ request. Clients can retry such requests immediately
-- or after a waiting period.
_OutOfCapacityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OutOfCapacityException =
  Core._MatchServiceError
    defaultService
    "OutOfCapacityException"

-- | The requested tagging operation did not succeed. This may be due to
-- invalid tag format or the maximum tag limit may have been exceeded.
-- Resolve the issue before retrying.
_TaggingFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TaggingFailedException =
  Core._MatchServiceError
    defaultService
    "TaggingFailedException"

-- | The service is unable to resolve the routing for a particular alias
-- because it has a terminal @RoutingStrategy@ associated with it. The
-- message returned in this exception is the message defined in the routing
-- strategy itself. Such requests should only be retried if the routing
-- strategy for the specified alias is modified.
_TerminalRoutingStrategyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TerminalRoutingStrategyException =
  Core._MatchServiceError
    defaultService
    "TerminalRoutingStrategyException"

-- | The client failed authentication. Clients should not retry such
-- requests.
_UnauthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | The requested operation is not supported in the Region specified.
_UnsupportedRegionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedRegionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedRegionException"
