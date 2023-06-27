{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _IllegalStatusException,
    _InternalException,
    _InvalidEventPatternException,
    _InvalidStateException,
    _LimitExceededException,
    _ManagedRuleException,
    _OperationDisabledException,
    _PolicyLengthExceededException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,

    -- * ApiDestinationHttpMethod
    ApiDestinationHttpMethod (..),

    -- * ApiDestinationState
    ApiDestinationState (..),

    -- * ArchiveState
    ArchiveState (..),

    -- * AssignPublicIp
    AssignPublicIp (..),

    -- * ConnectionAuthorizationType
    ConnectionAuthorizationType (..),

    -- * ConnectionOAuthHttpMethod
    ConnectionOAuthHttpMethod (..),

    -- * ConnectionState
    ConnectionState (..),

    -- * EndpointState
    EndpointState (..),

    -- * EventSourceState
    EventSourceState (..),

    -- * LaunchType
    LaunchType (..),

    -- * PlacementConstraintType
    PlacementConstraintType (..),

    -- * PlacementStrategyType
    PlacementStrategyType (..),

    -- * PropagateTags
    PropagateTags (..),

    -- * ReplayState
    ReplayState (..),

    -- * ReplicationState
    ReplicationState (..),

    -- * RuleState
    RuleState (..),

    -- * ApiDestination
    ApiDestination (..),
    newApiDestination,
    apiDestination_apiDestinationArn,
    apiDestination_apiDestinationState,
    apiDestination_connectionArn,
    apiDestination_creationTime,
    apiDestination_httpMethod,
    apiDestination_invocationEndpoint,
    apiDestination_invocationRateLimitPerSecond,
    apiDestination_lastModifiedTime,
    apiDestination_name,

    -- * Archive
    Archive (..),
    newArchive,
    archive_archiveName,
    archive_creationTime,
    archive_eventCount,
    archive_eventSourceArn,
    archive_retentionDays,
    archive_sizeBytes,
    archive_state,
    archive_stateReason,

    -- * AwsVpcConfiguration
    AwsVpcConfiguration (..),
    newAwsVpcConfiguration,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- * BatchArrayProperties
    BatchArrayProperties (..),
    newBatchArrayProperties,
    batchArrayProperties_size,

    -- * BatchParameters
    BatchParameters (..),
    newBatchParameters,
    batchParameters_arrayProperties,
    batchParameters_retryStrategy,
    batchParameters_jobDefinition,
    batchParameters_jobName,

    -- * BatchRetryStrategy
    BatchRetryStrategy (..),
    newBatchRetryStrategy,
    batchRetryStrategy_attempts,

    -- * CapacityProviderStrategyItem
    CapacityProviderStrategyItem (..),
    newCapacityProviderStrategyItem,
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- * Condition
    Condition (..),
    newCondition,
    condition_type,
    condition_key,
    condition_value,

    -- * Connection
    Connection (..),
    newConnection,
    connection_authorizationType,
    connection_connectionArn,
    connection_connectionState,
    connection_creationTime,
    connection_lastAuthorizedTime,
    connection_lastModifiedTime,
    connection_name,
    connection_stateReason,

    -- * ConnectionApiKeyAuthResponseParameters
    ConnectionApiKeyAuthResponseParameters (..),
    newConnectionApiKeyAuthResponseParameters,
    connectionApiKeyAuthResponseParameters_apiKeyName,

    -- * ConnectionAuthResponseParameters
    ConnectionAuthResponseParameters (..),
    newConnectionAuthResponseParameters,
    connectionAuthResponseParameters_apiKeyAuthParameters,
    connectionAuthResponseParameters_basicAuthParameters,
    connectionAuthResponseParameters_invocationHttpParameters,
    connectionAuthResponseParameters_oAuthParameters,

    -- * ConnectionBasicAuthResponseParameters
    ConnectionBasicAuthResponseParameters (..),
    newConnectionBasicAuthResponseParameters,
    connectionBasicAuthResponseParameters_username,

    -- * ConnectionBodyParameter
    ConnectionBodyParameter (..),
    newConnectionBodyParameter,
    connectionBodyParameter_isValueSecret,
    connectionBodyParameter_key,
    connectionBodyParameter_value,

    -- * ConnectionHeaderParameter
    ConnectionHeaderParameter (..),
    newConnectionHeaderParameter,
    connectionHeaderParameter_isValueSecret,
    connectionHeaderParameter_key,
    connectionHeaderParameter_value,

    -- * ConnectionHttpParameters
    ConnectionHttpParameters (..),
    newConnectionHttpParameters,
    connectionHttpParameters_bodyParameters,
    connectionHttpParameters_headerParameters,
    connectionHttpParameters_queryStringParameters,

    -- * ConnectionOAuthClientResponseParameters
    ConnectionOAuthClientResponseParameters (..),
    newConnectionOAuthClientResponseParameters,
    connectionOAuthClientResponseParameters_clientID,

    -- * ConnectionOAuthResponseParameters
    ConnectionOAuthResponseParameters (..),
    newConnectionOAuthResponseParameters,
    connectionOAuthResponseParameters_authorizationEndpoint,
    connectionOAuthResponseParameters_clientParameters,
    connectionOAuthResponseParameters_httpMethod,
    connectionOAuthResponseParameters_oAuthHttpParameters,

    -- * ConnectionQueryStringParameter
    ConnectionQueryStringParameter (..),
    newConnectionQueryStringParameter,
    connectionQueryStringParameter_isValueSecret,
    connectionQueryStringParameter_key,
    connectionQueryStringParameter_value,

    -- * CreateConnectionApiKeyAuthRequestParameters
    CreateConnectionApiKeyAuthRequestParameters (..),
    newCreateConnectionApiKeyAuthRequestParameters,
    createConnectionApiKeyAuthRequestParameters_apiKeyName,
    createConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- * CreateConnectionAuthRequestParameters
    CreateConnectionAuthRequestParameters (..),
    newCreateConnectionAuthRequestParameters,
    createConnectionAuthRequestParameters_apiKeyAuthParameters,
    createConnectionAuthRequestParameters_basicAuthParameters,
    createConnectionAuthRequestParameters_invocationHttpParameters,
    createConnectionAuthRequestParameters_oAuthParameters,

    -- * CreateConnectionBasicAuthRequestParameters
    CreateConnectionBasicAuthRequestParameters (..),
    newCreateConnectionBasicAuthRequestParameters,
    createConnectionBasicAuthRequestParameters_username,
    createConnectionBasicAuthRequestParameters_password,

    -- * CreateConnectionOAuthClientRequestParameters
    CreateConnectionOAuthClientRequestParameters (..),
    newCreateConnectionOAuthClientRequestParameters,
    createConnectionOAuthClientRequestParameters_clientID,
    createConnectionOAuthClientRequestParameters_clientSecret,

    -- * CreateConnectionOAuthRequestParameters
    CreateConnectionOAuthRequestParameters (..),
    newCreateConnectionOAuthRequestParameters,
    createConnectionOAuthRequestParameters_oAuthHttpParameters,
    createConnectionOAuthRequestParameters_clientParameters,
    createConnectionOAuthRequestParameters_authorizationEndpoint,
    createConnectionOAuthRequestParameters_httpMethod,

    -- * DeadLetterConfig
    DeadLetterConfig (..),
    newDeadLetterConfig,
    deadLetterConfig_arn,

    -- * EcsParameters
    EcsParameters (..),
    newEcsParameters,
    ecsParameters_capacityProviderStrategy,
    ecsParameters_enableECSManagedTags,
    ecsParameters_enableExecuteCommand,
    ecsParameters_group,
    ecsParameters_launchType,
    ecsParameters_networkConfiguration,
    ecsParameters_placementConstraints,
    ecsParameters_placementStrategy,
    ecsParameters_platformVersion,
    ecsParameters_propagateTags,
    ecsParameters_referenceId,
    ecsParameters_tags,
    ecsParameters_taskCount,
    ecsParameters_taskDefinitionArn,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_arn,
    endpoint_creationTime,
    endpoint_description,
    endpoint_endpointId,
    endpoint_endpointUrl,
    endpoint_eventBuses,
    endpoint_lastModifiedTime,
    endpoint_name,
    endpoint_replicationConfig,
    endpoint_roleArn,
    endpoint_routingConfig,
    endpoint_state,
    endpoint_stateReason,

    -- * EndpointEventBus
    EndpointEventBus (..),
    newEndpointEventBus,
    endpointEventBus_eventBusArn,

    -- * EventBus
    EventBus (..),
    newEventBus,
    eventBus_arn,
    eventBus_name,
    eventBus_policy,

    -- * EventSource
    EventSource (..),
    newEventSource,
    eventSource_arn,
    eventSource_createdBy,
    eventSource_creationTime,
    eventSource_expirationTime,
    eventSource_name,
    eventSource_state,

    -- * FailoverConfig
    FailoverConfig (..),
    newFailoverConfig,
    failoverConfig_primary,
    failoverConfig_secondary,

    -- * HttpParameters
    HttpParameters (..),
    newHttpParameters,
    httpParameters_headerParameters,
    httpParameters_pathParameterValues,
    httpParameters_queryStringParameters,

    -- * InputTransformer
    InputTransformer (..),
    newInputTransformer,
    inputTransformer_inputPathsMap,
    inputTransformer_inputTemplate,

    -- * KinesisParameters
    KinesisParameters (..),
    newKinesisParameters,
    kinesisParameters_partitionKeyPath,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_awsvpcConfiguration,

    -- * PartnerEventSource
    PartnerEventSource (..),
    newPartnerEventSource,
    partnerEventSource_arn,
    partnerEventSource_name,

    -- * PartnerEventSourceAccount
    PartnerEventSourceAccount (..),
    newPartnerEventSourceAccount,
    partnerEventSourceAccount_account,
    partnerEventSourceAccount_creationTime,
    partnerEventSourceAccount_expirationTime,
    partnerEventSourceAccount_state,

    -- * PlacementConstraint
    PlacementConstraint (..),
    newPlacementConstraint,
    placementConstraint_expression,
    placementConstraint_type,

    -- * PlacementStrategy
    PlacementStrategy (..),
    newPlacementStrategy,
    placementStrategy_field,
    placementStrategy_type,

    -- * Primary
    Primary (..),
    newPrimary,
    primary_healthCheck,

    -- * PutEventsRequestEntry
    PutEventsRequestEntry (..),
    newPutEventsRequestEntry,
    putEventsRequestEntry_detail,
    putEventsRequestEntry_detailType,
    putEventsRequestEntry_eventBusName,
    putEventsRequestEntry_resources,
    putEventsRequestEntry_source,
    putEventsRequestEntry_time,
    putEventsRequestEntry_traceHeader,

    -- * PutEventsResultEntry
    PutEventsResultEntry (..),
    newPutEventsResultEntry,
    putEventsResultEntry_errorCode,
    putEventsResultEntry_errorMessage,
    putEventsResultEntry_eventId,

    -- * PutPartnerEventsRequestEntry
    PutPartnerEventsRequestEntry (..),
    newPutPartnerEventsRequestEntry,
    putPartnerEventsRequestEntry_detail,
    putPartnerEventsRequestEntry_detailType,
    putPartnerEventsRequestEntry_resources,
    putPartnerEventsRequestEntry_source,
    putPartnerEventsRequestEntry_time,

    -- * PutPartnerEventsResultEntry
    PutPartnerEventsResultEntry (..),
    newPutPartnerEventsResultEntry,
    putPartnerEventsResultEntry_errorCode,
    putPartnerEventsResultEntry_errorMessage,
    putPartnerEventsResultEntry_eventId,

    -- * PutTargetsResultEntry
    PutTargetsResultEntry (..),
    newPutTargetsResultEntry,
    putTargetsResultEntry_errorCode,
    putTargetsResultEntry_errorMessage,
    putTargetsResultEntry_targetId,

    -- * RedshiftDataParameters
    RedshiftDataParameters (..),
    newRedshiftDataParameters,
    redshiftDataParameters_dbUser,
    redshiftDataParameters_secretManagerArn,
    redshiftDataParameters_sql,
    redshiftDataParameters_sqls,
    redshiftDataParameters_statementName,
    redshiftDataParameters_withEvent,
    redshiftDataParameters_database,

    -- * RemoveTargetsResultEntry
    RemoveTargetsResultEntry (..),
    newRemoveTargetsResultEntry,
    removeTargetsResultEntry_errorCode,
    removeTargetsResultEntry_errorMessage,
    removeTargetsResultEntry_targetId,

    -- * Replay
    Replay (..),
    newReplay,
    replay_eventEndTime,
    replay_eventLastReplayedTime,
    replay_eventSourceArn,
    replay_eventStartTime,
    replay_replayEndTime,
    replay_replayName,
    replay_replayStartTime,
    replay_state,
    replay_stateReason,

    -- * ReplayDestination
    ReplayDestination (..),
    newReplayDestination,
    replayDestination_filterArns,
    replayDestination_arn,

    -- * ReplicationConfig
    ReplicationConfig (..),
    newReplicationConfig,
    replicationConfig_state,

    -- * RetryPolicy
    RetryPolicy (..),
    newRetryPolicy,
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- * RoutingConfig
    RoutingConfig (..),
    newRoutingConfig,
    routingConfig_failoverConfig,

    -- * Rule
    Rule (..),
    newRule,
    rule_arn,
    rule_description,
    rule_eventBusName,
    rule_eventPattern,
    rule_managedBy,
    rule_name,
    rule_roleArn,
    rule_scheduleExpression,
    rule_state,

    -- * RunCommandParameters
    RunCommandParameters (..),
    newRunCommandParameters,
    runCommandParameters_runCommandTargets,

    -- * RunCommandTarget
    RunCommandTarget (..),
    newRunCommandTarget,
    runCommandTarget_key,
    runCommandTarget_values,

    -- * SageMakerPipelineParameter
    SageMakerPipelineParameter (..),
    newSageMakerPipelineParameter,
    sageMakerPipelineParameter_name,
    sageMakerPipelineParameter_value,

    -- * SageMakerPipelineParameters
    SageMakerPipelineParameters (..),
    newSageMakerPipelineParameters,
    sageMakerPipelineParameters_pipelineParameterList,

    -- * Secondary
    Secondary (..),
    newSecondary,
    secondary_route,

    -- * SqsParameters
    SqsParameters (..),
    newSqsParameters,
    sqsParameters_messageGroupId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Target
    Target (..),
    newTarget,
    target_batchParameters,
    target_deadLetterConfig,
    target_ecsParameters,
    target_httpParameters,
    target_input,
    target_inputPath,
    target_inputTransformer,
    target_kinesisParameters,
    target_redshiftDataParameters,
    target_retryPolicy,
    target_roleArn,
    target_runCommandParameters,
    target_sageMakerPipelineParameters,
    target_sqsParameters,
    target_id,
    target_arn,

    -- * UpdateConnectionApiKeyAuthRequestParameters
    UpdateConnectionApiKeyAuthRequestParameters (..),
    newUpdateConnectionApiKeyAuthRequestParameters,
    updateConnectionApiKeyAuthRequestParameters_apiKeyName,
    updateConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- * UpdateConnectionAuthRequestParameters
    UpdateConnectionAuthRequestParameters (..),
    newUpdateConnectionAuthRequestParameters,
    updateConnectionAuthRequestParameters_apiKeyAuthParameters,
    updateConnectionAuthRequestParameters_basicAuthParameters,
    updateConnectionAuthRequestParameters_invocationHttpParameters,
    updateConnectionAuthRequestParameters_oAuthParameters,

    -- * UpdateConnectionBasicAuthRequestParameters
    UpdateConnectionBasicAuthRequestParameters (..),
    newUpdateConnectionBasicAuthRequestParameters,
    updateConnectionBasicAuthRequestParameters_password,
    updateConnectionBasicAuthRequestParameters_username,

    -- * UpdateConnectionOAuthClientRequestParameters
    UpdateConnectionOAuthClientRequestParameters (..),
    newUpdateConnectionOAuthClientRequestParameters,
    updateConnectionOAuthClientRequestParameters_clientID,
    updateConnectionOAuthClientRequestParameters_clientSecret,

    -- * UpdateConnectionOAuthRequestParameters
    UpdateConnectionOAuthRequestParameters (..),
    newUpdateConnectionOAuthRequestParameters,
    updateConnectionOAuthRequestParameters_authorizationEndpoint,
    updateConnectionOAuthRequestParameters_clientParameters,
    updateConnectionOAuthRequestParameters_httpMethod,
    updateConnectionOAuthRequestParameters_oAuthHttpParameters,
  )
where

import Amazonka.CloudWatchEvents.Types.ApiDestination
import Amazonka.CloudWatchEvents.Types.ApiDestinationHttpMethod
import Amazonka.CloudWatchEvents.Types.ApiDestinationState
import Amazonka.CloudWatchEvents.Types.Archive
import Amazonka.CloudWatchEvents.Types.ArchiveState
import Amazonka.CloudWatchEvents.Types.AssignPublicIp
import Amazonka.CloudWatchEvents.Types.AwsVpcConfiguration
import Amazonka.CloudWatchEvents.Types.BatchArrayProperties
import Amazonka.CloudWatchEvents.Types.BatchParameters
import Amazonka.CloudWatchEvents.Types.BatchRetryStrategy
import Amazonka.CloudWatchEvents.Types.CapacityProviderStrategyItem
import Amazonka.CloudWatchEvents.Types.Condition
import Amazonka.CloudWatchEvents.Types.Connection
import Amazonka.CloudWatchEvents.Types.ConnectionApiKeyAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionAuthorizationType
import Amazonka.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionBodyParameter
import Amazonka.CloudWatchEvents.Types.ConnectionHeaderParameter
import Amazonka.CloudWatchEvents.Types.ConnectionHttpParameters
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthClientResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionQueryStringParameter
import Amazonka.CloudWatchEvents.Types.ConnectionState
import Amazonka.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.DeadLetterConfig
import Amazonka.CloudWatchEvents.Types.EcsParameters
import Amazonka.CloudWatchEvents.Types.Endpoint
import Amazonka.CloudWatchEvents.Types.EndpointEventBus
import Amazonka.CloudWatchEvents.Types.EndpointState
import Amazonka.CloudWatchEvents.Types.EventBus
import Amazonka.CloudWatchEvents.Types.EventSource
import Amazonka.CloudWatchEvents.Types.EventSourceState
import Amazonka.CloudWatchEvents.Types.FailoverConfig
import Amazonka.CloudWatchEvents.Types.HttpParameters
import Amazonka.CloudWatchEvents.Types.InputTransformer
import Amazonka.CloudWatchEvents.Types.KinesisParameters
import Amazonka.CloudWatchEvents.Types.LaunchType
import Amazonka.CloudWatchEvents.Types.NetworkConfiguration
import Amazonka.CloudWatchEvents.Types.PartnerEventSource
import Amazonka.CloudWatchEvents.Types.PartnerEventSourceAccount
import Amazonka.CloudWatchEvents.Types.PlacementConstraint
import Amazonka.CloudWatchEvents.Types.PlacementConstraintType
import Amazonka.CloudWatchEvents.Types.PlacementStrategy
import Amazonka.CloudWatchEvents.Types.PlacementStrategyType
import Amazonka.CloudWatchEvents.Types.Primary
import Amazonka.CloudWatchEvents.Types.PropagateTags
import Amazonka.CloudWatchEvents.Types.PutEventsRequestEntry
import Amazonka.CloudWatchEvents.Types.PutEventsResultEntry
import Amazonka.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
import Amazonka.CloudWatchEvents.Types.PutPartnerEventsResultEntry
import Amazonka.CloudWatchEvents.Types.PutTargetsResultEntry
import Amazonka.CloudWatchEvents.Types.RedshiftDataParameters
import Amazonka.CloudWatchEvents.Types.RemoveTargetsResultEntry
import Amazonka.CloudWatchEvents.Types.Replay
import Amazonka.CloudWatchEvents.Types.ReplayDestination
import Amazonka.CloudWatchEvents.Types.ReplayState
import Amazonka.CloudWatchEvents.Types.ReplicationConfig
import Amazonka.CloudWatchEvents.Types.ReplicationState
import Amazonka.CloudWatchEvents.Types.RetryPolicy
import Amazonka.CloudWatchEvents.Types.RoutingConfig
import Amazonka.CloudWatchEvents.Types.Rule
import Amazonka.CloudWatchEvents.Types.RuleState
import Amazonka.CloudWatchEvents.Types.RunCommandParameters
import Amazonka.CloudWatchEvents.Types.RunCommandTarget
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameter
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameters
import Amazonka.CloudWatchEvents.Types.Secondary
import Amazonka.CloudWatchEvents.Types.SqsParameters
import Amazonka.CloudWatchEvents.Types.Tag
import Amazonka.CloudWatchEvents.Types.Target
import Amazonka.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-10-07@ of the Amazon EventBridge SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudWatchEvents",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "events",
      Core.signingName = "events",
      Core.version = "2015-10-07",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CloudWatchEvents",
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

-- | There is concurrent modification on a rule, target, archive, or replay.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | An error occurred because a replay can be canceled only when the state
-- is Running or Starting.
_IllegalStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IllegalStatusException =
  Core._MatchServiceError
    defaultService
    "IllegalStatusException"

-- | This exception occurs due to unexpected causes.
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"

-- | The event pattern is not valid.
_InvalidEventPatternException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidEventPatternException"

-- | The specified state is not a valid state for an event source.
_InvalidStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The request failed because it attempted to create resource beyond the
-- allowed service quota.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | This rule was created by an Amazon Web Services service on behalf of
-- your account. It is managed by that service. If you see this error in
-- response to @DeleteRule@ or @RemoveTargets@, you can use the @Force@
-- parameter in those calls to delete the rule or remove targets from the
-- rule. You cannot modify these managed rules by using @DisableRule@,
-- @EnableRule@, @PutTargets@, @PutRule@, @TagResource@, or
-- @UntagResource@.
_ManagedRuleException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ManagedRuleException =
  Core._MatchServiceError
    defaultService
    "ManagedRuleException"

-- | The operation you are attempting is not available in this region.
_OperationDisabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationDisabledException =
  Core._MatchServiceError
    defaultService
    "OperationDisabledException"

-- | The event bus policy is too long. For more information, see the limits.
_PolicyLengthExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyLengthExceededException"

-- | The resource you are trying to create already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | An entity that you specified does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
