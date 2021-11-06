{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ManagedRuleException,
    _IllegalStatusException,
    _PolicyLengthExceededException,
    _ResourceAlreadyExistsException,
    _OperationDisabledException,
    _ConcurrentModificationException,
    _InvalidEventPatternException,
    _InternalException,
    _ResourceNotFoundException,
    _InvalidStateException,
    _LimitExceededException,

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

    -- * RuleState
    RuleState (..),

    -- * ApiDestination
    ApiDestination (..),
    newApiDestination,
    apiDestination_creationTime,
    apiDestination_httpMethod,
    apiDestination_invocationEndpoint,
    apiDestination_lastModifiedTime,
    apiDestination_name,
    apiDestination_invocationRateLimitPerSecond,
    apiDestination_apiDestinationState,
    apiDestination_connectionArn,
    apiDestination_apiDestinationArn,

    -- * Archive
    Archive (..),
    newArchive,
    archive_creationTime,
    archive_sizeBytes,
    archive_eventSourceArn,
    archive_state,
    archive_eventCount,
    archive_archiveName,
    archive_retentionDays,
    archive_stateReason,

    -- * AwsVpcConfiguration
    AwsVpcConfiguration (..),
    newAwsVpcConfiguration,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_subnets,

    -- * BatchArrayProperties
    BatchArrayProperties (..),
    newBatchArrayProperties,
    batchArrayProperties_size,

    -- * BatchParameters
    BatchParameters (..),
    newBatchParameters,
    batchParameters_retryStrategy,
    batchParameters_arrayProperties,
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
    connection_creationTime,
    connection_lastModifiedTime,
    connection_name,
    connection_lastAuthorizedTime,
    connection_authorizationType,
    connection_connectionArn,
    connection_stateReason,
    connection_connectionState,

    -- * ConnectionApiKeyAuthResponseParameters
    ConnectionApiKeyAuthResponseParameters (..),
    newConnectionApiKeyAuthResponseParameters,
    connectionApiKeyAuthResponseParameters_apiKeyName,

    -- * ConnectionAuthResponseParameters
    ConnectionAuthResponseParameters (..),
    newConnectionAuthResponseParameters,
    connectionAuthResponseParameters_oAuthParameters,
    connectionAuthResponseParameters_invocationHttpParameters,
    connectionAuthResponseParameters_apiKeyAuthParameters,
    connectionAuthResponseParameters_basicAuthParameters,

    -- * ConnectionBasicAuthResponseParameters
    ConnectionBasicAuthResponseParameters (..),
    newConnectionBasicAuthResponseParameters,
    connectionBasicAuthResponseParameters_username,

    -- * ConnectionBodyParameter
    ConnectionBodyParameter (..),
    newConnectionBodyParameter,
    connectionBodyParameter_isValueSecret,
    connectionBodyParameter_value,
    connectionBodyParameter_key,

    -- * ConnectionHeaderParameter
    ConnectionHeaderParameter (..),
    newConnectionHeaderParameter,
    connectionHeaderParameter_isValueSecret,
    connectionHeaderParameter_value,
    connectionHeaderParameter_key,

    -- * ConnectionHttpParameters
    ConnectionHttpParameters (..),
    newConnectionHttpParameters,
    connectionHttpParameters_queryStringParameters,
    connectionHttpParameters_headerParameters,
    connectionHttpParameters_bodyParameters,

    -- * ConnectionOAuthClientResponseParameters
    ConnectionOAuthClientResponseParameters (..),
    newConnectionOAuthClientResponseParameters,
    connectionOAuthClientResponseParameters_clientID,

    -- * ConnectionOAuthResponseParameters
    ConnectionOAuthResponseParameters (..),
    newConnectionOAuthResponseParameters,
    connectionOAuthResponseParameters_httpMethod,
    connectionOAuthResponseParameters_clientParameters,
    connectionOAuthResponseParameters_oAuthHttpParameters,
    connectionOAuthResponseParameters_authorizationEndpoint,

    -- * ConnectionQueryStringParameter
    ConnectionQueryStringParameter (..),
    newConnectionQueryStringParameter,
    connectionQueryStringParameter_isValueSecret,
    connectionQueryStringParameter_value,
    connectionQueryStringParameter_key,

    -- * CreateConnectionApiKeyAuthRequestParameters
    CreateConnectionApiKeyAuthRequestParameters (..),
    newCreateConnectionApiKeyAuthRequestParameters,
    createConnectionApiKeyAuthRequestParameters_apiKeyName,
    createConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- * CreateConnectionAuthRequestParameters
    CreateConnectionAuthRequestParameters (..),
    newCreateConnectionAuthRequestParameters,
    createConnectionAuthRequestParameters_oAuthParameters,
    createConnectionAuthRequestParameters_invocationHttpParameters,
    createConnectionAuthRequestParameters_apiKeyAuthParameters,
    createConnectionAuthRequestParameters_basicAuthParameters,

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
    ecsParameters_group,
    ecsParameters_propagateTags,
    ecsParameters_platformVersion,
    ecsParameters_enableECSManagedTags,
    ecsParameters_referenceId,
    ecsParameters_placementConstraints,
    ecsParameters_placementStrategy,
    ecsParameters_launchType,
    ecsParameters_capacityProviderStrategy,
    ecsParameters_taskCount,
    ecsParameters_networkConfiguration,
    ecsParameters_tags,
    ecsParameters_enableExecuteCommand,
    ecsParameters_taskDefinitionArn,

    -- * EventBus
    EventBus (..),
    newEventBus,
    eventBus_arn,
    eventBus_name,
    eventBus_policy,

    -- * EventSource
    EventSource (..),
    newEventSource,
    eventSource_creationTime,
    eventSource_state,
    eventSource_arn,
    eventSource_createdBy,
    eventSource_name,
    eventSource_expirationTime,

    -- * HttpParameters
    HttpParameters (..),
    newHttpParameters,
    httpParameters_pathParameterValues,
    httpParameters_queryStringParameters,
    httpParameters_headerParameters,

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
    partnerEventSourceAccount_creationTime,
    partnerEventSourceAccount_state,
    partnerEventSourceAccount_account,
    partnerEventSourceAccount_expirationTime,

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

    -- * PutEventsRequestEntry
    PutEventsRequestEntry (..),
    newPutEventsRequestEntry,
    putEventsRequestEntry_time,
    putEventsRequestEntry_detailType,
    putEventsRequestEntry_resources,
    putEventsRequestEntry_eventBusName,
    putEventsRequestEntry_source,
    putEventsRequestEntry_traceHeader,
    putEventsRequestEntry_detail,

    -- * PutEventsResultEntry
    PutEventsResultEntry (..),
    newPutEventsResultEntry,
    putEventsResultEntry_errorCode,
    putEventsResultEntry_errorMessage,
    putEventsResultEntry_eventId,

    -- * PutPartnerEventsRequestEntry
    PutPartnerEventsRequestEntry (..),
    newPutPartnerEventsRequestEntry,
    putPartnerEventsRequestEntry_time,
    putPartnerEventsRequestEntry_detailType,
    putPartnerEventsRequestEntry_resources,
    putPartnerEventsRequestEntry_source,
    putPartnerEventsRequestEntry_detail,

    -- * PutPartnerEventsResultEntry
    PutPartnerEventsResultEntry (..),
    newPutPartnerEventsResultEntry,
    putPartnerEventsResultEntry_errorCode,
    putPartnerEventsResultEntry_errorMessage,
    putPartnerEventsResultEntry_eventId,

    -- * PutTargetsResultEntry
    PutTargetsResultEntry (..),
    newPutTargetsResultEntry,
    putTargetsResultEntry_targetId,
    putTargetsResultEntry_errorCode,
    putTargetsResultEntry_errorMessage,

    -- * RedshiftDataParameters
    RedshiftDataParameters (..),
    newRedshiftDataParameters,
    redshiftDataParameters_dbUser,
    redshiftDataParameters_secretManagerArn,
    redshiftDataParameters_statementName,
    redshiftDataParameters_withEvent,
    redshiftDataParameters_database,
    redshiftDataParameters_sql,

    -- * RemoveTargetsResultEntry
    RemoveTargetsResultEntry (..),
    newRemoveTargetsResultEntry,
    removeTargetsResultEntry_targetId,
    removeTargetsResultEntry_errorCode,
    removeTargetsResultEntry_errorMessage,

    -- * Replay
    Replay (..),
    newReplay,
    replay_eventSourceArn,
    replay_state,
    replay_eventEndTime,
    replay_replayStartTime,
    replay_replayEndTime,
    replay_eventLastReplayedTime,
    replay_eventStartTime,
    replay_replayName,
    replay_stateReason,

    -- * ReplayDestination
    ReplayDestination (..),
    newReplayDestination,
    replayDestination_filterArns,
    replayDestination_arn,

    -- * RetryPolicy
    RetryPolicy (..),
    newRetryPolicy,
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- * Rule
    Rule (..),
    newRule,
    rule_eventPattern,
    rule_state,
    rule_arn,
    rule_eventBusName,
    rule_scheduleExpression,
    rule_name,
    rule_description,
    rule_managedBy,
    rule_roleArn,

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
    target_runCommandParameters,
    target_httpParameters,
    target_kinesisParameters,
    target_inputTransformer,
    target_deadLetterConfig,
    target_sageMakerPipelineParameters,
    target_sqsParameters,
    target_input,
    target_batchParameters,
    target_redshiftDataParameters,
    target_ecsParameters,
    target_retryPolicy,
    target_inputPath,
    target_roleArn,
    target_id,
    target_arn,

    -- * UpdateConnectionApiKeyAuthRequestParameters
    UpdateConnectionApiKeyAuthRequestParameters (..),
    newUpdateConnectionApiKeyAuthRequestParameters,
    updateConnectionApiKeyAuthRequestParameters_apiKeyValue,
    updateConnectionApiKeyAuthRequestParameters_apiKeyName,

    -- * UpdateConnectionAuthRequestParameters
    UpdateConnectionAuthRequestParameters (..),
    newUpdateConnectionAuthRequestParameters,
    updateConnectionAuthRequestParameters_oAuthParameters,
    updateConnectionAuthRequestParameters_invocationHttpParameters,
    updateConnectionAuthRequestParameters_apiKeyAuthParameters,
    updateConnectionAuthRequestParameters_basicAuthParameters,

    -- * UpdateConnectionBasicAuthRequestParameters
    UpdateConnectionBasicAuthRequestParameters (..),
    newUpdateConnectionBasicAuthRequestParameters,
    updateConnectionBasicAuthRequestParameters_username,
    updateConnectionBasicAuthRequestParameters_password,

    -- * UpdateConnectionOAuthClientRequestParameters
    UpdateConnectionOAuthClientRequestParameters (..),
    newUpdateConnectionOAuthClientRequestParameters,
    updateConnectionOAuthClientRequestParameters_clientID,
    updateConnectionOAuthClientRequestParameters_clientSecret,

    -- * UpdateConnectionOAuthRequestParameters
    UpdateConnectionOAuthRequestParameters (..),
    newUpdateConnectionOAuthRequestParameters,
    updateConnectionOAuthRequestParameters_httpMethod,
    updateConnectionOAuthRequestParameters_clientParameters,
    updateConnectionOAuthRequestParameters_oAuthHttpParameters,
    updateConnectionOAuthRequestParameters_authorizationEndpoint,
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
import Amazonka.CloudWatchEvents.Types.EventBus
import Amazonka.CloudWatchEvents.Types.EventSource
import Amazonka.CloudWatchEvents.Types.EventSourceState
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
import Amazonka.CloudWatchEvents.Types.RetryPolicy
import Amazonka.CloudWatchEvents.Types.Rule
import Amazonka.CloudWatchEvents.Types.RuleState
import Amazonka.CloudWatchEvents.Types.RunCommandParameters
import Amazonka.CloudWatchEvents.Types.RunCommandTarget
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameter
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameters
import Amazonka.CloudWatchEvents.Types.SqsParameters
import Amazonka.CloudWatchEvents.Types.Tag
import Amazonka.CloudWatchEvents.Types.Target
import Amazonka.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-10-07@ of the Amazon EventBridge SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CloudWatchEvents",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "events",
      Core._serviceSigningName = "events",
      Core._serviceVersion = "2015-10-07",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CloudWatchEvents",
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

-- | This rule was created by an Amazon Web Services service on behalf of
-- your account. It is managed by that service. If you see this error in
-- response to @DeleteRule@ or @RemoveTargets@, you can use the @Force@
-- parameter in those calls to delete the rule or remove targets from the
-- rule. You cannot modify these managed rules by using @DisableRule@,
-- @EnableRule@, @PutTargets@, @PutRule@, @TagResource@, or
-- @UntagResource@.
_ManagedRuleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ManagedRuleException =
  Core._MatchServiceError
    defaultService
    "ManagedRuleException"

-- | An error occurred because a replay can be canceled only when the state
-- is Running or Starting.
_IllegalStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalStatusException =
  Core._MatchServiceError
    defaultService
    "IllegalStatusException"

-- | The event bus policy is too long. For more information, see the limits.
_PolicyLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyLengthExceededException"

-- | The resource you are trying to create already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The operation you are attempting is not available in this region.
_OperationDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationDisabledException =
  Core._MatchServiceError
    defaultService
    "OperationDisabledException"

-- | There is concurrent modification on a rule, target, archive, or replay.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The event pattern is not valid.
_InvalidEventPatternException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEventPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidEventPatternException"

-- | This exception occurs due to unexpected causes.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"

-- | An entity that you specified does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified state is not a valid state for an event source.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The request failed because it attempted to create resource beyond the
-- allowed service quota.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
