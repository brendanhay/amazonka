{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ManagedRuleException,
    _InvalidStateException,
    _ResourceAlreadyExistsException,
    _InternalException,
    _ConcurrentModificationException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _PolicyLengthExceededException,
    _IllegalStatusException,
    _OperationDisabledException,
    _InvalidEventPatternException,

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

    -- * ReplayState
    ReplayState (..),

    -- * RuleState
    RuleState (..),

    -- * ApiDestination
    ApiDestination (..),
    newApiDestination,
    apiDestination_httpMethod,
    apiDestination_creationTime,
    apiDestination_apiDestinationArn,
    apiDestination_invocationEndpoint,
    apiDestination_apiDestinationState,
    apiDestination_connectionArn,
    apiDestination_name,
    apiDestination_lastModifiedTime,
    apiDestination_invocationRateLimitPerSecond,

    -- * Archive
    Archive (..),
    newArchive,
    archive_eventCount,
    archive_eventSourceArn,
    archive_creationTime,
    archive_stateReason,
    archive_archiveName,
    archive_state,
    archive_sizeBytes,
    archive_retentionDays,

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
    connection_connectionState,
    connection_stateReason,
    connection_authorizationType,
    connection_connectionArn,
    connection_name,
    connection_lastModifiedTime,
    connection_lastAuthorizedTime,

    -- * ConnectionApiKeyAuthResponseParameters
    ConnectionApiKeyAuthResponseParameters (..),
    newConnectionApiKeyAuthResponseParameters,
    connectionApiKeyAuthResponseParameters_apiKeyName,

    -- * ConnectionAuthResponseParameters
    ConnectionAuthResponseParameters (..),
    newConnectionAuthResponseParameters,
    connectionAuthResponseParameters_basicAuthParameters,
    connectionAuthResponseParameters_oAuthParameters,
    connectionAuthResponseParameters_apiKeyAuthParameters,
    connectionAuthResponseParameters_invocationHttpParameters,

    -- * ConnectionBasicAuthResponseParameters
    ConnectionBasicAuthResponseParameters (..),
    newConnectionBasicAuthResponseParameters,
    connectionBasicAuthResponseParameters_username,

    -- * ConnectionBodyParameter
    ConnectionBodyParameter (..),
    newConnectionBodyParameter,
    connectionBodyParameter_key,
    connectionBodyParameter_isValueSecret,
    connectionBodyParameter_value,

    -- * ConnectionHeaderParameter
    ConnectionHeaderParameter (..),
    newConnectionHeaderParameter,
    connectionHeaderParameter_key,
    connectionHeaderParameter_isValueSecret,
    connectionHeaderParameter_value,

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
    connectionOAuthResponseParameters_authorizationEndpoint,
    connectionOAuthResponseParameters_oAuthHttpParameters,

    -- * ConnectionQueryStringParameter
    ConnectionQueryStringParameter (..),
    newConnectionQueryStringParameter,
    connectionQueryStringParameter_key,
    connectionQueryStringParameter_isValueSecret,
    connectionQueryStringParameter_value,

    -- * CreateConnectionApiKeyAuthRequestParameters
    CreateConnectionApiKeyAuthRequestParameters (..),
    newCreateConnectionApiKeyAuthRequestParameters,
    createConnectionApiKeyAuthRequestParameters_apiKeyName,
    createConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- * CreateConnectionAuthRequestParameters
    CreateConnectionAuthRequestParameters (..),
    newCreateConnectionAuthRequestParameters,
    createConnectionAuthRequestParameters_basicAuthParameters,
    createConnectionAuthRequestParameters_oAuthParameters,
    createConnectionAuthRequestParameters_apiKeyAuthParameters,
    createConnectionAuthRequestParameters_invocationHttpParameters,

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
    ecsParameters_networkConfiguration,
    ecsParameters_platformVersion,
    ecsParameters_launchType,
    ecsParameters_group,
    ecsParameters_taskCount,
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
    eventSource_expirationTime,
    eventSource_arn,
    eventSource_state,
    eventSource_name,
    eventSource_createdBy,

    -- * HttpParameters
    HttpParameters (..),
    newHttpParameters,
    httpParameters_queryStringParameters,
    httpParameters_pathParameterValues,
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
    partnerEventSourceAccount_expirationTime,
    partnerEventSourceAccount_state,
    partnerEventSourceAccount_account,

    -- * PutEventsRequestEntry
    PutEventsRequestEntry (..),
    newPutEventsRequestEntry,
    putEventsRequestEntry_detailType,
    putEventsRequestEntry_source,
    putEventsRequestEntry_eventBusName,
    putEventsRequestEntry_detail,
    putEventsRequestEntry_resources,
    putEventsRequestEntry_traceHeader,
    putEventsRequestEntry_time,

    -- * PutEventsResultEntry
    PutEventsResultEntry (..),
    newPutEventsResultEntry,
    putEventsResultEntry_eventId,
    putEventsResultEntry_errorMessage,
    putEventsResultEntry_errorCode,

    -- * PutPartnerEventsRequestEntry
    PutPartnerEventsRequestEntry (..),
    newPutPartnerEventsRequestEntry,
    putPartnerEventsRequestEntry_detailType,
    putPartnerEventsRequestEntry_source,
    putPartnerEventsRequestEntry_detail,
    putPartnerEventsRequestEntry_resources,
    putPartnerEventsRequestEntry_time,

    -- * PutPartnerEventsResultEntry
    PutPartnerEventsResultEntry (..),
    newPutPartnerEventsResultEntry,
    putPartnerEventsResultEntry_eventId,
    putPartnerEventsResultEntry_errorMessage,
    putPartnerEventsResultEntry_errorCode,

    -- * PutTargetsResultEntry
    PutTargetsResultEntry (..),
    newPutTargetsResultEntry,
    putTargetsResultEntry_targetId,
    putTargetsResultEntry_errorMessage,
    putTargetsResultEntry_errorCode,

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
    removeTargetsResultEntry_errorMessage,
    removeTargetsResultEntry_errorCode,

    -- * Replay
    Replay (..),
    newReplay,
    replay_eventSourceArn,
    replay_eventStartTime,
    replay_replayStartTime,
    replay_stateReason,
    replay_state,
    replay_replayName,
    replay_eventLastReplayedTime,
    replay_replayEndTime,
    replay_eventEndTime,

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
    rule_roleArn,
    rule_arn,
    rule_eventBusName,
    rule_state,
    rule_scheduleExpression,
    rule_name,
    rule_managedBy,
    rule_description,

    -- * RunCommandParameters
    RunCommandParameters (..),
    newRunCommandParameters,
    runCommandParameters_runCommandTargets,

    -- * RunCommandTarget
    RunCommandTarget (..),
    newRunCommandTarget,
    runCommandTarget_key,
    runCommandTarget_values,

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
    target_ecsParameters,
    target_runCommandParameters,
    target_roleArn,
    target_redshiftDataParameters,
    target_batchParameters,
    target_input,
    target_inputPath,
    target_deadLetterConfig,
    target_retryPolicy,
    target_httpParameters,
    target_sqsParameters,
    target_inputTransformer,
    target_kinesisParameters,
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
    updateConnectionAuthRequestParameters_basicAuthParameters,
    updateConnectionAuthRequestParameters_oAuthParameters,
    updateConnectionAuthRequestParameters_apiKeyAuthParameters,
    updateConnectionAuthRequestParameters_invocationHttpParameters,

    -- * UpdateConnectionBasicAuthRequestParameters
    UpdateConnectionBasicAuthRequestParameters (..),
    newUpdateConnectionBasicAuthRequestParameters,
    updateConnectionBasicAuthRequestParameters_password,
    updateConnectionBasicAuthRequestParameters_username,

    -- * UpdateConnectionOAuthClientRequestParameters
    UpdateConnectionOAuthClientRequestParameters (..),
    newUpdateConnectionOAuthClientRequestParameters,
    updateConnectionOAuthClientRequestParameters_clientSecret,
    updateConnectionOAuthClientRequestParameters_clientID,

    -- * UpdateConnectionOAuthRequestParameters
    UpdateConnectionOAuthRequestParameters (..),
    newUpdateConnectionOAuthRequestParameters,
    updateConnectionOAuthRequestParameters_httpMethod,
    updateConnectionOAuthRequestParameters_clientParameters,
    updateConnectionOAuthRequestParameters_authorizationEndpoint,
    updateConnectionOAuthRequestParameters_oAuthHttpParameters,
  )
where

import Network.AWS.CloudWatchEvents.Types.ApiDestination
import Network.AWS.CloudWatchEvents.Types.ApiDestinationHttpMethod
import Network.AWS.CloudWatchEvents.Types.ApiDestinationState
import Network.AWS.CloudWatchEvents.Types.Archive
import Network.AWS.CloudWatchEvents.Types.ArchiveState
import Network.AWS.CloudWatchEvents.Types.AssignPublicIp
import Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration
import Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
import Network.AWS.CloudWatchEvents.Types.BatchParameters
import Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
import Network.AWS.CloudWatchEvents.Types.Condition
import Network.AWS.CloudWatchEvents.Types.Connection
import Network.AWS.CloudWatchEvents.Types.ConnectionApiKeyAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionAuthorizationType
import Network.AWS.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionBodyParameter
import Network.AWS.CloudWatchEvents.Types.ConnectionHeaderParameter
import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthClientResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionQueryStringParameter
import Network.AWS.CloudWatchEvents.Types.ConnectionState
import Network.AWS.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
import Network.AWS.CloudWatchEvents.Types.EcsParameters
import Network.AWS.CloudWatchEvents.Types.EventBus
import Network.AWS.CloudWatchEvents.Types.EventSource
import Network.AWS.CloudWatchEvents.Types.EventSourceState
import Network.AWS.CloudWatchEvents.Types.HttpParameters
import Network.AWS.CloudWatchEvents.Types.InputTransformer
import Network.AWS.CloudWatchEvents.Types.KinesisParameters
import Network.AWS.CloudWatchEvents.Types.LaunchType
import Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
import Network.AWS.CloudWatchEvents.Types.PartnerEventSource
import Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
import Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
import Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.Replay
import Network.AWS.CloudWatchEvents.Types.ReplayDestination
import Network.AWS.CloudWatchEvents.Types.ReplayState
import Network.AWS.CloudWatchEvents.Types.RetryPolicy
import Network.AWS.CloudWatchEvents.Types.Rule
import Network.AWS.CloudWatchEvents.Types.RuleState
import Network.AWS.CloudWatchEvents.Types.RunCommandParameters
import Network.AWS.CloudWatchEvents.Types.RunCommandTarget
import Network.AWS.CloudWatchEvents.Types.SqsParameters
import Network.AWS.CloudWatchEvents.Types.Tag
import Network.AWS.CloudWatchEvents.Types.Target
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

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
      Core._serviceTimeout = Core.Just 70,
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | This rule was created by an AWS service on behalf of your account. It is
-- managed by that service. If you see this error in response to
-- @DeleteRule@ or @RemoveTargets@, you can use the @Force@ parameter in
-- those calls to delete the rule or remove targets from the rule. You
-- cannot modify these managed rules by using @DisableRule@, @EnableRule@,
-- @PutTargets@, @PutRule@, @TagResource@, or @UntagResource@.
_ManagedRuleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ManagedRuleException =
  Core._MatchServiceError
    defaultService
    "ManagedRuleException"

-- | The specified state is not a valid state for an event source.
_InvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The resource you are trying to create already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | This exception occurs due to unexpected causes.
_InternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"

-- | There is concurrent modification on a rule, target, archive, or replay.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The request failed because it attempted to create resource beyond the
-- allowed service quota.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | An entity that you specified does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The event bus policy is too long. For more information, see the limits.
_PolicyLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyLengthExceededException"

-- | An error occurred because a replay can be canceled only when the state
-- is Running or Starting.
_IllegalStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalStatusException =
  Core._MatchServiceError
    defaultService
    "IllegalStatusException"

-- | The operation you are attempting is not available in this region.
_OperationDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationDisabledException =
  Core._MatchServiceError
    defaultService
    "OperationDisabledException"

-- | The event pattern is not valid.
_InvalidEventPatternException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEventPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidEventPatternException"
