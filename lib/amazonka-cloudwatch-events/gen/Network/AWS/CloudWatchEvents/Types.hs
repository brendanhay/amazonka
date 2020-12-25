-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types
  ( -- * Service configuration
    mkServiceConfig,

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

    -- * QueryStringValue
    QueryStringValue (..),

    -- * PathParameter
    PathParameter (..),

    -- * TargetId
    TargetId (..),

    -- * TargetArn
    TargetArn (..),

    -- * RunCommandParameters
    RunCommandParameters (..),
    mkRunCommandParameters,
    rcpRunCommandTargets,

    -- * EventPattern
    EventPattern (..),

    -- * HeaderValue
    HeaderValue (..),

    -- * RunCommandTarget
    RunCommandTarget (..),
    mkRunCommandTarget,
    rctKey,
    rctValues,

    -- * DbUser
    DbUser (..),

    -- * EventBusNameOrArn
    EventBusNameOrArn (..),

    -- * PartnerEventSource
    PartnerEventSource (..),
    mkPartnerEventSource,
    pesArn,
    pesName,

    -- * BatchRetryStrategy
    BatchRetryStrategy (..),
    mkBatchRetryStrategy,
    brsAttempts,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * HttpParameters
    HttpParameters (..),
    mkHttpParameters,
    hpHeaderParameters,
    hpPathParameterValues,
    hpQueryStringParameters,

    -- * KinesisParameters
    KinesisParameters (..),
    mkKinesisParameters,
    kpPartitionKeyPath,

    -- * Arn
    Arn (..),

    -- * Database
    Database (..),

    -- * PutPartnerEventsResultEntry
    PutPartnerEventsResultEntry (..),
    mkPutPartnerEventsResultEntry,
    ppereErrorCode,
    ppereErrorMessage,
    ppereEventId,

    -- * String
    String (..),

    -- * InputTransformer
    InputTransformer (..),
    mkInputTransformer,
    itInputTemplate,
    itInputPathsMap,

    -- * ArchiveName
    ArchiveName (..),

    -- * ReplayStateReason
    ReplayStateReason (..),

    -- * DeadLetterConfig
    DeadLetterConfig (..),
    mkDeadLetterConfig,
    dlcArn,

    -- * EventBus
    EventBus (..),
    mkEventBus,
    ebArn,
    ebName,
    ebPolicy,

    -- * SqsParameters
    SqsParameters (..),
    mkSqsParameters,
    spMessageGroupId,

    -- * Replay
    Replay (..),
    mkReplay,
    rfEventEndTime,
    rfEventLastReplayedTime,
    rfEventSourceArn,
    rfEventStartTime,
    rfReplayEndTime,
    rfReplayName,
    rfReplayStartTime,
    rfState,
    rfStateReason,

    -- * PutEventsResultEntry
    PutEventsResultEntry (..),
    mkPutEventsResultEntry,
    pereErrorCode,
    pereErrorMessage,
    pereEventId,

    -- * CreatedBy
    CreatedBy (..),

    -- * PartnerEventSourceAccount
    PartnerEventSourceAccount (..),
    mkPartnerEventSourceAccount,
    pesaAccount,
    pesaCreationTime,
    pesaExpirationTime,
    pesaState,

    -- * NonPartnerEventBusName
    NonPartnerEventBusName (..),

    -- * Action
    Action (..),

    -- * EventResource
    EventResource (..),

    -- * Rule
    Rule (..),
    mkRule,
    rArn,
    rDescription,
    rEventBusName,
    rEventPattern,
    rManagedBy,
    rName,
    rRoleArn,
    rScheduleExpression,
    rState,

    -- * ReplayArn
    ReplayArn (..),

    -- * NonPartnerEventBusNameOrArn
    NonPartnerEventBusNameOrArn (..),

    -- * BatchParameters
    BatchParameters (..),
    mkBatchParameters,
    bpJobDefinition,
    bpJobName,
    bpArrayProperties,
    bpRetryStrategy,

    -- * ArchiveStateReason
    ArchiveStateReason (..),

    -- * AssignPublicIp
    AssignPublicIp (..),

    -- * RuleName
    RuleName (..),

    -- * RedshiftDataParameters
    RedshiftDataParameters (..),
    mkRedshiftDataParameters,
    rdpDatabase,
    rdpSql,
    rdpDbUser,
    rdpSecretManagerArn,
    rdpStatementName,
    rdpWithEvent,

    -- * RemoveTargetsResultEntry
    RemoveTargetsResultEntry (..),
    mkRemoveTargetsResultEntry,
    rtreErrorCode,
    rtreErrorMessage,
    rtreTargetId,

    -- * AccountId
    AccountId (..),

    -- * ReplayState
    ReplayState (..),

    -- * NextToken
    NextToken (..),

    -- * RunCommandTargetValue
    RunCommandTargetValue (..),

    -- * BatchArrayProperties
    BatchArrayProperties (..),
    mkBatchArrayProperties,
    bapSize,

    -- * ArchiveDescription
    ArchiveDescription (..),

    -- * ReplayName
    ReplayName (..),

    -- * EcsParameters
    EcsParameters (..),
    mkEcsParameters,
    epTaskDefinitionArn,
    epGroup,
    epLaunchType,
    epNetworkConfiguration,
    epPlatformVersion,
    epTaskCount,

    -- * TargetInput
    TargetInput (..),

    -- * EventBusName
    EventBusName (..),

    -- * ReplayDestination
    ReplayDestination (..),
    mkReplayDestination,
    rdArn,
    rdFilterArns,

    -- * ArchiveArn
    ArchiveArn (..),

    -- * ScheduleExpression
    ScheduleExpression (..),

    -- * InputTransformerPathKey
    InputTransformerPathKey (..),

    -- * Principal
    Principal (..),

    -- * TargetInputPath
    TargetInputPath (..),

    -- * StatementName
    StatementName (..),

    -- * Archive
    Archive (..),
    mkArchive,
    aArchiveName,
    aCreationTime,
    aEventCount,
    aEventSourceArn,
    aRetentionDays,
    aSizeBytes,
    aState,
    aStateReason,

    -- * ErrorCode
    ErrorCode (..),

    -- * PutPartnerEventsRequestEntry
    PutPartnerEventsRequestEntry (..),
    mkPutPartnerEventsRequestEntry,
    ppereDetail,
    ppereDetailType,
    ppereResources,
    ppereSource,
    ppereTime,

    -- * ReplayDescription
    ReplayDescription (..),

    -- * ArchiveState
    ArchiveState (..),

    -- * LaunchType
    LaunchType (..),

    -- * TagKey
    TagKey (..),

    -- * HeaderKey
    HeaderKey (..),

    -- * RuleDescription
    RuleDescription (..),

    -- * AwsVpcConfiguration
    AwsVpcConfiguration (..),
    mkAwsVpcConfiguration,
    avcSubnets,
    avcAssignPublicIp,
    avcSecurityGroups,

    -- * QueryStringKey
    QueryStringKey (..),

    -- * RetryPolicy
    RetryPolicy (..),
    mkRetryPolicy,
    rpMaximumEventAgeInSeconds,
    rpMaximumRetryAttempts,

    -- * StatementId
    StatementId (..),

    -- * Condition
    Condition (..),
    mkCondition,
    cType,
    cKey,
    cValue,

    -- * ErrorMessage
    ErrorMessage (..),

    -- * RuleArn
    RuleArn (..),

    -- * PutEventsRequestEntry
    PutEventsRequestEntry (..),
    mkPutEventsRequestEntry,
    pereDetail,
    pereDetailType,
    pereEventBusName,
    pereResources,
    pereSource,
    pereTime,

    -- * EventSourceName
    EventSourceName (..),

    -- * ManagedBy
    ManagedBy (..),

    -- * PutTargetsResultEntry
    PutTargetsResultEntry (..),
    mkPutTargetsResultEntry,
    ptreErrorCode,
    ptreErrorMessage,
    ptreTargetId,

    -- * Sql
    Sql (..),

    -- * EventSourceState
    EventSourceState (..),

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    mkNetworkConfiguration,
    ncAwsvpcConfiguration,

    -- * EventSource
    EventSource (..),
    mkEventSource,
    esArn,
    esCreatedBy,
    esCreationTime,
    esExpirationTime,
    esName,
    esState,

    -- * RuleState
    RuleState (..),

    -- * MessageGroupId
    MessageGroupId (..),

    -- * EventId
    EventId (..),

    -- * Target
    Target (..),
    mkTarget,
    tId,
    tArn,
    tBatchParameters,
    tDeadLetterConfig,
    tEcsParameters,
    tHttpParameters,
    tInput,
    tInputPath,
    tInputTransformer,
    tKinesisParameters,
    tRedshiftDataParameters,
    tRetryPolicy,
    tRoleArn,
    tRunCommandParameters,
    tSqsParameters,

    -- * RoleArn
    RoleArn (..),

    -- * Key
    Key (..),

    -- * Name
    Name (..),

    -- * Account
    Account (..),

    -- * Value
    Value (..),

    -- * EventSourceArn
    EventSourceArn (..),

    -- * NamePrefix
    NamePrefix (..),

    -- * Description
    Description (..),

    -- * PartitionKeyPath
    PartitionKeyPath (..),

    -- * StateReason
    StateReason (..),

    -- * InputTemplate
    InputTemplate (..),

    -- * SecretManagerArn
    SecretManagerArn (..),

    -- * Source
    Source (..),
  )
where

import Network.AWS.CloudWatchEvents.Types.Account
import Network.AWS.CloudWatchEvents.Types.AccountId
import Network.AWS.CloudWatchEvents.Types.Action
import Network.AWS.CloudWatchEvents.Types.Archive
import Network.AWS.CloudWatchEvents.Types.ArchiveArn
import Network.AWS.CloudWatchEvents.Types.ArchiveDescription
import Network.AWS.CloudWatchEvents.Types.ArchiveName
import Network.AWS.CloudWatchEvents.Types.ArchiveState
import Network.AWS.CloudWatchEvents.Types.ArchiveStateReason
import Network.AWS.CloudWatchEvents.Types.Arn
import Network.AWS.CloudWatchEvents.Types.AssignPublicIp
import Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration
import Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
import Network.AWS.CloudWatchEvents.Types.BatchParameters
import Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
import Network.AWS.CloudWatchEvents.Types.Condition
import Network.AWS.CloudWatchEvents.Types.CreatedBy
import Network.AWS.CloudWatchEvents.Types.Database
import Network.AWS.CloudWatchEvents.Types.DbUser
import Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
import Network.AWS.CloudWatchEvents.Types.Description
import Network.AWS.CloudWatchEvents.Types.EcsParameters
import Network.AWS.CloudWatchEvents.Types.ErrorCode
import Network.AWS.CloudWatchEvents.Types.ErrorMessage
import Network.AWS.CloudWatchEvents.Types.EventBus
import Network.AWS.CloudWatchEvents.Types.EventBusName
import Network.AWS.CloudWatchEvents.Types.EventBusNameOrArn
import Network.AWS.CloudWatchEvents.Types.EventId
import Network.AWS.CloudWatchEvents.Types.EventPattern
import Network.AWS.CloudWatchEvents.Types.EventResource
import Network.AWS.CloudWatchEvents.Types.EventSource
import Network.AWS.CloudWatchEvents.Types.EventSourceArn
import Network.AWS.CloudWatchEvents.Types.EventSourceName
import Network.AWS.CloudWatchEvents.Types.EventSourceState
import Network.AWS.CloudWatchEvents.Types.HeaderKey
import Network.AWS.CloudWatchEvents.Types.HeaderValue
import Network.AWS.CloudWatchEvents.Types.HttpParameters
import Network.AWS.CloudWatchEvents.Types.InputTemplate
import Network.AWS.CloudWatchEvents.Types.InputTransformer
import Network.AWS.CloudWatchEvents.Types.InputTransformerPathKey
import Network.AWS.CloudWatchEvents.Types.Key
import Network.AWS.CloudWatchEvents.Types.KinesisParameters
import Network.AWS.CloudWatchEvents.Types.LaunchType
import Network.AWS.CloudWatchEvents.Types.ManagedBy
import Network.AWS.CloudWatchEvents.Types.MessageGroupId
import Network.AWS.CloudWatchEvents.Types.Name
import Network.AWS.CloudWatchEvents.Types.NamePrefix
import Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
import Network.AWS.CloudWatchEvents.Types.NextToken
import Network.AWS.CloudWatchEvents.Types.NonPartnerEventBusName
import Network.AWS.CloudWatchEvents.Types.NonPartnerEventBusNameOrArn
import Network.AWS.CloudWatchEvents.Types.PartitionKeyPath
import Network.AWS.CloudWatchEvents.Types.PartnerEventSource
import Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
import Network.AWS.CloudWatchEvents.Types.PathParameter
import Network.AWS.CloudWatchEvents.Types.Principal
import Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.QueryStringKey
import Network.AWS.CloudWatchEvents.Types.QueryStringValue
import Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
import Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.Replay
import Network.AWS.CloudWatchEvents.Types.ReplayArn
import Network.AWS.CloudWatchEvents.Types.ReplayDescription
import Network.AWS.CloudWatchEvents.Types.ReplayDestination
import Network.AWS.CloudWatchEvents.Types.ReplayName
import Network.AWS.CloudWatchEvents.Types.ReplayState
import Network.AWS.CloudWatchEvents.Types.ReplayStateReason
import Network.AWS.CloudWatchEvents.Types.RetryPolicy
import Network.AWS.CloudWatchEvents.Types.RoleArn
import Network.AWS.CloudWatchEvents.Types.Rule
import Network.AWS.CloudWatchEvents.Types.RuleArn
import Network.AWS.CloudWatchEvents.Types.RuleDescription
import Network.AWS.CloudWatchEvents.Types.RuleName
import Network.AWS.CloudWatchEvents.Types.RuleState
import Network.AWS.CloudWatchEvents.Types.RunCommandParameters
import Network.AWS.CloudWatchEvents.Types.RunCommandTarget
import Network.AWS.CloudWatchEvents.Types.RunCommandTargetValue
import Network.AWS.CloudWatchEvents.Types.ScheduleExpression
import Network.AWS.CloudWatchEvents.Types.SecretManagerArn
import Network.AWS.CloudWatchEvents.Types.Source
import Network.AWS.CloudWatchEvents.Types.Sql
import Network.AWS.CloudWatchEvents.Types.SqsParameters
import Network.AWS.CloudWatchEvents.Types.StateReason
import Network.AWS.CloudWatchEvents.Types.StatementId
import Network.AWS.CloudWatchEvents.Types.StatementName
import Network.AWS.CloudWatchEvents.Types.String
import Network.AWS.CloudWatchEvents.Types.Tag
import Network.AWS.CloudWatchEvents.Types.TagKey
import Network.AWS.CloudWatchEvents.Types.Target
import Network.AWS.CloudWatchEvents.Types.TargetArn
import Network.AWS.CloudWatchEvents.Types.TargetId
import Network.AWS.CloudWatchEvents.Types.TargetInput
import Network.AWS.CloudWatchEvents.Types.TargetInputPath
import Network.AWS.CloudWatchEvents.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-10-07@ of the Amazon EventBridge SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CloudWatchEvents",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "events",
      Core._svcVersion = "2015-10-07",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CloudWatchEvents",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | This rule was created by an AWS service on behalf of your account. It is managed by that service. If you see this error in response to @DeleteRule@ or @RemoveTargets@ , you can use the @Force@ parameter in those calls to delete the rule or remove targets from the rule. You cannot modify these managed rules by using @DisableRule@ , @EnableRule@ , @PutTargets@ , @PutRule@ , @TagResource@ , or @UntagResource@ .
_ManagedRuleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ManagedRuleException =
  Core._MatchServiceError mkServiceConfig "ManagedRuleException"
{-# DEPRECATED _ManagedRuleException "Use generic-lens or generic-optics instead." #-}

-- | An error occurred because a replay can be canceled only when the state is Running or Starting.
_IllegalStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalStatusException =
  Core._MatchServiceError mkServiceConfig "IllegalStatusException"
{-# DEPRECATED _IllegalStatusException "Use generic-lens or generic-optics instead." #-}

-- | The event bus policy is too long. For more information, see the limits.
_PolicyLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "PolicyLengthExceededException"
{-# DEPRECATED _PolicyLengthExceededException "Use generic-lens or generic-optics instead." #-}

-- | The resource you are trying to create already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAlreadyExistsException"
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The operation you are attempting is not available in this region.
_OperationDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationDisabledException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationDisabledException"
{-# DEPRECATED _OperationDisabledException "Use generic-lens or generic-optics instead." #-}

-- | There is concurrent modification on a rule, target, archive, or replay.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | The event pattern is not valid.
_InvalidEventPatternException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEventPatternException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidEventPatternException"
{-# DEPRECATED _InvalidEventPatternException "Use generic-lens or generic-optics instead." #-}

-- | This exception occurs due to unexpected causes.
_InternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError mkServiceConfig "InternalException"
{-# DEPRECATED _InternalException "Use generic-lens or generic-optics instead." #-}

-- | An entity that you specified does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified state is not a valid state for an event source.
_InvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError mkServiceConfig "InvalidStateException"
{-# DEPRECATED _InvalidStateException "Use generic-lens or generic-optics instead." #-}

-- | The request failed because it attempted to create resource beyond the allowed service quota.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
