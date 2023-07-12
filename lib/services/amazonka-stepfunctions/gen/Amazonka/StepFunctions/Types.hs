{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StepFunctions.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ActivityDoesNotExist,
    _ActivityLimitExceeded,
    _ActivityWorkerLimitExceeded,
    _ExecutionAlreadyExists,
    _ExecutionDoesNotExist,
    _ExecutionLimitExceeded,
    _InvalidArn,
    _InvalidDefinition,
    _InvalidExecutionInput,
    _InvalidLoggingConfiguration,
    _InvalidName,
    _InvalidOutput,
    _InvalidToken,
    _InvalidTracingConfiguration,
    _MissingRequiredParameter,
    _ResourceNotFound,
    _StateMachineAlreadyExists,
    _StateMachineDeleting,
    _StateMachineDoesNotExist,
    _StateMachineLimitExceeded,
    _StateMachineTypeNotSupported,
    _TaskDoesNotExist,
    _TaskTimedOut,
    _TooManyTags,
    _ValidationException,

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * HistoryEventType
    HistoryEventType (..),

    -- * LogLevel
    LogLevel (..),

    -- * MapRunStatus
    MapRunStatus (..),

    -- * StateMachineStatus
    StateMachineStatus (..),

    -- * StateMachineType
    StateMachineType (..),

    -- * SyncExecutionStatus
    SyncExecutionStatus (..),

    -- * ActivityFailedEventDetails
    ActivityFailedEventDetails (..),
    newActivityFailedEventDetails,
    activityFailedEventDetails_cause,
    activityFailedEventDetails_error,

    -- * ActivityListItem
    ActivityListItem (..),
    newActivityListItem,
    activityListItem_activityArn,
    activityListItem_name,
    activityListItem_creationDate,

    -- * ActivityScheduleFailedEventDetails
    ActivityScheduleFailedEventDetails (..),
    newActivityScheduleFailedEventDetails,
    activityScheduleFailedEventDetails_cause,
    activityScheduleFailedEventDetails_error,

    -- * ActivityScheduledEventDetails
    ActivityScheduledEventDetails (..),
    newActivityScheduledEventDetails,
    activityScheduledEventDetails_heartbeatInSeconds,
    activityScheduledEventDetails_input,
    activityScheduledEventDetails_inputDetails,
    activityScheduledEventDetails_timeoutInSeconds,
    activityScheduledEventDetails_resource,

    -- * ActivityStartedEventDetails
    ActivityStartedEventDetails (..),
    newActivityStartedEventDetails,
    activityStartedEventDetails_workerName,

    -- * ActivitySucceededEventDetails
    ActivitySucceededEventDetails (..),
    newActivitySucceededEventDetails,
    activitySucceededEventDetails_output,
    activitySucceededEventDetails_outputDetails,

    -- * ActivityTimedOutEventDetails
    ActivityTimedOutEventDetails (..),
    newActivityTimedOutEventDetails,
    activityTimedOutEventDetails_cause,
    activityTimedOutEventDetails_error,

    -- * BillingDetails
    BillingDetails (..),
    newBillingDetails,
    billingDetails_billedDurationInMilliseconds,
    billingDetails_billedMemoryUsedInMB,

    -- * CloudWatchEventsExecutionDataDetails
    CloudWatchEventsExecutionDataDetails (..),
    newCloudWatchEventsExecutionDataDetails,
    cloudWatchEventsExecutionDataDetails_included,

    -- * CloudWatchLogsLogGroup
    CloudWatchLogsLogGroup (..),
    newCloudWatchLogsLogGroup,
    cloudWatchLogsLogGroup_logGroupArn,

    -- * ExecutionAbortedEventDetails
    ExecutionAbortedEventDetails (..),
    newExecutionAbortedEventDetails,
    executionAbortedEventDetails_cause,
    executionAbortedEventDetails_error,

    -- * ExecutionFailedEventDetails
    ExecutionFailedEventDetails (..),
    newExecutionFailedEventDetails,
    executionFailedEventDetails_cause,
    executionFailedEventDetails_error,

    -- * ExecutionListItem
    ExecutionListItem (..),
    newExecutionListItem,
    executionListItem_itemCount,
    executionListItem_mapRunArn,
    executionListItem_stopDate,
    executionListItem_executionArn,
    executionListItem_stateMachineArn,
    executionListItem_name,
    executionListItem_status,
    executionListItem_startDate,

    -- * ExecutionStartedEventDetails
    ExecutionStartedEventDetails (..),
    newExecutionStartedEventDetails,
    executionStartedEventDetails_input,
    executionStartedEventDetails_inputDetails,
    executionStartedEventDetails_roleArn,

    -- * ExecutionSucceededEventDetails
    ExecutionSucceededEventDetails (..),
    newExecutionSucceededEventDetails,
    executionSucceededEventDetails_output,
    executionSucceededEventDetails_outputDetails,

    -- * ExecutionTimedOutEventDetails
    ExecutionTimedOutEventDetails (..),
    newExecutionTimedOutEventDetails,
    executionTimedOutEventDetails_cause,
    executionTimedOutEventDetails_error,

    -- * HistoryEvent
    HistoryEvent (..),
    newHistoryEvent,
    historyEvent_activityFailedEventDetails,
    historyEvent_activityScheduleFailedEventDetails,
    historyEvent_activityScheduledEventDetails,
    historyEvent_activityStartedEventDetails,
    historyEvent_activitySucceededEventDetails,
    historyEvent_activityTimedOutEventDetails,
    historyEvent_executionAbortedEventDetails,
    historyEvent_executionFailedEventDetails,
    historyEvent_executionStartedEventDetails,
    historyEvent_executionSucceededEventDetails,
    historyEvent_executionTimedOutEventDetails,
    historyEvent_lambdaFunctionFailedEventDetails,
    historyEvent_lambdaFunctionScheduleFailedEventDetails,
    historyEvent_lambdaFunctionScheduledEventDetails,
    historyEvent_lambdaFunctionStartFailedEventDetails,
    historyEvent_lambdaFunctionSucceededEventDetails,
    historyEvent_lambdaFunctionTimedOutEventDetails,
    historyEvent_mapIterationAbortedEventDetails,
    historyEvent_mapIterationFailedEventDetails,
    historyEvent_mapIterationStartedEventDetails,
    historyEvent_mapIterationSucceededEventDetails,
    historyEvent_mapRunFailedEventDetails,
    historyEvent_mapRunStartedEventDetails,
    historyEvent_mapStateStartedEventDetails,
    historyEvent_previousEventId,
    historyEvent_stateEnteredEventDetails,
    historyEvent_stateExitedEventDetails,
    historyEvent_taskFailedEventDetails,
    historyEvent_taskScheduledEventDetails,
    historyEvent_taskStartFailedEventDetails,
    historyEvent_taskStartedEventDetails,
    historyEvent_taskSubmitFailedEventDetails,
    historyEvent_taskSubmittedEventDetails,
    historyEvent_taskSucceededEventDetails,
    historyEvent_taskTimedOutEventDetails,
    historyEvent_timestamp,
    historyEvent_type,
    historyEvent_id,

    -- * HistoryEventExecutionDataDetails
    HistoryEventExecutionDataDetails (..),
    newHistoryEventExecutionDataDetails,
    historyEventExecutionDataDetails_truncated,

    -- * LambdaFunctionFailedEventDetails
    LambdaFunctionFailedEventDetails (..),
    newLambdaFunctionFailedEventDetails,
    lambdaFunctionFailedEventDetails_cause,
    lambdaFunctionFailedEventDetails_error,

    -- * LambdaFunctionScheduleFailedEventDetails
    LambdaFunctionScheduleFailedEventDetails (..),
    newLambdaFunctionScheduleFailedEventDetails,
    lambdaFunctionScheduleFailedEventDetails_cause,
    lambdaFunctionScheduleFailedEventDetails_error,

    -- * LambdaFunctionScheduledEventDetails
    LambdaFunctionScheduledEventDetails (..),
    newLambdaFunctionScheduledEventDetails,
    lambdaFunctionScheduledEventDetails_input,
    lambdaFunctionScheduledEventDetails_inputDetails,
    lambdaFunctionScheduledEventDetails_taskCredentials,
    lambdaFunctionScheduledEventDetails_timeoutInSeconds,
    lambdaFunctionScheduledEventDetails_resource,

    -- * LambdaFunctionStartFailedEventDetails
    LambdaFunctionStartFailedEventDetails (..),
    newLambdaFunctionStartFailedEventDetails,
    lambdaFunctionStartFailedEventDetails_cause,
    lambdaFunctionStartFailedEventDetails_error,

    -- * LambdaFunctionSucceededEventDetails
    LambdaFunctionSucceededEventDetails (..),
    newLambdaFunctionSucceededEventDetails,
    lambdaFunctionSucceededEventDetails_output,
    lambdaFunctionSucceededEventDetails_outputDetails,

    -- * LambdaFunctionTimedOutEventDetails
    LambdaFunctionTimedOutEventDetails (..),
    newLambdaFunctionTimedOutEventDetails,
    lambdaFunctionTimedOutEventDetails_cause,
    lambdaFunctionTimedOutEventDetails_error,

    -- * LogDestination
    LogDestination (..),
    newLogDestination,
    logDestination_cloudWatchLogsLogGroup,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_destinations,
    loggingConfiguration_includeExecutionData,
    loggingConfiguration_level,

    -- * MapIterationEventDetails
    MapIterationEventDetails (..),
    newMapIterationEventDetails,
    mapIterationEventDetails_index,
    mapIterationEventDetails_name,

    -- * MapRunExecutionCounts
    MapRunExecutionCounts (..),
    newMapRunExecutionCounts,
    mapRunExecutionCounts_pending,
    mapRunExecutionCounts_running,
    mapRunExecutionCounts_succeeded,
    mapRunExecutionCounts_failed,
    mapRunExecutionCounts_timedOut,
    mapRunExecutionCounts_aborted,
    mapRunExecutionCounts_total,
    mapRunExecutionCounts_resultsWritten,

    -- * MapRunFailedEventDetails
    MapRunFailedEventDetails (..),
    newMapRunFailedEventDetails,
    mapRunFailedEventDetails_cause,
    mapRunFailedEventDetails_error,

    -- * MapRunItemCounts
    MapRunItemCounts (..),
    newMapRunItemCounts,
    mapRunItemCounts_pending,
    mapRunItemCounts_running,
    mapRunItemCounts_succeeded,
    mapRunItemCounts_failed,
    mapRunItemCounts_timedOut,
    mapRunItemCounts_aborted,
    mapRunItemCounts_total,
    mapRunItemCounts_resultsWritten,

    -- * MapRunListItem
    MapRunListItem (..),
    newMapRunListItem,
    mapRunListItem_stopDate,
    mapRunListItem_executionArn,
    mapRunListItem_mapRunArn,
    mapRunListItem_stateMachineArn,
    mapRunListItem_startDate,

    -- * MapRunStartedEventDetails
    MapRunStartedEventDetails (..),
    newMapRunStartedEventDetails,
    mapRunStartedEventDetails_mapRunArn,

    -- * MapStateStartedEventDetails
    MapStateStartedEventDetails (..),
    newMapStateStartedEventDetails,
    mapStateStartedEventDetails_length,

    -- * StateEnteredEventDetails
    StateEnteredEventDetails (..),
    newStateEnteredEventDetails,
    stateEnteredEventDetails_input,
    stateEnteredEventDetails_inputDetails,
    stateEnteredEventDetails_name,

    -- * StateExitedEventDetails
    StateExitedEventDetails (..),
    newStateExitedEventDetails,
    stateExitedEventDetails_output,
    stateExitedEventDetails_outputDetails,
    stateExitedEventDetails_name,

    -- * StateMachineListItem
    StateMachineListItem (..),
    newStateMachineListItem,
    stateMachineListItem_stateMachineArn,
    stateMachineListItem_name,
    stateMachineListItem_type,
    stateMachineListItem_creationDate,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TaskCredentials
    TaskCredentials (..),
    newTaskCredentials,
    taskCredentials_roleArn,

    -- * TaskFailedEventDetails
    TaskFailedEventDetails (..),
    newTaskFailedEventDetails,
    taskFailedEventDetails_cause,
    taskFailedEventDetails_error,
    taskFailedEventDetails_resourceType,
    taskFailedEventDetails_resource,

    -- * TaskScheduledEventDetails
    TaskScheduledEventDetails (..),
    newTaskScheduledEventDetails,
    taskScheduledEventDetails_heartbeatInSeconds,
    taskScheduledEventDetails_taskCredentials,
    taskScheduledEventDetails_timeoutInSeconds,
    taskScheduledEventDetails_resourceType,
    taskScheduledEventDetails_resource,
    taskScheduledEventDetails_region,
    taskScheduledEventDetails_parameters,

    -- * TaskStartFailedEventDetails
    TaskStartFailedEventDetails (..),
    newTaskStartFailedEventDetails,
    taskStartFailedEventDetails_cause,
    taskStartFailedEventDetails_error,
    taskStartFailedEventDetails_resourceType,
    taskStartFailedEventDetails_resource,

    -- * TaskStartedEventDetails
    TaskStartedEventDetails (..),
    newTaskStartedEventDetails,
    taskStartedEventDetails_resourceType,
    taskStartedEventDetails_resource,

    -- * TaskSubmitFailedEventDetails
    TaskSubmitFailedEventDetails (..),
    newTaskSubmitFailedEventDetails,
    taskSubmitFailedEventDetails_cause,
    taskSubmitFailedEventDetails_error,
    taskSubmitFailedEventDetails_resourceType,
    taskSubmitFailedEventDetails_resource,

    -- * TaskSubmittedEventDetails
    TaskSubmittedEventDetails (..),
    newTaskSubmittedEventDetails,
    taskSubmittedEventDetails_output,
    taskSubmittedEventDetails_outputDetails,
    taskSubmittedEventDetails_resourceType,
    taskSubmittedEventDetails_resource,

    -- * TaskSucceededEventDetails
    TaskSucceededEventDetails (..),
    newTaskSucceededEventDetails,
    taskSucceededEventDetails_output,
    taskSucceededEventDetails_outputDetails,
    taskSucceededEventDetails_resourceType,
    taskSucceededEventDetails_resource,

    -- * TaskTimedOutEventDetails
    TaskTimedOutEventDetails (..),
    newTaskTimedOutEventDetails,
    taskTimedOutEventDetails_cause,
    taskTimedOutEventDetails_error,
    taskTimedOutEventDetails_resourceType,
    taskTimedOutEventDetails_resource,

    -- * TracingConfiguration
    TracingConfiguration (..),
    newTracingConfiguration,
    tracingConfiguration_enabled,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.StepFunctions.Types.ActivityFailedEventDetails
import Amazonka.StepFunctions.Types.ActivityListItem
import Amazonka.StepFunctions.Types.ActivityScheduleFailedEventDetails
import Amazonka.StepFunctions.Types.ActivityScheduledEventDetails
import Amazonka.StepFunctions.Types.ActivityStartedEventDetails
import Amazonka.StepFunctions.Types.ActivitySucceededEventDetails
import Amazonka.StepFunctions.Types.ActivityTimedOutEventDetails
import Amazonka.StepFunctions.Types.BillingDetails
import Amazonka.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
import Amazonka.StepFunctions.Types.CloudWatchLogsLogGroup
import Amazonka.StepFunctions.Types.ExecutionAbortedEventDetails
import Amazonka.StepFunctions.Types.ExecutionFailedEventDetails
import Amazonka.StepFunctions.Types.ExecutionListItem
import Amazonka.StepFunctions.Types.ExecutionStartedEventDetails
import Amazonka.StepFunctions.Types.ExecutionStatus
import Amazonka.StepFunctions.Types.ExecutionSucceededEventDetails
import Amazonka.StepFunctions.Types.ExecutionTimedOutEventDetails
import Amazonka.StepFunctions.Types.HistoryEvent
import Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails
import Amazonka.StepFunctions.Types.HistoryEventType
import Amazonka.StepFunctions.Types.LambdaFunctionFailedEventDetails
import Amazonka.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
import Amazonka.StepFunctions.Types.LambdaFunctionScheduledEventDetails
import Amazonka.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
import Amazonka.StepFunctions.Types.LambdaFunctionSucceededEventDetails
import Amazonka.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
import Amazonka.StepFunctions.Types.LogDestination
import Amazonka.StepFunctions.Types.LogLevel
import Amazonka.StepFunctions.Types.LoggingConfiguration
import Amazonka.StepFunctions.Types.MapIterationEventDetails
import Amazonka.StepFunctions.Types.MapRunExecutionCounts
import Amazonka.StepFunctions.Types.MapRunFailedEventDetails
import Amazonka.StepFunctions.Types.MapRunItemCounts
import Amazonka.StepFunctions.Types.MapRunListItem
import Amazonka.StepFunctions.Types.MapRunStartedEventDetails
import Amazonka.StepFunctions.Types.MapRunStatus
import Amazonka.StepFunctions.Types.MapStateStartedEventDetails
import Amazonka.StepFunctions.Types.StateEnteredEventDetails
import Amazonka.StepFunctions.Types.StateExitedEventDetails
import Amazonka.StepFunctions.Types.StateMachineListItem
import Amazonka.StepFunctions.Types.StateMachineStatus
import Amazonka.StepFunctions.Types.StateMachineType
import Amazonka.StepFunctions.Types.SyncExecutionStatus
import Amazonka.StepFunctions.Types.Tag
import Amazonka.StepFunctions.Types.TaskCredentials
import Amazonka.StepFunctions.Types.TaskFailedEventDetails
import Amazonka.StepFunctions.Types.TaskScheduledEventDetails
import Amazonka.StepFunctions.Types.TaskStartFailedEventDetails
import Amazonka.StepFunctions.Types.TaskStartedEventDetails
import Amazonka.StepFunctions.Types.TaskSubmitFailedEventDetails
import Amazonka.StepFunctions.Types.TaskSubmittedEventDetails
import Amazonka.StepFunctions.Types.TaskSucceededEventDetails
import Amazonka.StepFunctions.Types.TaskTimedOutEventDetails
import Amazonka.StepFunctions.Types.TracingConfiguration

-- | API version @2016-11-23@ of the Amazon Step Functions SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "StepFunctions",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "states",
      Core.signingName = "states",
      Core.version = "2016-11-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "StepFunctions",
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

-- | The specified activity does not exist.
_ActivityDoesNotExist :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ActivityDoesNotExist =
  Core._MatchServiceError
    defaultService
    "ActivityDoesNotExist"

-- | The maximum number of activities has been reached. Existing activities
-- must be deleted before a new activity can be created.
_ActivityLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ActivityLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ActivityLimitExceeded"

-- | The maximum number of workers concurrently polling for activity tasks
-- has been reached.
_ActivityWorkerLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ActivityWorkerLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ActivityWorkerLimitExceeded"

-- | The execution has the same @name@ as another execution (but a different
-- @input@).
--
-- Executions with the same @name@ and @input@ are considered idempotent.
_ExecutionAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExecutionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ExecutionAlreadyExists"

-- | The specified execution does not exist.
_ExecutionDoesNotExist :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExecutionDoesNotExist =
  Core._MatchServiceError
    defaultService
    "ExecutionDoesNotExist"

-- | The maximum number of running executions has been reached. Running
-- executions must end or be stopped before a new execution can be started.
_ExecutionLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExecutionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ExecutionLimitExceeded"

-- | The provided Amazon Resource Name (ARN) is not valid.
_InvalidArn :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArn =
  Core._MatchServiceError defaultService "InvalidArn"

-- | The provided Amazon States Language definition is not valid.
_InvalidDefinition :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDefinition =
  Core._MatchServiceError
    defaultService
    "InvalidDefinition"

-- | The provided JSON input data is not valid.
_InvalidExecutionInput :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidExecutionInput =
  Core._MatchServiceError
    defaultService
    "InvalidExecutionInput"

_InvalidLoggingConfiguration :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLoggingConfiguration =
  Core._MatchServiceError
    defaultService
    "InvalidLoggingConfiguration"

-- | The provided name is not valid.
_InvalidName :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidName =
  Core._MatchServiceError
    defaultService
    "InvalidName"

-- | The provided JSON output data is not valid.
_InvalidOutput :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOutput =
  Core._MatchServiceError
    defaultService
    "InvalidOutput"

-- | The provided token is not valid.
_InvalidToken :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidToken =
  Core._MatchServiceError
    defaultService
    "InvalidToken"

-- | Your @tracingConfiguration@ key does not match, or @enabled@ has not
-- been set to @true@ or @false@.
_InvalidTracingConfiguration :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTracingConfiguration =
  Core._MatchServiceError
    defaultService
    "InvalidTracingConfiguration"

-- | Request is missing a required parameter. This error occurs if both
-- @definition@ and @roleArn@ are not specified.
_MissingRequiredParameter :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingRequiredParameter =
  Core._MatchServiceError
    defaultService
    "MissingRequiredParameter"

-- | Could not find the referenced resource. Only state machine and activity
-- ARNs are supported.
_ResourceNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"

-- | A state machine with the same name but a different definition or role
-- ARN already exists.
_StateMachineAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StateMachineAlreadyExists =
  Core._MatchServiceError
    defaultService
    "StateMachineAlreadyExists"

-- | The specified state machine is being deleted.
_StateMachineDeleting :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StateMachineDeleting =
  Core._MatchServiceError
    defaultService
    "StateMachineDeleting"

-- | The specified state machine does not exist.
_StateMachineDoesNotExist :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StateMachineDoesNotExist =
  Core._MatchServiceError
    defaultService
    "StateMachineDoesNotExist"

-- | The maximum number of state machines has been reached. Existing state
-- machines must be deleted before a new state machine can be created.
_StateMachineLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StateMachineLimitExceeded =
  Core._MatchServiceError
    defaultService
    "StateMachineLimitExceeded"

_StateMachineTypeNotSupported :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StateMachineTypeNotSupported =
  Core._MatchServiceError
    defaultService
    "StateMachineTypeNotSupported"

-- | Prism for TaskDoesNotExist' errors.
_TaskDoesNotExist :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TaskDoesNotExist =
  Core._MatchServiceError
    defaultService
    "TaskDoesNotExist"

-- | Prism for TaskTimedOut' errors.
_TaskTimedOut :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TaskTimedOut =
  Core._MatchServiceError
    defaultService
    "TaskTimedOut"

-- | You\'ve exceeded the number of tags allowed for a resource. See the
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html Limits Topic>
-- in the Step Functions Developer Guide.
_TooManyTags :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTags =
  Core._MatchServiceError
    defaultService
    "TooManyTags"

-- | The input does not satisfy the constraints specified by an Amazon Web
-- Services service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
