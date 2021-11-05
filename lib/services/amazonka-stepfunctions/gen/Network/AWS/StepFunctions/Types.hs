{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StepFunctions.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ExecutionLimitExceeded,
    _InvalidDefinition,
    _StateMachineLimitExceeded,
    _ExecutionAlreadyExists,
    _StateMachineAlreadyExists,
    _TaskTimedOut,
    _InvalidExecutionInput,
    _InvalidOutput,
    _InvalidName,
    _TaskDoesNotExist,
    _ActivityDoesNotExist,
    _StateMachineDeleting,
    _StateMachineTypeNotSupported,
    _MissingRequiredParameter,
    _InvalidArn,
    _InvalidToken,
    _InvalidLoggingConfiguration,
    _ActivityWorkerLimitExceeded,
    _InvalidTracingConfiguration,
    _ActivityLimitExceeded,
    _TooManyTags,
    _ExecutionDoesNotExist,
    _StateMachineDoesNotExist,
    _ResourceNotFound,

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * HistoryEventType
    HistoryEventType (..),

    -- * LogLevel
    LogLevel (..),

    -- * StateMachineStatus
    StateMachineStatus (..),

    -- * StateMachineType
    StateMachineType (..),

    -- * SyncExecutionStatus
    SyncExecutionStatus (..),

    -- * ActivityFailedEventDetails
    ActivityFailedEventDetails (..),
    newActivityFailedEventDetails,
    activityFailedEventDetails_error,
    activityFailedEventDetails_cause,

    -- * ActivityListItem
    ActivityListItem (..),
    newActivityListItem,
    activityListItem_activityArn,
    activityListItem_name,
    activityListItem_creationDate,

    -- * ActivityScheduleFailedEventDetails
    ActivityScheduleFailedEventDetails (..),
    newActivityScheduleFailedEventDetails,
    activityScheduleFailedEventDetails_error,
    activityScheduleFailedEventDetails_cause,

    -- * ActivityScheduledEventDetails
    ActivityScheduledEventDetails (..),
    newActivityScheduledEventDetails,
    activityScheduledEventDetails_heartbeatInSeconds,
    activityScheduledEventDetails_inputDetails,
    activityScheduledEventDetails_input,
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
    activityTimedOutEventDetails_error,
    activityTimedOutEventDetails_cause,

    -- * BillingDetails
    BillingDetails (..),
    newBillingDetails,
    billingDetails_billedMemoryUsedInMB,
    billingDetails_billedDurationInMilliseconds,

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
    executionAbortedEventDetails_error,
    executionAbortedEventDetails_cause,

    -- * ExecutionFailedEventDetails
    ExecutionFailedEventDetails (..),
    newExecutionFailedEventDetails,
    executionFailedEventDetails_error,
    executionFailedEventDetails_cause,

    -- * ExecutionListItem
    ExecutionListItem (..),
    newExecutionListItem,
    executionListItem_stopDate,
    executionListItem_executionArn,
    executionListItem_stateMachineArn,
    executionListItem_name,
    executionListItem_status,
    executionListItem_startDate,

    -- * ExecutionStartedEventDetails
    ExecutionStartedEventDetails (..),
    newExecutionStartedEventDetails,
    executionStartedEventDetails_inputDetails,
    executionStartedEventDetails_input,
    executionStartedEventDetails_roleArn,

    -- * ExecutionSucceededEventDetails
    ExecutionSucceededEventDetails (..),
    newExecutionSucceededEventDetails,
    executionSucceededEventDetails_output,
    executionSucceededEventDetails_outputDetails,

    -- * ExecutionTimedOutEventDetails
    ExecutionTimedOutEventDetails (..),
    newExecutionTimedOutEventDetails,
    executionTimedOutEventDetails_error,
    executionTimedOutEventDetails_cause,

    -- * HistoryEvent
    HistoryEvent (..),
    newHistoryEvent,
    historyEvent_mapStateStartedEventDetails,
    historyEvent_taskSubmitFailedEventDetails,
    historyEvent_taskStartedEventDetails,
    historyEvent_activityStartedEventDetails,
    historyEvent_taskSubmittedEventDetails,
    historyEvent_lambdaFunctionStartFailedEventDetails,
    historyEvent_taskStartFailedEventDetails,
    historyEvent_stateExitedEventDetails,
    historyEvent_lambdaFunctionSucceededEventDetails,
    historyEvent_taskSucceededEventDetails,
    historyEvent_activitySucceededEventDetails,
    historyEvent_mapIterationAbortedEventDetails,
    historyEvent_mapIterationSucceededEventDetails,
    historyEvent_mapIterationStartedEventDetails,
    historyEvent_lambdaFunctionTimedOutEventDetails,
    historyEvent_taskTimedOutEventDetails,
    historyEvent_activityTimedOutEventDetails,
    historyEvent_executionFailedEventDetails,
    historyEvent_executionAbortedEventDetails,
    historyEvent_executionSucceededEventDetails,
    historyEvent_lambdaFunctionScheduledEventDetails,
    historyEvent_taskScheduledEventDetails,
    historyEvent_activityScheduledEventDetails,
    historyEvent_executionStartedEventDetails,
    historyEvent_activityScheduleFailedEventDetails,
    historyEvent_lambdaFunctionScheduleFailedEventDetails,
    historyEvent_stateEnteredEventDetails,
    historyEvent_previousEventId,
    historyEvent_activityFailedEventDetails,
    historyEvent_taskFailedEventDetails,
    historyEvent_lambdaFunctionFailedEventDetails,
    historyEvent_executionTimedOutEventDetails,
    historyEvent_mapIterationFailedEventDetails,
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
    lambdaFunctionFailedEventDetails_error,
    lambdaFunctionFailedEventDetails_cause,

    -- * LambdaFunctionScheduleFailedEventDetails
    LambdaFunctionScheduleFailedEventDetails (..),
    newLambdaFunctionScheduleFailedEventDetails,
    lambdaFunctionScheduleFailedEventDetails_error,
    lambdaFunctionScheduleFailedEventDetails_cause,

    -- * LambdaFunctionScheduledEventDetails
    LambdaFunctionScheduledEventDetails (..),
    newLambdaFunctionScheduledEventDetails,
    lambdaFunctionScheduledEventDetails_inputDetails,
    lambdaFunctionScheduledEventDetails_input,
    lambdaFunctionScheduledEventDetails_timeoutInSeconds,
    lambdaFunctionScheduledEventDetails_resource,

    -- * LambdaFunctionStartFailedEventDetails
    LambdaFunctionStartFailedEventDetails (..),
    newLambdaFunctionStartFailedEventDetails,
    lambdaFunctionStartFailedEventDetails_error,
    lambdaFunctionStartFailedEventDetails_cause,

    -- * LambdaFunctionSucceededEventDetails
    LambdaFunctionSucceededEventDetails (..),
    newLambdaFunctionSucceededEventDetails,
    lambdaFunctionSucceededEventDetails_output,
    lambdaFunctionSucceededEventDetails_outputDetails,

    -- * LambdaFunctionTimedOutEventDetails
    LambdaFunctionTimedOutEventDetails (..),
    newLambdaFunctionTimedOutEventDetails,
    lambdaFunctionTimedOutEventDetails_error,
    lambdaFunctionTimedOutEventDetails_cause,

    -- * LogDestination
    LogDestination (..),
    newLogDestination,
    logDestination_cloudWatchLogsLogGroup,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_includeExecutionData,
    loggingConfiguration_destinations,
    loggingConfiguration_level,

    -- * MapIterationEventDetails
    MapIterationEventDetails (..),
    newMapIterationEventDetails,
    mapIterationEventDetails_name,
    mapIterationEventDetails_index,

    -- * MapStateStartedEventDetails
    MapStateStartedEventDetails (..),
    newMapStateStartedEventDetails,
    mapStateStartedEventDetails_length,

    -- * StateEnteredEventDetails
    StateEnteredEventDetails (..),
    newStateEnteredEventDetails,
    stateEnteredEventDetails_inputDetails,
    stateEnteredEventDetails_input,
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
    tag_value,
    tag_key,

    -- * TaskFailedEventDetails
    TaskFailedEventDetails (..),
    newTaskFailedEventDetails,
    taskFailedEventDetails_error,
    taskFailedEventDetails_cause,
    taskFailedEventDetails_resourceType,
    taskFailedEventDetails_resource,

    -- * TaskScheduledEventDetails
    TaskScheduledEventDetails (..),
    newTaskScheduledEventDetails,
    taskScheduledEventDetails_heartbeatInSeconds,
    taskScheduledEventDetails_timeoutInSeconds,
    taskScheduledEventDetails_resourceType,
    taskScheduledEventDetails_resource,
    taskScheduledEventDetails_region,
    taskScheduledEventDetails_parameters,

    -- * TaskStartFailedEventDetails
    TaskStartFailedEventDetails (..),
    newTaskStartFailedEventDetails,
    taskStartFailedEventDetails_error,
    taskStartFailedEventDetails_cause,
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
    taskSubmitFailedEventDetails_error,
    taskSubmitFailedEventDetails_cause,
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
    taskTimedOutEventDetails_error,
    taskTimedOutEventDetails_cause,
    taskTimedOutEventDetails_resourceType,
    taskTimedOutEventDetails_resource,

    -- * TracingConfiguration
    TracingConfiguration (..),
    newTracingConfiguration,
    tracingConfiguration_enabled,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
import Amazonka.StepFunctions.Types.MapStateStartedEventDetails
import Amazonka.StepFunctions.Types.StateEnteredEventDetails
import Amazonka.StepFunctions.Types.StateExitedEventDetails
import Amazonka.StepFunctions.Types.StateMachineListItem
import Amazonka.StepFunctions.Types.StateMachineStatus
import Amazonka.StepFunctions.Types.StateMachineType
import Amazonka.StepFunctions.Types.SyncExecutionStatus
import Amazonka.StepFunctions.Types.Tag
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
    { Core._serviceAbbrev = "StepFunctions",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "states",
      Core._serviceSigningName = "states",
      Core._serviceVersion = "2016-11-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "StepFunctions",
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

-- | The maximum number of running executions has been reached. Running
-- executions must end or be stopped before a new execution can be started.
_ExecutionLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExecutionLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ExecutionLimitExceeded"

-- | The provided Amazon States Language definition is invalid.
_InvalidDefinition :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDefinition =
  Core._MatchServiceError
    defaultService
    "InvalidDefinition"

-- | The maximum number of state machines has been reached. Existing state
-- machines must be deleted before a new state machine can be created.
_StateMachineLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StateMachineLimitExceeded =
  Core._MatchServiceError
    defaultService
    "StateMachineLimitExceeded"

-- | The execution has the same @name@ as another execution (but a different
-- @input@).
--
-- Executions with the same @name@ and @input@ are considered idempotent.
_ExecutionAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExecutionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ExecutionAlreadyExists"

-- | A state machine with the same name but a different definition or role
-- ARN already exists.
_StateMachineAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StateMachineAlreadyExists =
  Core._MatchServiceError
    defaultService
    "StateMachineAlreadyExists"

-- | Prism for TaskTimedOut' errors.
_TaskTimedOut :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaskTimedOut =
  Core._MatchServiceError
    defaultService
    "TaskTimedOut"

-- | The provided JSON input data is invalid.
_InvalidExecutionInput :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExecutionInput =
  Core._MatchServiceError
    defaultService
    "InvalidExecutionInput"

-- | The provided JSON output data is invalid.
_InvalidOutput :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOutput =
  Core._MatchServiceError
    defaultService
    "InvalidOutput"

-- | The provided name is invalid.
_InvalidName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidName =
  Core._MatchServiceError
    defaultService
    "InvalidName"

-- | Prism for TaskDoesNotExist' errors.
_TaskDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaskDoesNotExist =
  Core._MatchServiceError
    defaultService
    "TaskDoesNotExist"

-- | The specified activity does not exist.
_ActivityDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ActivityDoesNotExist =
  Core._MatchServiceError
    defaultService
    "ActivityDoesNotExist"

-- | The specified state machine is being deleted.
_StateMachineDeleting :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StateMachineDeleting =
  Core._MatchServiceError
    defaultService
    "StateMachineDeleting"

-- |
_StateMachineTypeNotSupported :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StateMachineTypeNotSupported =
  Core._MatchServiceError
    defaultService
    "StateMachineTypeNotSupported"

-- | Request is missing a required parameter. This error occurs if both
-- @definition@ and @roleArn@ are not specified.
_MissingRequiredParameter :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameter =
  Core._MatchServiceError
    defaultService
    "MissingRequiredParameter"

-- | The provided Amazon Resource Name (ARN) is invalid.
_InvalidArn :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArn =
  Core._MatchServiceError defaultService "InvalidArn"

-- | The provided token is invalid.
_InvalidToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidToken =
  Core._MatchServiceError
    defaultService
    "InvalidToken"

-- |
_InvalidLoggingConfiguration :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLoggingConfiguration =
  Core._MatchServiceError
    defaultService
    "InvalidLoggingConfiguration"

-- | The maximum number of workers concurrently polling for activity tasks
-- has been reached.
_ActivityWorkerLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ActivityWorkerLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ActivityWorkerLimitExceeded"

-- | Your @tracingConfiguration@ key does not match, or @enabled@ has not
-- been set to @true@ or @false@.
_InvalidTracingConfiguration :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTracingConfiguration =
  Core._MatchServiceError
    defaultService
    "InvalidTracingConfiguration"

-- | The maximum number of activities has been reached. Existing activities
-- must be deleted before a new activity can be created.
_ActivityLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ActivityLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ActivityLimitExceeded"

-- | You\'ve exceeded the number of tags allowed for a resource. See the
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html Limits Topic>
-- in the AWS Step Functions Developer Guide.
_TooManyTags :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTags =
  Core._MatchServiceError
    defaultService
    "TooManyTags"

-- | The specified execution does not exist.
_ExecutionDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExecutionDoesNotExist =
  Core._MatchServiceError
    defaultService
    "ExecutionDoesNotExist"

-- | The specified state machine does not exist.
_StateMachineDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StateMachineDoesNotExist =
  Core._MatchServiceError
    defaultService
    "StateMachineDoesNotExist"

-- | Could not find the referenced resource. Only state machine and activity
-- ARNs are supported.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
