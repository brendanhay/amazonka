{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ExecutionLimitExceeded,
    _ExecutionAlreadyExists,
    _StateMachineTypeNotSupported,
    _ExecutionDoesNotExist,
    _InvalidName,
    _InvalidOutput,
    _ActivityLimitExceeded,
    _InvalidExecutionInput,
    _InvalidLoggingConfiguration,
    _TaskTimedOut,
    _StateMachineLimitExceeded,
    _InvalidArn,
    _InvalidDefinition,
    _MissingRequiredParameter,
    _StateMachineAlreadyExists,
    _ResourceNotFound,
    _StateMachineDoesNotExist,
    _TaskDoesNotExist,
    _StateMachineDeleting,
    _ActivityDoesNotExist,
    _TooManyTags,
    _ActivityWorkerLimitExceeded,
    _InvalidTracingConfiguration,
    _InvalidToken,

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
    activityTimedOutEventDetails_cause,
    activityTimedOutEventDetails_error,

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
    executionStartedEventDetails_roleArn,
    executionStartedEventDetails_input,

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
    historyEvent_executionFailedEventDetails,
    historyEvent_mapIterationStartedEventDetails,
    historyEvent_taskSubmitFailedEventDetails,
    historyEvent_mapIterationAbortedEventDetails,
    historyEvent_mapIterationSucceededEventDetails,
    historyEvent_mapIterationFailedEventDetails,
    historyEvent_executionTimedOutEventDetails,
    historyEvent_previousEventId,
    historyEvent_executionStartedEventDetails,
    historyEvent_lambdaFunctionScheduleFailedEventDetails,
    historyEvent_activityScheduleFailedEventDetails,
    historyEvent_taskScheduledEventDetails,
    historyEvent_activityScheduledEventDetails,
    historyEvent_lambdaFunctionScheduledEventDetails,
    historyEvent_executionSucceededEventDetails,
    historyEvent_executionAbortedEventDetails,
    historyEvent_mapStateStartedEventDetails,
    historyEvent_lambdaFunctionTimedOutEventDetails,
    historyEvent_activityTimedOutEventDetails,
    historyEvent_taskTimedOutEventDetails,
    historyEvent_lambdaFunctionStartFailedEventDetails,
    historyEvent_taskStartFailedEventDetails,
    historyEvent_taskFailedEventDetails,
    historyEvent_taskSucceededEventDetails,
    historyEvent_stateExitedEventDetails,
    historyEvent_stateEnteredEventDetails,
    historyEvent_lambdaFunctionFailedEventDetails,
    historyEvent_activityFailedEventDetails,
    historyEvent_activitySucceededEventDetails,
    historyEvent_lambdaFunctionSucceededEventDetails,
    historyEvent_taskSubmittedEventDetails,
    historyEvent_activityStartedEventDetails,
    historyEvent_taskStartedEventDetails,
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
    lambdaFunctionScheduledEventDetails_inputDetails,
    lambdaFunctionScheduledEventDetails_input,
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
    loggingConfiguration_level,
    loggingConfiguration_includeExecutionData,

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
    tag_key,
    tag_value,

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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.StepFunctions.Types.ActivityFailedEventDetails
import Network.AWS.StepFunctions.Types.ActivityListItem
import Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
import Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
import Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
import Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
import Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
import Network.AWS.StepFunctions.Types.BillingDetails
import Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
import Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
import Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionListItem
import Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionStatus
import Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
import Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
import Network.AWS.StepFunctions.Types.HistoryEvent
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
import Network.AWS.StepFunctions.Types.HistoryEventType
import Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
import Network.AWS.StepFunctions.Types.LogDestination
import Network.AWS.StepFunctions.Types.LogLevel
import Network.AWS.StepFunctions.Types.LoggingConfiguration
import Network.AWS.StepFunctions.Types.MapIterationEventDetails
import Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
import Network.AWS.StepFunctions.Types.StateEnteredEventDetails
import Network.AWS.StepFunctions.Types.StateExitedEventDetails
import Network.AWS.StepFunctions.Types.StateMachineListItem
import Network.AWS.StepFunctions.Types.StateMachineStatus
import Network.AWS.StepFunctions.Types.StateMachineType
import Network.AWS.StepFunctions.Types.SyncExecutionStatus
import Network.AWS.StepFunctions.Types.Tag
import Network.AWS.StepFunctions.Types.TaskFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskScheduledEventDetails
import Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskStartedEventDetails
import Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
import Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
import Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
import Network.AWS.StepFunctions.Types.TracingConfiguration

-- | API version @2016-11-23@ of the Amazon Step Functions SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "StepFunctions",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "states",
      Prelude._svcVersion = "2016-11-23",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "StepFunctions",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The maximum number of running executions has been reached. Running
-- executions must end or be stopped before a new execution can be started.
_ExecutionLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExecutionLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "ExecutionLimitExceeded"

-- | The execution has the same @name@ as another execution (but a different
-- @input@).
--
-- Executions with the same @name@ and @input@ are considered idempotent.
_ExecutionAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExecutionAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "ExecutionAlreadyExists"

-- |
_StateMachineTypeNotSupported :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StateMachineTypeNotSupported =
  Prelude._MatchServiceError
    defaultService
    "StateMachineTypeNotSupported"

-- | The specified execution does not exist.
_ExecutionDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExecutionDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "ExecutionDoesNotExist"

-- | The provided name is invalid.
_InvalidName :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidName =
  Prelude._MatchServiceError
    defaultService
    "InvalidName"

-- | The provided JSON output data is invalid.
_InvalidOutput :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOutput =
  Prelude._MatchServiceError
    defaultService
    "InvalidOutput"

-- | The maximum number of activities has been reached. Existing activities
-- must be deleted before a new activity can be created.
_ActivityLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ActivityLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "ActivityLimitExceeded"

-- | The provided JSON input data is invalid.
_InvalidExecutionInput :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidExecutionInput =
  Prelude._MatchServiceError
    defaultService
    "InvalidExecutionInput"

-- |
_InvalidLoggingConfiguration :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLoggingConfiguration =
  Prelude._MatchServiceError
    defaultService
    "InvalidLoggingConfiguration"

-- | Prism for TaskTimedOut' errors.
_TaskTimedOut :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TaskTimedOut =
  Prelude._MatchServiceError
    defaultService
    "TaskTimedOut"

-- | The maximum number of state machines has been reached. Existing state
-- machines must be deleted before a new state machine can be created.
_StateMachineLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StateMachineLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "StateMachineLimitExceeded"

-- | The provided Amazon Resource Name (ARN) is invalid.
_InvalidArn :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArn =
  Prelude._MatchServiceError
    defaultService
    "InvalidArn"

-- | The provided Amazon States Language definition is invalid.
_InvalidDefinition :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDefinition =
  Prelude._MatchServiceError
    defaultService
    "InvalidDefinition"

-- | Request is missing a required parameter. This error occurs if both
-- @definition@ and @roleArn@ are not specified.
_MissingRequiredParameter :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingRequiredParameter =
  Prelude._MatchServiceError
    defaultService
    "MissingRequiredParameter"

-- | A state machine with the same name but a different definition or role
-- ARN already exists.
_StateMachineAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StateMachineAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "StateMachineAlreadyExists"

-- | Could not find the referenced resource. Only state machine and activity
-- ARNs are supported.
_ResourceNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFound =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFound"

-- | The specified state machine does not exist.
_StateMachineDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StateMachineDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "StateMachineDoesNotExist"

-- | Prism for TaskDoesNotExist' errors.
_TaskDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TaskDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "TaskDoesNotExist"

-- | The specified state machine is being deleted.
_StateMachineDeleting :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StateMachineDeleting =
  Prelude._MatchServiceError
    defaultService
    "StateMachineDeleting"

-- | The specified activity does not exist.
_ActivityDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ActivityDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "ActivityDoesNotExist"

-- | You\'ve exceeded the number of tags allowed for a resource. See the
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html Limits Topic>
-- in the AWS Step Functions Developer Guide.
_TooManyTags :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTags =
  Prelude._MatchServiceError
    defaultService
    "TooManyTags"

-- | The maximum number of workers concurrently polling for activity tasks
-- has been reached.
_ActivityWorkerLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ActivityWorkerLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "ActivityWorkerLimitExceeded"

-- | Your @tracingConfiguration@ key does not match, or @enabled@ has not
-- been set to @true@ or @false@.
_InvalidTracingConfiguration :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTracingConfiguration =
  Prelude._MatchServiceError
    defaultService
    "InvalidTracingConfiguration"

-- | The provided token is invalid.
_InvalidToken :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidToken =
  Prelude._MatchServiceError
    defaultService
    "InvalidToken"
