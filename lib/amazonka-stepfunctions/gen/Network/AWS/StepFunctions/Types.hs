{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types
  ( -- * Service Configuration
    stepFunctions,

    -- * Errors

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
    ActivityFailedEventDetails,
    activityFailedEventDetails,
    afedError,
    afedCause,

    -- * ActivityListItem
    ActivityListItem,
    activityListItem,
    aliActivityARN,
    aliName,
    aliCreationDate,

    -- * ActivityScheduleFailedEventDetails
    ActivityScheduleFailedEventDetails,
    activityScheduleFailedEventDetails,
    asfedError,
    asfedCause,

    -- * ActivityScheduledEventDetails
    ActivityScheduledEventDetails,
    activityScheduledEventDetails,
    asedHeartbeatInSeconds,
    asedInputDetails,
    asedInput,
    asedTimeoutInSeconds,
    asedResource,

    -- * ActivityStartedEventDetails
    ActivityStartedEventDetails,
    activityStartedEventDetails,
    asedWorkerName,

    -- * ActivitySucceededEventDetails
    ActivitySucceededEventDetails,
    activitySucceededEventDetails,
    asedOutput,
    asedOutputDetails,

    -- * ActivityTimedOutEventDetails
    ActivityTimedOutEventDetails,
    activityTimedOutEventDetails,
    atoedError,
    atoedCause,

    -- * BillingDetails
    BillingDetails,
    billingDetails,
    bdBilledMemoryUsedInMB,
    bdBilledDurationInMilliseconds,

    -- * CloudWatchEventsExecutionDataDetails
    CloudWatchEventsExecutionDataDetails,
    cloudWatchEventsExecutionDataDetails,
    cweeddIncluded,

    -- * CloudWatchLogsLogGroup
    CloudWatchLogsLogGroup,
    cloudWatchLogsLogGroup,
    cwllgLogGroupARN,

    -- * ExecutionAbortedEventDetails
    ExecutionAbortedEventDetails,
    executionAbortedEventDetails,
    eaedError,
    eaedCause,

    -- * ExecutionFailedEventDetails
    ExecutionFailedEventDetails,
    executionFailedEventDetails,
    efedError,
    efedCause,

    -- * ExecutionListItem
    ExecutionListItem,
    executionListItem,
    eliStopDate,
    eliExecutionARN,
    eliStateMachineARN,
    eliName,
    eliStatus,
    eliStartDate,

    -- * ExecutionStartedEventDetails
    ExecutionStartedEventDetails,
    executionStartedEventDetails,
    esedInputDetails,
    esedInput,
    esedRoleARN,

    -- * ExecutionSucceededEventDetails
    ExecutionSucceededEventDetails,
    executionSucceededEventDetails,
    esedOutput,
    esedOutputDetails,

    -- * ExecutionTimedOutEventDetails
    ExecutionTimedOutEventDetails,
    executionTimedOutEventDetails,
    etoedError,
    etoedCause,

    -- * HistoryEvent
    HistoryEvent,
    historyEvent,
    heMapStateStartedEventDetails,
    heTaskSubmitFailedEventDetails,
    heTaskStartedEventDetails,
    heActivityStartedEventDetails,
    heTaskSubmittedEventDetails,
    heLambdaFunctionStartFailedEventDetails,
    heTaskStartFailedEventDetails,
    heStateExitedEventDetails,
    heLambdaFunctionSucceededEventDetails,
    heTaskSucceededEventDetails,
    heActivitySucceededEventDetails,
    heMapIterationAbortedEventDetails,
    heMapIterationSucceededEventDetails,
    heMapIterationStartedEventDetails,
    heLambdaFunctionTimedOutEventDetails,
    heTaskTimedOutEventDetails,
    heActivityTimedOutEventDetails,
    heExecutionFailedEventDetails,
    heExecutionAbortedEventDetails,
    heExecutionSucceededEventDetails,
    heLambdaFunctionScheduledEventDetails,
    heTaskScheduledEventDetails,
    heActivityScheduledEventDetails,
    heExecutionStartedEventDetails,
    heActivityScheduleFailedEventDetails,
    heLambdaFunctionScheduleFailedEventDetails,
    heStateEnteredEventDetails,
    hePreviousEventId,
    heActivityFailedEventDetails,
    heTaskFailedEventDetails,
    heLambdaFunctionFailedEventDetails,
    heExecutionTimedOutEventDetails,
    heMapIterationFailedEventDetails,
    heTimestamp,
    heType,
    heId,

    -- * HistoryEventExecutionDataDetails
    HistoryEventExecutionDataDetails,
    historyEventExecutionDataDetails,
    heeddTruncated,

    -- * LambdaFunctionFailedEventDetails
    LambdaFunctionFailedEventDetails,
    lambdaFunctionFailedEventDetails,
    lffedError,
    lffedCause,

    -- * LambdaFunctionScheduleFailedEventDetails
    LambdaFunctionScheduleFailedEventDetails,
    lambdaFunctionScheduleFailedEventDetails,
    lError,
    lCause,

    -- * LambdaFunctionScheduledEventDetails
    LambdaFunctionScheduledEventDetails,
    lambdaFunctionScheduledEventDetails,
    lfsedInputDetails,
    lfsedInput,
    lfsedTimeoutInSeconds,
    lfsedResource,

    -- * LambdaFunctionStartFailedEventDetails
    LambdaFunctionStartFailedEventDetails,
    lambdaFunctionStartFailedEventDetails,
    lfsfedError,
    lfsfedCause,

    -- * LambdaFunctionSucceededEventDetails
    LambdaFunctionSucceededEventDetails,
    lambdaFunctionSucceededEventDetails,
    lfsedOutput,
    lfsedOutputDetails,

    -- * LambdaFunctionTimedOutEventDetails
    LambdaFunctionTimedOutEventDetails,
    lambdaFunctionTimedOutEventDetails,
    lftoedError,
    lftoedCause,

    -- * LogDestination
    LogDestination,
    logDestination,
    ldCloudWatchLogsLogGroup,

    -- * LoggingConfiguration
    LoggingConfiguration,
    loggingConfiguration,
    lcIncludeExecutionData,
    lcDestinations,
    lcLevel,

    -- * MapIterationEventDetails
    MapIterationEventDetails,
    mapIterationEventDetails,
    miedName,
    miedIndex,

    -- * MapStateStartedEventDetails
    MapStateStartedEventDetails,
    mapStateStartedEventDetails,
    mssedLength,

    -- * StateEnteredEventDetails
    StateEnteredEventDetails,
    stateEnteredEventDetails,
    sInputDetails,
    sInput,
    sName,

    -- * StateExitedEventDetails
    StateExitedEventDetails,
    stateExitedEventDetails,
    seedOutput,
    seedOutputDetails,
    seedName,

    -- * StateMachineListItem
    StateMachineListItem,
    stateMachineListItem,
    smliStateMachineARN,
    smliName,
    smliType,
    smliCreationDate,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TaskFailedEventDetails
    TaskFailedEventDetails,
    taskFailedEventDetails,
    tfedError,
    tfedCause,
    tfedResourceType,
    tfedResource,

    -- * TaskScheduledEventDetails
    TaskScheduledEventDetails,
    taskScheduledEventDetails,
    tasHeartbeatInSeconds,
    tasTimeoutInSeconds,
    tasResourceType,
    tasResource,
    tasRegion,
    tasParameters,

    -- * TaskStartFailedEventDetails
    TaskStartFailedEventDetails,
    taskStartFailedEventDetails,
    tsfedsError,
    tsfedsCause,
    tsfedsResourceType,
    tsfedsResource,

    -- * TaskStartedEventDetails
    TaskStartedEventDetails,
    taskStartedEventDetails,
    tsedResourceType,
    tsedResource,

    -- * TaskSubmitFailedEventDetails
    TaskSubmitFailedEventDetails,
    taskSubmitFailedEventDetails,
    tsfedError,
    tsfedCause,
    tsfedResourceType,
    tsfedResource,

    -- * TaskSubmittedEventDetails
    TaskSubmittedEventDetails,
    taskSubmittedEventDetails,
    tOutput,
    tOutputDetails,
    tResourceType,
    tResource,

    -- * TaskSucceededEventDetails
    TaskSucceededEventDetails,
    taskSucceededEventDetails,
    tsedsOutput,
    tsedsOutputDetails,
    tsedsResourceType,
    tsedsResource,

    -- * TaskTimedOutEventDetails
    TaskTimedOutEventDetails,
    taskTimedOutEventDetails,
    ttoedError,
    ttoedCause,
    ttoedResourceType,
    ttoedResource,

    -- * TracingConfiguration
    TracingConfiguration,
    tracingConfiguration,
    tcEnabled,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
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
stepFunctions :: Service
stepFunctions =
  Service
    { _svcAbbrev = "StepFunctions",
      _svcSigner = v4,
      _svcPrefix = "states",
      _svcVersion = "2016-11-23",
      _svcEndpoint = defaultEndpoint stepFunctions,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "StepFunctions",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
