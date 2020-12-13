-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types
  ( -- * Service configuration
    stepFunctionsService,

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
    ActivityFailedEventDetails (..),
    mkActivityFailedEventDetails,
    afedError,
    afedCause,

    -- * ActivityListItem
    ActivityListItem (..),
    mkActivityListItem,
    aliActivityARN,
    aliName,
    aliCreationDate,

    -- * ActivityScheduleFailedEventDetails
    ActivityScheduleFailedEventDetails (..),
    mkActivityScheduleFailedEventDetails,
    asfedError,
    asfedCause,

    -- * ActivityScheduledEventDetails
    ActivityScheduledEventDetails (..),
    mkActivityScheduledEventDetails,
    asedHeartbeatInSeconds,
    asedInputDetails,
    asedInput,
    asedTimeoutInSeconds,
    asedResource,

    -- * ActivityStartedEventDetails
    ActivityStartedEventDetails (..),
    mkActivityStartedEventDetails,
    asedWorkerName,

    -- * ActivitySucceededEventDetails
    ActivitySucceededEventDetails (..),
    mkActivitySucceededEventDetails,
    asedOutput,
    asedOutputDetails,

    -- * ActivityTimedOutEventDetails
    ActivityTimedOutEventDetails (..),
    mkActivityTimedOutEventDetails,
    atoedError,
    atoedCause,

    -- * BillingDetails
    BillingDetails (..),
    mkBillingDetails,
    bdBilledMemoryUsedInMB,
    bdBilledDurationInMilliseconds,

    -- * CloudWatchEventsExecutionDataDetails
    CloudWatchEventsExecutionDataDetails (..),
    mkCloudWatchEventsExecutionDataDetails,
    cweeddIncluded,

    -- * CloudWatchLogsLogGroup
    CloudWatchLogsLogGroup (..),
    mkCloudWatchLogsLogGroup,
    cwllgLogGroupARN,

    -- * ExecutionAbortedEventDetails
    ExecutionAbortedEventDetails (..),
    mkExecutionAbortedEventDetails,
    eaedError,
    eaedCause,

    -- * ExecutionFailedEventDetails
    ExecutionFailedEventDetails (..),
    mkExecutionFailedEventDetails,
    efedError,
    efedCause,

    -- * ExecutionListItem
    ExecutionListItem (..),
    mkExecutionListItem,
    eliStopDate,
    eliStatus,
    eliStartDate,
    eliName,
    eliStateMachineARN,
    eliExecutionARN,

    -- * ExecutionStartedEventDetails
    ExecutionStartedEventDetails (..),
    mkExecutionStartedEventDetails,
    esedInputDetails,
    esedInput,
    esedRoleARN,

    -- * ExecutionSucceededEventDetails
    ExecutionSucceededEventDetails (..),
    mkExecutionSucceededEventDetails,
    esedOutput,
    esedOutputDetails,

    -- * ExecutionTimedOutEventDetails
    ExecutionTimedOutEventDetails (..),
    mkExecutionTimedOutEventDetails,
    etoedError,
    etoedCause,

    -- * HistoryEvent
    HistoryEvent (..),
    mkHistoryEvent,
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
    heId,
    heActivityScheduleFailedEventDetails,
    heLambdaFunctionScheduleFailedEventDetails,
    heStateEnteredEventDetails,
    hePreviousEventId,
    heActivityFailedEventDetails,
    heTaskFailedEventDetails,
    heLambdaFunctionFailedEventDetails,
    heType,
    heExecutionTimedOutEventDetails,
    heMapIterationFailedEventDetails,
    heTimestamp,

    -- * HistoryEventExecutionDataDetails
    HistoryEventExecutionDataDetails (..),
    mkHistoryEventExecutionDataDetails,
    heeddTruncated,

    -- * LambdaFunctionFailedEventDetails
    LambdaFunctionFailedEventDetails (..),
    mkLambdaFunctionFailedEventDetails,
    lffedError,
    lffedCause,

    -- * LambdaFunctionScheduleFailedEventDetails
    LambdaFunctionScheduleFailedEventDetails (..),
    mkLambdaFunctionScheduleFailedEventDetails,
    lError,
    lCause,

    -- * LambdaFunctionScheduledEventDetails
    LambdaFunctionScheduledEventDetails (..),
    mkLambdaFunctionScheduledEventDetails,
    lfsedInputDetails,
    lfsedInput,
    lfsedTimeoutInSeconds,
    lfsedResource,

    -- * LambdaFunctionStartFailedEventDetails
    LambdaFunctionStartFailedEventDetails (..),
    mkLambdaFunctionStartFailedEventDetails,
    lfsfedError,
    lfsfedCause,

    -- * LambdaFunctionSucceededEventDetails
    LambdaFunctionSucceededEventDetails (..),
    mkLambdaFunctionSucceededEventDetails,
    lfsedOutput,
    lfsedOutputDetails,

    -- * LambdaFunctionTimedOutEventDetails
    LambdaFunctionTimedOutEventDetails (..),
    mkLambdaFunctionTimedOutEventDetails,
    lftoedError,
    lftoedCause,

    -- * LogDestination
    LogDestination (..),
    mkLogDestination,
    ldCloudWatchLogsLogGroup,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    mkLoggingConfiguration,
    lcIncludeExecutionData,
    lcDestinations,
    lcLevel,

    -- * MapIterationEventDetails
    MapIterationEventDetails (..),
    mkMapIterationEventDetails,
    miedName,
    miedIndex,

    -- * MapStateStartedEventDetails
    MapStateStartedEventDetails (..),
    mkMapStateStartedEventDetails,
    mssedLength,

    -- * StateEnteredEventDetails
    StateEnteredEventDetails (..),
    mkStateEnteredEventDetails,
    sInputDetails,
    sInput,
    sName,

    -- * StateExitedEventDetails
    StateExitedEventDetails (..),
    mkStateExitedEventDetails,
    seedName,
    seedOutput,
    seedOutputDetails,

    -- * StateMachineListItem
    StateMachineListItem (..),
    mkStateMachineListItem,
    smliName,
    smliStateMachineARN,
    smliCreationDate,
    smliType,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TaskFailedEventDetails
    TaskFailedEventDetails (..),
    mkTaskFailedEventDetails,
    tfedResourceType,
    tfedError,
    tfedCause,
    tfedResource,

    -- * TaskScheduledEventDetails
    TaskScheduledEventDetails (..),
    mkTaskScheduledEventDetails,
    tsedfHeartbeatInSeconds,
    tsedfResourceType,
    tsedfTimeoutInSeconds,
    tsedfResource,
    tsedfParameters,
    tsedfRegion,

    -- * TaskStartFailedEventDetails
    TaskStartFailedEventDetails (..),
    mkTaskStartFailedEventDetails,
    tsfedsResourceType,
    tsfedsError,
    tsfedsCause,
    tsfedsResource,

    -- * TaskStartedEventDetails
    TaskStartedEventDetails (..),
    mkTaskStartedEventDetails,
    tsedResourceType,
    tsedResource,

    -- * TaskSubmitFailedEventDetails
    TaskSubmitFailedEventDetails (..),
    mkTaskSubmitFailedEventDetails,
    tsfedResourceType,
    tsfedError,
    tsfedCause,
    tsfedResource,

    -- * TaskSubmittedEventDetails
    TaskSubmittedEventDetails (..),
    mkTaskSubmittedEventDetails,
    tResourceType,
    tOutput,
    tResource,
    tOutputDetails,

    -- * TaskSucceededEventDetails
    TaskSucceededEventDetails (..),
    mkTaskSucceededEventDetails,
    tsedsResourceType,
    tsedsOutput,
    tsedsResource,
    tsedsOutputDetails,

    -- * TaskTimedOutEventDetails
    TaskTimedOutEventDetails (..),
    mkTaskTimedOutEventDetails,
    ttoedResourceType,
    ttoedError,
    ttoedCause,
    ttoedResource,

    -- * TracingConfiguration
    TracingConfiguration (..),
    mkTracingConfiguration,
    tcEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
stepFunctionsService :: Lude.Service
stepFunctionsService =
  Lude.Service
    { Lude._svcAbbrev = "StepFunctions",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "states",
      Lude._svcVersion = "2016-11-23",
      Lude._svcEndpoint = Lude.defaultEndpoint stepFunctionsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "StepFunctions",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
