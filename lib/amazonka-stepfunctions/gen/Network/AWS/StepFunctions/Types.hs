{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types
    (
    -- * Service Configuration
      stepFunctions

    -- * Errors
    , _ExecutionLimitExceeded
    , _InvalidDefinition
    , _StateMachineLimitExceeded
    , _ExecutionAlreadyExists
    , _StateMachineAlreadyExists
    , _TaskTimedOut
    , _InvalidExecutionInput
    , _InvalidOutput
    , _InvalidName
    , _TaskDoesNotExist
    , _ActivityDoesNotExist
    , _StateMachineDeleting
    , _MissingRequiredParameter
    , _InvalidARN
    , _InvalidToken
    , _ActivityWorkerLimitExceeded
    , _ActivityLimitExceeded
    , _ExecutionDoesNotExist
    , _StateMachineDoesNotExist

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * HistoryEventType
    , HistoryEventType (..)

    -- * StateMachineStatus
    , StateMachineStatus (..)

    -- * ActivityFailedEventDetails
    , ActivityFailedEventDetails
    , activityFailedEventDetails
    , afedError
    , afedCause

    -- * ActivityListItem
    , ActivityListItem
    , activityListItem
    , aliActivityARN
    , aliName
    , aliCreationDate

    -- * ActivityScheduleFailedEventDetails
    , ActivityScheduleFailedEventDetails
    , activityScheduleFailedEventDetails
    , asfedError
    , asfedCause

    -- * ActivityScheduledEventDetails
    , ActivityScheduledEventDetails
    , activityScheduledEventDetails
    , asedHeartbeatInSeconds
    , asedInput
    , asedTimeoutInSeconds
    , asedResource

    -- * ActivityStartedEventDetails
    , ActivityStartedEventDetails
    , activityStartedEventDetails
    , asedWorkerName

    -- * ActivitySucceededEventDetails
    , ActivitySucceededEventDetails
    , activitySucceededEventDetails
    , asedOutput

    -- * ActivityTimedOutEventDetails
    , ActivityTimedOutEventDetails
    , activityTimedOutEventDetails
    , atoedError
    , atoedCause

    -- * ExecutionAbortedEventDetails
    , ExecutionAbortedEventDetails
    , executionAbortedEventDetails
    , eaedError
    , eaedCause

    -- * ExecutionFailedEventDetails
    , ExecutionFailedEventDetails
    , executionFailedEventDetails
    , efedError
    , efedCause

    -- * ExecutionListItem
    , ExecutionListItem
    , executionListItem
    , eliStopDate
    , eliExecutionARN
    , eliStateMachineARN
    , eliName
    , eliStatus
    , eliStartDate

    -- * ExecutionStartedEventDetails
    , ExecutionStartedEventDetails
    , executionStartedEventDetails
    , esedInput
    , esedRoleARN

    -- * ExecutionSucceededEventDetails
    , ExecutionSucceededEventDetails
    , executionSucceededEventDetails
    , esedOutput

    -- * ExecutionTimedOutEventDetails
    , ExecutionTimedOutEventDetails
    , executionTimedOutEventDetails
    , etoedError
    , etoedCause

    -- * HistoryEvent
    , HistoryEvent
    , historyEvent
    , heActivityStartedEventDetails
    , heLambdaFunctionStartFailedEventDetails
    , heStateExitedEventDetails
    , heLambdaFunctionSucceededEventDetails
    , heActivitySucceededEventDetails
    , heLambdaFunctionTimedOutEventDetails
    , heActivityTimedOutEventDetails
    , heExecutionFailedEventDetails
    , heExecutionAbortedEventDetails
    , heExecutionSucceededEventDetails
    , heLambdaFunctionScheduledEventDetails
    , heActivityScheduledEventDetails
    , heExecutionStartedEventDetails
    , heActivityScheduleFailedEventDetails
    , heLambdaFunctionScheduleFailedEventDetails
    , heStateEnteredEventDetails
    , hePreviousEventId
    , heActivityFailedEventDetails
    , heLambdaFunctionFailedEventDetails
    , heExecutionTimedOutEventDetails
    , heTimestamp
    , heType
    , heId

    -- * LambdaFunctionFailedEventDetails
    , LambdaFunctionFailedEventDetails
    , lambdaFunctionFailedEventDetails
    , lffedError
    , lffedCause

    -- * LambdaFunctionScheduleFailedEventDetails
    , LambdaFunctionScheduleFailedEventDetails
    , lambdaFunctionScheduleFailedEventDetails
    , lError
    , lCause

    -- * LambdaFunctionScheduledEventDetails
    , LambdaFunctionScheduledEventDetails
    , lambdaFunctionScheduledEventDetails
    , lfsedInput
    , lfsedTimeoutInSeconds
    , lfsedResource

    -- * LambdaFunctionStartFailedEventDetails
    , LambdaFunctionStartFailedEventDetails
    , lambdaFunctionStartFailedEventDetails
    , lfsfedError
    , lfsfedCause

    -- * LambdaFunctionSucceededEventDetails
    , LambdaFunctionSucceededEventDetails
    , lambdaFunctionSucceededEventDetails
    , lfsedOutput

    -- * LambdaFunctionTimedOutEventDetails
    , LambdaFunctionTimedOutEventDetails
    , lambdaFunctionTimedOutEventDetails
    , lftoedError
    , lftoedCause

    -- * StateEnteredEventDetails
    , StateEnteredEventDetails
    , stateEnteredEventDetails
    , sInput
    , sName

    -- * StateExitedEventDetails
    , StateExitedEventDetails
    , stateExitedEventDetails
    , seedOutput
    , seedName

    -- * StateMachineListItem
    , StateMachineListItem
    , stateMachineListItem
    , smliStateMachineARN
    , smliName
    , smliCreationDate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.StepFunctions.Types.Product
import Network.AWS.StepFunctions.Types.Sum

-- | API version @2016-11-23@ of the Amazon Step Functions SDK configuration.
stepFunctions :: Service
stepFunctions =
  Service
    { _svcAbbrev = "StepFunctions"
    , _svcSigner = v4
    , _svcPrefix = "states"
    , _svcVersion = "2016-11-23"
    , _svcEndpoint = defaultEndpoint stepFunctions
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "StepFunctions"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The maximum number of running executions has been reached. Running executions must end or be stopped before a new execution can be started.
--
--
_ExecutionLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_ExecutionLimitExceeded =
  _MatchServiceError stepFunctions "ExecutionLimitExceeded"


-- | The provided Amazon States Language definition is invalid.
--
--
_InvalidDefinition :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDefinition = _MatchServiceError stepFunctions "InvalidDefinition"


-- | The maximum number of state machines has been reached. Existing state machines must be deleted before a new state machine can be created.
--
--
_StateMachineLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_StateMachineLimitExceeded =
  _MatchServiceError stepFunctions "StateMachineLimitExceeded"


-- | The execution has the same @name@ as another execution (but a different @input@ ).
--
--
_ExecutionAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_ExecutionAlreadyExists =
  _MatchServiceError stepFunctions "ExecutionAlreadyExists"


-- | A state machine with the same name but a different definition or role ARN already exists.
--
--
_StateMachineAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_StateMachineAlreadyExists =
  _MatchServiceError stepFunctions "StateMachineAlreadyExists"


-- | Prism for TaskTimedOut' errors.
_TaskTimedOut :: AsError a => Getting (First ServiceError) a ServiceError
_TaskTimedOut = _MatchServiceError stepFunctions "TaskTimedOut"


-- | The provided JSON input data is invalid.
--
--
_InvalidExecutionInput :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidExecutionInput =
  _MatchServiceError stepFunctions "InvalidExecutionInput"


-- | The provided JSON output data is invalid.
--
--
_InvalidOutput :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOutput = _MatchServiceError stepFunctions "InvalidOutput"


-- | The provided name is invalid.
--
--
_InvalidName :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidName = _MatchServiceError stepFunctions "InvalidName"


-- | Prism for TaskDoesNotExist' errors.
_TaskDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_TaskDoesNotExist = _MatchServiceError stepFunctions "TaskDoesNotExist"


-- | The specified activity does not exist.
--
--
_ActivityDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_ActivityDoesNotExist = _MatchServiceError stepFunctions "ActivityDoesNotExist"


-- | The specified state machine is being deleted.
--
--
_StateMachineDeleting :: AsError a => Getting (First ServiceError) a ServiceError
_StateMachineDeleting = _MatchServiceError stepFunctions "StateMachineDeleting"


-- | Request is missing a required parameter. This error occurs if both @definition@ and @roleArn@ are not specified.
--
--
_MissingRequiredParameter :: AsError a => Getting (First ServiceError) a ServiceError
_MissingRequiredParameter =
  _MatchServiceError stepFunctions "MissingRequiredParameter"


-- | The provided Amazon Resource Name (ARN) is invalid.
--
--
_InvalidARN :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARN = _MatchServiceError stepFunctions "InvalidArn"


-- | The provided token is invalid.
--
--
_InvalidToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidToken = _MatchServiceError stepFunctions "InvalidToken"


-- | The maximum number of workers concurrently polling for activity tasks has been reached.
--
--
_ActivityWorkerLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_ActivityWorkerLimitExceeded =
  _MatchServiceError stepFunctions "ActivityWorkerLimitExceeded"


-- | The maximum number of activities has been reached. Existing activities must be deleted before a new activity can be created.
--
--
_ActivityLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_ActivityLimitExceeded =
  _MatchServiceError stepFunctions "ActivityLimitExceeded"


-- | The specified execution does not exist.
--
--
_ExecutionDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_ExecutionDoesNotExist =
  _MatchServiceError stepFunctions "ExecutionDoesNotExist"


-- | The specified state machine does not exist.
--
--
_StateMachineDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_StateMachineDoesNotExist =
  _MatchServiceError stepFunctions "StateMachineDoesNotExist"

