{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.Sum where

import Network.AWS.Prelude

data ExecutionStatus
  = Aborted
  | Failed
  | Running
  | Succeeded
  | TimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "aborted" -> pure Aborted
        "failed" -> pure Failed
        "running" -> pure Running
        "succeeded" -> pure Succeeded
        "timed_out" -> pure TimedOut
        e -> fromTextError $ "Failure parsing ExecutionStatus from value: '" <> e
           <> "'. Accepted values: aborted, failed, running, succeeded, timed_out"

instance ToText ExecutionStatus where
    toText = \case
        Aborted -> "ABORTED"
        Failed -> "FAILED"
        Running -> "RUNNING"
        Succeeded -> "SUCCEEDED"
        TimedOut -> "TIMED_OUT"

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance ToJSON ExecutionStatus where
    toJSON = toJSONText

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"

data HistoryEventType
  = ActivityFailed
  | ActivityScheduleFailed
  | ActivityScheduled
  | ActivityStarted
  | ActivitySucceeded
  | ActivityTimedOut
  | ChoiceStateEntered
  | ChoiceStateExited
  | ExecutionAborted
  | ExecutionFailed
  | ExecutionStarted
  | ExecutionSucceeded
  | ExecutionTimedOut
  | FailStateEntered
  | LambdaFunctionFailed
  | LambdaFunctionScheduleFailed
  | LambdaFunctionScheduled
  | LambdaFunctionStartFailed
  | LambdaFunctionStarted
  | LambdaFunctionSucceeded
  | LambdaFunctionTimedOut
  | ParallelStateAborted
  | ParallelStateEntered
  | ParallelStateExited
  | ParallelStateFailed
  | ParallelStateStarted
  | ParallelStateSucceeded
  | PassStateEntered
  | PassStateExited
  | SucceedStateEntered
  | SucceedStateExited
  | TaskStateAborted
  | TaskStateEntered
  | TaskStateExited
  | WaitStateAborted
  | WaitStateEntered
  | WaitStateExited
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HistoryEventType where
    parser = takeLowerText >>= \case
        "activityfailed" -> pure ActivityFailed
        "activityschedulefailed" -> pure ActivityScheduleFailed
        "activityscheduled" -> pure ActivityScheduled
        "activitystarted" -> pure ActivityStarted
        "activitysucceeded" -> pure ActivitySucceeded
        "activitytimedout" -> pure ActivityTimedOut
        "choicestateentered" -> pure ChoiceStateEntered
        "choicestateexited" -> pure ChoiceStateExited
        "executionaborted" -> pure ExecutionAborted
        "executionfailed" -> pure ExecutionFailed
        "executionstarted" -> pure ExecutionStarted
        "executionsucceeded" -> pure ExecutionSucceeded
        "executiontimedout" -> pure ExecutionTimedOut
        "failstateentered" -> pure FailStateEntered
        "lambdafunctionfailed" -> pure LambdaFunctionFailed
        "lambdafunctionschedulefailed" -> pure LambdaFunctionScheduleFailed
        "lambdafunctionscheduled" -> pure LambdaFunctionScheduled
        "lambdafunctionstartfailed" -> pure LambdaFunctionStartFailed
        "lambdafunctionstarted" -> pure LambdaFunctionStarted
        "lambdafunctionsucceeded" -> pure LambdaFunctionSucceeded
        "lambdafunctiontimedout" -> pure LambdaFunctionTimedOut
        "parallelstateaborted" -> pure ParallelStateAborted
        "parallelstateentered" -> pure ParallelStateEntered
        "parallelstateexited" -> pure ParallelStateExited
        "parallelstatefailed" -> pure ParallelStateFailed
        "parallelstatestarted" -> pure ParallelStateStarted
        "parallelstatesucceeded" -> pure ParallelStateSucceeded
        "passstateentered" -> pure PassStateEntered
        "passstateexited" -> pure PassStateExited
        "succeedstateentered" -> pure SucceedStateEntered
        "succeedstateexited" -> pure SucceedStateExited
        "taskstateaborted" -> pure TaskStateAborted
        "taskstateentered" -> pure TaskStateEntered
        "taskstateexited" -> pure TaskStateExited
        "waitstateaborted" -> pure WaitStateAborted
        "waitstateentered" -> pure WaitStateEntered
        "waitstateexited" -> pure WaitStateExited
        e -> fromTextError $ "Failure parsing HistoryEventType from value: '" <> e
           <> "'. Accepted values: activityfailed, activityschedulefailed, activityscheduled, activitystarted, activitysucceeded, activitytimedout, choicestateentered, choicestateexited, executionaborted, executionfailed, executionstarted, executionsucceeded, executiontimedout, failstateentered, lambdafunctionfailed, lambdafunctionschedulefailed, lambdafunctionscheduled, lambdafunctionstartfailed, lambdafunctionstarted, lambdafunctionsucceeded, lambdafunctiontimedout, parallelstateaborted, parallelstateentered, parallelstateexited, parallelstatefailed, parallelstatestarted, parallelstatesucceeded, passstateentered, passstateexited, succeedstateentered, succeedstateexited, taskstateaborted, taskstateentered, taskstateexited, waitstateaborted, waitstateentered, waitstateexited"

instance ToText HistoryEventType where
    toText = \case
        ActivityFailed -> "ActivityFailed"
        ActivityScheduleFailed -> "ActivityScheduleFailed"
        ActivityScheduled -> "ActivityScheduled"
        ActivityStarted -> "ActivityStarted"
        ActivitySucceeded -> "ActivitySucceeded"
        ActivityTimedOut -> "ActivityTimedOut"
        ChoiceStateEntered -> "ChoiceStateEntered"
        ChoiceStateExited -> "ChoiceStateExited"
        ExecutionAborted -> "ExecutionAborted"
        ExecutionFailed -> "ExecutionFailed"
        ExecutionStarted -> "ExecutionStarted"
        ExecutionSucceeded -> "ExecutionSucceeded"
        ExecutionTimedOut -> "ExecutionTimedOut"
        FailStateEntered -> "FailStateEntered"
        LambdaFunctionFailed -> "LambdaFunctionFailed"
        LambdaFunctionScheduleFailed -> "LambdaFunctionScheduleFailed"
        LambdaFunctionScheduled -> "LambdaFunctionScheduled"
        LambdaFunctionStartFailed -> "LambdaFunctionStartFailed"
        LambdaFunctionStarted -> "LambdaFunctionStarted"
        LambdaFunctionSucceeded -> "LambdaFunctionSucceeded"
        LambdaFunctionTimedOut -> "LambdaFunctionTimedOut"
        ParallelStateAborted -> "ParallelStateAborted"
        ParallelStateEntered -> "ParallelStateEntered"
        ParallelStateExited -> "ParallelStateExited"
        ParallelStateFailed -> "ParallelStateFailed"
        ParallelStateStarted -> "ParallelStateStarted"
        ParallelStateSucceeded -> "ParallelStateSucceeded"
        PassStateEntered -> "PassStateEntered"
        PassStateExited -> "PassStateExited"
        SucceedStateEntered -> "SucceedStateEntered"
        SucceedStateExited -> "SucceedStateExited"
        TaskStateAborted -> "TaskStateAborted"
        TaskStateEntered -> "TaskStateEntered"
        TaskStateExited -> "TaskStateExited"
        WaitStateAborted -> "WaitStateAborted"
        WaitStateEntered -> "WaitStateEntered"
        WaitStateExited -> "WaitStateExited"

instance Hashable     HistoryEventType
instance NFData       HistoryEventType
instance ToByteString HistoryEventType
instance ToQuery      HistoryEventType
instance ToHeader     HistoryEventType

instance FromJSON HistoryEventType where
    parseJSON = parseJSONText "HistoryEventType"

data StateMachineStatus
  = Active
  | Deleting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StateMachineStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "deleting" -> pure Deleting
        e -> fromTextError $ "Failure parsing StateMachineStatus from value: '" <> e
           <> "'. Accepted values: active, deleting"

instance ToText StateMachineStatus where
    toText = \case
        Active -> "ACTIVE"
        Deleting -> "DELETING"

instance Hashable     StateMachineStatus
instance NFData       StateMachineStatus
instance ToByteString StateMachineStatus
instance ToQuery      StateMachineStatus
instance ToHeader     StateMachineStatus

instance FromJSON StateMachineStatus where
    parseJSON = parseJSONText "StateMachineStatus"
