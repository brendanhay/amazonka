{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEventType where

import Network.AWS.Prelude

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
  | MapIterationAborted
  | MapIterationFailed
  | MapIterationStarted
  | MapIterationSucceeded
  | MapStateAborted
  | MapStateEntered
  | MapStateExited
  | MapStateFailed
  | MapStateStarted
  | MapStateSucceeded
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
  | TaskFailed
  | TaskScheduled
  | TaskStartFailed
  | TaskStarted
  | TaskStateAborted
  | TaskStateEntered
  | TaskStateExited
  | TaskSubmitFailed
  | TaskSubmitted
  | TaskSucceeded
  | TaskTimedOut
  | WaitStateAborted
  | WaitStateEntered
  | WaitStateExited
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HistoryEventType where
  parser =
    takeLowerText >>= \case
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
      "mapiterationaborted" -> pure MapIterationAborted
      "mapiterationfailed" -> pure MapIterationFailed
      "mapiterationstarted" -> pure MapIterationStarted
      "mapiterationsucceeded" -> pure MapIterationSucceeded
      "mapstateaborted" -> pure MapStateAborted
      "mapstateentered" -> pure MapStateEntered
      "mapstateexited" -> pure MapStateExited
      "mapstatefailed" -> pure MapStateFailed
      "mapstatestarted" -> pure MapStateStarted
      "mapstatesucceeded" -> pure MapStateSucceeded
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
      "taskfailed" -> pure TaskFailed
      "taskscheduled" -> pure TaskScheduled
      "taskstartfailed" -> pure TaskStartFailed
      "taskstarted" -> pure TaskStarted
      "taskstateaborted" -> pure TaskStateAborted
      "taskstateentered" -> pure TaskStateEntered
      "taskstateexited" -> pure TaskStateExited
      "tasksubmitfailed" -> pure TaskSubmitFailed
      "tasksubmitted" -> pure TaskSubmitted
      "tasksucceeded" -> pure TaskSucceeded
      "tasktimedout" -> pure TaskTimedOut
      "waitstateaborted" -> pure WaitStateAborted
      "waitstateentered" -> pure WaitStateEntered
      "waitstateexited" -> pure WaitStateExited
      e ->
        fromTextError $
          "Failure parsing HistoryEventType from value: '" <> e
            <> "'. Accepted values: activityfailed, activityschedulefailed, activityscheduled, activitystarted, activitysucceeded, activitytimedout, choicestateentered, choicestateexited, executionaborted, executionfailed, executionstarted, executionsucceeded, executiontimedout, failstateentered, lambdafunctionfailed, lambdafunctionschedulefailed, lambdafunctionscheduled, lambdafunctionstartfailed, lambdafunctionstarted, lambdafunctionsucceeded, lambdafunctiontimedout, mapiterationaborted, mapiterationfailed, mapiterationstarted, mapiterationsucceeded, mapstateaborted, mapstateentered, mapstateexited, mapstatefailed, mapstatestarted, mapstatesucceeded, parallelstateaborted, parallelstateentered, parallelstateexited, parallelstatefailed, parallelstatestarted, parallelstatesucceeded, passstateentered, passstateexited, succeedstateentered, succeedstateexited, taskfailed, taskscheduled, taskstartfailed, taskstarted, taskstateaborted, taskstateentered, taskstateexited, tasksubmitfailed, tasksubmitted, tasksucceeded, tasktimedout, waitstateaborted, waitstateentered, waitstateexited"

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
    MapIterationAborted -> "MapIterationAborted"
    MapIterationFailed -> "MapIterationFailed"
    MapIterationStarted -> "MapIterationStarted"
    MapIterationSucceeded -> "MapIterationSucceeded"
    MapStateAborted -> "MapStateAborted"
    MapStateEntered -> "MapStateEntered"
    MapStateExited -> "MapStateExited"
    MapStateFailed -> "MapStateFailed"
    MapStateStarted -> "MapStateStarted"
    MapStateSucceeded -> "MapStateSucceeded"
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
    TaskFailed -> "TaskFailed"
    TaskScheduled -> "TaskScheduled"
    TaskStartFailed -> "TaskStartFailed"
    TaskStarted -> "TaskStarted"
    TaskStateAborted -> "TaskStateAborted"
    TaskStateEntered -> "TaskStateEntered"
    TaskStateExited -> "TaskStateExited"
    TaskSubmitFailed -> "TaskSubmitFailed"
    TaskSubmitted -> "TaskSubmitted"
    TaskSucceeded -> "TaskSucceeded"
    TaskTimedOut -> "TaskTimedOut"
    WaitStateAborted -> "WaitStateAborted"
    WaitStateEntered -> "WaitStateEntered"
    WaitStateExited -> "WaitStateExited"

instance Hashable HistoryEventType

instance NFData HistoryEventType

instance ToByteString HistoryEventType

instance ToQuery HistoryEventType

instance ToHeader HistoryEventType

instance FromJSON HistoryEventType where
  parseJSON = parseJSONText "HistoryEventType"
