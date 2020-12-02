{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.EventType where

import Network.AWS.Prelude

data EventType
  = ActivityTaskCancelRequested
  | ActivityTaskCanceled
  | ActivityTaskCompleted
  | ActivityTaskFailed
  | ActivityTaskScheduled
  | ActivityTaskStarted
  | ActivityTaskTimedOut
  | CancelTimerFailed
  | CancelWorkflowExecutionFailed
  | ChildWorkflowExecutionCanceled
  | ChildWorkflowExecutionCompleted
  | ChildWorkflowExecutionFailed
  | ChildWorkflowExecutionStarted
  | ChildWorkflowExecutionTerminated
  | ChildWorkflowExecutionTimedOut
  | CompleteWorkflowExecutionFailed
  | ContinueAsNewWorkflowExecutionFailed
  | DecisionTaskCompleted
  | DecisionTaskScheduled
  | DecisionTaskStarted
  | DecisionTaskTimedOut
  | ExternalWorkflowExecutionCancelRequested
  | ExternalWorkflowExecutionSignaled
  | FailWorkflowExecutionFailed
  | LambdaFunctionCompleted
  | LambdaFunctionFailed
  | LambdaFunctionScheduled
  | LambdaFunctionStarted
  | LambdaFunctionTimedOut
  | MarkerRecorded
  | RecordMarkerFailed
  | RequestCancelActivityTaskFailed
  | RequestCancelExternalWorkflowExecutionFailed
  | RequestCancelExternalWorkflowExecutionInitiated
  | ScheduleActivityTaskFailed
  | ScheduleLambdaFunctionFailed
  | SignalExternalWorkflowExecutionFailed
  | SignalExternalWorkflowExecutionInitiated
  | StartChildWorkflowExecutionFailed
  | StartChildWorkflowExecutionInitiated
  | StartLambdaFunctionFailed
  | StartTimerFailed
  | TimerCanceled
  | TimerFired
  | TimerStarted
  | WorkflowExecutionCancelRequested
  | WorkflowExecutionCanceled
  | WorkflowExecutionCompleted
  | WorkflowExecutionContinuedAsNew
  | WorkflowExecutionFailed
  | WorkflowExecutionSignaled
  | WorkflowExecutionStarted
  | WorkflowExecutionTerminated
  | WorkflowExecutionTimedOut
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

instance FromText EventType where
  parser =
    takeLowerText >>= \case
      "activitytaskcancelrequested" -> pure ActivityTaskCancelRequested
      "activitytaskcanceled" -> pure ActivityTaskCanceled
      "activitytaskcompleted" -> pure ActivityTaskCompleted
      "activitytaskfailed" -> pure ActivityTaskFailed
      "activitytaskscheduled" -> pure ActivityTaskScheduled
      "activitytaskstarted" -> pure ActivityTaskStarted
      "activitytasktimedout" -> pure ActivityTaskTimedOut
      "canceltimerfailed" -> pure CancelTimerFailed
      "cancelworkflowexecutionfailed" -> pure CancelWorkflowExecutionFailed
      "childworkflowexecutioncanceled" -> pure ChildWorkflowExecutionCanceled
      "childworkflowexecutioncompleted" -> pure ChildWorkflowExecutionCompleted
      "childworkflowexecutionfailed" -> pure ChildWorkflowExecutionFailed
      "childworkflowexecutionstarted" -> pure ChildWorkflowExecutionStarted
      "childworkflowexecutionterminated" -> pure ChildWorkflowExecutionTerminated
      "childworkflowexecutiontimedout" -> pure ChildWorkflowExecutionTimedOut
      "completeworkflowexecutionfailed" -> pure CompleteWorkflowExecutionFailed
      "continueasnewworkflowexecutionfailed" -> pure ContinueAsNewWorkflowExecutionFailed
      "decisiontaskcompleted" -> pure DecisionTaskCompleted
      "decisiontaskscheduled" -> pure DecisionTaskScheduled
      "decisiontaskstarted" -> pure DecisionTaskStarted
      "decisiontasktimedout" -> pure DecisionTaskTimedOut
      "externalworkflowexecutioncancelrequested" -> pure ExternalWorkflowExecutionCancelRequested
      "externalworkflowexecutionsignaled" -> pure ExternalWorkflowExecutionSignaled
      "failworkflowexecutionfailed" -> pure FailWorkflowExecutionFailed
      "lambdafunctioncompleted" -> pure LambdaFunctionCompleted
      "lambdafunctionfailed" -> pure LambdaFunctionFailed
      "lambdafunctionscheduled" -> pure LambdaFunctionScheduled
      "lambdafunctionstarted" -> pure LambdaFunctionStarted
      "lambdafunctiontimedout" -> pure LambdaFunctionTimedOut
      "markerrecorded" -> pure MarkerRecorded
      "recordmarkerfailed" -> pure RecordMarkerFailed
      "requestcancelactivitytaskfailed" -> pure RequestCancelActivityTaskFailed
      "requestcancelexternalworkflowexecutionfailed" -> pure RequestCancelExternalWorkflowExecutionFailed
      "requestcancelexternalworkflowexecutioninitiated" -> pure RequestCancelExternalWorkflowExecutionInitiated
      "scheduleactivitytaskfailed" -> pure ScheduleActivityTaskFailed
      "schedulelambdafunctionfailed" -> pure ScheduleLambdaFunctionFailed
      "signalexternalworkflowexecutionfailed" -> pure SignalExternalWorkflowExecutionFailed
      "signalexternalworkflowexecutioninitiated" -> pure SignalExternalWorkflowExecutionInitiated
      "startchildworkflowexecutionfailed" -> pure StartChildWorkflowExecutionFailed
      "startchildworkflowexecutioninitiated" -> pure StartChildWorkflowExecutionInitiated
      "startlambdafunctionfailed" -> pure StartLambdaFunctionFailed
      "starttimerfailed" -> pure StartTimerFailed
      "timercanceled" -> pure TimerCanceled
      "timerfired" -> pure TimerFired
      "timerstarted" -> pure TimerStarted
      "workflowexecutioncancelrequested" -> pure WorkflowExecutionCancelRequested
      "workflowexecutioncanceled" -> pure WorkflowExecutionCanceled
      "workflowexecutioncompleted" -> pure WorkflowExecutionCompleted
      "workflowexecutioncontinuedasnew" -> pure WorkflowExecutionContinuedAsNew
      "workflowexecutionfailed" -> pure WorkflowExecutionFailed
      "workflowexecutionsignaled" -> pure WorkflowExecutionSignaled
      "workflowexecutionstarted" -> pure WorkflowExecutionStarted
      "workflowexecutionterminated" -> pure WorkflowExecutionTerminated
      "workflowexecutiontimedout" -> pure WorkflowExecutionTimedOut
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: activitytaskcancelrequested, activitytaskcanceled, activitytaskcompleted, activitytaskfailed, activitytaskscheduled, activitytaskstarted, activitytasktimedout, canceltimerfailed, cancelworkflowexecutionfailed, childworkflowexecutioncanceled, childworkflowexecutioncompleted, childworkflowexecutionfailed, childworkflowexecutionstarted, childworkflowexecutionterminated, childworkflowexecutiontimedout, completeworkflowexecutionfailed, continueasnewworkflowexecutionfailed, decisiontaskcompleted, decisiontaskscheduled, decisiontaskstarted, decisiontasktimedout, externalworkflowexecutioncancelrequested, externalworkflowexecutionsignaled, failworkflowexecutionfailed, lambdafunctioncompleted, lambdafunctionfailed, lambdafunctionscheduled, lambdafunctionstarted, lambdafunctiontimedout, markerrecorded, recordmarkerfailed, requestcancelactivitytaskfailed, requestcancelexternalworkflowexecutionfailed, requestcancelexternalworkflowexecutioninitiated, scheduleactivitytaskfailed, schedulelambdafunctionfailed, signalexternalworkflowexecutionfailed, signalexternalworkflowexecutioninitiated, startchildworkflowexecutionfailed, startchildworkflowexecutioninitiated, startlambdafunctionfailed, starttimerfailed, timercanceled, timerfired, timerstarted, workflowexecutioncancelrequested, workflowexecutioncanceled, workflowexecutioncompleted, workflowexecutioncontinuedasnew, workflowexecutionfailed, workflowexecutionsignaled, workflowexecutionstarted, workflowexecutionterminated, workflowexecutiontimedout"

instance ToText EventType where
  toText = \case
    ActivityTaskCancelRequested -> "ActivityTaskCancelRequested"
    ActivityTaskCanceled -> "ActivityTaskCanceled"
    ActivityTaskCompleted -> "ActivityTaskCompleted"
    ActivityTaskFailed -> "ActivityTaskFailed"
    ActivityTaskScheduled -> "ActivityTaskScheduled"
    ActivityTaskStarted -> "ActivityTaskStarted"
    ActivityTaskTimedOut -> "ActivityTaskTimedOut"
    CancelTimerFailed -> "CancelTimerFailed"
    CancelWorkflowExecutionFailed -> "CancelWorkflowExecutionFailed"
    ChildWorkflowExecutionCanceled -> "ChildWorkflowExecutionCanceled"
    ChildWorkflowExecutionCompleted -> "ChildWorkflowExecutionCompleted"
    ChildWorkflowExecutionFailed -> "ChildWorkflowExecutionFailed"
    ChildWorkflowExecutionStarted -> "ChildWorkflowExecutionStarted"
    ChildWorkflowExecutionTerminated -> "ChildWorkflowExecutionTerminated"
    ChildWorkflowExecutionTimedOut -> "ChildWorkflowExecutionTimedOut"
    CompleteWorkflowExecutionFailed -> "CompleteWorkflowExecutionFailed"
    ContinueAsNewWorkflowExecutionFailed -> "ContinueAsNewWorkflowExecutionFailed"
    DecisionTaskCompleted -> "DecisionTaskCompleted"
    DecisionTaskScheduled -> "DecisionTaskScheduled"
    DecisionTaskStarted -> "DecisionTaskStarted"
    DecisionTaskTimedOut -> "DecisionTaskTimedOut"
    ExternalWorkflowExecutionCancelRequested -> "ExternalWorkflowExecutionCancelRequested"
    ExternalWorkflowExecutionSignaled -> "ExternalWorkflowExecutionSignaled"
    FailWorkflowExecutionFailed -> "FailWorkflowExecutionFailed"
    LambdaFunctionCompleted -> "LambdaFunctionCompleted"
    LambdaFunctionFailed -> "LambdaFunctionFailed"
    LambdaFunctionScheduled -> "LambdaFunctionScheduled"
    LambdaFunctionStarted -> "LambdaFunctionStarted"
    LambdaFunctionTimedOut -> "LambdaFunctionTimedOut"
    MarkerRecorded -> "MarkerRecorded"
    RecordMarkerFailed -> "RecordMarkerFailed"
    RequestCancelActivityTaskFailed -> "RequestCancelActivityTaskFailed"
    RequestCancelExternalWorkflowExecutionFailed -> "RequestCancelExternalWorkflowExecutionFailed"
    RequestCancelExternalWorkflowExecutionInitiated -> "RequestCancelExternalWorkflowExecutionInitiated"
    ScheduleActivityTaskFailed -> "ScheduleActivityTaskFailed"
    ScheduleLambdaFunctionFailed -> "ScheduleLambdaFunctionFailed"
    SignalExternalWorkflowExecutionFailed -> "SignalExternalWorkflowExecutionFailed"
    SignalExternalWorkflowExecutionInitiated -> "SignalExternalWorkflowExecutionInitiated"
    StartChildWorkflowExecutionFailed -> "StartChildWorkflowExecutionFailed"
    StartChildWorkflowExecutionInitiated -> "StartChildWorkflowExecutionInitiated"
    StartLambdaFunctionFailed -> "StartLambdaFunctionFailed"
    StartTimerFailed -> "StartTimerFailed"
    TimerCanceled -> "TimerCanceled"
    TimerFired -> "TimerFired"
    TimerStarted -> "TimerStarted"
    WorkflowExecutionCancelRequested -> "WorkflowExecutionCancelRequested"
    WorkflowExecutionCanceled -> "WorkflowExecutionCanceled"
    WorkflowExecutionCompleted -> "WorkflowExecutionCompleted"
    WorkflowExecutionContinuedAsNew -> "WorkflowExecutionContinuedAsNew"
    WorkflowExecutionFailed -> "WorkflowExecutionFailed"
    WorkflowExecutionSignaled -> "WorkflowExecutionSignaled"
    WorkflowExecutionStarted -> "WorkflowExecutionStarted"
    WorkflowExecutionTerminated -> "WorkflowExecutionTerminated"
    WorkflowExecutionTimedOut -> "WorkflowExecutionTimedOut"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance FromJSON EventType where
  parseJSON = parseJSONText "EventType"
