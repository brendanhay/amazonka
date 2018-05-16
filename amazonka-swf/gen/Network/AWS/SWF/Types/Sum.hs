{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.Sum where

import Network.AWS.Prelude

data ActivityTaskTimeoutType
  = ATTTHeartbeat
  | ATTTScheduleToClose
  | ATTTScheduleToStart
  | ATTTStartToClose
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActivityTaskTimeoutType where
    parser = takeLowerText >>= \case
        "heartbeat" -> pure ATTTHeartbeat
        "schedule_to_close" -> pure ATTTScheduleToClose
        "schedule_to_start" -> pure ATTTScheduleToStart
        "start_to_close" -> pure ATTTStartToClose
        e -> fromTextError $ "Failure parsing ActivityTaskTimeoutType from value: '" <> e
           <> "'. Accepted values: heartbeat, schedule_to_close, schedule_to_start, start_to_close"

instance ToText ActivityTaskTimeoutType where
    toText = \case
        ATTTHeartbeat -> "HEARTBEAT"
        ATTTScheduleToClose -> "SCHEDULE_TO_CLOSE"
        ATTTScheduleToStart -> "SCHEDULE_TO_START"
        ATTTStartToClose -> "START_TO_CLOSE"

instance Hashable     ActivityTaskTimeoutType
instance NFData       ActivityTaskTimeoutType
instance ToByteString ActivityTaskTimeoutType
instance ToQuery      ActivityTaskTimeoutType
instance ToHeader     ActivityTaskTimeoutType

instance FromJSON ActivityTaskTimeoutType where
    parseJSON = parseJSONText "ActivityTaskTimeoutType"

data CancelTimerFailedCause
  = CTFCOperationNotPermitted
  | CTFCTimerIdUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CancelTimerFailedCause where
    parser = takeLowerText >>= \case
        "operation_not_permitted" -> pure CTFCOperationNotPermitted
        "timer_id_unknown" -> pure CTFCTimerIdUnknown
        e -> fromTextError $ "Failure parsing CancelTimerFailedCause from value: '" <> e
           <> "'. Accepted values: operation_not_permitted, timer_id_unknown"

instance ToText CancelTimerFailedCause where
    toText = \case
        CTFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CTFCTimerIdUnknown -> "TIMER_ID_UNKNOWN"

instance Hashable     CancelTimerFailedCause
instance NFData       CancelTimerFailedCause
instance ToByteString CancelTimerFailedCause
instance ToQuery      CancelTimerFailedCause
instance ToHeader     CancelTimerFailedCause

instance FromJSON CancelTimerFailedCause where
    parseJSON = parseJSONText "CancelTimerFailedCause"

data CancelWorkflowExecutionFailedCause
  = COperationNotPermitted
  | CUnhandledDecision
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CancelWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "operation_not_permitted" -> pure COperationNotPermitted
        "unhandled_decision" -> pure CUnhandledDecision
        e -> fromTextError $ "Failure parsing CancelWorkflowExecutionFailedCause from value: '" <> e
           <> "'. Accepted values: operation_not_permitted, unhandled_decision"

instance ToText CancelWorkflowExecutionFailedCause where
    toText = \case
        COperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable     CancelWorkflowExecutionFailedCause
instance NFData       CancelWorkflowExecutionFailedCause
instance ToByteString CancelWorkflowExecutionFailedCause
instance ToQuery      CancelWorkflowExecutionFailedCause
instance ToHeader     CancelWorkflowExecutionFailedCause

instance FromJSON CancelWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CancelWorkflowExecutionFailedCause"

data ChildPolicy
  = Abandon
  | RequestCancel
  | Terminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChildPolicy where
    parser = takeLowerText >>= \case
        "abandon" -> pure Abandon
        "request_cancel" -> pure RequestCancel
        "terminate" -> pure Terminate
        e -> fromTextError $ "Failure parsing ChildPolicy from value: '" <> e
           <> "'. Accepted values: abandon, request_cancel, terminate"

instance ToText ChildPolicy where
    toText = \case
        Abandon -> "ABANDON"
        RequestCancel -> "REQUEST_CANCEL"
        Terminate -> "TERMINATE"

instance Hashable     ChildPolicy
instance NFData       ChildPolicy
instance ToByteString ChildPolicy
instance ToQuery      ChildPolicy
instance ToHeader     ChildPolicy

instance ToJSON ChildPolicy where
    toJSON = toJSONText

instance FromJSON ChildPolicy where
    parseJSON = parseJSONText "ChildPolicy"

data CloseStatus
  = Canceled
  | Completed
  | ContinuedAsNew
  | Failed
  | Terminated
  | TimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CloseStatus where
    parser = takeLowerText >>= \case
        "canceled" -> pure Canceled
        "completed" -> pure Completed
        "continued_as_new" -> pure ContinuedAsNew
        "failed" -> pure Failed
        "terminated" -> pure Terminated
        "timed_out" -> pure TimedOut
        e -> fromTextError $ "Failure parsing CloseStatus from value: '" <> e
           <> "'. Accepted values: canceled, completed, continued_as_new, failed, terminated, timed_out"

instance ToText CloseStatus where
    toText = \case
        Canceled -> "CANCELED"
        Completed -> "COMPLETED"
        ContinuedAsNew -> "CONTINUED_AS_NEW"
        Failed -> "FAILED"
        Terminated -> "TERMINATED"
        TimedOut -> "TIMED_OUT"

instance Hashable     CloseStatus
instance NFData       CloseStatus
instance ToByteString CloseStatus
instance ToQuery      CloseStatus
instance ToHeader     CloseStatus

instance ToJSON CloseStatus where
    toJSON = toJSONText

instance FromJSON CloseStatus where
    parseJSON = parseJSONText "CloseStatus"

data CompleteWorkflowExecutionFailedCause
  = CWEFCOperationNotPermitted
  | CWEFCUnhandledDecision
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompleteWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "operation_not_permitted" -> pure CWEFCOperationNotPermitted
        "unhandled_decision" -> pure CWEFCUnhandledDecision
        e -> fromTextError $ "Failure parsing CompleteWorkflowExecutionFailedCause from value: '" <> e
           <> "'. Accepted values: operation_not_permitted, unhandled_decision"

instance ToText CompleteWorkflowExecutionFailedCause where
    toText = \case
        CWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CWEFCUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable     CompleteWorkflowExecutionFailedCause
instance NFData       CompleteWorkflowExecutionFailedCause
instance ToByteString CompleteWorkflowExecutionFailedCause
instance ToQuery      CompleteWorkflowExecutionFailedCause
instance ToHeader     CompleteWorkflowExecutionFailedCause

instance FromJSON CompleteWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CompleteWorkflowExecutionFailedCause"

data ContinueAsNewWorkflowExecutionFailedCause
  = CANWEFCContinueAsNewWorkflowExecutionRateExceeded
  | CANWEFCDefaultChildPolicyUndefined
  | CANWEFCDefaultExecutionStartToCloseTimeoutUndefined
  | CANWEFCDefaultTaskListUndefined
  | CANWEFCDefaultTaskStartToCloseTimeoutUndefined
  | CANWEFCOperationNotPermitted
  | CANWEFCUnhandledDecision
  | CANWEFCWorkflowTypeDeprecated
  | CANWEFCWorkflowTypeDoesNotExist
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContinueAsNewWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "continue_as_new_workflow_execution_rate_exceeded" -> pure CANWEFCContinueAsNewWorkflowExecutionRateExceeded
        "default_child_policy_undefined" -> pure CANWEFCDefaultChildPolicyUndefined
        "default_execution_start_to_close_timeout_undefined" -> pure CANWEFCDefaultExecutionStartToCloseTimeoutUndefined
        "default_task_list_undefined" -> pure CANWEFCDefaultTaskListUndefined
        "default_task_start_to_close_timeout_undefined" -> pure CANWEFCDefaultTaskStartToCloseTimeoutUndefined
        "operation_not_permitted" -> pure CANWEFCOperationNotPermitted
        "unhandled_decision" -> pure CANWEFCUnhandledDecision
        "workflow_type_deprecated" -> pure CANWEFCWorkflowTypeDeprecated
        "workflow_type_does_not_exist" -> pure CANWEFCWorkflowTypeDoesNotExist
        e -> fromTextError $ "Failure parsing ContinueAsNewWorkflowExecutionFailedCause from value: '" <> e
           <> "'. Accepted values: continue_as_new_workflow_execution_rate_exceeded, default_child_policy_undefined, default_execution_start_to_close_timeout_undefined, default_task_list_undefined, default_task_start_to_close_timeout_undefined, operation_not_permitted, unhandled_decision, workflow_type_deprecated, workflow_type_does_not_exist"

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
    toText = \case
        CANWEFCContinueAsNewWorkflowExecutionRateExceeded -> "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        CANWEFCDefaultChildPolicyUndefined -> "DEFAULT_CHILD_POLICY_UNDEFINED"
        CANWEFCDefaultExecutionStartToCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        CANWEFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
        CANWEFCDefaultTaskStartToCloseTimeoutUndefined -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        CANWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CANWEFCUnhandledDecision -> "UNHANDLED_DECISION"
        CANWEFCWorkflowTypeDeprecated -> "WORKFLOW_TYPE_DEPRECATED"
        CANWEFCWorkflowTypeDoesNotExist -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Hashable     ContinueAsNewWorkflowExecutionFailedCause
instance NFData       ContinueAsNewWorkflowExecutionFailedCause
instance ToByteString ContinueAsNewWorkflowExecutionFailedCause
instance ToQuery      ContinueAsNewWorkflowExecutionFailedCause
instance ToHeader     ContinueAsNewWorkflowExecutionFailedCause

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "ContinueAsNewWorkflowExecutionFailedCause"

data DecisionTaskTimeoutType =
  StartToClose
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DecisionTaskTimeoutType where
    parser = takeLowerText >>= \case
        "start_to_close" -> pure StartToClose
        e -> fromTextError $ "Failure parsing DecisionTaskTimeoutType from value: '" <> e
           <> "'. Accepted values: start_to_close"

instance ToText DecisionTaskTimeoutType where
    toText = \case
        StartToClose -> "START_TO_CLOSE"

instance Hashable     DecisionTaskTimeoutType
instance NFData       DecisionTaskTimeoutType
instance ToByteString DecisionTaskTimeoutType
instance ToQuery      DecisionTaskTimeoutType
instance ToHeader     DecisionTaskTimeoutType

instance FromJSON DecisionTaskTimeoutType where
    parseJSON = parseJSONText "DecisionTaskTimeoutType"

data DecisionType
  = CancelTimer
  | CancelWorkflowExecution
  | CompleteWorkflowExecution
  | ContinueAsNewWorkflowExecution
  | FailWorkflowExecution
  | RecordMarker
  | RequestCancelActivityTask
  | RequestCancelExternalWorkflowExecution
  | ScheduleActivityTask
  | ScheduleLambdaFunction
  | SignalExternalWorkflowExecution
  | StartChildWorkflowExecution
  | StartTimer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DecisionType where
    parser = takeLowerText >>= \case
        "canceltimer" -> pure CancelTimer
        "cancelworkflowexecution" -> pure CancelWorkflowExecution
        "completeworkflowexecution" -> pure CompleteWorkflowExecution
        "continueasnewworkflowexecution" -> pure ContinueAsNewWorkflowExecution
        "failworkflowexecution" -> pure FailWorkflowExecution
        "recordmarker" -> pure RecordMarker
        "requestcancelactivitytask" -> pure RequestCancelActivityTask
        "requestcancelexternalworkflowexecution" -> pure RequestCancelExternalWorkflowExecution
        "scheduleactivitytask" -> pure ScheduleActivityTask
        "schedulelambdafunction" -> pure ScheduleLambdaFunction
        "signalexternalworkflowexecution" -> pure SignalExternalWorkflowExecution
        "startchildworkflowexecution" -> pure StartChildWorkflowExecution
        "starttimer" -> pure StartTimer
        e -> fromTextError $ "Failure parsing DecisionType from value: '" <> e
           <> "'. Accepted values: canceltimer, cancelworkflowexecution, completeworkflowexecution, continueasnewworkflowexecution, failworkflowexecution, recordmarker, requestcancelactivitytask, requestcancelexternalworkflowexecution, scheduleactivitytask, schedulelambdafunction, signalexternalworkflowexecution, startchildworkflowexecution, starttimer"

instance ToText DecisionType where
    toText = \case
        CancelTimer -> "CancelTimer"
        CancelWorkflowExecution -> "CancelWorkflowExecution"
        CompleteWorkflowExecution -> "CompleteWorkflowExecution"
        ContinueAsNewWorkflowExecution -> "ContinueAsNewWorkflowExecution"
        FailWorkflowExecution -> "FailWorkflowExecution"
        RecordMarker -> "RecordMarker"
        RequestCancelActivityTask -> "RequestCancelActivityTask"
        RequestCancelExternalWorkflowExecution -> "RequestCancelExternalWorkflowExecution"
        ScheduleActivityTask -> "ScheduleActivityTask"
        ScheduleLambdaFunction -> "ScheduleLambdaFunction"
        SignalExternalWorkflowExecution -> "SignalExternalWorkflowExecution"
        StartChildWorkflowExecution -> "StartChildWorkflowExecution"
        StartTimer -> "StartTimer"

instance Hashable     DecisionType
instance NFData       DecisionType
instance ToByteString DecisionType
instance ToQuery      DecisionType
instance ToHeader     DecisionType

instance ToJSON DecisionType where
    toJSON = toJSONText

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventType where
    parser = takeLowerText >>= \case
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
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
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

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromJSON EventType where
    parseJSON = parseJSONText "EventType"

data ExecutionStatus
  = Closed
  | Open
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "closed" -> pure Closed
        "open" -> pure Open
        e -> fromTextError $ "Failure parsing ExecutionStatus from value: '" <> e
           <> "'. Accepted values: closed, open"

instance ToText ExecutionStatus where
    toText = \case
        Closed -> "CLOSED"
        Open -> "OPEN"

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"

data FailWorkflowExecutionFailedCause
  = FWEFCOperationNotPermitted
  | FWEFCUnhandledDecision
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FailWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "operation_not_permitted" -> pure FWEFCOperationNotPermitted
        "unhandled_decision" -> pure FWEFCUnhandledDecision
        e -> fromTextError $ "Failure parsing FailWorkflowExecutionFailedCause from value: '" <> e
           <> "'. Accepted values: operation_not_permitted, unhandled_decision"

instance ToText FailWorkflowExecutionFailedCause where
    toText = \case
        FWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        FWEFCUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable     FailWorkflowExecutionFailedCause
instance NFData       FailWorkflowExecutionFailedCause
instance ToByteString FailWorkflowExecutionFailedCause
instance ToQuery      FailWorkflowExecutionFailedCause
instance ToHeader     FailWorkflowExecutionFailedCause

instance FromJSON FailWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "FailWorkflowExecutionFailedCause"

data LambdaFunctionTimeoutType =
  LFTTStartToClose
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LambdaFunctionTimeoutType where
    parser = takeLowerText >>= \case
        "start_to_close" -> pure LFTTStartToClose
        e -> fromTextError $ "Failure parsing LambdaFunctionTimeoutType from value: '" <> e
           <> "'. Accepted values: start_to_close"

instance ToText LambdaFunctionTimeoutType where
    toText = \case
        LFTTStartToClose -> "START_TO_CLOSE"

instance Hashable     LambdaFunctionTimeoutType
instance NFData       LambdaFunctionTimeoutType
instance ToByteString LambdaFunctionTimeoutType
instance ToQuery      LambdaFunctionTimeoutType
instance ToHeader     LambdaFunctionTimeoutType

instance FromJSON LambdaFunctionTimeoutType where
    parseJSON = parseJSONText "LambdaFunctionTimeoutType"

data RecordMarkerFailedCause =
  OperationNotPermitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecordMarkerFailedCause where
    parser = takeLowerText >>= \case
        "operation_not_permitted" -> pure OperationNotPermitted
        e -> fromTextError $ "Failure parsing RecordMarkerFailedCause from value: '" <> e
           <> "'. Accepted values: operation_not_permitted"

instance ToText RecordMarkerFailedCause where
    toText = \case
        OperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable     RecordMarkerFailedCause
instance NFData       RecordMarkerFailedCause
instance ToByteString RecordMarkerFailedCause
instance ToQuery      RecordMarkerFailedCause
instance ToHeader     RecordMarkerFailedCause

instance FromJSON RecordMarkerFailedCause where
    parseJSON = parseJSONText "RecordMarkerFailedCause"

data RegistrationStatus
  = Deprecated
  | Registered
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "deprecated" -> pure Deprecated
        "registered" -> pure Registered
        e -> fromTextError $ "Failure parsing RegistrationStatus from value: '" <> e
           <> "'. Accepted values: deprecated, registered"

instance ToText RegistrationStatus where
    toText = \case
        Deprecated -> "DEPRECATED"
        Registered -> "REGISTERED"

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

instance FromJSON RegistrationStatus where
    parseJSON = parseJSONText "RegistrationStatus"

data RequestCancelActivityTaskFailedCause
  = RCATFCActivityIdUnknown
  | RCATFCOperationNotPermitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequestCancelActivityTaskFailedCause where
    parser = takeLowerText >>= \case
        "activity_id_unknown" -> pure RCATFCActivityIdUnknown
        "operation_not_permitted" -> pure RCATFCOperationNotPermitted
        e -> fromTextError $ "Failure parsing RequestCancelActivityTaskFailedCause from value: '" <> e
           <> "'. Accepted values: activity_id_unknown, operation_not_permitted"

instance ToText RequestCancelActivityTaskFailedCause where
    toText = \case
        RCATFCActivityIdUnknown -> "ACTIVITY_ID_UNKNOWN"
        RCATFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable     RequestCancelActivityTaskFailedCause
instance NFData       RequestCancelActivityTaskFailedCause
instance ToByteString RequestCancelActivityTaskFailedCause
instance ToQuery      RequestCancelActivityTaskFailedCause
instance ToHeader     RequestCancelActivityTaskFailedCause

instance FromJSON RequestCancelActivityTaskFailedCause where
    parseJSON = parseJSONText "RequestCancelActivityTaskFailedCause"

data RequestCancelExternalWorkflowExecutionFailedCause
  = RCEWEFCOperationNotPermitted
  | RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded
  | RCEWEFCUnknownExternalWorkflowExecution
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequestCancelExternalWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "operation_not_permitted" -> pure RCEWEFCOperationNotPermitted
        "request_cancel_external_workflow_execution_rate_exceeded" -> pure RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded
        "unknown_external_workflow_execution" -> pure RCEWEFCUnknownExternalWorkflowExecution
        e -> fromTextError $ "Failure parsing RequestCancelExternalWorkflowExecutionFailedCause from value: '" <> e
           <> "'. Accepted values: operation_not_permitted, request_cancel_external_workflow_execution_rate_exceeded, unknown_external_workflow_execution"

instance ToText RequestCancelExternalWorkflowExecutionFailedCause where
    toText = \case
        RCEWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded -> "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        RCEWEFCUnknownExternalWorkflowExecution -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Hashable     RequestCancelExternalWorkflowExecutionFailedCause
instance NFData       RequestCancelExternalWorkflowExecutionFailedCause
instance ToByteString RequestCancelExternalWorkflowExecutionFailedCause
instance ToQuery      RequestCancelExternalWorkflowExecutionFailedCause
instance ToHeader     RequestCancelExternalWorkflowExecutionFailedCause

instance FromJSON RequestCancelExternalWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "RequestCancelExternalWorkflowExecutionFailedCause"

data ScheduleActivityTaskFailedCause
  = SATFCActivityCreationRateExceeded
  | SATFCActivityIdAlreadyInUse
  | SATFCActivityTypeDeprecated
  | SATFCActivityTypeDoesNotExist
  | SATFCDefaultHeartbeatTimeoutUndefined
  | SATFCDefaultScheduleToCloseTimeoutUndefined
  | SATFCDefaultScheduleToStartTimeoutUndefined
  | SATFCDefaultStartToCloseTimeoutUndefined
  | SATFCDefaultTaskListUndefined
  | SATFCOpenActivitiesLimitExceeded
  | SATFCOperationNotPermitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScheduleActivityTaskFailedCause where
    parser = takeLowerText >>= \case
        "activity_creation_rate_exceeded" -> pure SATFCActivityCreationRateExceeded
        "activity_id_already_in_use" -> pure SATFCActivityIdAlreadyInUse
        "activity_type_deprecated" -> pure SATFCActivityTypeDeprecated
        "activity_type_does_not_exist" -> pure SATFCActivityTypeDoesNotExist
        "default_heartbeat_timeout_undefined" -> pure SATFCDefaultHeartbeatTimeoutUndefined
        "default_schedule_to_close_timeout_undefined" -> pure SATFCDefaultScheduleToCloseTimeoutUndefined
        "default_schedule_to_start_timeout_undefined" -> pure SATFCDefaultScheduleToStartTimeoutUndefined
        "default_start_to_close_timeout_undefined" -> pure SATFCDefaultStartToCloseTimeoutUndefined
        "default_task_list_undefined" -> pure SATFCDefaultTaskListUndefined
        "open_activities_limit_exceeded" -> pure SATFCOpenActivitiesLimitExceeded
        "operation_not_permitted" -> pure SATFCOperationNotPermitted
        e -> fromTextError $ "Failure parsing ScheduleActivityTaskFailedCause from value: '" <> e
           <> "'. Accepted values: activity_creation_rate_exceeded, activity_id_already_in_use, activity_type_deprecated, activity_type_does_not_exist, default_heartbeat_timeout_undefined, default_schedule_to_close_timeout_undefined, default_schedule_to_start_timeout_undefined, default_start_to_close_timeout_undefined, default_task_list_undefined, open_activities_limit_exceeded, operation_not_permitted"

instance ToText ScheduleActivityTaskFailedCause where
    toText = \case
        SATFCActivityCreationRateExceeded -> "ACTIVITY_CREATION_RATE_EXCEEDED"
        SATFCActivityIdAlreadyInUse -> "ACTIVITY_ID_ALREADY_IN_USE"
        SATFCActivityTypeDeprecated -> "ACTIVITY_TYPE_DEPRECATED"
        SATFCActivityTypeDoesNotExist -> "ACTIVITY_TYPE_DOES_NOT_EXIST"
        SATFCDefaultHeartbeatTimeoutUndefined -> "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
        SATFCDefaultScheduleToCloseTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
        SATFCDefaultScheduleToStartTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
        SATFCDefaultStartToCloseTimeoutUndefined -> "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SATFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
        SATFCOpenActivitiesLimitExceeded -> "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
        SATFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable     ScheduleActivityTaskFailedCause
instance NFData       ScheduleActivityTaskFailedCause
instance ToByteString ScheduleActivityTaskFailedCause
instance ToQuery      ScheduleActivityTaskFailedCause
instance ToHeader     ScheduleActivityTaskFailedCause

instance FromJSON ScheduleActivityTaskFailedCause where
    parseJSON = parseJSONText "ScheduleActivityTaskFailedCause"

data ScheduleLambdaFunctionFailedCause
  = IdAlreadyInUse
  | LambdaFunctionCreationRateExceeded
  | LambdaServiceNotAvailableInRegion
  | OpenLambdaFunctionsLimitExceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScheduleLambdaFunctionFailedCause where
    parser = takeLowerText >>= \case
        "id_already_in_use" -> pure IdAlreadyInUse
        "lambda_function_creation_rate_exceeded" -> pure LambdaFunctionCreationRateExceeded
        "lambda_service_not_available_in_region" -> pure LambdaServiceNotAvailableInRegion
        "open_lambda_functions_limit_exceeded" -> pure OpenLambdaFunctionsLimitExceeded
        e -> fromTextError $ "Failure parsing ScheduleLambdaFunctionFailedCause from value: '" <> e
           <> "'. Accepted values: id_already_in_use, lambda_function_creation_rate_exceeded, lambda_service_not_available_in_region, open_lambda_functions_limit_exceeded"

instance ToText ScheduleLambdaFunctionFailedCause where
    toText = \case
        IdAlreadyInUse -> "ID_ALREADY_IN_USE"
        LambdaFunctionCreationRateExceeded -> "LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED"
        LambdaServiceNotAvailableInRegion -> "LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION"
        OpenLambdaFunctionsLimitExceeded -> "OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED"

instance Hashable     ScheduleLambdaFunctionFailedCause
instance NFData       ScheduleLambdaFunctionFailedCause
instance ToByteString ScheduleLambdaFunctionFailedCause
instance ToQuery      ScheduleLambdaFunctionFailedCause
instance ToHeader     ScheduleLambdaFunctionFailedCause

instance FromJSON ScheduleLambdaFunctionFailedCause where
    parseJSON = parseJSONText "ScheduleLambdaFunctionFailedCause"

data SignalExternalWorkflowExecutionFailedCause
  = SEWEFCOperationNotPermitted
  | SEWEFCSignalExternalWorkflowExecutionRateExceeded
  | SEWEFCUnknownExternalWorkflowExecution
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SignalExternalWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "operation_not_permitted" -> pure SEWEFCOperationNotPermitted
        "signal_external_workflow_execution_rate_exceeded" -> pure SEWEFCSignalExternalWorkflowExecutionRateExceeded
        "unknown_external_workflow_execution" -> pure SEWEFCUnknownExternalWorkflowExecution
        e -> fromTextError $ "Failure parsing SignalExternalWorkflowExecutionFailedCause from value: '" <> e
           <> "'. Accepted values: operation_not_permitted, signal_external_workflow_execution_rate_exceeded, unknown_external_workflow_execution"

instance ToText SignalExternalWorkflowExecutionFailedCause where
    toText = \case
        SEWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        SEWEFCSignalExternalWorkflowExecutionRateExceeded -> "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        SEWEFCUnknownExternalWorkflowExecution -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Hashable     SignalExternalWorkflowExecutionFailedCause
instance NFData       SignalExternalWorkflowExecutionFailedCause
instance ToByteString SignalExternalWorkflowExecutionFailedCause
instance ToQuery      SignalExternalWorkflowExecutionFailedCause
instance ToHeader     SignalExternalWorkflowExecutionFailedCause

instance FromJSON SignalExternalWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "SignalExternalWorkflowExecutionFailedCause"

data StartChildWorkflowExecutionFailedCause
  = SCWEFCChildCreationRateExceeded
  | SCWEFCDefaultChildPolicyUndefined
  | SCWEFCDefaultExecutionStartToCloseTimeoutUndefined
  | SCWEFCDefaultTaskListUndefined
  | SCWEFCDefaultTaskStartToCloseTimeoutUndefined
  | SCWEFCOpenChildrenLimitExceeded
  | SCWEFCOpenWorkflowsLimitExceeded
  | SCWEFCOperationNotPermitted
  | SCWEFCWorkflowAlreadyRunning
  | SCWEFCWorkflowTypeDeprecated
  | SCWEFCWorkflowTypeDoesNotExist
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StartChildWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "child_creation_rate_exceeded" -> pure SCWEFCChildCreationRateExceeded
        "default_child_policy_undefined" -> pure SCWEFCDefaultChildPolicyUndefined
        "default_execution_start_to_close_timeout_undefined" -> pure SCWEFCDefaultExecutionStartToCloseTimeoutUndefined
        "default_task_list_undefined" -> pure SCWEFCDefaultTaskListUndefined
        "default_task_start_to_close_timeout_undefined" -> pure SCWEFCDefaultTaskStartToCloseTimeoutUndefined
        "open_children_limit_exceeded" -> pure SCWEFCOpenChildrenLimitExceeded
        "open_workflows_limit_exceeded" -> pure SCWEFCOpenWorkflowsLimitExceeded
        "operation_not_permitted" -> pure SCWEFCOperationNotPermitted
        "workflow_already_running" -> pure SCWEFCWorkflowAlreadyRunning
        "workflow_type_deprecated" -> pure SCWEFCWorkflowTypeDeprecated
        "workflow_type_does_not_exist" -> pure SCWEFCWorkflowTypeDoesNotExist
        e -> fromTextError $ "Failure parsing StartChildWorkflowExecutionFailedCause from value: '" <> e
           <> "'. Accepted values: child_creation_rate_exceeded, default_child_policy_undefined, default_execution_start_to_close_timeout_undefined, default_task_list_undefined, default_task_start_to_close_timeout_undefined, open_children_limit_exceeded, open_workflows_limit_exceeded, operation_not_permitted, workflow_already_running, workflow_type_deprecated, workflow_type_does_not_exist"

instance ToText StartChildWorkflowExecutionFailedCause where
    toText = \case
        SCWEFCChildCreationRateExceeded -> "CHILD_CREATION_RATE_EXCEEDED"
        SCWEFCDefaultChildPolicyUndefined -> "DEFAULT_CHILD_POLICY_UNDEFINED"
        SCWEFCDefaultExecutionStartToCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SCWEFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
        SCWEFCDefaultTaskStartToCloseTimeoutUndefined -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SCWEFCOpenChildrenLimitExceeded -> "OPEN_CHILDREN_LIMIT_EXCEEDED"
        SCWEFCOpenWorkflowsLimitExceeded -> "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
        SCWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        SCWEFCWorkflowAlreadyRunning -> "WORKFLOW_ALREADY_RUNNING"
        SCWEFCWorkflowTypeDeprecated -> "WORKFLOW_TYPE_DEPRECATED"
        SCWEFCWorkflowTypeDoesNotExist -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Hashable     StartChildWorkflowExecutionFailedCause
instance NFData       StartChildWorkflowExecutionFailedCause
instance ToByteString StartChildWorkflowExecutionFailedCause
instance ToQuery      StartChildWorkflowExecutionFailedCause
instance ToHeader     StartChildWorkflowExecutionFailedCause

instance FromJSON StartChildWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "StartChildWorkflowExecutionFailedCause"

data StartLambdaFunctionFailedCause =
  AssumeRoleFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StartLambdaFunctionFailedCause where
    parser = takeLowerText >>= \case
        "assume_role_failed" -> pure AssumeRoleFailed
        e -> fromTextError $ "Failure parsing StartLambdaFunctionFailedCause from value: '" <> e
           <> "'. Accepted values: assume_role_failed"

instance ToText StartLambdaFunctionFailedCause where
    toText = \case
        AssumeRoleFailed -> "ASSUME_ROLE_FAILED"

instance Hashable     StartLambdaFunctionFailedCause
instance NFData       StartLambdaFunctionFailedCause
instance ToByteString StartLambdaFunctionFailedCause
instance ToQuery      StartLambdaFunctionFailedCause
instance ToHeader     StartLambdaFunctionFailedCause

instance FromJSON StartLambdaFunctionFailedCause where
    parseJSON = parseJSONText "StartLambdaFunctionFailedCause"

data StartTimerFailedCause
  = STFCOpenTimersLimitExceeded
  | STFCOperationNotPermitted
  | STFCTimerCreationRateExceeded
  | STFCTimerIdAlreadyInUse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StartTimerFailedCause where
    parser = takeLowerText >>= \case
        "open_timers_limit_exceeded" -> pure STFCOpenTimersLimitExceeded
        "operation_not_permitted" -> pure STFCOperationNotPermitted
        "timer_creation_rate_exceeded" -> pure STFCTimerCreationRateExceeded
        "timer_id_already_in_use" -> pure STFCTimerIdAlreadyInUse
        e -> fromTextError $ "Failure parsing StartTimerFailedCause from value: '" <> e
           <> "'. Accepted values: open_timers_limit_exceeded, operation_not_permitted, timer_creation_rate_exceeded, timer_id_already_in_use"

instance ToText StartTimerFailedCause where
    toText = \case
        STFCOpenTimersLimitExceeded -> "OPEN_TIMERS_LIMIT_EXCEEDED"
        STFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        STFCTimerCreationRateExceeded -> "TIMER_CREATION_RATE_EXCEEDED"
        STFCTimerIdAlreadyInUse -> "TIMER_ID_ALREADY_IN_USE"

instance Hashable     StartTimerFailedCause
instance NFData       StartTimerFailedCause
instance ToByteString StartTimerFailedCause
instance ToQuery      StartTimerFailedCause
instance ToHeader     StartTimerFailedCause

instance FromJSON StartTimerFailedCause where
    parseJSON = parseJSONText "StartTimerFailedCause"

data WorkflowExecutionCancelRequestedCause =
  ChildPolicyApplied
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkflowExecutionCancelRequestedCause where
    parser = takeLowerText >>= \case
        "child_policy_applied" -> pure ChildPolicyApplied
        e -> fromTextError $ "Failure parsing WorkflowExecutionCancelRequestedCause from value: '" <> e
           <> "'. Accepted values: child_policy_applied"

instance ToText WorkflowExecutionCancelRequestedCause where
    toText = \case
        ChildPolicyApplied -> "CHILD_POLICY_APPLIED"

instance Hashable     WorkflowExecutionCancelRequestedCause
instance NFData       WorkflowExecutionCancelRequestedCause
instance ToByteString WorkflowExecutionCancelRequestedCause
instance ToQuery      WorkflowExecutionCancelRequestedCause
instance ToHeader     WorkflowExecutionCancelRequestedCause

instance FromJSON WorkflowExecutionCancelRequestedCause where
    parseJSON = parseJSONText "WorkflowExecutionCancelRequestedCause"

data WorkflowExecutionTerminatedCause
  = WETCChildPolicyApplied
  | WETCEventLimitExceeded
  | WETCOperatorInitiated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkflowExecutionTerminatedCause where
    parser = takeLowerText >>= \case
        "child_policy_applied" -> pure WETCChildPolicyApplied
        "event_limit_exceeded" -> pure WETCEventLimitExceeded
        "operator_initiated" -> pure WETCOperatorInitiated
        e -> fromTextError $ "Failure parsing WorkflowExecutionTerminatedCause from value: '" <> e
           <> "'. Accepted values: child_policy_applied, event_limit_exceeded, operator_initiated"

instance ToText WorkflowExecutionTerminatedCause where
    toText = \case
        WETCChildPolicyApplied -> "CHILD_POLICY_APPLIED"
        WETCEventLimitExceeded -> "EVENT_LIMIT_EXCEEDED"
        WETCOperatorInitiated -> "OPERATOR_INITIATED"

instance Hashable     WorkflowExecutionTerminatedCause
instance NFData       WorkflowExecutionTerminatedCause
instance ToByteString WorkflowExecutionTerminatedCause
instance ToQuery      WorkflowExecutionTerminatedCause
instance ToHeader     WorkflowExecutionTerminatedCause

instance FromJSON WorkflowExecutionTerminatedCause where
    parseJSON = parseJSONText "WorkflowExecutionTerminatedCause"

data WorkflowExecutionTimeoutType =
  WETTStartToClose
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkflowExecutionTimeoutType where
    parser = takeLowerText >>= \case
        "start_to_close" -> pure WETTStartToClose
        e -> fromTextError $ "Failure parsing WorkflowExecutionTimeoutType from value: '" <> e
           <> "'. Accepted values: start_to_close"

instance ToText WorkflowExecutionTimeoutType where
    toText = \case
        WETTStartToClose -> "START_TO_CLOSE"

instance Hashable     WorkflowExecutionTimeoutType
instance NFData       WorkflowExecutionTimeoutType
instance ToByteString WorkflowExecutionTimeoutType
instance ToQuery      WorkflowExecutionTimeoutType
instance ToHeader     WorkflowExecutionTimeoutType

instance FromJSON WorkflowExecutionTimeoutType where
    parseJSON = parseJSONText "WorkflowExecutionTimeoutType"
