{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.EventType
  ( EventType
      ( EventType',
        ActivityTaskCancelRequested,
        ActivityTaskCanceled,
        ActivityTaskCompleted,
        ActivityTaskFailed,
        ActivityTaskScheduled,
        ActivityTaskStarted,
        ActivityTaskTimedOut,
        CancelTimerFailed,
        CancelWorkflowExecutionFailed,
        ChildWorkflowExecutionCanceled,
        ChildWorkflowExecutionCompleted,
        ChildWorkflowExecutionFailed,
        ChildWorkflowExecutionStarted,
        ChildWorkflowExecutionTerminated,
        ChildWorkflowExecutionTimedOut,
        CompleteWorkflowExecutionFailed,
        ContinueAsNewWorkflowExecutionFailed,
        DecisionTaskCompleted,
        DecisionTaskScheduled,
        DecisionTaskStarted,
        DecisionTaskTimedOut,
        ExternalWorkflowExecutionCancelRequested,
        ExternalWorkflowExecutionSignaled,
        FailWorkflowExecutionFailed,
        LambdaFunctionCompleted,
        LambdaFunctionFailed,
        LambdaFunctionScheduled,
        LambdaFunctionStarted,
        LambdaFunctionTimedOut,
        MarkerRecorded,
        RecordMarkerFailed,
        RequestCancelActivityTaskFailed,
        RequestCancelExternalWorkflowExecutionFailed,
        RequestCancelExternalWorkflowExecutionInitiated,
        ScheduleActivityTaskFailed,
        ScheduleLambdaFunctionFailed,
        SignalExternalWorkflowExecutionFailed,
        SignalExternalWorkflowExecutionInitiated,
        StartChildWorkflowExecutionFailed,
        StartChildWorkflowExecutionInitiated,
        StartLambdaFunctionFailed,
        StartTimerFailed,
        TimerCanceled,
        TimerFired,
        TimerStarted,
        WorkflowExecutionCancelRequested,
        WorkflowExecutionCanceled,
        WorkflowExecutionCompleted,
        WorkflowExecutionContinuedAsNew,
        WorkflowExecutionFailed,
        WorkflowExecutionSignaled,
        WorkflowExecutionStarted,
        WorkflowExecutionTerminated,
        WorkflowExecutionTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventType = EventType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ActivityTaskCancelRequested :: EventType
pattern ActivityTaskCancelRequested = EventType' "ActivityTaskCancelRequested"

pattern ActivityTaskCanceled :: EventType
pattern ActivityTaskCanceled = EventType' "ActivityTaskCanceled"

pattern ActivityTaskCompleted :: EventType
pattern ActivityTaskCompleted = EventType' "ActivityTaskCompleted"

pattern ActivityTaskFailed :: EventType
pattern ActivityTaskFailed = EventType' "ActivityTaskFailed"

pattern ActivityTaskScheduled :: EventType
pattern ActivityTaskScheduled = EventType' "ActivityTaskScheduled"

pattern ActivityTaskStarted :: EventType
pattern ActivityTaskStarted = EventType' "ActivityTaskStarted"

pattern ActivityTaskTimedOut :: EventType
pattern ActivityTaskTimedOut = EventType' "ActivityTaskTimedOut"

pattern CancelTimerFailed :: EventType
pattern CancelTimerFailed = EventType' "CancelTimerFailed"

pattern CancelWorkflowExecutionFailed :: EventType
pattern CancelWorkflowExecutionFailed = EventType' "CancelWorkflowExecutionFailed"

pattern ChildWorkflowExecutionCanceled :: EventType
pattern ChildWorkflowExecutionCanceled = EventType' "ChildWorkflowExecutionCanceled"

pattern ChildWorkflowExecutionCompleted :: EventType
pattern ChildWorkflowExecutionCompleted = EventType' "ChildWorkflowExecutionCompleted"

pattern ChildWorkflowExecutionFailed :: EventType
pattern ChildWorkflowExecutionFailed = EventType' "ChildWorkflowExecutionFailed"

pattern ChildWorkflowExecutionStarted :: EventType
pattern ChildWorkflowExecutionStarted = EventType' "ChildWorkflowExecutionStarted"

pattern ChildWorkflowExecutionTerminated :: EventType
pattern ChildWorkflowExecutionTerminated = EventType' "ChildWorkflowExecutionTerminated"

pattern ChildWorkflowExecutionTimedOut :: EventType
pattern ChildWorkflowExecutionTimedOut = EventType' "ChildWorkflowExecutionTimedOut"

pattern CompleteWorkflowExecutionFailed :: EventType
pattern CompleteWorkflowExecutionFailed = EventType' "CompleteWorkflowExecutionFailed"

pattern ContinueAsNewWorkflowExecutionFailed :: EventType
pattern ContinueAsNewWorkflowExecutionFailed = EventType' "ContinueAsNewWorkflowExecutionFailed"

pattern DecisionTaskCompleted :: EventType
pattern DecisionTaskCompleted = EventType' "DecisionTaskCompleted"

pattern DecisionTaskScheduled :: EventType
pattern DecisionTaskScheduled = EventType' "DecisionTaskScheduled"

pattern DecisionTaskStarted :: EventType
pattern DecisionTaskStarted = EventType' "DecisionTaskStarted"

pattern DecisionTaskTimedOut :: EventType
pattern DecisionTaskTimedOut = EventType' "DecisionTaskTimedOut"

pattern ExternalWorkflowExecutionCancelRequested :: EventType
pattern ExternalWorkflowExecutionCancelRequested = EventType' "ExternalWorkflowExecutionCancelRequested"

pattern ExternalWorkflowExecutionSignaled :: EventType
pattern ExternalWorkflowExecutionSignaled = EventType' "ExternalWorkflowExecutionSignaled"

pattern FailWorkflowExecutionFailed :: EventType
pattern FailWorkflowExecutionFailed = EventType' "FailWorkflowExecutionFailed"

pattern LambdaFunctionCompleted :: EventType
pattern LambdaFunctionCompleted = EventType' "LambdaFunctionCompleted"

pattern LambdaFunctionFailed :: EventType
pattern LambdaFunctionFailed = EventType' "LambdaFunctionFailed"

pattern LambdaFunctionScheduled :: EventType
pattern LambdaFunctionScheduled = EventType' "LambdaFunctionScheduled"

pattern LambdaFunctionStarted :: EventType
pattern LambdaFunctionStarted = EventType' "LambdaFunctionStarted"

pattern LambdaFunctionTimedOut :: EventType
pattern LambdaFunctionTimedOut = EventType' "LambdaFunctionTimedOut"

pattern MarkerRecorded :: EventType
pattern MarkerRecorded = EventType' "MarkerRecorded"

pattern RecordMarkerFailed :: EventType
pattern RecordMarkerFailed = EventType' "RecordMarkerFailed"

pattern RequestCancelActivityTaskFailed :: EventType
pattern RequestCancelActivityTaskFailed = EventType' "RequestCancelActivityTaskFailed"

pattern RequestCancelExternalWorkflowExecutionFailed :: EventType
pattern RequestCancelExternalWorkflowExecutionFailed = EventType' "RequestCancelExternalWorkflowExecutionFailed"

pattern RequestCancelExternalWorkflowExecutionInitiated :: EventType
pattern RequestCancelExternalWorkflowExecutionInitiated = EventType' "RequestCancelExternalWorkflowExecutionInitiated"

pattern ScheduleActivityTaskFailed :: EventType
pattern ScheduleActivityTaskFailed = EventType' "ScheduleActivityTaskFailed"

pattern ScheduleLambdaFunctionFailed :: EventType
pattern ScheduleLambdaFunctionFailed = EventType' "ScheduleLambdaFunctionFailed"

pattern SignalExternalWorkflowExecutionFailed :: EventType
pattern SignalExternalWorkflowExecutionFailed = EventType' "SignalExternalWorkflowExecutionFailed"

pattern SignalExternalWorkflowExecutionInitiated :: EventType
pattern SignalExternalWorkflowExecutionInitiated = EventType' "SignalExternalWorkflowExecutionInitiated"

pattern StartChildWorkflowExecutionFailed :: EventType
pattern StartChildWorkflowExecutionFailed = EventType' "StartChildWorkflowExecutionFailed"

pattern StartChildWorkflowExecutionInitiated :: EventType
pattern StartChildWorkflowExecutionInitiated = EventType' "StartChildWorkflowExecutionInitiated"

pattern StartLambdaFunctionFailed :: EventType
pattern StartLambdaFunctionFailed = EventType' "StartLambdaFunctionFailed"

pattern StartTimerFailed :: EventType
pattern StartTimerFailed = EventType' "StartTimerFailed"

pattern TimerCanceled :: EventType
pattern TimerCanceled = EventType' "TimerCanceled"

pattern TimerFired :: EventType
pattern TimerFired = EventType' "TimerFired"

pattern TimerStarted :: EventType
pattern TimerStarted = EventType' "TimerStarted"

pattern WorkflowExecutionCancelRequested :: EventType
pattern WorkflowExecutionCancelRequested = EventType' "WorkflowExecutionCancelRequested"

pattern WorkflowExecutionCanceled :: EventType
pattern WorkflowExecutionCanceled = EventType' "WorkflowExecutionCanceled"

pattern WorkflowExecutionCompleted :: EventType
pattern WorkflowExecutionCompleted = EventType' "WorkflowExecutionCompleted"

pattern WorkflowExecutionContinuedAsNew :: EventType
pattern WorkflowExecutionContinuedAsNew = EventType' "WorkflowExecutionContinuedAsNew"

pattern WorkflowExecutionFailed :: EventType
pattern WorkflowExecutionFailed = EventType' "WorkflowExecutionFailed"

pattern WorkflowExecutionSignaled :: EventType
pattern WorkflowExecutionSignaled = EventType' "WorkflowExecutionSignaled"

pattern WorkflowExecutionStarted :: EventType
pattern WorkflowExecutionStarted = EventType' "WorkflowExecutionStarted"

pattern WorkflowExecutionTerminated :: EventType
pattern WorkflowExecutionTerminated = EventType' "WorkflowExecutionTerminated"

pattern WorkflowExecutionTimedOut :: EventType
pattern WorkflowExecutionTimedOut = EventType' "WorkflowExecutionTimedOut"

{-# COMPLETE
  ActivityTaskCancelRequested,
  ActivityTaskCanceled,
  ActivityTaskCompleted,
  ActivityTaskFailed,
  ActivityTaskScheduled,
  ActivityTaskStarted,
  ActivityTaskTimedOut,
  CancelTimerFailed,
  CancelWorkflowExecutionFailed,
  ChildWorkflowExecutionCanceled,
  ChildWorkflowExecutionCompleted,
  ChildWorkflowExecutionFailed,
  ChildWorkflowExecutionStarted,
  ChildWorkflowExecutionTerminated,
  ChildWorkflowExecutionTimedOut,
  CompleteWorkflowExecutionFailed,
  ContinueAsNewWorkflowExecutionFailed,
  DecisionTaskCompleted,
  DecisionTaskScheduled,
  DecisionTaskStarted,
  DecisionTaskTimedOut,
  ExternalWorkflowExecutionCancelRequested,
  ExternalWorkflowExecutionSignaled,
  FailWorkflowExecutionFailed,
  LambdaFunctionCompleted,
  LambdaFunctionFailed,
  LambdaFunctionScheduled,
  LambdaFunctionStarted,
  LambdaFunctionTimedOut,
  MarkerRecorded,
  RecordMarkerFailed,
  RequestCancelActivityTaskFailed,
  RequestCancelExternalWorkflowExecutionFailed,
  RequestCancelExternalWorkflowExecutionInitiated,
  ScheduleActivityTaskFailed,
  ScheduleLambdaFunctionFailed,
  SignalExternalWorkflowExecutionFailed,
  SignalExternalWorkflowExecutionInitiated,
  StartChildWorkflowExecutionFailed,
  StartChildWorkflowExecutionInitiated,
  StartLambdaFunctionFailed,
  StartTimerFailed,
  TimerCanceled,
  TimerFired,
  TimerStarted,
  WorkflowExecutionCancelRequested,
  WorkflowExecutionCanceled,
  WorkflowExecutionCompleted,
  WorkflowExecutionContinuedAsNew,
  WorkflowExecutionFailed,
  WorkflowExecutionSignaled,
  WorkflowExecutionStarted,
  WorkflowExecutionTerminated,
  WorkflowExecutionTimedOut,
  EventType'
  #-}
