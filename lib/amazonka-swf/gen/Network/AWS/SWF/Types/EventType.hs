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
        WorkflowExecutionStarted,
        WorkflowExecutionCancelRequested,
        WorkflowExecutionCompleted,
        CompleteWorkflowExecutionFailed,
        WorkflowExecutionFailed,
        FailWorkflowExecutionFailed,
        WorkflowExecutionTimedOut,
        WorkflowExecutionCanceled,
        CancelWorkflowExecutionFailed,
        WorkflowExecutionContinuedAsNew,
        ContinueAsNewWorkflowExecutionFailed,
        WorkflowExecutionTerminated,
        DecisionTaskScheduled,
        DecisionTaskStarted,
        DecisionTaskCompleted,
        DecisionTaskTimedOut,
        ActivityTaskScheduled,
        ScheduleActivityTaskFailed,
        ActivityTaskStarted,
        ActivityTaskCompleted,
        ActivityTaskFailed,
        ActivityTaskTimedOut,
        ActivityTaskCanceled,
        ActivityTaskCancelRequested,
        RequestCancelActivityTaskFailed,
        WorkflowExecutionSignaled,
        MarkerRecorded,
        RecordMarkerFailed,
        TimerStarted,
        StartTimerFailed,
        TimerFired,
        TimerCanceled,
        CancelTimerFailed,
        StartChildWorkflowExecutionInitiated,
        StartChildWorkflowExecutionFailed,
        ChildWorkflowExecutionStarted,
        ChildWorkflowExecutionCompleted,
        ChildWorkflowExecutionFailed,
        ChildWorkflowExecutionTimedOut,
        ChildWorkflowExecutionCanceled,
        ChildWorkflowExecutionTerminated,
        SignalExternalWorkflowExecutionInitiated,
        SignalExternalWorkflowExecutionFailed,
        ExternalWorkflowExecutionSignaled,
        RequestCancelExternalWorkflowExecutionInitiated,
        RequestCancelExternalWorkflowExecutionFailed,
        ExternalWorkflowExecutionCancelRequested,
        LambdaFunctionScheduled,
        LambdaFunctionStarted,
        LambdaFunctionCompleted,
        LambdaFunctionFailed,
        LambdaFunctionTimedOut,
        ScheduleLambdaFunctionFailed,
        StartLambdaFunctionFailed
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

pattern WorkflowExecutionStarted :: EventType
pattern WorkflowExecutionStarted = EventType' "WorkflowExecutionStarted"

pattern WorkflowExecutionCancelRequested :: EventType
pattern WorkflowExecutionCancelRequested = EventType' "WorkflowExecutionCancelRequested"

pattern WorkflowExecutionCompleted :: EventType
pattern WorkflowExecutionCompleted = EventType' "WorkflowExecutionCompleted"

pattern CompleteWorkflowExecutionFailed :: EventType
pattern CompleteWorkflowExecutionFailed = EventType' "CompleteWorkflowExecutionFailed"

pattern WorkflowExecutionFailed :: EventType
pattern WorkflowExecutionFailed = EventType' "WorkflowExecutionFailed"

pattern FailWorkflowExecutionFailed :: EventType
pattern FailWorkflowExecutionFailed = EventType' "FailWorkflowExecutionFailed"

pattern WorkflowExecutionTimedOut :: EventType
pattern WorkflowExecutionTimedOut = EventType' "WorkflowExecutionTimedOut"

pattern WorkflowExecutionCanceled :: EventType
pattern WorkflowExecutionCanceled = EventType' "WorkflowExecutionCanceled"

pattern CancelWorkflowExecutionFailed :: EventType
pattern CancelWorkflowExecutionFailed = EventType' "CancelWorkflowExecutionFailed"

pattern WorkflowExecutionContinuedAsNew :: EventType
pattern WorkflowExecutionContinuedAsNew = EventType' "WorkflowExecutionContinuedAsNew"

pattern ContinueAsNewWorkflowExecutionFailed :: EventType
pattern ContinueAsNewWorkflowExecutionFailed = EventType' "ContinueAsNewWorkflowExecutionFailed"

pattern WorkflowExecutionTerminated :: EventType
pattern WorkflowExecutionTerminated = EventType' "WorkflowExecutionTerminated"

pattern DecisionTaskScheduled :: EventType
pattern DecisionTaskScheduled = EventType' "DecisionTaskScheduled"

pattern DecisionTaskStarted :: EventType
pattern DecisionTaskStarted = EventType' "DecisionTaskStarted"

pattern DecisionTaskCompleted :: EventType
pattern DecisionTaskCompleted = EventType' "DecisionTaskCompleted"

pattern DecisionTaskTimedOut :: EventType
pattern DecisionTaskTimedOut = EventType' "DecisionTaskTimedOut"

pattern ActivityTaskScheduled :: EventType
pattern ActivityTaskScheduled = EventType' "ActivityTaskScheduled"

pattern ScheduleActivityTaskFailed :: EventType
pattern ScheduleActivityTaskFailed = EventType' "ScheduleActivityTaskFailed"

pattern ActivityTaskStarted :: EventType
pattern ActivityTaskStarted = EventType' "ActivityTaskStarted"

pattern ActivityTaskCompleted :: EventType
pattern ActivityTaskCompleted = EventType' "ActivityTaskCompleted"

pattern ActivityTaskFailed :: EventType
pattern ActivityTaskFailed = EventType' "ActivityTaskFailed"

pattern ActivityTaskTimedOut :: EventType
pattern ActivityTaskTimedOut = EventType' "ActivityTaskTimedOut"

pattern ActivityTaskCanceled :: EventType
pattern ActivityTaskCanceled = EventType' "ActivityTaskCanceled"

pattern ActivityTaskCancelRequested :: EventType
pattern ActivityTaskCancelRequested = EventType' "ActivityTaskCancelRequested"

pattern RequestCancelActivityTaskFailed :: EventType
pattern RequestCancelActivityTaskFailed = EventType' "RequestCancelActivityTaskFailed"

pattern WorkflowExecutionSignaled :: EventType
pattern WorkflowExecutionSignaled = EventType' "WorkflowExecutionSignaled"

pattern MarkerRecorded :: EventType
pattern MarkerRecorded = EventType' "MarkerRecorded"

pattern RecordMarkerFailed :: EventType
pattern RecordMarkerFailed = EventType' "RecordMarkerFailed"

pattern TimerStarted :: EventType
pattern TimerStarted = EventType' "TimerStarted"

pattern StartTimerFailed :: EventType
pattern StartTimerFailed = EventType' "StartTimerFailed"

pattern TimerFired :: EventType
pattern TimerFired = EventType' "TimerFired"

pattern TimerCanceled :: EventType
pattern TimerCanceled = EventType' "TimerCanceled"

pattern CancelTimerFailed :: EventType
pattern CancelTimerFailed = EventType' "CancelTimerFailed"

pattern StartChildWorkflowExecutionInitiated :: EventType
pattern StartChildWorkflowExecutionInitiated = EventType' "StartChildWorkflowExecutionInitiated"

pattern StartChildWorkflowExecutionFailed :: EventType
pattern StartChildWorkflowExecutionFailed = EventType' "StartChildWorkflowExecutionFailed"

pattern ChildWorkflowExecutionStarted :: EventType
pattern ChildWorkflowExecutionStarted = EventType' "ChildWorkflowExecutionStarted"

pattern ChildWorkflowExecutionCompleted :: EventType
pattern ChildWorkflowExecutionCompleted = EventType' "ChildWorkflowExecutionCompleted"

pattern ChildWorkflowExecutionFailed :: EventType
pattern ChildWorkflowExecutionFailed = EventType' "ChildWorkflowExecutionFailed"

pattern ChildWorkflowExecutionTimedOut :: EventType
pattern ChildWorkflowExecutionTimedOut = EventType' "ChildWorkflowExecutionTimedOut"

pattern ChildWorkflowExecutionCanceled :: EventType
pattern ChildWorkflowExecutionCanceled = EventType' "ChildWorkflowExecutionCanceled"

pattern ChildWorkflowExecutionTerminated :: EventType
pattern ChildWorkflowExecutionTerminated = EventType' "ChildWorkflowExecutionTerminated"

pattern SignalExternalWorkflowExecutionInitiated :: EventType
pattern SignalExternalWorkflowExecutionInitiated = EventType' "SignalExternalWorkflowExecutionInitiated"

pattern SignalExternalWorkflowExecutionFailed :: EventType
pattern SignalExternalWorkflowExecutionFailed = EventType' "SignalExternalWorkflowExecutionFailed"

pattern ExternalWorkflowExecutionSignaled :: EventType
pattern ExternalWorkflowExecutionSignaled = EventType' "ExternalWorkflowExecutionSignaled"

pattern RequestCancelExternalWorkflowExecutionInitiated :: EventType
pattern RequestCancelExternalWorkflowExecutionInitiated = EventType' "RequestCancelExternalWorkflowExecutionInitiated"

pattern RequestCancelExternalWorkflowExecutionFailed :: EventType
pattern RequestCancelExternalWorkflowExecutionFailed = EventType' "RequestCancelExternalWorkflowExecutionFailed"

pattern ExternalWorkflowExecutionCancelRequested :: EventType
pattern ExternalWorkflowExecutionCancelRequested = EventType' "ExternalWorkflowExecutionCancelRequested"

pattern LambdaFunctionScheduled :: EventType
pattern LambdaFunctionScheduled = EventType' "LambdaFunctionScheduled"

pattern LambdaFunctionStarted :: EventType
pattern LambdaFunctionStarted = EventType' "LambdaFunctionStarted"

pattern LambdaFunctionCompleted :: EventType
pattern LambdaFunctionCompleted = EventType' "LambdaFunctionCompleted"

pattern LambdaFunctionFailed :: EventType
pattern LambdaFunctionFailed = EventType' "LambdaFunctionFailed"

pattern LambdaFunctionTimedOut :: EventType
pattern LambdaFunctionTimedOut = EventType' "LambdaFunctionTimedOut"

pattern ScheduleLambdaFunctionFailed :: EventType
pattern ScheduleLambdaFunctionFailed = EventType' "ScheduleLambdaFunctionFailed"

pattern StartLambdaFunctionFailed :: EventType
pattern StartLambdaFunctionFailed = EventType' "StartLambdaFunctionFailed"

{-# COMPLETE
  WorkflowExecutionStarted,
  WorkflowExecutionCancelRequested,
  WorkflowExecutionCompleted,
  CompleteWorkflowExecutionFailed,
  WorkflowExecutionFailed,
  FailWorkflowExecutionFailed,
  WorkflowExecutionTimedOut,
  WorkflowExecutionCanceled,
  CancelWorkflowExecutionFailed,
  WorkflowExecutionContinuedAsNew,
  ContinueAsNewWorkflowExecutionFailed,
  WorkflowExecutionTerminated,
  DecisionTaskScheduled,
  DecisionTaskStarted,
  DecisionTaskCompleted,
  DecisionTaskTimedOut,
  ActivityTaskScheduled,
  ScheduleActivityTaskFailed,
  ActivityTaskStarted,
  ActivityTaskCompleted,
  ActivityTaskFailed,
  ActivityTaskTimedOut,
  ActivityTaskCanceled,
  ActivityTaskCancelRequested,
  RequestCancelActivityTaskFailed,
  WorkflowExecutionSignaled,
  MarkerRecorded,
  RecordMarkerFailed,
  TimerStarted,
  StartTimerFailed,
  TimerFired,
  TimerCanceled,
  CancelTimerFailed,
  StartChildWorkflowExecutionInitiated,
  StartChildWorkflowExecutionFailed,
  ChildWorkflowExecutionStarted,
  ChildWorkflowExecutionCompleted,
  ChildWorkflowExecutionFailed,
  ChildWorkflowExecutionTimedOut,
  ChildWorkflowExecutionCanceled,
  ChildWorkflowExecutionTerminated,
  SignalExternalWorkflowExecutionInitiated,
  SignalExternalWorkflowExecutionFailed,
  ExternalWorkflowExecutionSignaled,
  RequestCancelExternalWorkflowExecutionInitiated,
  RequestCancelExternalWorkflowExecutionFailed,
  ExternalWorkflowExecutionCancelRequested,
  LambdaFunctionScheduled,
  LambdaFunctionStarted,
  LambdaFunctionCompleted,
  LambdaFunctionFailed,
  LambdaFunctionTimedOut,
  ScheduleLambdaFunctionFailed,
  StartLambdaFunctionFailed,
  EventType'
  #-}
