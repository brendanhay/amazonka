{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.EventType
  ( EventType
    ( EventType'
    , EventTypeWorkflowExecutionStarted
    , EventTypeWorkflowExecutionCancelRequested
    , EventTypeWorkflowExecutionCompleted
    , EventTypeCompleteWorkflowExecutionFailed
    , EventTypeWorkflowExecutionFailed
    , EventTypeFailWorkflowExecutionFailed
    , EventTypeWorkflowExecutionTimedOut
    , EventTypeWorkflowExecutionCanceled
    , EventTypeCancelWorkflowExecutionFailed
    , EventTypeWorkflowExecutionContinuedAsNew
    , EventTypeContinueAsNewWorkflowExecutionFailed
    , EventTypeWorkflowExecutionTerminated
    , EventTypeDecisionTaskScheduled
    , EventTypeDecisionTaskStarted
    , EventTypeDecisionTaskCompleted
    , EventTypeDecisionTaskTimedOut
    , EventTypeActivityTaskScheduled
    , EventTypeScheduleActivityTaskFailed
    , EventTypeActivityTaskStarted
    , EventTypeActivityTaskCompleted
    , EventTypeActivityTaskFailed
    , EventTypeActivityTaskTimedOut
    , EventTypeActivityTaskCanceled
    , EventTypeActivityTaskCancelRequested
    , EventTypeRequestCancelActivityTaskFailed
    , EventTypeWorkflowExecutionSignaled
    , EventTypeMarkerRecorded
    , EventTypeRecordMarkerFailed
    , EventTypeTimerStarted
    , EventTypeStartTimerFailed
    , EventTypeTimerFired
    , EventTypeTimerCanceled
    , EventTypeCancelTimerFailed
    , EventTypeStartChildWorkflowExecutionInitiated
    , EventTypeStartChildWorkflowExecutionFailed
    , EventTypeChildWorkflowExecutionStarted
    , EventTypeChildWorkflowExecutionCompleted
    , EventTypeChildWorkflowExecutionFailed
    , EventTypeChildWorkflowExecutionTimedOut
    , EventTypeChildWorkflowExecutionCanceled
    , EventTypeChildWorkflowExecutionTerminated
    , EventTypeSignalExternalWorkflowExecutionInitiated
    , EventTypeSignalExternalWorkflowExecutionFailed
    , EventTypeExternalWorkflowExecutionSignaled
    , EventTypeRequestCancelExternalWorkflowExecutionInitiated
    , EventTypeRequestCancelExternalWorkflowExecutionFailed
    , EventTypeExternalWorkflowExecutionCancelRequested
    , EventTypeLambdaFunctionScheduled
    , EventTypeLambdaFunctionStarted
    , EventTypeLambdaFunctionCompleted
    , EventTypeLambdaFunctionFailed
    , EventTypeLambdaFunctionTimedOut
    , EventTypeScheduleLambdaFunctionFailed
    , EventTypeStartLambdaFunctionFailed
    , fromEventType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EventType = EventType'{fromEventType :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern EventTypeWorkflowExecutionStarted :: EventType
pattern EventTypeWorkflowExecutionStarted = EventType' "WorkflowExecutionStarted"

pattern EventTypeWorkflowExecutionCancelRequested :: EventType
pattern EventTypeWorkflowExecutionCancelRequested = EventType' "WorkflowExecutionCancelRequested"

pattern EventTypeWorkflowExecutionCompleted :: EventType
pattern EventTypeWorkflowExecutionCompleted = EventType' "WorkflowExecutionCompleted"

pattern EventTypeCompleteWorkflowExecutionFailed :: EventType
pattern EventTypeCompleteWorkflowExecutionFailed = EventType' "CompleteWorkflowExecutionFailed"

pattern EventTypeWorkflowExecutionFailed :: EventType
pattern EventTypeWorkflowExecutionFailed = EventType' "WorkflowExecutionFailed"

pattern EventTypeFailWorkflowExecutionFailed :: EventType
pattern EventTypeFailWorkflowExecutionFailed = EventType' "FailWorkflowExecutionFailed"

pattern EventTypeWorkflowExecutionTimedOut :: EventType
pattern EventTypeWorkflowExecutionTimedOut = EventType' "WorkflowExecutionTimedOut"

pattern EventTypeWorkflowExecutionCanceled :: EventType
pattern EventTypeWorkflowExecutionCanceled = EventType' "WorkflowExecutionCanceled"

pattern EventTypeCancelWorkflowExecutionFailed :: EventType
pattern EventTypeCancelWorkflowExecutionFailed = EventType' "CancelWorkflowExecutionFailed"

pattern EventTypeWorkflowExecutionContinuedAsNew :: EventType
pattern EventTypeWorkflowExecutionContinuedAsNew = EventType' "WorkflowExecutionContinuedAsNew"

pattern EventTypeContinueAsNewWorkflowExecutionFailed :: EventType
pattern EventTypeContinueAsNewWorkflowExecutionFailed = EventType' "ContinueAsNewWorkflowExecutionFailed"

pattern EventTypeWorkflowExecutionTerminated :: EventType
pattern EventTypeWorkflowExecutionTerminated = EventType' "WorkflowExecutionTerminated"

pattern EventTypeDecisionTaskScheduled :: EventType
pattern EventTypeDecisionTaskScheduled = EventType' "DecisionTaskScheduled"

pattern EventTypeDecisionTaskStarted :: EventType
pattern EventTypeDecisionTaskStarted = EventType' "DecisionTaskStarted"

pattern EventTypeDecisionTaskCompleted :: EventType
pattern EventTypeDecisionTaskCompleted = EventType' "DecisionTaskCompleted"

pattern EventTypeDecisionTaskTimedOut :: EventType
pattern EventTypeDecisionTaskTimedOut = EventType' "DecisionTaskTimedOut"

pattern EventTypeActivityTaskScheduled :: EventType
pattern EventTypeActivityTaskScheduled = EventType' "ActivityTaskScheduled"

pattern EventTypeScheduleActivityTaskFailed :: EventType
pattern EventTypeScheduleActivityTaskFailed = EventType' "ScheduleActivityTaskFailed"

pattern EventTypeActivityTaskStarted :: EventType
pattern EventTypeActivityTaskStarted = EventType' "ActivityTaskStarted"

pattern EventTypeActivityTaskCompleted :: EventType
pattern EventTypeActivityTaskCompleted = EventType' "ActivityTaskCompleted"

pattern EventTypeActivityTaskFailed :: EventType
pattern EventTypeActivityTaskFailed = EventType' "ActivityTaskFailed"

pattern EventTypeActivityTaskTimedOut :: EventType
pattern EventTypeActivityTaskTimedOut = EventType' "ActivityTaskTimedOut"

pattern EventTypeActivityTaskCanceled :: EventType
pattern EventTypeActivityTaskCanceled = EventType' "ActivityTaskCanceled"

pattern EventTypeActivityTaskCancelRequested :: EventType
pattern EventTypeActivityTaskCancelRequested = EventType' "ActivityTaskCancelRequested"

pattern EventTypeRequestCancelActivityTaskFailed :: EventType
pattern EventTypeRequestCancelActivityTaskFailed = EventType' "RequestCancelActivityTaskFailed"

pattern EventTypeWorkflowExecutionSignaled :: EventType
pattern EventTypeWorkflowExecutionSignaled = EventType' "WorkflowExecutionSignaled"

pattern EventTypeMarkerRecorded :: EventType
pattern EventTypeMarkerRecorded = EventType' "MarkerRecorded"

pattern EventTypeRecordMarkerFailed :: EventType
pattern EventTypeRecordMarkerFailed = EventType' "RecordMarkerFailed"

pattern EventTypeTimerStarted :: EventType
pattern EventTypeTimerStarted = EventType' "TimerStarted"

pattern EventTypeStartTimerFailed :: EventType
pattern EventTypeStartTimerFailed = EventType' "StartTimerFailed"

pattern EventTypeTimerFired :: EventType
pattern EventTypeTimerFired = EventType' "TimerFired"

pattern EventTypeTimerCanceled :: EventType
pattern EventTypeTimerCanceled = EventType' "TimerCanceled"

pattern EventTypeCancelTimerFailed :: EventType
pattern EventTypeCancelTimerFailed = EventType' "CancelTimerFailed"

pattern EventTypeStartChildWorkflowExecutionInitiated :: EventType
pattern EventTypeStartChildWorkflowExecutionInitiated = EventType' "StartChildWorkflowExecutionInitiated"

pattern EventTypeStartChildWorkflowExecutionFailed :: EventType
pattern EventTypeStartChildWorkflowExecutionFailed = EventType' "StartChildWorkflowExecutionFailed"

pattern EventTypeChildWorkflowExecutionStarted :: EventType
pattern EventTypeChildWorkflowExecutionStarted = EventType' "ChildWorkflowExecutionStarted"

pattern EventTypeChildWorkflowExecutionCompleted :: EventType
pattern EventTypeChildWorkflowExecutionCompleted = EventType' "ChildWorkflowExecutionCompleted"

pattern EventTypeChildWorkflowExecutionFailed :: EventType
pattern EventTypeChildWorkflowExecutionFailed = EventType' "ChildWorkflowExecutionFailed"

pattern EventTypeChildWorkflowExecutionTimedOut :: EventType
pattern EventTypeChildWorkflowExecutionTimedOut = EventType' "ChildWorkflowExecutionTimedOut"

pattern EventTypeChildWorkflowExecutionCanceled :: EventType
pattern EventTypeChildWorkflowExecutionCanceled = EventType' "ChildWorkflowExecutionCanceled"

pattern EventTypeChildWorkflowExecutionTerminated :: EventType
pattern EventTypeChildWorkflowExecutionTerminated = EventType' "ChildWorkflowExecutionTerminated"

pattern EventTypeSignalExternalWorkflowExecutionInitiated :: EventType
pattern EventTypeSignalExternalWorkflowExecutionInitiated = EventType' "SignalExternalWorkflowExecutionInitiated"

pattern EventTypeSignalExternalWorkflowExecutionFailed :: EventType
pattern EventTypeSignalExternalWorkflowExecutionFailed = EventType' "SignalExternalWorkflowExecutionFailed"

pattern EventTypeExternalWorkflowExecutionSignaled :: EventType
pattern EventTypeExternalWorkflowExecutionSignaled = EventType' "ExternalWorkflowExecutionSignaled"

pattern EventTypeRequestCancelExternalWorkflowExecutionInitiated :: EventType
pattern EventTypeRequestCancelExternalWorkflowExecutionInitiated = EventType' "RequestCancelExternalWorkflowExecutionInitiated"

pattern EventTypeRequestCancelExternalWorkflowExecutionFailed :: EventType
pattern EventTypeRequestCancelExternalWorkflowExecutionFailed = EventType' "RequestCancelExternalWorkflowExecutionFailed"

pattern EventTypeExternalWorkflowExecutionCancelRequested :: EventType
pattern EventTypeExternalWorkflowExecutionCancelRequested = EventType' "ExternalWorkflowExecutionCancelRequested"

pattern EventTypeLambdaFunctionScheduled :: EventType
pattern EventTypeLambdaFunctionScheduled = EventType' "LambdaFunctionScheduled"

pattern EventTypeLambdaFunctionStarted :: EventType
pattern EventTypeLambdaFunctionStarted = EventType' "LambdaFunctionStarted"

pattern EventTypeLambdaFunctionCompleted :: EventType
pattern EventTypeLambdaFunctionCompleted = EventType' "LambdaFunctionCompleted"

pattern EventTypeLambdaFunctionFailed :: EventType
pattern EventTypeLambdaFunctionFailed = EventType' "LambdaFunctionFailed"

pattern EventTypeLambdaFunctionTimedOut :: EventType
pattern EventTypeLambdaFunctionTimedOut = EventType' "LambdaFunctionTimedOut"

pattern EventTypeScheduleLambdaFunctionFailed :: EventType
pattern EventTypeScheduleLambdaFunctionFailed = EventType' "ScheduleLambdaFunctionFailed"

pattern EventTypeStartLambdaFunctionFailed :: EventType
pattern EventTypeStartLambdaFunctionFailed = EventType' "StartLambdaFunctionFailed"

{-# COMPLETE 
  EventTypeWorkflowExecutionStarted,

  EventTypeWorkflowExecutionCancelRequested,

  EventTypeWorkflowExecutionCompleted,

  EventTypeCompleteWorkflowExecutionFailed,

  EventTypeWorkflowExecutionFailed,

  EventTypeFailWorkflowExecutionFailed,

  EventTypeWorkflowExecutionTimedOut,

  EventTypeWorkflowExecutionCanceled,

  EventTypeCancelWorkflowExecutionFailed,

  EventTypeWorkflowExecutionContinuedAsNew,

  EventTypeContinueAsNewWorkflowExecutionFailed,

  EventTypeWorkflowExecutionTerminated,

  EventTypeDecisionTaskScheduled,

  EventTypeDecisionTaskStarted,

  EventTypeDecisionTaskCompleted,

  EventTypeDecisionTaskTimedOut,

  EventTypeActivityTaskScheduled,

  EventTypeScheduleActivityTaskFailed,

  EventTypeActivityTaskStarted,

  EventTypeActivityTaskCompleted,

  EventTypeActivityTaskFailed,

  EventTypeActivityTaskTimedOut,

  EventTypeActivityTaskCanceled,

  EventTypeActivityTaskCancelRequested,

  EventTypeRequestCancelActivityTaskFailed,

  EventTypeWorkflowExecutionSignaled,

  EventTypeMarkerRecorded,

  EventTypeRecordMarkerFailed,

  EventTypeTimerStarted,

  EventTypeStartTimerFailed,

  EventTypeTimerFired,

  EventTypeTimerCanceled,

  EventTypeCancelTimerFailed,

  EventTypeStartChildWorkflowExecutionInitiated,

  EventTypeStartChildWorkflowExecutionFailed,

  EventTypeChildWorkflowExecutionStarted,

  EventTypeChildWorkflowExecutionCompleted,

  EventTypeChildWorkflowExecutionFailed,

  EventTypeChildWorkflowExecutionTimedOut,

  EventTypeChildWorkflowExecutionCanceled,

  EventTypeChildWorkflowExecutionTerminated,

  EventTypeSignalExternalWorkflowExecutionInitiated,

  EventTypeSignalExternalWorkflowExecutionFailed,

  EventTypeExternalWorkflowExecutionSignaled,

  EventTypeRequestCancelExternalWorkflowExecutionInitiated,

  EventTypeRequestCancelExternalWorkflowExecutionFailed,

  EventTypeExternalWorkflowExecutionCancelRequested,

  EventTypeLambdaFunctionScheduled,

  EventTypeLambdaFunctionStarted,

  EventTypeLambdaFunctionCompleted,

  EventTypeLambdaFunctionFailed,

  EventTypeLambdaFunctionTimedOut,

  EventTypeScheduleLambdaFunctionFailed,

  EventTypeStartLambdaFunctionFailed,
  EventType'
  #-}
