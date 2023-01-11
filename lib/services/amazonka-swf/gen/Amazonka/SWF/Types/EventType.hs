{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SWF.Types.EventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.EventType
  ( EventType
      ( ..,
        EventType_ActivityTaskCancelRequested,
        EventType_ActivityTaskCanceled,
        EventType_ActivityTaskCompleted,
        EventType_ActivityTaskFailed,
        EventType_ActivityTaskScheduled,
        EventType_ActivityTaskStarted,
        EventType_ActivityTaskTimedOut,
        EventType_CancelTimerFailed,
        EventType_CancelWorkflowExecutionFailed,
        EventType_ChildWorkflowExecutionCanceled,
        EventType_ChildWorkflowExecutionCompleted,
        EventType_ChildWorkflowExecutionFailed,
        EventType_ChildWorkflowExecutionStarted,
        EventType_ChildWorkflowExecutionTerminated,
        EventType_ChildWorkflowExecutionTimedOut,
        EventType_CompleteWorkflowExecutionFailed,
        EventType_ContinueAsNewWorkflowExecutionFailed,
        EventType_DecisionTaskCompleted,
        EventType_DecisionTaskScheduled,
        EventType_DecisionTaskStarted,
        EventType_DecisionTaskTimedOut,
        EventType_ExternalWorkflowExecutionCancelRequested,
        EventType_ExternalWorkflowExecutionSignaled,
        EventType_FailWorkflowExecutionFailed,
        EventType_LambdaFunctionCompleted,
        EventType_LambdaFunctionFailed,
        EventType_LambdaFunctionScheduled,
        EventType_LambdaFunctionStarted,
        EventType_LambdaFunctionTimedOut,
        EventType_MarkerRecorded,
        EventType_RecordMarkerFailed,
        EventType_RequestCancelActivityTaskFailed,
        EventType_RequestCancelExternalWorkflowExecutionFailed,
        EventType_RequestCancelExternalWorkflowExecutionInitiated,
        EventType_ScheduleActivityTaskFailed,
        EventType_ScheduleLambdaFunctionFailed,
        EventType_SignalExternalWorkflowExecutionFailed,
        EventType_SignalExternalWorkflowExecutionInitiated,
        EventType_StartChildWorkflowExecutionFailed,
        EventType_StartChildWorkflowExecutionInitiated,
        EventType_StartLambdaFunctionFailed,
        EventType_StartTimerFailed,
        EventType_TimerCanceled,
        EventType_TimerFired,
        EventType_TimerStarted,
        EventType_WorkflowExecutionCancelRequested,
        EventType_WorkflowExecutionCanceled,
        EventType_WorkflowExecutionCompleted,
        EventType_WorkflowExecutionContinuedAsNew,
        EventType_WorkflowExecutionFailed,
        EventType_WorkflowExecutionSignaled,
        EventType_WorkflowExecutionStarted,
        EventType_WorkflowExecutionTerminated,
        EventType_WorkflowExecutionTimedOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventType = EventType'
  { fromEventType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern EventType_ActivityTaskCancelRequested :: EventType
pattern EventType_ActivityTaskCancelRequested = EventType' "ActivityTaskCancelRequested"

pattern EventType_ActivityTaskCanceled :: EventType
pattern EventType_ActivityTaskCanceled = EventType' "ActivityTaskCanceled"

pattern EventType_ActivityTaskCompleted :: EventType
pattern EventType_ActivityTaskCompleted = EventType' "ActivityTaskCompleted"

pattern EventType_ActivityTaskFailed :: EventType
pattern EventType_ActivityTaskFailed = EventType' "ActivityTaskFailed"

pattern EventType_ActivityTaskScheduled :: EventType
pattern EventType_ActivityTaskScheduled = EventType' "ActivityTaskScheduled"

pattern EventType_ActivityTaskStarted :: EventType
pattern EventType_ActivityTaskStarted = EventType' "ActivityTaskStarted"

pattern EventType_ActivityTaskTimedOut :: EventType
pattern EventType_ActivityTaskTimedOut = EventType' "ActivityTaskTimedOut"

pattern EventType_CancelTimerFailed :: EventType
pattern EventType_CancelTimerFailed = EventType' "CancelTimerFailed"

pattern EventType_CancelWorkflowExecutionFailed :: EventType
pattern EventType_CancelWorkflowExecutionFailed = EventType' "CancelWorkflowExecutionFailed"

pattern EventType_ChildWorkflowExecutionCanceled :: EventType
pattern EventType_ChildWorkflowExecutionCanceled = EventType' "ChildWorkflowExecutionCanceled"

pattern EventType_ChildWorkflowExecutionCompleted :: EventType
pattern EventType_ChildWorkflowExecutionCompleted = EventType' "ChildWorkflowExecutionCompleted"

pattern EventType_ChildWorkflowExecutionFailed :: EventType
pattern EventType_ChildWorkflowExecutionFailed = EventType' "ChildWorkflowExecutionFailed"

pattern EventType_ChildWorkflowExecutionStarted :: EventType
pattern EventType_ChildWorkflowExecutionStarted = EventType' "ChildWorkflowExecutionStarted"

pattern EventType_ChildWorkflowExecutionTerminated :: EventType
pattern EventType_ChildWorkflowExecutionTerminated = EventType' "ChildWorkflowExecutionTerminated"

pattern EventType_ChildWorkflowExecutionTimedOut :: EventType
pattern EventType_ChildWorkflowExecutionTimedOut = EventType' "ChildWorkflowExecutionTimedOut"

pattern EventType_CompleteWorkflowExecutionFailed :: EventType
pattern EventType_CompleteWorkflowExecutionFailed = EventType' "CompleteWorkflowExecutionFailed"

pattern EventType_ContinueAsNewWorkflowExecutionFailed :: EventType
pattern EventType_ContinueAsNewWorkflowExecutionFailed = EventType' "ContinueAsNewWorkflowExecutionFailed"

pattern EventType_DecisionTaskCompleted :: EventType
pattern EventType_DecisionTaskCompleted = EventType' "DecisionTaskCompleted"

pattern EventType_DecisionTaskScheduled :: EventType
pattern EventType_DecisionTaskScheduled = EventType' "DecisionTaskScheduled"

pattern EventType_DecisionTaskStarted :: EventType
pattern EventType_DecisionTaskStarted = EventType' "DecisionTaskStarted"

pattern EventType_DecisionTaskTimedOut :: EventType
pattern EventType_DecisionTaskTimedOut = EventType' "DecisionTaskTimedOut"

pattern EventType_ExternalWorkflowExecutionCancelRequested :: EventType
pattern EventType_ExternalWorkflowExecutionCancelRequested = EventType' "ExternalWorkflowExecutionCancelRequested"

pattern EventType_ExternalWorkflowExecutionSignaled :: EventType
pattern EventType_ExternalWorkflowExecutionSignaled = EventType' "ExternalWorkflowExecutionSignaled"

pattern EventType_FailWorkflowExecutionFailed :: EventType
pattern EventType_FailWorkflowExecutionFailed = EventType' "FailWorkflowExecutionFailed"

pattern EventType_LambdaFunctionCompleted :: EventType
pattern EventType_LambdaFunctionCompleted = EventType' "LambdaFunctionCompleted"

pattern EventType_LambdaFunctionFailed :: EventType
pattern EventType_LambdaFunctionFailed = EventType' "LambdaFunctionFailed"

pattern EventType_LambdaFunctionScheduled :: EventType
pattern EventType_LambdaFunctionScheduled = EventType' "LambdaFunctionScheduled"

pattern EventType_LambdaFunctionStarted :: EventType
pattern EventType_LambdaFunctionStarted = EventType' "LambdaFunctionStarted"

pattern EventType_LambdaFunctionTimedOut :: EventType
pattern EventType_LambdaFunctionTimedOut = EventType' "LambdaFunctionTimedOut"

pattern EventType_MarkerRecorded :: EventType
pattern EventType_MarkerRecorded = EventType' "MarkerRecorded"

pattern EventType_RecordMarkerFailed :: EventType
pattern EventType_RecordMarkerFailed = EventType' "RecordMarkerFailed"

pattern EventType_RequestCancelActivityTaskFailed :: EventType
pattern EventType_RequestCancelActivityTaskFailed = EventType' "RequestCancelActivityTaskFailed"

pattern EventType_RequestCancelExternalWorkflowExecutionFailed :: EventType
pattern EventType_RequestCancelExternalWorkflowExecutionFailed = EventType' "RequestCancelExternalWorkflowExecutionFailed"

pattern EventType_RequestCancelExternalWorkflowExecutionInitiated :: EventType
pattern EventType_RequestCancelExternalWorkflowExecutionInitiated = EventType' "RequestCancelExternalWorkflowExecutionInitiated"

pattern EventType_ScheduleActivityTaskFailed :: EventType
pattern EventType_ScheduleActivityTaskFailed = EventType' "ScheduleActivityTaskFailed"

pattern EventType_ScheduleLambdaFunctionFailed :: EventType
pattern EventType_ScheduleLambdaFunctionFailed = EventType' "ScheduleLambdaFunctionFailed"

pattern EventType_SignalExternalWorkflowExecutionFailed :: EventType
pattern EventType_SignalExternalWorkflowExecutionFailed = EventType' "SignalExternalWorkflowExecutionFailed"

pattern EventType_SignalExternalWorkflowExecutionInitiated :: EventType
pattern EventType_SignalExternalWorkflowExecutionInitiated = EventType' "SignalExternalWorkflowExecutionInitiated"

pattern EventType_StartChildWorkflowExecutionFailed :: EventType
pattern EventType_StartChildWorkflowExecutionFailed = EventType' "StartChildWorkflowExecutionFailed"

pattern EventType_StartChildWorkflowExecutionInitiated :: EventType
pattern EventType_StartChildWorkflowExecutionInitiated = EventType' "StartChildWorkflowExecutionInitiated"

pattern EventType_StartLambdaFunctionFailed :: EventType
pattern EventType_StartLambdaFunctionFailed = EventType' "StartLambdaFunctionFailed"

pattern EventType_StartTimerFailed :: EventType
pattern EventType_StartTimerFailed = EventType' "StartTimerFailed"

pattern EventType_TimerCanceled :: EventType
pattern EventType_TimerCanceled = EventType' "TimerCanceled"

pattern EventType_TimerFired :: EventType
pattern EventType_TimerFired = EventType' "TimerFired"

pattern EventType_TimerStarted :: EventType
pattern EventType_TimerStarted = EventType' "TimerStarted"

pattern EventType_WorkflowExecutionCancelRequested :: EventType
pattern EventType_WorkflowExecutionCancelRequested = EventType' "WorkflowExecutionCancelRequested"

pattern EventType_WorkflowExecutionCanceled :: EventType
pattern EventType_WorkflowExecutionCanceled = EventType' "WorkflowExecutionCanceled"

pattern EventType_WorkflowExecutionCompleted :: EventType
pattern EventType_WorkflowExecutionCompleted = EventType' "WorkflowExecutionCompleted"

pattern EventType_WorkflowExecutionContinuedAsNew :: EventType
pattern EventType_WorkflowExecutionContinuedAsNew = EventType' "WorkflowExecutionContinuedAsNew"

pattern EventType_WorkflowExecutionFailed :: EventType
pattern EventType_WorkflowExecutionFailed = EventType' "WorkflowExecutionFailed"

pattern EventType_WorkflowExecutionSignaled :: EventType
pattern EventType_WorkflowExecutionSignaled = EventType' "WorkflowExecutionSignaled"

pattern EventType_WorkflowExecutionStarted :: EventType
pattern EventType_WorkflowExecutionStarted = EventType' "WorkflowExecutionStarted"

pattern EventType_WorkflowExecutionTerminated :: EventType
pattern EventType_WorkflowExecutionTerminated = EventType' "WorkflowExecutionTerminated"

pattern EventType_WorkflowExecutionTimedOut :: EventType
pattern EventType_WorkflowExecutionTimedOut = EventType' "WorkflowExecutionTimedOut"

{-# COMPLETE
  EventType_ActivityTaskCancelRequested,
  EventType_ActivityTaskCanceled,
  EventType_ActivityTaskCompleted,
  EventType_ActivityTaskFailed,
  EventType_ActivityTaskScheduled,
  EventType_ActivityTaskStarted,
  EventType_ActivityTaskTimedOut,
  EventType_CancelTimerFailed,
  EventType_CancelWorkflowExecutionFailed,
  EventType_ChildWorkflowExecutionCanceled,
  EventType_ChildWorkflowExecutionCompleted,
  EventType_ChildWorkflowExecutionFailed,
  EventType_ChildWorkflowExecutionStarted,
  EventType_ChildWorkflowExecutionTerminated,
  EventType_ChildWorkflowExecutionTimedOut,
  EventType_CompleteWorkflowExecutionFailed,
  EventType_ContinueAsNewWorkflowExecutionFailed,
  EventType_DecisionTaskCompleted,
  EventType_DecisionTaskScheduled,
  EventType_DecisionTaskStarted,
  EventType_DecisionTaskTimedOut,
  EventType_ExternalWorkflowExecutionCancelRequested,
  EventType_ExternalWorkflowExecutionSignaled,
  EventType_FailWorkflowExecutionFailed,
  EventType_LambdaFunctionCompleted,
  EventType_LambdaFunctionFailed,
  EventType_LambdaFunctionScheduled,
  EventType_LambdaFunctionStarted,
  EventType_LambdaFunctionTimedOut,
  EventType_MarkerRecorded,
  EventType_RecordMarkerFailed,
  EventType_RequestCancelActivityTaskFailed,
  EventType_RequestCancelExternalWorkflowExecutionFailed,
  EventType_RequestCancelExternalWorkflowExecutionInitiated,
  EventType_ScheduleActivityTaskFailed,
  EventType_ScheduleLambdaFunctionFailed,
  EventType_SignalExternalWorkflowExecutionFailed,
  EventType_SignalExternalWorkflowExecutionInitiated,
  EventType_StartChildWorkflowExecutionFailed,
  EventType_StartChildWorkflowExecutionInitiated,
  EventType_StartLambdaFunctionFailed,
  EventType_StartTimerFailed,
  EventType_TimerCanceled,
  EventType_TimerFired,
  EventType_TimerStarted,
  EventType_WorkflowExecutionCancelRequested,
  EventType_WorkflowExecutionCanceled,
  EventType_WorkflowExecutionCompleted,
  EventType_WorkflowExecutionContinuedAsNew,
  EventType_WorkflowExecutionFailed,
  EventType_WorkflowExecutionSignaled,
  EventType_WorkflowExecutionStarted,
  EventType_WorkflowExecutionTerminated,
  EventType_WorkflowExecutionTimedOut,
  EventType'
  #-}
