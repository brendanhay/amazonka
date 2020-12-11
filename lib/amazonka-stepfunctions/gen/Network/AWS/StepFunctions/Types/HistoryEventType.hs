-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEventType
  ( HistoryEventType
      ( HistoryEventType',
        ActivityFailed,
        ActivityScheduleFailed,
        ActivityScheduled,
        ActivityStarted,
        ActivitySucceeded,
        ActivityTimedOut,
        ChoiceStateEntered,
        ChoiceStateExited,
        ExecutionAborted,
        ExecutionFailed,
        ExecutionStarted,
        ExecutionSucceeded,
        ExecutionTimedOut,
        FailStateEntered,
        LambdaFunctionFailed,
        LambdaFunctionScheduleFailed,
        LambdaFunctionScheduled,
        LambdaFunctionStartFailed,
        LambdaFunctionStarted,
        LambdaFunctionSucceeded,
        LambdaFunctionTimedOut,
        MapIterationAborted,
        MapIterationFailed,
        MapIterationStarted,
        MapIterationSucceeded,
        MapStateAborted,
        MapStateEntered,
        MapStateExited,
        MapStateFailed,
        MapStateStarted,
        MapStateSucceeded,
        ParallelStateAborted,
        ParallelStateEntered,
        ParallelStateExited,
        ParallelStateFailed,
        ParallelStateStarted,
        ParallelStateSucceeded,
        PassStateEntered,
        PassStateExited,
        SucceedStateEntered,
        SucceedStateExited,
        TaskFailed,
        TaskScheduled,
        TaskStartFailed,
        TaskStarted,
        TaskStateAborted,
        TaskStateEntered,
        TaskStateExited,
        TaskSubmitFailed,
        TaskSubmitted,
        TaskSucceeded,
        TaskTimedOut,
        WaitStateAborted,
        WaitStateEntered,
        WaitStateExited
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HistoryEventType = HistoryEventType' Lude.Text
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

pattern ActivityFailed :: HistoryEventType
pattern ActivityFailed = HistoryEventType' "ActivityFailed"

pattern ActivityScheduleFailed :: HistoryEventType
pattern ActivityScheduleFailed = HistoryEventType' "ActivityScheduleFailed"

pattern ActivityScheduled :: HistoryEventType
pattern ActivityScheduled = HistoryEventType' "ActivityScheduled"

pattern ActivityStarted :: HistoryEventType
pattern ActivityStarted = HistoryEventType' "ActivityStarted"

pattern ActivitySucceeded :: HistoryEventType
pattern ActivitySucceeded = HistoryEventType' "ActivitySucceeded"

pattern ActivityTimedOut :: HistoryEventType
pattern ActivityTimedOut = HistoryEventType' "ActivityTimedOut"

pattern ChoiceStateEntered :: HistoryEventType
pattern ChoiceStateEntered = HistoryEventType' "ChoiceStateEntered"

pattern ChoiceStateExited :: HistoryEventType
pattern ChoiceStateExited = HistoryEventType' "ChoiceStateExited"

pattern ExecutionAborted :: HistoryEventType
pattern ExecutionAborted = HistoryEventType' "ExecutionAborted"

pattern ExecutionFailed :: HistoryEventType
pattern ExecutionFailed = HistoryEventType' "ExecutionFailed"

pattern ExecutionStarted :: HistoryEventType
pattern ExecutionStarted = HistoryEventType' "ExecutionStarted"

pattern ExecutionSucceeded :: HistoryEventType
pattern ExecutionSucceeded = HistoryEventType' "ExecutionSucceeded"

pattern ExecutionTimedOut :: HistoryEventType
pattern ExecutionTimedOut = HistoryEventType' "ExecutionTimedOut"

pattern FailStateEntered :: HistoryEventType
pattern FailStateEntered = HistoryEventType' "FailStateEntered"

pattern LambdaFunctionFailed :: HistoryEventType
pattern LambdaFunctionFailed = HistoryEventType' "LambdaFunctionFailed"

pattern LambdaFunctionScheduleFailed :: HistoryEventType
pattern LambdaFunctionScheduleFailed = HistoryEventType' "LambdaFunctionScheduleFailed"

pattern LambdaFunctionScheduled :: HistoryEventType
pattern LambdaFunctionScheduled = HistoryEventType' "LambdaFunctionScheduled"

pattern LambdaFunctionStartFailed :: HistoryEventType
pattern LambdaFunctionStartFailed = HistoryEventType' "LambdaFunctionStartFailed"

pattern LambdaFunctionStarted :: HistoryEventType
pattern LambdaFunctionStarted = HistoryEventType' "LambdaFunctionStarted"

pattern LambdaFunctionSucceeded :: HistoryEventType
pattern LambdaFunctionSucceeded = HistoryEventType' "LambdaFunctionSucceeded"

pattern LambdaFunctionTimedOut :: HistoryEventType
pattern LambdaFunctionTimedOut = HistoryEventType' "LambdaFunctionTimedOut"

pattern MapIterationAborted :: HistoryEventType
pattern MapIterationAborted = HistoryEventType' "MapIterationAborted"

pattern MapIterationFailed :: HistoryEventType
pattern MapIterationFailed = HistoryEventType' "MapIterationFailed"

pattern MapIterationStarted :: HistoryEventType
pattern MapIterationStarted = HistoryEventType' "MapIterationStarted"

pattern MapIterationSucceeded :: HistoryEventType
pattern MapIterationSucceeded = HistoryEventType' "MapIterationSucceeded"

pattern MapStateAborted :: HistoryEventType
pattern MapStateAborted = HistoryEventType' "MapStateAborted"

pattern MapStateEntered :: HistoryEventType
pattern MapStateEntered = HistoryEventType' "MapStateEntered"

pattern MapStateExited :: HistoryEventType
pattern MapStateExited = HistoryEventType' "MapStateExited"

pattern MapStateFailed :: HistoryEventType
pattern MapStateFailed = HistoryEventType' "MapStateFailed"

pattern MapStateStarted :: HistoryEventType
pattern MapStateStarted = HistoryEventType' "MapStateStarted"

pattern MapStateSucceeded :: HistoryEventType
pattern MapStateSucceeded = HistoryEventType' "MapStateSucceeded"

pattern ParallelStateAborted :: HistoryEventType
pattern ParallelStateAborted = HistoryEventType' "ParallelStateAborted"

pattern ParallelStateEntered :: HistoryEventType
pattern ParallelStateEntered = HistoryEventType' "ParallelStateEntered"

pattern ParallelStateExited :: HistoryEventType
pattern ParallelStateExited = HistoryEventType' "ParallelStateExited"

pattern ParallelStateFailed :: HistoryEventType
pattern ParallelStateFailed = HistoryEventType' "ParallelStateFailed"

pattern ParallelStateStarted :: HistoryEventType
pattern ParallelStateStarted = HistoryEventType' "ParallelStateStarted"

pattern ParallelStateSucceeded :: HistoryEventType
pattern ParallelStateSucceeded = HistoryEventType' "ParallelStateSucceeded"

pattern PassStateEntered :: HistoryEventType
pattern PassStateEntered = HistoryEventType' "PassStateEntered"

pattern PassStateExited :: HistoryEventType
pattern PassStateExited = HistoryEventType' "PassStateExited"

pattern SucceedStateEntered :: HistoryEventType
pattern SucceedStateEntered = HistoryEventType' "SucceedStateEntered"

pattern SucceedStateExited :: HistoryEventType
pattern SucceedStateExited = HistoryEventType' "SucceedStateExited"

pattern TaskFailed :: HistoryEventType
pattern TaskFailed = HistoryEventType' "TaskFailed"

pattern TaskScheduled :: HistoryEventType
pattern TaskScheduled = HistoryEventType' "TaskScheduled"

pattern TaskStartFailed :: HistoryEventType
pattern TaskStartFailed = HistoryEventType' "TaskStartFailed"

pattern TaskStarted :: HistoryEventType
pattern TaskStarted = HistoryEventType' "TaskStarted"

pattern TaskStateAborted :: HistoryEventType
pattern TaskStateAborted = HistoryEventType' "TaskStateAborted"

pattern TaskStateEntered :: HistoryEventType
pattern TaskStateEntered = HistoryEventType' "TaskStateEntered"

pattern TaskStateExited :: HistoryEventType
pattern TaskStateExited = HistoryEventType' "TaskStateExited"

pattern TaskSubmitFailed :: HistoryEventType
pattern TaskSubmitFailed = HistoryEventType' "TaskSubmitFailed"

pattern TaskSubmitted :: HistoryEventType
pattern TaskSubmitted = HistoryEventType' "TaskSubmitted"

pattern TaskSucceeded :: HistoryEventType
pattern TaskSucceeded = HistoryEventType' "TaskSucceeded"

pattern TaskTimedOut :: HistoryEventType
pattern TaskTimedOut = HistoryEventType' "TaskTimedOut"

pattern WaitStateAborted :: HistoryEventType
pattern WaitStateAborted = HistoryEventType' "WaitStateAborted"

pattern WaitStateEntered :: HistoryEventType
pattern WaitStateEntered = HistoryEventType' "WaitStateEntered"

pattern WaitStateExited :: HistoryEventType
pattern WaitStateExited = HistoryEventType' "WaitStateExited"

{-# COMPLETE
  ActivityFailed,
  ActivityScheduleFailed,
  ActivityScheduled,
  ActivityStarted,
  ActivitySucceeded,
  ActivityTimedOut,
  ChoiceStateEntered,
  ChoiceStateExited,
  ExecutionAborted,
  ExecutionFailed,
  ExecutionStarted,
  ExecutionSucceeded,
  ExecutionTimedOut,
  FailStateEntered,
  LambdaFunctionFailed,
  LambdaFunctionScheduleFailed,
  LambdaFunctionScheduled,
  LambdaFunctionStartFailed,
  LambdaFunctionStarted,
  LambdaFunctionSucceeded,
  LambdaFunctionTimedOut,
  MapIterationAborted,
  MapIterationFailed,
  MapIterationStarted,
  MapIterationSucceeded,
  MapStateAborted,
  MapStateEntered,
  MapStateExited,
  MapStateFailed,
  MapStateStarted,
  MapStateSucceeded,
  ParallelStateAborted,
  ParallelStateEntered,
  ParallelStateExited,
  ParallelStateFailed,
  ParallelStateStarted,
  ParallelStateSucceeded,
  PassStateEntered,
  PassStateExited,
  SucceedStateEntered,
  SucceedStateExited,
  TaskFailed,
  TaskScheduled,
  TaskStartFailed,
  TaskStarted,
  TaskStateAborted,
  TaskStateEntered,
  TaskStateExited,
  TaskSubmitFailed,
  TaskSubmitted,
  TaskSucceeded,
  TaskTimedOut,
  WaitStateAborted,
  WaitStateEntered,
  WaitStateExited,
  HistoryEventType'
  #-}
