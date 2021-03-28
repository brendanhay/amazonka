{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.HistoryEventType
  ( HistoryEventType
    ( HistoryEventType'
    , HistoryEventTypeActivityFailed
    , HistoryEventTypeActivityScheduled
    , HistoryEventTypeActivityScheduleFailed
    , HistoryEventTypeActivityStarted
    , HistoryEventTypeActivitySucceeded
    , HistoryEventTypeActivityTimedOut
    , HistoryEventTypeChoiceStateEntered
    , HistoryEventTypeChoiceStateExited
    , HistoryEventTypeExecutionAborted
    , HistoryEventTypeExecutionFailed
    , HistoryEventTypeExecutionStarted
    , HistoryEventTypeExecutionSucceeded
    , HistoryEventTypeExecutionTimedOut
    , HistoryEventTypeFailStateEntered
    , HistoryEventTypeLambdaFunctionFailed
    , HistoryEventTypeLambdaFunctionScheduled
    , HistoryEventTypeLambdaFunctionScheduleFailed
    , HistoryEventTypeLambdaFunctionStarted
    , HistoryEventTypeLambdaFunctionStartFailed
    , HistoryEventTypeLambdaFunctionSucceeded
    , HistoryEventTypeLambdaFunctionTimedOut
    , HistoryEventTypeMapIterationAborted
    , HistoryEventTypeMapIterationFailed
    , HistoryEventTypeMapIterationStarted
    , HistoryEventTypeMapIterationSucceeded
    , HistoryEventTypeMapStateAborted
    , HistoryEventTypeMapStateEntered
    , HistoryEventTypeMapStateExited
    , HistoryEventTypeMapStateFailed
    , HistoryEventTypeMapStateStarted
    , HistoryEventTypeMapStateSucceeded
    , HistoryEventTypeParallelStateAborted
    , HistoryEventTypeParallelStateEntered
    , HistoryEventTypeParallelStateExited
    , HistoryEventTypeParallelStateFailed
    , HistoryEventTypeParallelStateStarted
    , HistoryEventTypeParallelStateSucceeded
    , HistoryEventTypePassStateEntered
    , HistoryEventTypePassStateExited
    , HistoryEventTypeSucceedStateEntered
    , HistoryEventTypeSucceedStateExited
    , HistoryEventTypeTaskFailed
    , HistoryEventTypeTaskScheduled
    , HistoryEventTypeTaskStarted
    , HistoryEventTypeTaskStartFailed
    , HistoryEventTypeTaskStateAborted
    , HistoryEventTypeTaskStateEntered
    , HistoryEventTypeTaskStateExited
    , HistoryEventTypeTaskSubmitFailed
    , HistoryEventTypeTaskSubmitted
    , HistoryEventTypeTaskSucceeded
    , HistoryEventTypeTaskTimedOut
    , HistoryEventTypeWaitStateAborted
    , HistoryEventTypeWaitStateEntered
    , HistoryEventTypeWaitStateExited
    , fromHistoryEventType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype HistoryEventType = HistoryEventType'{fromHistoryEventType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern HistoryEventTypeActivityFailed :: HistoryEventType
pattern HistoryEventTypeActivityFailed = HistoryEventType' "ActivityFailed"

pattern HistoryEventTypeActivityScheduled :: HistoryEventType
pattern HistoryEventTypeActivityScheduled = HistoryEventType' "ActivityScheduled"

pattern HistoryEventTypeActivityScheduleFailed :: HistoryEventType
pattern HistoryEventTypeActivityScheduleFailed = HistoryEventType' "ActivityScheduleFailed"

pattern HistoryEventTypeActivityStarted :: HistoryEventType
pattern HistoryEventTypeActivityStarted = HistoryEventType' "ActivityStarted"

pattern HistoryEventTypeActivitySucceeded :: HistoryEventType
pattern HistoryEventTypeActivitySucceeded = HistoryEventType' "ActivitySucceeded"

pattern HistoryEventTypeActivityTimedOut :: HistoryEventType
pattern HistoryEventTypeActivityTimedOut = HistoryEventType' "ActivityTimedOut"

pattern HistoryEventTypeChoiceStateEntered :: HistoryEventType
pattern HistoryEventTypeChoiceStateEntered = HistoryEventType' "ChoiceStateEntered"

pattern HistoryEventTypeChoiceStateExited :: HistoryEventType
pattern HistoryEventTypeChoiceStateExited = HistoryEventType' "ChoiceStateExited"

pattern HistoryEventTypeExecutionAborted :: HistoryEventType
pattern HistoryEventTypeExecutionAborted = HistoryEventType' "ExecutionAborted"

pattern HistoryEventTypeExecutionFailed :: HistoryEventType
pattern HistoryEventTypeExecutionFailed = HistoryEventType' "ExecutionFailed"

pattern HistoryEventTypeExecutionStarted :: HistoryEventType
pattern HistoryEventTypeExecutionStarted = HistoryEventType' "ExecutionStarted"

pattern HistoryEventTypeExecutionSucceeded :: HistoryEventType
pattern HistoryEventTypeExecutionSucceeded = HistoryEventType' "ExecutionSucceeded"

pattern HistoryEventTypeExecutionTimedOut :: HistoryEventType
pattern HistoryEventTypeExecutionTimedOut = HistoryEventType' "ExecutionTimedOut"

pattern HistoryEventTypeFailStateEntered :: HistoryEventType
pattern HistoryEventTypeFailStateEntered = HistoryEventType' "FailStateEntered"

pattern HistoryEventTypeLambdaFunctionFailed :: HistoryEventType
pattern HistoryEventTypeLambdaFunctionFailed = HistoryEventType' "LambdaFunctionFailed"

pattern HistoryEventTypeLambdaFunctionScheduled :: HistoryEventType
pattern HistoryEventTypeLambdaFunctionScheduled = HistoryEventType' "LambdaFunctionScheduled"

pattern HistoryEventTypeLambdaFunctionScheduleFailed :: HistoryEventType
pattern HistoryEventTypeLambdaFunctionScheduleFailed = HistoryEventType' "LambdaFunctionScheduleFailed"

pattern HistoryEventTypeLambdaFunctionStarted :: HistoryEventType
pattern HistoryEventTypeLambdaFunctionStarted = HistoryEventType' "LambdaFunctionStarted"

pattern HistoryEventTypeLambdaFunctionStartFailed :: HistoryEventType
pattern HistoryEventTypeLambdaFunctionStartFailed = HistoryEventType' "LambdaFunctionStartFailed"

pattern HistoryEventTypeLambdaFunctionSucceeded :: HistoryEventType
pattern HistoryEventTypeLambdaFunctionSucceeded = HistoryEventType' "LambdaFunctionSucceeded"

pattern HistoryEventTypeLambdaFunctionTimedOut :: HistoryEventType
pattern HistoryEventTypeLambdaFunctionTimedOut = HistoryEventType' "LambdaFunctionTimedOut"

pattern HistoryEventTypeMapIterationAborted :: HistoryEventType
pattern HistoryEventTypeMapIterationAborted = HistoryEventType' "MapIterationAborted"

pattern HistoryEventTypeMapIterationFailed :: HistoryEventType
pattern HistoryEventTypeMapIterationFailed = HistoryEventType' "MapIterationFailed"

pattern HistoryEventTypeMapIterationStarted :: HistoryEventType
pattern HistoryEventTypeMapIterationStarted = HistoryEventType' "MapIterationStarted"

pattern HistoryEventTypeMapIterationSucceeded :: HistoryEventType
pattern HistoryEventTypeMapIterationSucceeded = HistoryEventType' "MapIterationSucceeded"

pattern HistoryEventTypeMapStateAborted :: HistoryEventType
pattern HistoryEventTypeMapStateAborted = HistoryEventType' "MapStateAborted"

pattern HistoryEventTypeMapStateEntered :: HistoryEventType
pattern HistoryEventTypeMapStateEntered = HistoryEventType' "MapStateEntered"

pattern HistoryEventTypeMapStateExited :: HistoryEventType
pattern HistoryEventTypeMapStateExited = HistoryEventType' "MapStateExited"

pattern HistoryEventTypeMapStateFailed :: HistoryEventType
pattern HistoryEventTypeMapStateFailed = HistoryEventType' "MapStateFailed"

pattern HistoryEventTypeMapStateStarted :: HistoryEventType
pattern HistoryEventTypeMapStateStarted = HistoryEventType' "MapStateStarted"

pattern HistoryEventTypeMapStateSucceeded :: HistoryEventType
pattern HistoryEventTypeMapStateSucceeded = HistoryEventType' "MapStateSucceeded"

pattern HistoryEventTypeParallelStateAborted :: HistoryEventType
pattern HistoryEventTypeParallelStateAborted = HistoryEventType' "ParallelStateAborted"

pattern HistoryEventTypeParallelStateEntered :: HistoryEventType
pattern HistoryEventTypeParallelStateEntered = HistoryEventType' "ParallelStateEntered"

pattern HistoryEventTypeParallelStateExited :: HistoryEventType
pattern HistoryEventTypeParallelStateExited = HistoryEventType' "ParallelStateExited"

pattern HistoryEventTypeParallelStateFailed :: HistoryEventType
pattern HistoryEventTypeParallelStateFailed = HistoryEventType' "ParallelStateFailed"

pattern HistoryEventTypeParallelStateStarted :: HistoryEventType
pattern HistoryEventTypeParallelStateStarted = HistoryEventType' "ParallelStateStarted"

pattern HistoryEventTypeParallelStateSucceeded :: HistoryEventType
pattern HistoryEventTypeParallelStateSucceeded = HistoryEventType' "ParallelStateSucceeded"

pattern HistoryEventTypePassStateEntered :: HistoryEventType
pattern HistoryEventTypePassStateEntered = HistoryEventType' "PassStateEntered"

pattern HistoryEventTypePassStateExited :: HistoryEventType
pattern HistoryEventTypePassStateExited = HistoryEventType' "PassStateExited"

pattern HistoryEventTypeSucceedStateEntered :: HistoryEventType
pattern HistoryEventTypeSucceedStateEntered = HistoryEventType' "SucceedStateEntered"

pattern HistoryEventTypeSucceedStateExited :: HistoryEventType
pattern HistoryEventTypeSucceedStateExited = HistoryEventType' "SucceedStateExited"

pattern HistoryEventTypeTaskFailed :: HistoryEventType
pattern HistoryEventTypeTaskFailed = HistoryEventType' "TaskFailed"

pattern HistoryEventTypeTaskScheduled :: HistoryEventType
pattern HistoryEventTypeTaskScheduled = HistoryEventType' "TaskScheduled"

pattern HistoryEventTypeTaskStarted :: HistoryEventType
pattern HistoryEventTypeTaskStarted = HistoryEventType' "TaskStarted"

pattern HistoryEventTypeTaskStartFailed :: HistoryEventType
pattern HistoryEventTypeTaskStartFailed = HistoryEventType' "TaskStartFailed"

pattern HistoryEventTypeTaskStateAborted :: HistoryEventType
pattern HistoryEventTypeTaskStateAborted = HistoryEventType' "TaskStateAborted"

pattern HistoryEventTypeTaskStateEntered :: HistoryEventType
pattern HistoryEventTypeTaskStateEntered = HistoryEventType' "TaskStateEntered"

pattern HistoryEventTypeTaskStateExited :: HistoryEventType
pattern HistoryEventTypeTaskStateExited = HistoryEventType' "TaskStateExited"

pattern HistoryEventTypeTaskSubmitFailed :: HistoryEventType
pattern HistoryEventTypeTaskSubmitFailed = HistoryEventType' "TaskSubmitFailed"

pattern HistoryEventTypeTaskSubmitted :: HistoryEventType
pattern HistoryEventTypeTaskSubmitted = HistoryEventType' "TaskSubmitted"

pattern HistoryEventTypeTaskSucceeded :: HistoryEventType
pattern HistoryEventTypeTaskSucceeded = HistoryEventType' "TaskSucceeded"

pattern HistoryEventTypeTaskTimedOut :: HistoryEventType
pattern HistoryEventTypeTaskTimedOut = HistoryEventType' "TaskTimedOut"

pattern HistoryEventTypeWaitStateAborted :: HistoryEventType
pattern HistoryEventTypeWaitStateAborted = HistoryEventType' "WaitStateAborted"

pattern HistoryEventTypeWaitStateEntered :: HistoryEventType
pattern HistoryEventTypeWaitStateEntered = HistoryEventType' "WaitStateEntered"

pattern HistoryEventTypeWaitStateExited :: HistoryEventType
pattern HistoryEventTypeWaitStateExited = HistoryEventType' "WaitStateExited"

{-# COMPLETE 
  HistoryEventTypeActivityFailed,

  HistoryEventTypeActivityScheduled,

  HistoryEventTypeActivityScheduleFailed,

  HistoryEventTypeActivityStarted,

  HistoryEventTypeActivitySucceeded,

  HistoryEventTypeActivityTimedOut,

  HistoryEventTypeChoiceStateEntered,

  HistoryEventTypeChoiceStateExited,

  HistoryEventTypeExecutionAborted,

  HistoryEventTypeExecutionFailed,

  HistoryEventTypeExecutionStarted,

  HistoryEventTypeExecutionSucceeded,

  HistoryEventTypeExecutionTimedOut,

  HistoryEventTypeFailStateEntered,

  HistoryEventTypeLambdaFunctionFailed,

  HistoryEventTypeLambdaFunctionScheduled,

  HistoryEventTypeLambdaFunctionScheduleFailed,

  HistoryEventTypeLambdaFunctionStarted,

  HistoryEventTypeLambdaFunctionStartFailed,

  HistoryEventTypeLambdaFunctionSucceeded,

  HistoryEventTypeLambdaFunctionTimedOut,

  HistoryEventTypeMapIterationAborted,

  HistoryEventTypeMapIterationFailed,

  HistoryEventTypeMapIterationStarted,

  HistoryEventTypeMapIterationSucceeded,

  HistoryEventTypeMapStateAborted,

  HistoryEventTypeMapStateEntered,

  HistoryEventTypeMapStateExited,

  HistoryEventTypeMapStateFailed,

  HistoryEventTypeMapStateStarted,

  HistoryEventTypeMapStateSucceeded,

  HistoryEventTypeParallelStateAborted,

  HistoryEventTypeParallelStateEntered,

  HistoryEventTypeParallelStateExited,

  HistoryEventTypeParallelStateFailed,

  HistoryEventTypeParallelStateStarted,

  HistoryEventTypeParallelStateSucceeded,

  HistoryEventTypePassStateEntered,

  HistoryEventTypePassStateExited,

  HistoryEventTypeSucceedStateEntered,

  HistoryEventTypeSucceedStateExited,

  HistoryEventTypeTaskFailed,

  HistoryEventTypeTaskScheduled,

  HistoryEventTypeTaskStarted,

  HistoryEventTypeTaskStartFailed,

  HistoryEventTypeTaskStateAborted,

  HistoryEventTypeTaskStateEntered,

  HistoryEventTypeTaskStateExited,

  HistoryEventTypeTaskSubmitFailed,

  HistoryEventTypeTaskSubmitted,

  HistoryEventTypeTaskSucceeded,

  HistoryEventTypeTaskTimedOut,

  HistoryEventTypeWaitStateAborted,

  HistoryEventTypeWaitStateEntered,

  HistoryEventTypeWaitStateExited,
  HistoryEventType'
  #-}
