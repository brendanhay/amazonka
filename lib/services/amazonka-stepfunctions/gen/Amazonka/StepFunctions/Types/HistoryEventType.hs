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
-- Module      : Amazonka.StepFunctions.Types.HistoryEventType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.HistoryEventType
  ( HistoryEventType
      ( ..,
        HistoryEventType_ActivityFailed,
        HistoryEventType_ActivityScheduleFailed,
        HistoryEventType_ActivityScheduled,
        HistoryEventType_ActivityStarted,
        HistoryEventType_ActivitySucceeded,
        HistoryEventType_ActivityTimedOut,
        HistoryEventType_ChoiceStateEntered,
        HistoryEventType_ChoiceStateExited,
        HistoryEventType_ExecutionAborted,
        HistoryEventType_ExecutionFailed,
        HistoryEventType_ExecutionStarted,
        HistoryEventType_ExecutionSucceeded,
        HistoryEventType_ExecutionTimedOut,
        HistoryEventType_FailStateEntered,
        HistoryEventType_LambdaFunctionFailed,
        HistoryEventType_LambdaFunctionScheduleFailed,
        HistoryEventType_LambdaFunctionScheduled,
        HistoryEventType_LambdaFunctionStartFailed,
        HistoryEventType_LambdaFunctionStarted,
        HistoryEventType_LambdaFunctionSucceeded,
        HistoryEventType_LambdaFunctionTimedOut,
        HistoryEventType_MapIterationAborted,
        HistoryEventType_MapIterationFailed,
        HistoryEventType_MapIterationStarted,
        HistoryEventType_MapIterationSucceeded,
        HistoryEventType_MapStateAborted,
        HistoryEventType_MapStateEntered,
        HistoryEventType_MapStateExited,
        HistoryEventType_MapStateFailed,
        HistoryEventType_MapStateStarted,
        HistoryEventType_MapStateSucceeded,
        HistoryEventType_ParallelStateAborted,
        HistoryEventType_ParallelStateEntered,
        HistoryEventType_ParallelStateExited,
        HistoryEventType_ParallelStateFailed,
        HistoryEventType_ParallelStateStarted,
        HistoryEventType_ParallelStateSucceeded,
        HistoryEventType_PassStateEntered,
        HistoryEventType_PassStateExited,
        HistoryEventType_SucceedStateEntered,
        HistoryEventType_SucceedStateExited,
        HistoryEventType_TaskFailed,
        HistoryEventType_TaskScheduled,
        HistoryEventType_TaskStartFailed,
        HistoryEventType_TaskStarted,
        HistoryEventType_TaskStateAborted,
        HistoryEventType_TaskStateEntered,
        HistoryEventType_TaskStateExited,
        HistoryEventType_TaskSubmitFailed,
        HistoryEventType_TaskSubmitted,
        HistoryEventType_TaskSucceeded,
        HistoryEventType_TaskTimedOut,
        HistoryEventType_WaitStateAborted,
        HistoryEventType_WaitStateEntered,
        HistoryEventType_WaitStateExited
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HistoryEventType = HistoryEventType'
  { fromHistoryEventType ::
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

pattern HistoryEventType_ActivityFailed :: HistoryEventType
pattern HistoryEventType_ActivityFailed = HistoryEventType' "ActivityFailed"

pattern HistoryEventType_ActivityScheduleFailed :: HistoryEventType
pattern HistoryEventType_ActivityScheduleFailed = HistoryEventType' "ActivityScheduleFailed"

pattern HistoryEventType_ActivityScheduled :: HistoryEventType
pattern HistoryEventType_ActivityScheduled = HistoryEventType' "ActivityScheduled"

pattern HistoryEventType_ActivityStarted :: HistoryEventType
pattern HistoryEventType_ActivityStarted = HistoryEventType' "ActivityStarted"

pattern HistoryEventType_ActivitySucceeded :: HistoryEventType
pattern HistoryEventType_ActivitySucceeded = HistoryEventType' "ActivitySucceeded"

pattern HistoryEventType_ActivityTimedOut :: HistoryEventType
pattern HistoryEventType_ActivityTimedOut = HistoryEventType' "ActivityTimedOut"

pattern HistoryEventType_ChoiceStateEntered :: HistoryEventType
pattern HistoryEventType_ChoiceStateEntered = HistoryEventType' "ChoiceStateEntered"

pattern HistoryEventType_ChoiceStateExited :: HistoryEventType
pattern HistoryEventType_ChoiceStateExited = HistoryEventType' "ChoiceStateExited"

pattern HistoryEventType_ExecutionAborted :: HistoryEventType
pattern HistoryEventType_ExecutionAborted = HistoryEventType' "ExecutionAborted"

pattern HistoryEventType_ExecutionFailed :: HistoryEventType
pattern HistoryEventType_ExecutionFailed = HistoryEventType' "ExecutionFailed"

pattern HistoryEventType_ExecutionStarted :: HistoryEventType
pattern HistoryEventType_ExecutionStarted = HistoryEventType' "ExecutionStarted"

pattern HistoryEventType_ExecutionSucceeded :: HistoryEventType
pattern HistoryEventType_ExecutionSucceeded = HistoryEventType' "ExecutionSucceeded"

pattern HistoryEventType_ExecutionTimedOut :: HistoryEventType
pattern HistoryEventType_ExecutionTimedOut = HistoryEventType' "ExecutionTimedOut"

pattern HistoryEventType_FailStateEntered :: HistoryEventType
pattern HistoryEventType_FailStateEntered = HistoryEventType' "FailStateEntered"

pattern HistoryEventType_LambdaFunctionFailed :: HistoryEventType
pattern HistoryEventType_LambdaFunctionFailed = HistoryEventType' "LambdaFunctionFailed"

pattern HistoryEventType_LambdaFunctionScheduleFailed :: HistoryEventType
pattern HistoryEventType_LambdaFunctionScheduleFailed = HistoryEventType' "LambdaFunctionScheduleFailed"

pattern HistoryEventType_LambdaFunctionScheduled :: HistoryEventType
pattern HistoryEventType_LambdaFunctionScheduled = HistoryEventType' "LambdaFunctionScheduled"

pattern HistoryEventType_LambdaFunctionStartFailed :: HistoryEventType
pattern HistoryEventType_LambdaFunctionStartFailed = HistoryEventType' "LambdaFunctionStartFailed"

pattern HistoryEventType_LambdaFunctionStarted :: HistoryEventType
pattern HistoryEventType_LambdaFunctionStarted = HistoryEventType' "LambdaFunctionStarted"

pattern HistoryEventType_LambdaFunctionSucceeded :: HistoryEventType
pattern HistoryEventType_LambdaFunctionSucceeded = HistoryEventType' "LambdaFunctionSucceeded"

pattern HistoryEventType_LambdaFunctionTimedOut :: HistoryEventType
pattern HistoryEventType_LambdaFunctionTimedOut = HistoryEventType' "LambdaFunctionTimedOut"

pattern HistoryEventType_MapIterationAborted :: HistoryEventType
pattern HistoryEventType_MapIterationAborted = HistoryEventType' "MapIterationAborted"

pattern HistoryEventType_MapIterationFailed :: HistoryEventType
pattern HistoryEventType_MapIterationFailed = HistoryEventType' "MapIterationFailed"

pattern HistoryEventType_MapIterationStarted :: HistoryEventType
pattern HistoryEventType_MapIterationStarted = HistoryEventType' "MapIterationStarted"

pattern HistoryEventType_MapIterationSucceeded :: HistoryEventType
pattern HistoryEventType_MapIterationSucceeded = HistoryEventType' "MapIterationSucceeded"

pattern HistoryEventType_MapStateAborted :: HistoryEventType
pattern HistoryEventType_MapStateAborted = HistoryEventType' "MapStateAborted"

pattern HistoryEventType_MapStateEntered :: HistoryEventType
pattern HistoryEventType_MapStateEntered = HistoryEventType' "MapStateEntered"

pattern HistoryEventType_MapStateExited :: HistoryEventType
pattern HistoryEventType_MapStateExited = HistoryEventType' "MapStateExited"

pattern HistoryEventType_MapStateFailed :: HistoryEventType
pattern HistoryEventType_MapStateFailed = HistoryEventType' "MapStateFailed"

pattern HistoryEventType_MapStateStarted :: HistoryEventType
pattern HistoryEventType_MapStateStarted = HistoryEventType' "MapStateStarted"

pattern HistoryEventType_MapStateSucceeded :: HistoryEventType
pattern HistoryEventType_MapStateSucceeded = HistoryEventType' "MapStateSucceeded"

pattern HistoryEventType_ParallelStateAborted :: HistoryEventType
pattern HistoryEventType_ParallelStateAborted = HistoryEventType' "ParallelStateAborted"

pattern HistoryEventType_ParallelStateEntered :: HistoryEventType
pattern HistoryEventType_ParallelStateEntered = HistoryEventType' "ParallelStateEntered"

pattern HistoryEventType_ParallelStateExited :: HistoryEventType
pattern HistoryEventType_ParallelStateExited = HistoryEventType' "ParallelStateExited"

pattern HistoryEventType_ParallelStateFailed :: HistoryEventType
pattern HistoryEventType_ParallelStateFailed = HistoryEventType' "ParallelStateFailed"

pattern HistoryEventType_ParallelStateStarted :: HistoryEventType
pattern HistoryEventType_ParallelStateStarted = HistoryEventType' "ParallelStateStarted"

pattern HistoryEventType_ParallelStateSucceeded :: HistoryEventType
pattern HistoryEventType_ParallelStateSucceeded = HistoryEventType' "ParallelStateSucceeded"

pattern HistoryEventType_PassStateEntered :: HistoryEventType
pattern HistoryEventType_PassStateEntered = HistoryEventType' "PassStateEntered"

pattern HistoryEventType_PassStateExited :: HistoryEventType
pattern HistoryEventType_PassStateExited = HistoryEventType' "PassStateExited"

pattern HistoryEventType_SucceedStateEntered :: HistoryEventType
pattern HistoryEventType_SucceedStateEntered = HistoryEventType' "SucceedStateEntered"

pattern HistoryEventType_SucceedStateExited :: HistoryEventType
pattern HistoryEventType_SucceedStateExited = HistoryEventType' "SucceedStateExited"

pattern HistoryEventType_TaskFailed :: HistoryEventType
pattern HistoryEventType_TaskFailed = HistoryEventType' "TaskFailed"

pattern HistoryEventType_TaskScheduled :: HistoryEventType
pattern HistoryEventType_TaskScheduled = HistoryEventType' "TaskScheduled"

pattern HistoryEventType_TaskStartFailed :: HistoryEventType
pattern HistoryEventType_TaskStartFailed = HistoryEventType' "TaskStartFailed"

pattern HistoryEventType_TaskStarted :: HistoryEventType
pattern HistoryEventType_TaskStarted = HistoryEventType' "TaskStarted"

pattern HistoryEventType_TaskStateAborted :: HistoryEventType
pattern HistoryEventType_TaskStateAborted = HistoryEventType' "TaskStateAborted"

pattern HistoryEventType_TaskStateEntered :: HistoryEventType
pattern HistoryEventType_TaskStateEntered = HistoryEventType' "TaskStateEntered"

pattern HistoryEventType_TaskStateExited :: HistoryEventType
pattern HistoryEventType_TaskStateExited = HistoryEventType' "TaskStateExited"

pattern HistoryEventType_TaskSubmitFailed :: HistoryEventType
pattern HistoryEventType_TaskSubmitFailed = HistoryEventType' "TaskSubmitFailed"

pattern HistoryEventType_TaskSubmitted :: HistoryEventType
pattern HistoryEventType_TaskSubmitted = HistoryEventType' "TaskSubmitted"

pattern HistoryEventType_TaskSucceeded :: HistoryEventType
pattern HistoryEventType_TaskSucceeded = HistoryEventType' "TaskSucceeded"

pattern HistoryEventType_TaskTimedOut :: HistoryEventType
pattern HistoryEventType_TaskTimedOut = HistoryEventType' "TaskTimedOut"

pattern HistoryEventType_WaitStateAborted :: HistoryEventType
pattern HistoryEventType_WaitStateAborted = HistoryEventType' "WaitStateAborted"

pattern HistoryEventType_WaitStateEntered :: HistoryEventType
pattern HistoryEventType_WaitStateEntered = HistoryEventType' "WaitStateEntered"

pattern HistoryEventType_WaitStateExited :: HistoryEventType
pattern HistoryEventType_WaitStateExited = HistoryEventType' "WaitStateExited"

{-# COMPLETE
  HistoryEventType_ActivityFailed,
  HistoryEventType_ActivityScheduleFailed,
  HistoryEventType_ActivityScheduled,
  HistoryEventType_ActivityStarted,
  HistoryEventType_ActivitySucceeded,
  HistoryEventType_ActivityTimedOut,
  HistoryEventType_ChoiceStateEntered,
  HistoryEventType_ChoiceStateExited,
  HistoryEventType_ExecutionAborted,
  HistoryEventType_ExecutionFailed,
  HistoryEventType_ExecutionStarted,
  HistoryEventType_ExecutionSucceeded,
  HistoryEventType_ExecutionTimedOut,
  HistoryEventType_FailStateEntered,
  HistoryEventType_LambdaFunctionFailed,
  HistoryEventType_LambdaFunctionScheduleFailed,
  HistoryEventType_LambdaFunctionScheduled,
  HistoryEventType_LambdaFunctionStartFailed,
  HistoryEventType_LambdaFunctionStarted,
  HistoryEventType_LambdaFunctionSucceeded,
  HistoryEventType_LambdaFunctionTimedOut,
  HistoryEventType_MapIterationAborted,
  HistoryEventType_MapIterationFailed,
  HistoryEventType_MapIterationStarted,
  HistoryEventType_MapIterationSucceeded,
  HistoryEventType_MapStateAborted,
  HistoryEventType_MapStateEntered,
  HistoryEventType_MapStateExited,
  HistoryEventType_MapStateFailed,
  HistoryEventType_MapStateStarted,
  HistoryEventType_MapStateSucceeded,
  HistoryEventType_ParallelStateAborted,
  HistoryEventType_ParallelStateEntered,
  HistoryEventType_ParallelStateExited,
  HistoryEventType_ParallelStateFailed,
  HistoryEventType_ParallelStateStarted,
  HistoryEventType_ParallelStateSucceeded,
  HistoryEventType_PassStateEntered,
  HistoryEventType_PassStateExited,
  HistoryEventType_SucceedStateEntered,
  HistoryEventType_SucceedStateExited,
  HistoryEventType_TaskFailed,
  HistoryEventType_TaskScheduled,
  HistoryEventType_TaskStartFailed,
  HistoryEventType_TaskStarted,
  HistoryEventType_TaskStateAborted,
  HistoryEventType_TaskStateEntered,
  HistoryEventType_TaskStateExited,
  HistoryEventType_TaskSubmitFailed,
  HistoryEventType_TaskSubmitted,
  HistoryEventType_TaskSucceeded,
  HistoryEventType_TaskTimedOut,
  HistoryEventType_WaitStateAborted,
  HistoryEventType_WaitStateEntered,
  HistoryEventType_WaitStateExited,
  HistoryEventType'
  #-}
