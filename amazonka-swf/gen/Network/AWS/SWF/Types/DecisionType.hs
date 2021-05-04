{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionType
  ( DecisionType
      ( ..,
        DecisionType_CancelTimer,
        DecisionType_CancelWorkflowExecution,
        DecisionType_CompleteWorkflowExecution,
        DecisionType_ContinueAsNewWorkflowExecution,
        DecisionType_FailWorkflowExecution,
        DecisionType_RecordMarker,
        DecisionType_RequestCancelActivityTask,
        DecisionType_RequestCancelExternalWorkflowExecution,
        DecisionType_ScheduleActivityTask,
        DecisionType_ScheduleLambdaFunction,
        DecisionType_SignalExternalWorkflowExecution,
        DecisionType_StartChildWorkflowExecution,
        DecisionType_StartTimer
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DecisionType = DecisionType'
  { fromDecisionType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern DecisionType_CancelTimer :: DecisionType
pattern DecisionType_CancelTimer = DecisionType' "CancelTimer"

pattern DecisionType_CancelWorkflowExecution :: DecisionType
pattern DecisionType_CancelWorkflowExecution = DecisionType' "CancelWorkflowExecution"

pattern DecisionType_CompleteWorkflowExecution :: DecisionType
pattern DecisionType_CompleteWorkflowExecution = DecisionType' "CompleteWorkflowExecution"

pattern DecisionType_ContinueAsNewWorkflowExecution :: DecisionType
pattern DecisionType_ContinueAsNewWorkflowExecution = DecisionType' "ContinueAsNewWorkflowExecution"

pattern DecisionType_FailWorkflowExecution :: DecisionType
pattern DecisionType_FailWorkflowExecution = DecisionType' "FailWorkflowExecution"

pattern DecisionType_RecordMarker :: DecisionType
pattern DecisionType_RecordMarker = DecisionType' "RecordMarker"

pattern DecisionType_RequestCancelActivityTask :: DecisionType
pattern DecisionType_RequestCancelActivityTask = DecisionType' "RequestCancelActivityTask"

pattern DecisionType_RequestCancelExternalWorkflowExecution :: DecisionType
pattern DecisionType_RequestCancelExternalWorkflowExecution = DecisionType' "RequestCancelExternalWorkflowExecution"

pattern DecisionType_ScheduleActivityTask :: DecisionType
pattern DecisionType_ScheduleActivityTask = DecisionType' "ScheduleActivityTask"

pattern DecisionType_ScheduleLambdaFunction :: DecisionType
pattern DecisionType_ScheduleLambdaFunction = DecisionType' "ScheduleLambdaFunction"

pattern DecisionType_SignalExternalWorkflowExecution :: DecisionType
pattern DecisionType_SignalExternalWorkflowExecution = DecisionType' "SignalExternalWorkflowExecution"

pattern DecisionType_StartChildWorkflowExecution :: DecisionType
pattern DecisionType_StartChildWorkflowExecution = DecisionType' "StartChildWorkflowExecution"

pattern DecisionType_StartTimer :: DecisionType
pattern DecisionType_StartTimer = DecisionType' "StartTimer"

{-# COMPLETE
  DecisionType_CancelTimer,
  DecisionType_CancelWorkflowExecution,
  DecisionType_CompleteWorkflowExecution,
  DecisionType_ContinueAsNewWorkflowExecution,
  DecisionType_FailWorkflowExecution,
  DecisionType_RecordMarker,
  DecisionType_RequestCancelActivityTask,
  DecisionType_RequestCancelExternalWorkflowExecution,
  DecisionType_ScheduleActivityTask,
  DecisionType_ScheduleLambdaFunction,
  DecisionType_SignalExternalWorkflowExecution,
  DecisionType_StartChildWorkflowExecution,
  DecisionType_StartTimer,
  DecisionType'
  #-}
