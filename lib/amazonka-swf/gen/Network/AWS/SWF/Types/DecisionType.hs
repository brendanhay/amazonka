{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.DecisionType
  ( DecisionType
    ( DecisionType'
    , DecisionTypeScheduleActivityTask
    , DecisionTypeRequestCancelActivityTask
    , DecisionTypeCompleteWorkflowExecution
    , DecisionTypeFailWorkflowExecution
    , DecisionTypeCancelWorkflowExecution
    , DecisionTypeContinueAsNewWorkflowExecution
    , DecisionTypeRecordMarker
    , DecisionTypeStartTimer
    , DecisionTypeCancelTimer
    , DecisionTypeSignalExternalWorkflowExecution
    , DecisionTypeRequestCancelExternalWorkflowExecution
    , DecisionTypeStartChildWorkflowExecution
    , DecisionTypeScheduleLambdaFunction
    , fromDecisionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DecisionType = DecisionType'{fromDecisionType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern DecisionTypeScheduleActivityTask :: DecisionType
pattern DecisionTypeScheduleActivityTask = DecisionType' "ScheduleActivityTask"

pattern DecisionTypeRequestCancelActivityTask :: DecisionType
pattern DecisionTypeRequestCancelActivityTask = DecisionType' "RequestCancelActivityTask"

pattern DecisionTypeCompleteWorkflowExecution :: DecisionType
pattern DecisionTypeCompleteWorkflowExecution = DecisionType' "CompleteWorkflowExecution"

pattern DecisionTypeFailWorkflowExecution :: DecisionType
pattern DecisionTypeFailWorkflowExecution = DecisionType' "FailWorkflowExecution"

pattern DecisionTypeCancelWorkflowExecution :: DecisionType
pattern DecisionTypeCancelWorkflowExecution = DecisionType' "CancelWorkflowExecution"

pattern DecisionTypeContinueAsNewWorkflowExecution :: DecisionType
pattern DecisionTypeContinueAsNewWorkflowExecution = DecisionType' "ContinueAsNewWorkflowExecution"

pattern DecisionTypeRecordMarker :: DecisionType
pattern DecisionTypeRecordMarker = DecisionType' "RecordMarker"

pattern DecisionTypeStartTimer :: DecisionType
pattern DecisionTypeStartTimer = DecisionType' "StartTimer"

pattern DecisionTypeCancelTimer :: DecisionType
pattern DecisionTypeCancelTimer = DecisionType' "CancelTimer"

pattern DecisionTypeSignalExternalWorkflowExecution :: DecisionType
pattern DecisionTypeSignalExternalWorkflowExecution = DecisionType' "SignalExternalWorkflowExecution"

pattern DecisionTypeRequestCancelExternalWorkflowExecution :: DecisionType
pattern DecisionTypeRequestCancelExternalWorkflowExecution = DecisionType' "RequestCancelExternalWorkflowExecution"

pattern DecisionTypeStartChildWorkflowExecution :: DecisionType
pattern DecisionTypeStartChildWorkflowExecution = DecisionType' "StartChildWorkflowExecution"

pattern DecisionTypeScheduleLambdaFunction :: DecisionType
pattern DecisionTypeScheduleLambdaFunction = DecisionType' "ScheduleLambdaFunction"

{-# COMPLETE 
  DecisionTypeScheduleActivityTask,

  DecisionTypeRequestCancelActivityTask,

  DecisionTypeCompleteWorkflowExecution,

  DecisionTypeFailWorkflowExecution,

  DecisionTypeCancelWorkflowExecution,

  DecisionTypeContinueAsNewWorkflowExecution,

  DecisionTypeRecordMarker,

  DecisionTypeStartTimer,

  DecisionTypeCancelTimer,

  DecisionTypeSignalExternalWorkflowExecution,

  DecisionTypeRequestCancelExternalWorkflowExecution,

  DecisionTypeStartChildWorkflowExecution,

  DecisionTypeScheduleLambdaFunction,
  DecisionType'
  #-}
