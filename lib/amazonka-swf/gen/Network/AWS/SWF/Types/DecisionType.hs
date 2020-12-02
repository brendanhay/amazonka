{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionType where

import Network.AWS.Prelude

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
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText DecisionType where
  parser =
    takeLowerText >>= \case
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
      e ->
        fromTextError $
          "Failure parsing DecisionType from value: '" <> e
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

instance Hashable DecisionType

instance NFData DecisionType

instance ToByteString DecisionType

instance ToQuery DecisionType

instance ToHeader DecisionType

instance ToJSON DecisionType where
  toJSON = toJSONText
