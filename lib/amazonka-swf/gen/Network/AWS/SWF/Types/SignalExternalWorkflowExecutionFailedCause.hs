{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause where

import Network.AWS.Prelude

data SignalExternalWorkflowExecutionFailedCause
  = SEWEFCOperationNotPermitted
  | SEWEFCSignalExternalWorkflowExecutionRateExceeded
  | SEWEFCUnknownExternalWorkflowExecution
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

instance FromText SignalExternalWorkflowExecutionFailedCause where
  parser =
    takeLowerText >>= \case
      "operation_not_permitted" -> pure SEWEFCOperationNotPermitted
      "signal_external_workflow_execution_rate_exceeded" -> pure SEWEFCSignalExternalWorkflowExecutionRateExceeded
      "unknown_external_workflow_execution" -> pure SEWEFCUnknownExternalWorkflowExecution
      e ->
        fromTextError $
          "Failure parsing SignalExternalWorkflowExecutionFailedCause from value: '" <> e
            <> "'. Accepted values: operation_not_permitted, signal_external_workflow_execution_rate_exceeded, unknown_external_workflow_execution"

instance ToText SignalExternalWorkflowExecutionFailedCause where
  toText = \case
    SEWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    SEWEFCSignalExternalWorkflowExecutionRateExceeded -> "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    SEWEFCUnknownExternalWorkflowExecution -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Hashable SignalExternalWorkflowExecutionFailedCause

instance NFData SignalExternalWorkflowExecutionFailedCause

instance ToByteString SignalExternalWorkflowExecutionFailedCause

instance ToQuery SignalExternalWorkflowExecutionFailedCause

instance ToHeader SignalExternalWorkflowExecutionFailedCause

instance FromJSON SignalExternalWorkflowExecutionFailedCause where
  parseJSON = parseJSONText "SignalExternalWorkflowExecutionFailedCause"
