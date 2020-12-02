{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedCause where

import Network.AWS.Prelude

data RequestCancelExternalWorkflowExecutionFailedCause
  = RCEWEFCOperationNotPermitted
  | RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded
  | RCEWEFCUnknownExternalWorkflowExecution
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

instance FromText RequestCancelExternalWorkflowExecutionFailedCause where
  parser =
    takeLowerText >>= \case
      "operation_not_permitted" -> pure RCEWEFCOperationNotPermitted
      "request_cancel_external_workflow_execution_rate_exceeded" -> pure RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded
      "unknown_external_workflow_execution" -> pure RCEWEFCUnknownExternalWorkflowExecution
      e ->
        fromTextError $
          "Failure parsing RequestCancelExternalWorkflowExecutionFailedCause from value: '" <> e
            <> "'. Accepted values: operation_not_permitted, request_cancel_external_workflow_execution_rate_exceeded, unknown_external_workflow_execution"

instance ToText RequestCancelExternalWorkflowExecutionFailedCause where
  toText = \case
    RCEWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded -> "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    RCEWEFCUnknownExternalWorkflowExecution -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Hashable RequestCancelExternalWorkflowExecutionFailedCause

instance NFData RequestCancelExternalWorkflowExecutionFailedCause

instance ToByteString RequestCancelExternalWorkflowExecutionFailedCause

instance ToQuery RequestCancelExternalWorkflowExecutionFailedCause

instance ToHeader RequestCancelExternalWorkflowExecutionFailedCause

instance FromJSON RequestCancelExternalWorkflowExecutionFailedCause where
  parseJSON = parseJSONText "RequestCancelExternalWorkflowExecutionFailedCause"
