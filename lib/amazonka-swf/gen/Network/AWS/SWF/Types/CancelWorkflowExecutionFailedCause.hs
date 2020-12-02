{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause where

import Network.AWS.Prelude

data CancelWorkflowExecutionFailedCause
  = COperationNotPermitted
  | CUnhandledDecision
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

instance FromText CancelWorkflowExecutionFailedCause where
  parser =
    takeLowerText >>= \case
      "operation_not_permitted" -> pure COperationNotPermitted
      "unhandled_decision" -> pure CUnhandledDecision
      e ->
        fromTextError $
          "Failure parsing CancelWorkflowExecutionFailedCause from value: '" <> e
            <> "'. Accepted values: operation_not_permitted, unhandled_decision"

instance ToText CancelWorkflowExecutionFailedCause where
  toText = \case
    COperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    CUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable CancelWorkflowExecutionFailedCause

instance NFData CancelWorkflowExecutionFailedCause

instance ToByteString CancelWorkflowExecutionFailedCause

instance ToQuery CancelWorkflowExecutionFailedCause

instance ToHeader CancelWorkflowExecutionFailedCause

instance FromJSON CancelWorkflowExecutionFailedCause where
  parseJSON = parseJSONText "CancelWorkflowExecutionFailedCause"
