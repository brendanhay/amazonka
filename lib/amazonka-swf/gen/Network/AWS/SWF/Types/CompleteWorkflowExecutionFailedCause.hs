{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedCause where

import Network.AWS.Prelude

data CompleteWorkflowExecutionFailedCause
  = CWEFCOperationNotPermitted
  | CWEFCUnhandledDecision
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

instance FromText CompleteWorkflowExecutionFailedCause where
  parser =
    takeLowerText >>= \case
      "operation_not_permitted" -> pure CWEFCOperationNotPermitted
      "unhandled_decision" -> pure CWEFCUnhandledDecision
      e ->
        fromTextError $
          "Failure parsing CompleteWorkflowExecutionFailedCause from value: '" <> e
            <> "'. Accepted values: operation_not_permitted, unhandled_decision"

instance ToText CompleteWorkflowExecutionFailedCause where
  toText = \case
    CWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    CWEFCUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable CompleteWorkflowExecutionFailedCause

instance NFData CompleteWorkflowExecutionFailedCause

instance ToByteString CompleteWorkflowExecutionFailedCause

instance ToQuery CompleteWorkflowExecutionFailedCause

instance ToHeader CompleteWorkflowExecutionFailedCause

instance FromJSON CompleteWorkflowExecutionFailedCause where
  parseJSON = parseJSONText "CompleteWorkflowExecutionFailedCause"
