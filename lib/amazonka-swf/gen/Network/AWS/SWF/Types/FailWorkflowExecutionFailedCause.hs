{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause where

import Network.AWS.Prelude

data FailWorkflowExecutionFailedCause
  = FWEFCOperationNotPermitted
  | FWEFCUnhandledDecision
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

instance FromText FailWorkflowExecutionFailedCause where
  parser =
    takeLowerText >>= \case
      "operation_not_permitted" -> pure FWEFCOperationNotPermitted
      "unhandled_decision" -> pure FWEFCUnhandledDecision
      e ->
        fromTextError $
          "Failure parsing FailWorkflowExecutionFailedCause from value: '" <> e
            <> "'. Accepted values: operation_not_permitted, unhandled_decision"

instance ToText FailWorkflowExecutionFailedCause where
  toText = \case
    FWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    FWEFCUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable FailWorkflowExecutionFailedCause

instance NFData FailWorkflowExecutionFailedCause

instance ToByteString FailWorkflowExecutionFailedCause

instance ToQuery FailWorkflowExecutionFailedCause

instance ToHeader FailWorkflowExecutionFailedCause

instance FromJSON FailWorkflowExecutionFailedCause where
  parseJSON = parseJSONText "FailWorkflowExecutionFailedCause"
