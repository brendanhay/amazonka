{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause where

import Network.AWS.Prelude

data WorkflowExecutionTerminatedCause
  = WETCChildPolicyApplied
  | WETCEventLimitExceeded
  | WETCOperatorInitiated
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

instance FromText WorkflowExecutionTerminatedCause where
  parser =
    takeLowerText >>= \case
      "child_policy_applied" -> pure WETCChildPolicyApplied
      "event_limit_exceeded" -> pure WETCEventLimitExceeded
      "operator_initiated" -> pure WETCOperatorInitiated
      e ->
        fromTextError $
          "Failure parsing WorkflowExecutionTerminatedCause from value: '" <> e
            <> "'. Accepted values: child_policy_applied, event_limit_exceeded, operator_initiated"

instance ToText WorkflowExecutionTerminatedCause where
  toText = \case
    WETCChildPolicyApplied -> "CHILD_POLICY_APPLIED"
    WETCEventLimitExceeded -> "EVENT_LIMIT_EXCEEDED"
    WETCOperatorInitiated -> "OPERATOR_INITIATED"

instance Hashable WorkflowExecutionTerminatedCause

instance NFData WorkflowExecutionTerminatedCause

instance ToByteString WorkflowExecutionTerminatedCause

instance ToQuery WorkflowExecutionTerminatedCause

instance ToHeader WorkflowExecutionTerminatedCause

instance FromJSON WorkflowExecutionTerminatedCause where
  parseJSON = parseJSONText "WorkflowExecutionTerminatedCause"
