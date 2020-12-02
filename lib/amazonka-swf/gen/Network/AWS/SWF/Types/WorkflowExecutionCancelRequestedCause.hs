{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause where

import Network.AWS.Prelude

data WorkflowExecutionCancelRequestedCause = ChildPolicyApplied
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

instance FromText WorkflowExecutionCancelRequestedCause where
  parser =
    takeLowerText >>= \case
      "child_policy_applied" -> pure ChildPolicyApplied
      e ->
        fromTextError $
          "Failure parsing WorkflowExecutionCancelRequestedCause from value: '" <> e
            <> "'. Accepted values: child_policy_applied"

instance ToText WorkflowExecutionCancelRequestedCause where
  toText = \case
    ChildPolicyApplied -> "CHILD_POLICY_APPLIED"

instance Hashable WorkflowExecutionCancelRequestedCause

instance NFData WorkflowExecutionCancelRequestedCause

instance ToByteString WorkflowExecutionCancelRequestedCause

instance ToQuery WorkflowExecutionCancelRequestedCause

instance ToHeader WorkflowExecutionCancelRequestedCause

instance FromJSON WorkflowExecutionCancelRequestedCause where
  parseJSON = parseJSONText "WorkflowExecutionCancelRequestedCause"
