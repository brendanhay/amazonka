{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ExecutionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ExecutionType where

import Network.AWS.Prelude

data ExecutionType
  = ApproveBudgetAction
  | ResetBudgetAction
  | RetryBudgetAction
  | ReverseBudgetAction
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

instance FromText ExecutionType where
  parser =
    takeLowerText >>= \case
      "approve_budget_action" -> pure ApproveBudgetAction
      "reset_budget_action" -> pure ResetBudgetAction
      "retry_budget_action" -> pure RetryBudgetAction
      "reverse_budget_action" -> pure ReverseBudgetAction
      e ->
        fromTextError $
          "Failure parsing ExecutionType from value: '" <> e
            <> "'. Accepted values: approve_budget_action, reset_budget_action, retry_budget_action, reverse_budget_action"

instance ToText ExecutionType where
  toText = \case
    ApproveBudgetAction -> "APPROVE_BUDGET_ACTION"
    ResetBudgetAction -> "RESET_BUDGET_ACTION"
    RetryBudgetAction -> "RETRY_BUDGET_ACTION"
    ReverseBudgetAction -> "REVERSE_BUDGET_ACTION"

instance Hashable ExecutionType

instance NFData ExecutionType

instance ToByteString ExecutionType

instance ToQuery ExecutionType

instance ToHeader ExecutionType

instance ToJSON ExecutionType where
  toJSON = toJSONText

instance FromJSON ExecutionType where
  parseJSON = parseJSONText "ExecutionType"
