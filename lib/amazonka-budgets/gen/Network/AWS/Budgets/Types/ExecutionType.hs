-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ExecutionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ExecutionType
  ( ExecutionType
      ( ExecutionType',
        ApproveBudgetAction,
        ResetBudgetAction,
        RetryBudgetAction,
        ReverseBudgetAction
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExecutionType = ExecutionType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ApproveBudgetAction :: ExecutionType
pattern ApproveBudgetAction = ExecutionType' "APPROVE_BUDGET_ACTION"

pattern ResetBudgetAction :: ExecutionType
pattern ResetBudgetAction = ExecutionType' "RESET_BUDGET_ACTION"

pattern RetryBudgetAction :: ExecutionType
pattern RetryBudgetAction = ExecutionType' "RETRY_BUDGET_ACTION"

pattern ReverseBudgetAction :: ExecutionType
pattern ReverseBudgetAction = ExecutionType' "REVERSE_BUDGET_ACTION"

{-# COMPLETE
  ApproveBudgetAction,
  ResetBudgetAction,
  RetryBudgetAction,
  ReverseBudgetAction,
  ExecutionType'
  #-}
