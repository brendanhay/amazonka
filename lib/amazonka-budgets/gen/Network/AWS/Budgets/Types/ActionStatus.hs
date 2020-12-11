-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionStatus
  ( ActionStatus
      ( ActionStatus',
        ExecutionFailure,
        ExecutionInProgress,
        ExecutionSuccess,
        Pending,
        ResetFailure,
        ResetInProgress,
        ReverseFailure,
        ReverseInProgress,
        ReverseSuccess,
        Standby
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionStatus = ActionStatus' Lude.Text
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

pattern ExecutionFailure :: ActionStatus
pattern ExecutionFailure = ActionStatus' "EXECUTION_FAILURE"

pattern ExecutionInProgress :: ActionStatus
pattern ExecutionInProgress = ActionStatus' "EXECUTION_IN_PROGRESS"

pattern ExecutionSuccess :: ActionStatus
pattern ExecutionSuccess = ActionStatus' "EXECUTION_SUCCESS"

pattern Pending :: ActionStatus
pattern Pending = ActionStatus' "PENDING"

pattern ResetFailure :: ActionStatus
pattern ResetFailure = ActionStatus' "RESET_FAILURE"

pattern ResetInProgress :: ActionStatus
pattern ResetInProgress = ActionStatus' "RESET_IN_PROGRESS"

pattern ReverseFailure :: ActionStatus
pattern ReverseFailure = ActionStatus' "REVERSE_FAILURE"

pattern ReverseInProgress :: ActionStatus
pattern ReverseInProgress = ActionStatus' "REVERSE_IN_PROGRESS"

pattern ReverseSuccess :: ActionStatus
pattern ReverseSuccess = ActionStatus' "REVERSE_SUCCESS"

pattern Standby :: ActionStatus
pattern Standby = ActionStatus' "STANDBY"

{-# COMPLETE
  ExecutionFailure,
  ExecutionInProgress,
  ExecutionSuccess,
  Pending,
  ResetFailure,
  ResetInProgress,
  ReverseFailure,
  ReverseInProgress,
  ReverseSuccess,
  Standby,
  ActionStatus'
  #-}
