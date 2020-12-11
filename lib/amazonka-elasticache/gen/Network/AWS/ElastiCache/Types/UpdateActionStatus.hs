-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateActionStatus
  ( UpdateActionStatus
      ( UpdateActionStatus',
        Complete,
        InProgress,
        NotApplicable,
        NotApplied,
        Scheduled,
        Scheduling,
        Stopped,
        Stopping,
        WaitingToStart
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UpdateActionStatus = UpdateActionStatus' Lude.Text
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

pattern Complete :: UpdateActionStatus
pattern Complete = UpdateActionStatus' "complete"

pattern InProgress :: UpdateActionStatus
pattern InProgress = UpdateActionStatus' "in-progress"

pattern NotApplicable :: UpdateActionStatus
pattern NotApplicable = UpdateActionStatus' "not-applicable"

pattern NotApplied :: UpdateActionStatus
pattern NotApplied = UpdateActionStatus' "not-applied"

pattern Scheduled :: UpdateActionStatus
pattern Scheduled = UpdateActionStatus' "scheduled"

pattern Scheduling :: UpdateActionStatus
pattern Scheduling = UpdateActionStatus' "scheduling"

pattern Stopped :: UpdateActionStatus
pattern Stopped = UpdateActionStatus' "stopped"

pattern Stopping :: UpdateActionStatus
pattern Stopping = UpdateActionStatus' "stopping"

pattern WaitingToStart :: UpdateActionStatus
pattern WaitingToStart = UpdateActionStatus' "waiting-to-start"

{-# COMPLETE
  Complete,
  InProgress,
  NotApplicable,
  NotApplied,
  Scheduled,
  Scheduling,
  Stopped,
  Stopping,
  WaitingToStart,
  UpdateActionStatus'
  #-}
