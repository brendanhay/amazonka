-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ScheduleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ScheduleStatus
  ( ScheduleStatus
      ( ScheduleStatus',
        SSFailed,
        SSPending,
        SSScheduled,
        SSStopped
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScheduleStatus = ScheduleStatus' Lude.Text
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

pattern SSFailed :: ScheduleStatus
pattern SSFailed = ScheduleStatus' "Failed"

pattern SSPending :: ScheduleStatus
pattern SSPending = ScheduleStatus' "Pending"

pattern SSScheduled :: ScheduleStatus
pattern SSScheduled = ScheduleStatus' "Scheduled"

pattern SSStopped :: ScheduleStatus
pattern SSStopped = ScheduleStatus' "Stopped"

{-# COMPLETE
  SSFailed,
  SSPending,
  SSScheduled,
  SSStopped,
  ScheduleStatus'
  #-}
