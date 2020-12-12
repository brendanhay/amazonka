{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignStatus
  ( CampaignStatus
      ( CampaignStatus',
        Completed,
        Deleted,
        Executing,
        Invalid,
        Paused,
        PendingNextRun,
        Scheduled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CampaignStatus = CampaignStatus' Lude.Text
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

pattern Completed :: CampaignStatus
pattern Completed = CampaignStatus' "COMPLETED"

pattern Deleted :: CampaignStatus
pattern Deleted = CampaignStatus' "DELETED"

pattern Executing :: CampaignStatus
pattern Executing = CampaignStatus' "EXECUTING"

pattern Invalid :: CampaignStatus
pattern Invalid = CampaignStatus' "INVALID"

pattern Paused :: CampaignStatus
pattern Paused = CampaignStatus' "PAUSED"

pattern PendingNextRun :: CampaignStatus
pattern PendingNextRun = CampaignStatus' "PENDING_NEXT_RUN"

pattern Scheduled :: CampaignStatus
pattern Scheduled = CampaignStatus' "SCHEDULED"

{-# COMPLETE
  Completed,
  Deleted,
  Executing,
  Invalid,
  Paused,
  PendingNextRun,
  Scheduled,
  CampaignStatus'
  #-}
