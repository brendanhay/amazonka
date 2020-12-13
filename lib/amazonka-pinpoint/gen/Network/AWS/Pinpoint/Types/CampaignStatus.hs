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
        Scheduled,
        Executing,
        PendingNextRun,
        Completed,
        Paused,
        Deleted,
        Invalid
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

pattern Scheduled :: CampaignStatus
pattern Scheduled = CampaignStatus' "SCHEDULED"

pattern Executing :: CampaignStatus
pattern Executing = CampaignStatus' "EXECUTING"

pattern PendingNextRun :: CampaignStatus
pattern PendingNextRun = CampaignStatus' "PENDING_NEXT_RUN"

pattern Completed :: CampaignStatus
pattern Completed = CampaignStatus' "COMPLETED"

pattern Paused :: CampaignStatus
pattern Paused = CampaignStatus' "PAUSED"

pattern Deleted :: CampaignStatus
pattern Deleted = CampaignStatus' "DELETED"

pattern Invalid :: CampaignStatus
pattern Invalid = CampaignStatus' "INVALID"

{-# COMPLETE
  Scheduled,
  Executing,
  PendingNextRun,
  Completed,
  Paused,
  Deleted,
  Invalid,
  CampaignStatus'
  #-}
