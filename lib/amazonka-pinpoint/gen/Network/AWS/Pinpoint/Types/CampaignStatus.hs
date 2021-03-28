{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CampaignStatus
  ( CampaignStatus
    ( CampaignStatus'
    , CampaignStatusScheduled
    , CampaignStatusExecuting
    , CampaignStatusPendingNextRun
    , CampaignStatusCompleted
    , CampaignStatusPaused
    , CampaignStatusDeleted
    , CampaignStatusInvalid
    , fromCampaignStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CampaignStatus = CampaignStatus'{fromCampaignStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern CampaignStatusScheduled :: CampaignStatus
pattern CampaignStatusScheduled = CampaignStatus' "SCHEDULED"

pattern CampaignStatusExecuting :: CampaignStatus
pattern CampaignStatusExecuting = CampaignStatus' "EXECUTING"

pattern CampaignStatusPendingNextRun :: CampaignStatus
pattern CampaignStatusPendingNextRun = CampaignStatus' "PENDING_NEXT_RUN"

pattern CampaignStatusCompleted :: CampaignStatus
pattern CampaignStatusCompleted = CampaignStatus' "COMPLETED"

pattern CampaignStatusPaused :: CampaignStatus
pattern CampaignStatusPaused = CampaignStatus' "PAUSED"

pattern CampaignStatusDeleted :: CampaignStatus
pattern CampaignStatusDeleted = CampaignStatus' "DELETED"

pattern CampaignStatusInvalid :: CampaignStatus
pattern CampaignStatusInvalid = CampaignStatus' "INVALID"

{-# COMPLETE 
  CampaignStatusScheduled,

  CampaignStatusExecuting,

  CampaignStatusPendingNextRun,

  CampaignStatusCompleted,

  CampaignStatusPaused,

  CampaignStatusDeleted,

  CampaignStatusInvalid,
  CampaignStatus'
  #-}
