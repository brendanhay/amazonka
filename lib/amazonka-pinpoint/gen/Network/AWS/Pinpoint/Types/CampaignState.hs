{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignState
  ( CampaignState (..),

    -- * Smart constructor
    mkCampaignState,

    -- * Lenses
    csCampaignStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status of a campaign.
--
-- /See:/ 'mkCampaignState' smart constructor.
newtype CampaignState = CampaignState'
  { -- | The current status of the campaign, or the current status of a treatment that belongs to an A/B test campaign.
    --
    -- If a campaign uses A/B testing, the campaign has a status of COMPLETED only if all campaign treatments have a status of COMPLETED. If you delete the segment that's associated with a campaign, the campaign fails and has a status of DELETED.
    campaignStatus :: Core.Maybe Types.CampaignStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignState' value with any optional fields omitted.
mkCampaignState ::
  CampaignState
mkCampaignState = CampaignState' {campaignStatus = Core.Nothing}

-- | The current status of the campaign, or the current status of a treatment that belongs to an A/B test campaign.
--
-- If a campaign uses A/B testing, the campaign has a status of COMPLETED only if all campaign treatments have a status of COMPLETED. If you delete the segment that's associated with a campaign, the campaign fails and has a status of DELETED.
--
-- /Note:/ Consider using 'campaignStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCampaignStatus :: Lens.Lens' CampaignState (Core.Maybe Types.CampaignStatus)
csCampaignStatus = Lens.field @"campaignStatus"
{-# DEPRECATED csCampaignStatus "Use generic-lens or generic-optics with 'campaignStatus' instead." #-}

instance Core.FromJSON CampaignState where
  parseJSON =
    Core.withObject "CampaignState" Core.$
      \x -> CampaignState' Core.<$> (x Core..:? "CampaignStatus")
