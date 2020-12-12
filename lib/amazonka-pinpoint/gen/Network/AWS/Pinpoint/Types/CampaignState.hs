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
import Network.AWS.Pinpoint.Types.CampaignStatus
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status of a campaign.
--
-- /See:/ 'mkCampaignState' smart constructor.
newtype CampaignState = CampaignState'
  { campaignStatus ::
      Lude.Maybe CampaignStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignState' with the minimum fields required to make a request.
--
-- * 'campaignStatus' - The current status of the campaign, or the current status of a treatment that belongs to an A/B test campaign.
--
-- If a campaign uses A/B testing, the campaign has a status of COMPLETED only if all campaign treatments have a status of COMPLETED. If you delete the segment that's associated with a campaign, the campaign fails and has a status of DELETED.
mkCampaignState ::
  CampaignState
mkCampaignState = CampaignState' {campaignStatus = Lude.Nothing}

-- | The current status of the campaign, or the current status of a treatment that belongs to an A/B test campaign.
--
-- If a campaign uses A/B testing, the campaign has a status of COMPLETED only if all campaign treatments have a status of COMPLETED. If you delete the segment that's associated with a campaign, the campaign fails and has a status of DELETED.
--
-- /Note:/ Consider using 'campaignStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCampaignStatus :: Lens.Lens' CampaignState (Lude.Maybe CampaignStatus)
csCampaignStatus = Lens.lens (campaignStatus :: CampaignState -> Lude.Maybe CampaignStatus) (\s a -> s {campaignStatus = a} :: CampaignState)
{-# DEPRECATED csCampaignStatus "Use generic-lens or generic-optics with 'campaignStatus' instead." #-}

instance Lude.FromJSON CampaignState where
  parseJSON =
    Lude.withObject
      "CampaignState"
      (\x -> CampaignState' Lude.<$> (x Lude..:? "CampaignStatus"))
