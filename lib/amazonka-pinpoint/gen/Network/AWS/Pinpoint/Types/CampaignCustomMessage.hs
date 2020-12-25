{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignCustomMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignCustomMessage
  ( CampaignCustomMessage (..),

    -- * Smart constructor
    mkCampaignCustomMessage,

    -- * Lenses
    ccmData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the contents of a message that's sent through a custom channel to recipients of a campaign.
--
-- /See:/ 'mkCampaignCustomMessage' smart constructor.
newtype CampaignCustomMessage = CampaignCustomMessage'
  { -- | The raw, JSON-formatted string to use as the payload for the message. The maximum size is 5 KB.
    data' :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignCustomMessage' value with any optional fields omitted.
mkCampaignCustomMessage ::
  CampaignCustomMessage
mkCampaignCustomMessage =
  CampaignCustomMessage' {data' = Core.Nothing}

-- | The raw, JSON-formatted string to use as the payload for the message. The maximum size is 5 KB.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmData :: Lens.Lens' CampaignCustomMessage (Core.Maybe Core.Text)
ccmData = Lens.field @"data'"
{-# DEPRECATED ccmData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Core.FromJSON CampaignCustomMessage where
  toJSON CampaignCustomMessage {..} =
    Core.object (Core.catMaybes [("Data" Core..=) Core.<$> data'])

instance Core.FromJSON CampaignCustomMessage where
  parseJSON =
    Core.withObject "CampaignCustomMessage" Core.$
      \x -> CampaignCustomMessage' Core.<$> (x Core..:? "Data")
