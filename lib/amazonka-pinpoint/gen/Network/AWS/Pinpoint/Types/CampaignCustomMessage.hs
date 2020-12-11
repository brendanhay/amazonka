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
import qualified Network.AWS.Prelude as Lude

-- | Specifies the contents of a message that's sent through a custom channel to recipients of a campaign.
--
-- /See:/ 'mkCampaignCustomMessage' smart constructor.
newtype CampaignCustomMessage = CampaignCustomMessage'
  { data' ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignCustomMessage' with the minimum fields required to make a request.
--
-- * 'data'' - The raw, JSON-formatted string to use as the payload for the message. The maximum size is 5 KB.
mkCampaignCustomMessage ::
  CampaignCustomMessage
mkCampaignCustomMessage =
  CampaignCustomMessage' {data' = Lude.Nothing}

-- | The raw, JSON-formatted string to use as the payload for the message. The maximum size is 5 KB.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmData :: Lens.Lens' CampaignCustomMessage (Lude.Maybe Lude.Text)
ccmData = Lens.lens (data' :: CampaignCustomMessage -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: CampaignCustomMessage)
{-# DEPRECATED ccmData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Lude.FromJSON CampaignCustomMessage where
  parseJSON =
    Lude.withObject
      "CampaignCustomMessage"
      (\x -> CampaignCustomMessage' Lude.<$> (x Lude..:? "Data"))

instance Lude.ToJSON CampaignCustomMessage where
  toJSON CampaignCustomMessage' {..} =
    Lude.object (Lude.catMaybes [("Data" Lude..=) Lude.<$> data'])
