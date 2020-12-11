-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignLimits
  ( CampaignLimits (..),

    -- * Smart constructor
    mkCampaignLimits,

    -- * Lenses
    clMessagesPerSecond,
    clDaily,
    clTotal,
    clMaximumDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | For a campaign, specifies limits on the messages that the campaign can send. For an application, specifies the default limits for messages that campaigns in the application can send.
--
-- /See:/ 'mkCampaignLimits' smart constructor.
data CampaignLimits = CampaignLimits'
  { messagesPerSecond ::
      Lude.Maybe Lude.Int,
    daily :: Lude.Maybe Lude.Int,
    total :: Lude.Maybe Lude.Int,
    maximumDuration :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignLimits' with the minimum fields required to make a request.
--
-- * 'daily' - The maximum number of messages that a campaign can send to a single endpoint during a 24-hour period. For an application, this value specifies the default limit for the number of messages that campaigns and journeys can send to a single endpoint during a 24-hour period. The maximum value is 100.
-- * 'maximumDuration' - The maximum amount of time, in seconds, that a campaign can attempt to deliver a message after the scheduled start time for the campaign. The minimum value is 60 seconds.
-- * 'messagesPerSecond' - The maximum number of messages that a campaign can send each second. For an application, this value specifies the default limit for the number of messages that campaigns can send each second. The minimum value is 50. The maximum value is 20,000.
-- * 'total' - The maximum number of messages that a campaign can send to a single endpoint during the course of the campaign. If a campaign recurs, this setting applies to all runs of the campaign. The maximum value is 100.
mkCampaignLimits ::
  CampaignLimits
mkCampaignLimits =
  CampaignLimits'
    { messagesPerSecond = Lude.Nothing,
      daily = Lude.Nothing,
      total = Lude.Nothing,
      maximumDuration = Lude.Nothing
    }

-- | The maximum number of messages that a campaign can send each second. For an application, this value specifies the default limit for the number of messages that campaigns can send each second. The minimum value is 50. The maximum value is 20,000.
--
-- /Note:/ Consider using 'messagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMessagesPerSecond :: Lens.Lens' CampaignLimits (Lude.Maybe Lude.Int)
clMessagesPerSecond = Lens.lens (messagesPerSecond :: CampaignLimits -> Lude.Maybe Lude.Int) (\s a -> s {messagesPerSecond = a} :: CampaignLimits)
{-# DEPRECATED clMessagesPerSecond "Use generic-lens or generic-optics with 'messagesPerSecond' instead." #-}

-- | The maximum number of messages that a campaign can send to a single endpoint during a 24-hour period. For an application, this value specifies the default limit for the number of messages that campaigns and journeys can send to a single endpoint during a 24-hour period. The maximum value is 100.
--
-- /Note:/ Consider using 'daily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clDaily :: Lens.Lens' CampaignLimits (Lude.Maybe Lude.Int)
clDaily = Lens.lens (daily :: CampaignLimits -> Lude.Maybe Lude.Int) (\s a -> s {daily = a} :: CampaignLimits)
{-# DEPRECATED clDaily "Use generic-lens or generic-optics with 'daily' instead." #-}

-- | The maximum number of messages that a campaign can send to a single endpoint during the course of the campaign. If a campaign recurs, this setting applies to all runs of the campaign. The maximum value is 100.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clTotal :: Lens.Lens' CampaignLimits (Lude.Maybe Lude.Int)
clTotal = Lens.lens (total :: CampaignLimits -> Lude.Maybe Lude.Int) (\s a -> s {total = a} :: CampaignLimits)
{-# DEPRECATED clTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The maximum amount of time, in seconds, that a campaign can attempt to deliver a message after the scheduled start time for the campaign. The minimum value is 60 seconds.
--
-- /Note:/ Consider using 'maximumDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumDuration :: Lens.Lens' CampaignLimits (Lude.Maybe Lude.Int)
clMaximumDuration = Lens.lens (maximumDuration :: CampaignLimits -> Lude.Maybe Lude.Int) (\s a -> s {maximumDuration = a} :: CampaignLimits)
{-# DEPRECATED clMaximumDuration "Use generic-lens or generic-optics with 'maximumDuration' instead." #-}

instance Lude.FromJSON CampaignLimits where
  parseJSON =
    Lude.withObject
      "CampaignLimits"
      ( \x ->
          CampaignLimits'
            Lude.<$> (x Lude..:? "MessagesPerSecond")
            Lude.<*> (x Lude..:? "Daily")
            Lude.<*> (x Lude..:? "Total")
            Lude.<*> (x Lude..:? "MaximumDuration")
      )

instance Lude.ToJSON CampaignLimits where
  toJSON CampaignLimits' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MessagesPerSecond" Lude..=) Lude.<$> messagesPerSecond,
            ("Daily" Lude..=) Lude.<$> daily,
            ("Total" Lude..=) Lude.<$> total,
            ("MaximumDuration" Lude..=) Lude.<$> maximumDuration
          ]
      )
