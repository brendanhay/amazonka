{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CampaignLimits
  ( CampaignLimits (..)
  -- * Smart constructor
  , mkCampaignLimits
  -- * Lenses
  , clDaily
  , clMaximumDuration
  , clMessagesPerSecond
  , clTotal
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | For a campaign, specifies limits on the messages that the campaign can send. For an application, specifies the default limits for messages that campaigns in the application can send.
--
-- /See:/ 'mkCampaignLimits' smart constructor.
data CampaignLimits = CampaignLimits'
  { daily :: Core.Maybe Core.Int
    -- ^ The maximum number of messages that a campaign can send to a single endpoint during a 24-hour period. For an application, this value specifies the default limit for the number of messages that campaigns and journeys can send to a single endpoint during a 24-hour period. The maximum value is 100.
  , maximumDuration :: Core.Maybe Core.Int
    -- ^ The maximum amount of time, in seconds, that a campaign can attempt to deliver a message after the scheduled start time for the campaign. The minimum value is 60 seconds.
  , messagesPerSecond :: Core.Maybe Core.Int
    -- ^ The maximum number of messages that a campaign can send each second. For an application, this value specifies the default limit for the number of messages that campaigns can send each second. The minimum value is 50. The maximum value is 20,000.
  , total :: Core.Maybe Core.Int
    -- ^ The maximum number of messages that a campaign can send to a single endpoint during the course of the campaign. If a campaign recurs, this setting applies to all runs of the campaign. The maximum value is 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignLimits' value with any optional fields omitted.
mkCampaignLimits
    :: CampaignLimits
mkCampaignLimits
  = CampaignLimits'{daily = Core.Nothing,
                    maximumDuration = Core.Nothing, messagesPerSecond = Core.Nothing,
                    total = Core.Nothing}

-- | The maximum number of messages that a campaign can send to a single endpoint during a 24-hour period. For an application, this value specifies the default limit for the number of messages that campaigns and journeys can send to a single endpoint during a 24-hour period. The maximum value is 100.
--
-- /Note:/ Consider using 'daily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clDaily :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
clDaily = Lens.field @"daily"
{-# INLINEABLE clDaily #-}
{-# DEPRECATED daily "Use generic-lens or generic-optics with 'daily' instead"  #-}

-- | The maximum amount of time, in seconds, that a campaign can attempt to deliver a message after the scheduled start time for the campaign. The minimum value is 60 seconds.
--
-- /Note:/ Consider using 'maximumDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumDuration :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
clMaximumDuration = Lens.field @"maximumDuration"
{-# INLINEABLE clMaximumDuration #-}
{-# DEPRECATED maximumDuration "Use generic-lens or generic-optics with 'maximumDuration' instead"  #-}

-- | The maximum number of messages that a campaign can send each second. For an application, this value specifies the default limit for the number of messages that campaigns can send each second. The minimum value is 50. The maximum value is 20,000.
--
-- /Note:/ Consider using 'messagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMessagesPerSecond :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
clMessagesPerSecond = Lens.field @"messagesPerSecond"
{-# INLINEABLE clMessagesPerSecond #-}
{-# DEPRECATED messagesPerSecond "Use generic-lens or generic-optics with 'messagesPerSecond' instead"  #-}

-- | The maximum number of messages that a campaign can send to a single endpoint during the course of the campaign. If a campaign recurs, this setting applies to all runs of the campaign. The maximum value is 100.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clTotal :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
clTotal = Lens.field @"total"
{-# INLINEABLE clTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

instance Core.FromJSON CampaignLimits where
        toJSON CampaignLimits{..}
          = Core.object
              (Core.catMaybes
                 [("Daily" Core..=) Core.<$> daily,
                  ("MaximumDuration" Core..=) Core.<$> maximumDuration,
                  ("MessagesPerSecond" Core..=) Core.<$> messagesPerSecond,
                  ("Total" Core..=) Core.<$> total])

instance Core.FromJSON CampaignLimits where
        parseJSON
          = Core.withObject "CampaignLimits" Core.$
              \ x ->
                CampaignLimits' Core.<$>
                  (x Core..:? "Daily") Core.<*> x Core..:? "MaximumDuration" Core.<*>
                    x Core..:? "MessagesPerSecond"
                    Core.<*> x Core..:? "Total"
