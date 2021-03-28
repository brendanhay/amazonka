{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ChannelSummary
  ( ChannelSummary (..)
  -- * Smart constructor
  , mkChannelSummary
  -- * Lenses
  , csChannelName
  , csChannelStorage
  , csCreationTime
  , csLastMessageArrivalTime
  , csLastUpdateTime
  , csStatus
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ChannelName as Types
import qualified Network.AWS.IoTAnalytics.Types.ChannelStatus as Types
import qualified Network.AWS.IoTAnalytics.Types.ChannelStorageSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of information about a channel.
--
-- /See:/ 'mkChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { channelName :: Core.Maybe Types.ChannelName
    -- ^ The name of the channel.
  , channelStorage :: Core.Maybe Types.ChannelStorageSummary
    -- ^ Where channel data is stored.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the channel was created.
  , lastMessageArrivalTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020. 
  , lastUpdateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time the channel was updated.
  , status :: Core.Maybe Types.ChannelStatus
    -- ^ The status of the channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ChannelSummary' value with any optional fields omitted.
mkChannelSummary
    :: ChannelSummary
mkChannelSummary
  = ChannelSummary'{channelName = Core.Nothing,
                    channelStorage = Core.Nothing, creationTime = Core.Nothing,
                    lastMessageArrivalTime = Core.Nothing,
                    lastUpdateTime = Core.Nothing, status = Core.Nothing}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csChannelName :: Lens.Lens' ChannelSummary (Core.Maybe Types.ChannelName)
csChannelName = Lens.field @"channelName"
{-# INLINEABLE csChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

-- | Where channel data is stored.
--
-- /Note:/ Consider using 'channelStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csChannelStorage :: Lens.Lens' ChannelSummary (Core.Maybe Types.ChannelStorageSummary)
csChannelStorage = Lens.field @"channelStorage"
{-# INLINEABLE csChannelStorage #-}
{-# DEPRECATED channelStorage "Use generic-lens or generic-optics with 'channelStorage' instead"  #-}

-- | When the channel was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreationTime :: Lens.Lens' ChannelSummary (Core.Maybe Core.NominalDiffTime)
csCreationTime = Lens.field @"creationTime"
{-# INLINEABLE csCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020. 
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLastMessageArrivalTime :: Lens.Lens' ChannelSummary (Core.Maybe Core.NominalDiffTime)
csLastMessageArrivalTime = Lens.field @"lastMessageArrivalTime"
{-# INLINEABLE csLastMessageArrivalTime #-}
{-# DEPRECATED lastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead"  #-}

-- | The last time the channel was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLastUpdateTime :: Lens.Lens' ChannelSummary (Core.Maybe Core.NominalDiffTime)
csLastUpdateTime = Lens.field @"lastUpdateTime"
{-# INLINEABLE csLastUpdateTime #-}
{-# DEPRECATED lastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead"  #-}

-- | The status of the channel.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatus :: Lens.Lens' ChannelSummary (Core.Maybe Types.ChannelStatus)
csStatus = Lens.field @"status"
{-# INLINEABLE csStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ChannelSummary where
        parseJSON
          = Core.withObject "ChannelSummary" Core.$
              \ x ->
                ChannelSummary' Core.<$>
                  (x Core..:? "channelName") Core.<*> x Core..:? "channelStorage"
                    Core.<*> x Core..:? "creationTime"
                    Core.<*> x Core..:? "lastMessageArrivalTime"
                    Core.<*> x Core..:? "lastUpdateTime"
                    Core.<*> x Core..:? "status"
