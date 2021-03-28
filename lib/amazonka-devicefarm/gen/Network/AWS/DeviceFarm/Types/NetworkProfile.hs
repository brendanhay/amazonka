{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.NetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.NetworkProfile
  ( NetworkProfile (..)
  -- * Smart constructor
  , mkNetworkProfile
  -- * Lenses
  , npArn
  , npDescription
  , npDownlinkBandwidthBits
  , npDownlinkDelayMs
  , npDownlinkJitterMs
  , npDownlinkLossPercent
  , npName
  , npType
  , npUplinkBandwidthBits
  , npUplinkDelayMs
  , npUplinkJitterMs
  , npUplinkLossPercent
  ) where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.Description as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.NetworkProfileType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An array of settings that describes characteristics of a network profile.
--
-- /See:/ 'mkNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the network profile.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the network profile.
  , downlinkBandwidthBits :: Core.Maybe Core.Integer
    -- ^ The data throughput rate in bits per second, as an integer from 0 to 104857600.
  , downlinkDelayMs :: Core.Maybe Core.Integer
    -- ^ Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
  , downlinkJitterMs :: Core.Maybe Core.Integer
    -- ^ Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
  , downlinkLossPercent :: Core.Maybe Core.Natural
    -- ^ Proportion of received packets that fail to arrive from 0 to 100 percent.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the network profile.
  , type' :: Core.Maybe Types.NetworkProfileType
    -- ^ The type of network profile. Valid values are listed here.
  , uplinkBandwidthBits :: Core.Maybe Core.Integer
    -- ^ The data throughput rate in bits per second, as an integer from 0 to 104857600.
  , uplinkDelayMs :: Core.Maybe Core.Integer
    -- ^ Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
  , uplinkJitterMs :: Core.Maybe Core.Integer
    -- ^ Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
  , uplinkLossPercent :: Core.Maybe Core.Natural
    -- ^ Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkProfile' value with any optional fields omitted.
mkNetworkProfile
    :: NetworkProfile
mkNetworkProfile
  = NetworkProfile'{arn = Core.Nothing, description = Core.Nothing,
                    downlinkBandwidthBits = Core.Nothing,
                    downlinkDelayMs = Core.Nothing, downlinkJitterMs = Core.Nothing,
                    downlinkLossPercent = Core.Nothing, name = Core.Nothing,
                    type' = Core.Nothing, uplinkBandwidthBits = Core.Nothing,
                    uplinkDelayMs = Core.Nothing, uplinkJitterMs = Core.Nothing,
                    uplinkLossPercent = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the network profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npArn :: Lens.Lens' NetworkProfile (Core.Maybe Types.Arn)
npArn = Lens.field @"arn"
{-# INLINEABLE npArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The description of the network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDescription :: Lens.Lens' NetworkProfile (Core.Maybe Types.Description)
npDescription = Lens.field @"description"
{-# INLINEABLE npDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'downlinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkBandwidthBits :: Lens.Lens' NetworkProfile (Core.Maybe Core.Integer)
npDownlinkBandwidthBits = Lens.field @"downlinkBandwidthBits"
{-# INLINEABLE npDownlinkBandwidthBits #-}
{-# DEPRECATED downlinkBandwidthBits "Use generic-lens or generic-optics with 'downlinkBandwidthBits' instead"  #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkDelayMs :: Lens.Lens' NetworkProfile (Core.Maybe Core.Integer)
npDownlinkDelayMs = Lens.field @"downlinkDelayMs"
{-# INLINEABLE npDownlinkDelayMs #-}
{-# DEPRECATED downlinkDelayMs "Use generic-lens or generic-optics with 'downlinkDelayMs' instead"  #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkJitterMs :: Lens.Lens' NetworkProfile (Core.Maybe Core.Integer)
npDownlinkJitterMs = Lens.field @"downlinkJitterMs"
{-# INLINEABLE npDownlinkJitterMs #-}
{-# DEPRECATED downlinkJitterMs "Use generic-lens or generic-optics with 'downlinkJitterMs' instead"  #-}

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'downlinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkLossPercent :: Lens.Lens' NetworkProfile (Core.Maybe Core.Natural)
npDownlinkLossPercent = Lens.field @"downlinkLossPercent"
{-# INLINEABLE npDownlinkLossPercent #-}
{-# DEPRECATED downlinkLossPercent "Use generic-lens or generic-optics with 'downlinkLossPercent' instead"  #-}

-- | The name of the network profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npName :: Lens.Lens' NetworkProfile (Core.Maybe Types.Name)
npName = Lens.field @"name"
{-# INLINEABLE npName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of network profile. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npType :: Lens.Lens' NetworkProfile (Core.Maybe Types.NetworkProfileType)
npType = Lens.field @"type'"
{-# INLINEABLE npType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'uplinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkBandwidthBits :: Lens.Lens' NetworkProfile (Core.Maybe Core.Integer)
npUplinkBandwidthBits = Lens.field @"uplinkBandwidthBits"
{-# INLINEABLE npUplinkBandwidthBits #-}
{-# DEPRECATED uplinkBandwidthBits "Use generic-lens or generic-optics with 'uplinkBandwidthBits' instead"  #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkDelayMs :: Lens.Lens' NetworkProfile (Core.Maybe Core.Integer)
npUplinkDelayMs = Lens.field @"uplinkDelayMs"
{-# INLINEABLE npUplinkDelayMs #-}
{-# DEPRECATED uplinkDelayMs "Use generic-lens or generic-optics with 'uplinkDelayMs' instead"  #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkJitterMs :: Lens.Lens' NetworkProfile (Core.Maybe Core.Integer)
npUplinkJitterMs = Lens.field @"uplinkJitterMs"
{-# INLINEABLE npUplinkJitterMs #-}
{-# DEPRECATED uplinkJitterMs "Use generic-lens or generic-optics with 'uplinkJitterMs' instead"  #-}

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'uplinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkLossPercent :: Lens.Lens' NetworkProfile (Core.Maybe Core.Natural)
npUplinkLossPercent = Lens.field @"uplinkLossPercent"
{-# INLINEABLE npUplinkLossPercent #-}
{-# DEPRECATED uplinkLossPercent "Use generic-lens or generic-optics with 'uplinkLossPercent' instead"  #-}

instance Core.FromJSON NetworkProfile where
        parseJSON
          = Core.withObject "NetworkProfile" Core.$
              \ x ->
                NetworkProfile' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "description" Core.<*>
                    x Core..:? "downlinkBandwidthBits"
                    Core.<*> x Core..:? "downlinkDelayMs"
                    Core.<*> x Core..:? "downlinkJitterMs"
                    Core.<*> x Core..:? "downlinkLossPercent"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "type"
                    Core.<*> x Core..:? "uplinkBandwidthBits"
                    Core.<*> x Core..:? "uplinkDelayMs"
                    Core.<*> x Core..:? "uplinkJitterMs"
                    Core.<*> x Core..:? "uplinkLossPercent"
