{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
  ( LaunchTemplateSpotMarketOptions (..)
  -- * Smart constructor
  , mkLaunchTemplateSpotMarketOptions
  -- * Lenses
  , ltsmoBlockDurationMinutes
  , ltsmoInstanceInterruptionBehavior
  , ltsmoMaxPrice
  , ltsmoSpotInstanceType
  , ltsmoValidUntil
  ) where

import qualified Network.AWS.EC2.Types.InstanceInterruptionBehavior as Types
import qualified Network.AWS.EC2.Types.SpotInstanceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The options for Spot Instances.
--
-- /See:/ 'mkLaunchTemplateSpotMarketOptions' smart constructor.
data LaunchTemplateSpotMarketOptions = LaunchTemplateSpotMarketOptions'
  { blockDurationMinutes :: Core.Maybe Core.Int
    -- ^ The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
  , instanceInterruptionBehavior :: Core.Maybe Types.InstanceInterruptionBehavior
    -- ^ The behavior when a Spot Instance is interrupted.
  , maxPrice :: Core.Maybe Core.Text
    -- ^ The maximum hourly price you're willing to pay for the Spot Instances.
  , spotInstanceType :: Core.Maybe Types.SpotInstanceType
    -- ^ The Spot Instance request type.
  , validUntil :: Core.Maybe Core.UTCTime
    -- ^ The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LaunchTemplateSpotMarketOptions' value with any optional fields omitted.
mkLaunchTemplateSpotMarketOptions
    :: LaunchTemplateSpotMarketOptions
mkLaunchTemplateSpotMarketOptions
  = LaunchTemplateSpotMarketOptions'{blockDurationMinutes =
                                       Core.Nothing,
                                     instanceInterruptionBehavior = Core.Nothing,
                                     maxPrice = Core.Nothing, spotInstanceType = Core.Nothing,
                                     validUntil = Core.Nothing}

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoBlockDurationMinutes :: Lens.Lens' LaunchTemplateSpotMarketOptions (Core.Maybe Core.Int)
ltsmoBlockDurationMinutes = Lens.field @"blockDurationMinutes"
{-# INLINEABLE ltsmoBlockDurationMinutes #-}
{-# DEPRECATED blockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead"  #-}

-- | The behavior when a Spot Instance is interrupted.
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoInstanceInterruptionBehavior :: Lens.Lens' LaunchTemplateSpotMarketOptions (Core.Maybe Types.InstanceInterruptionBehavior)
ltsmoInstanceInterruptionBehavior = Lens.field @"instanceInterruptionBehavior"
{-# INLINEABLE ltsmoInstanceInterruptionBehavior #-}
{-# DEPRECATED instanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead"  #-}

-- | The maximum hourly price you're willing to pay for the Spot Instances.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoMaxPrice :: Lens.Lens' LaunchTemplateSpotMarketOptions (Core.Maybe Core.Text)
ltsmoMaxPrice = Lens.field @"maxPrice"
{-# INLINEABLE ltsmoMaxPrice #-}
{-# DEPRECATED maxPrice "Use generic-lens or generic-optics with 'maxPrice' instead"  #-}

-- | The Spot Instance request type.
--
-- /Note:/ Consider using 'spotInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoSpotInstanceType :: Lens.Lens' LaunchTemplateSpotMarketOptions (Core.Maybe Types.SpotInstanceType)
ltsmoSpotInstanceType = Lens.field @"spotInstanceType"
{-# INLINEABLE ltsmoSpotInstanceType #-}
{-# DEPRECATED spotInstanceType "Use generic-lens or generic-optics with 'spotInstanceType' instead"  #-}

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoValidUntil :: Lens.Lens' LaunchTemplateSpotMarketOptions (Core.Maybe Core.UTCTime)
ltsmoValidUntil = Lens.field @"validUntil"
{-# INLINEABLE ltsmoValidUntil #-}
{-# DEPRECATED validUntil "Use generic-lens or generic-optics with 'validUntil' instead"  #-}

instance Core.FromXML LaunchTemplateSpotMarketOptions where
        parseXML x
          = LaunchTemplateSpotMarketOptions' Core.<$>
              (x Core..@? "blockDurationMinutes") Core.<*>
                x Core..@? "instanceInterruptionBehavior"
                Core.<*> x Core..@? "maxPrice"
                Core.<*> x Core..@? "spotInstanceType"
                Core.<*> x Core..@? "validUntil"
