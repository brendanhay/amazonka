-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
  ( LaunchTemplateSpotMarketOptions (..),

    -- * Smart constructor
    mkLaunchTemplateSpotMarketOptions,

    -- * Lenses
    ltsmoBlockDurationMinutes,
    ltsmoInstanceInterruptionBehavior,
    ltsmoValidUntil,
    ltsmoSpotInstanceType,
    ltsmoMaxPrice,
  )
where

import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The options for Spot Instances.
--
-- /See:/ 'mkLaunchTemplateSpotMarketOptions' smart constructor.
data LaunchTemplateSpotMarketOptions = LaunchTemplateSpotMarketOptions'
  { blockDurationMinutes ::
      Lude.Maybe Lude.Int,
    instanceInterruptionBehavior ::
      Lude.Maybe
        InstanceInterruptionBehavior,
    validUntil ::
      Lude.Maybe Lude.ISO8601,
    spotInstanceType ::
      Lude.Maybe SpotInstanceType,
    maxPrice ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateSpotMarketOptions' with the minimum fields required to make a request.
--
-- * 'blockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
-- * 'instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
-- * 'maxPrice' - The maximum hourly price you're willing to pay for the Spot Instances.
-- * 'spotInstanceType' - The Spot Instance request type.
-- * 'validUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
mkLaunchTemplateSpotMarketOptions ::
  LaunchTemplateSpotMarketOptions
mkLaunchTemplateSpotMarketOptions =
  LaunchTemplateSpotMarketOptions'
    { blockDurationMinutes =
        Lude.Nothing,
      instanceInterruptionBehavior = Lude.Nothing,
      validUntil = Lude.Nothing,
      spotInstanceType = Lude.Nothing,
      maxPrice = Lude.Nothing
    }

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoBlockDurationMinutes :: Lens.Lens' LaunchTemplateSpotMarketOptions (Lude.Maybe Lude.Int)
ltsmoBlockDurationMinutes = Lens.lens (blockDurationMinutes :: LaunchTemplateSpotMarketOptions -> Lude.Maybe Lude.Int) (\s a -> s {blockDurationMinutes = a} :: LaunchTemplateSpotMarketOptions)
{-# DEPRECATED ltsmoBlockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead." #-}

-- | The behavior when a Spot Instance is interrupted.
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoInstanceInterruptionBehavior :: Lens.Lens' LaunchTemplateSpotMarketOptions (Lude.Maybe InstanceInterruptionBehavior)
ltsmoInstanceInterruptionBehavior = Lens.lens (instanceInterruptionBehavior :: LaunchTemplateSpotMarketOptions -> Lude.Maybe InstanceInterruptionBehavior) (\s a -> s {instanceInterruptionBehavior = a} :: LaunchTemplateSpotMarketOptions)
{-# DEPRECATED ltsmoInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoValidUntil :: Lens.Lens' LaunchTemplateSpotMarketOptions (Lude.Maybe Lude.ISO8601)
ltsmoValidUntil = Lens.lens (validUntil :: LaunchTemplateSpotMarketOptions -> Lude.Maybe Lude.ISO8601) (\s a -> s {validUntil = a} :: LaunchTemplateSpotMarketOptions)
{-# DEPRECATED ltsmoValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | The Spot Instance request type.
--
-- /Note:/ Consider using 'spotInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoSpotInstanceType :: Lens.Lens' LaunchTemplateSpotMarketOptions (Lude.Maybe SpotInstanceType)
ltsmoSpotInstanceType = Lens.lens (spotInstanceType :: LaunchTemplateSpotMarketOptions -> Lude.Maybe SpotInstanceType) (\s a -> s {spotInstanceType = a} :: LaunchTemplateSpotMarketOptions)
{-# DEPRECATED ltsmoSpotInstanceType "Use generic-lens or generic-optics with 'spotInstanceType' instead." #-}

-- | The maximum hourly price you're willing to pay for the Spot Instances.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmoMaxPrice :: Lens.Lens' LaunchTemplateSpotMarketOptions (Lude.Maybe Lude.Text)
ltsmoMaxPrice = Lens.lens (maxPrice :: LaunchTemplateSpotMarketOptions -> Lude.Maybe Lude.Text) (\s a -> s {maxPrice = a} :: LaunchTemplateSpotMarketOptions)
{-# DEPRECATED ltsmoMaxPrice "Use generic-lens or generic-optics with 'maxPrice' instead." #-}

instance Lude.FromXML LaunchTemplateSpotMarketOptions where
  parseXML x =
    LaunchTemplateSpotMarketOptions'
      Lude.<$> (x Lude..@? "blockDurationMinutes")
      Lude.<*> (x Lude..@? "instanceInterruptionBehavior")
      Lude.<*> (x Lude..@? "validUntil")
      Lude.<*> (x Lude..@? "spotInstanceType")
      Lude.<*> (x Lude..@? "maxPrice")
