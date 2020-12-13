{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
  ( LaunchTemplateSpotMarketOptionsRequest (..),

    -- * Smart constructor
    mkLaunchTemplateSpotMarketOptionsRequest,

    -- * Lenses
    ltsmorBlockDurationMinutes,
    ltsmorInstanceInterruptionBehavior,
    ltsmorValidUntil,
    ltsmorSpotInstanceType,
    ltsmorMaxPrice,
  )
where

import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The options for Spot Instances.
--
-- /See:/ 'mkLaunchTemplateSpotMarketOptionsRequest' smart constructor.
data LaunchTemplateSpotMarketOptionsRequest = LaunchTemplateSpotMarketOptionsRequest'
  { -- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
    blockDurationMinutes :: Lude.Maybe Lude.Int,
    -- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
    instanceInterruptionBehavior :: Lude.Maybe InstanceInterruptionBehavior,
    -- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
    validUntil :: Lude.Maybe Lude.DateTime,
    -- | The Spot Instance request type.
    spotInstanceType :: Lude.Maybe SpotInstanceType,
    -- | The maximum hourly price you're willing to pay for the Spot Instances.
    maxPrice :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateSpotMarketOptionsRequest' with the minimum fields required to make a request.
--
-- * 'blockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
-- * 'instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
-- * 'validUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
-- * 'spotInstanceType' - The Spot Instance request type.
-- * 'maxPrice' - The maximum hourly price you're willing to pay for the Spot Instances.
mkLaunchTemplateSpotMarketOptionsRequest ::
  LaunchTemplateSpotMarketOptionsRequest
mkLaunchTemplateSpotMarketOptionsRequest =
  LaunchTemplateSpotMarketOptionsRequest'
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
ltsmorBlockDurationMinutes :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Lude.Maybe Lude.Int)
ltsmorBlockDurationMinutes = Lens.lens (blockDurationMinutes :: LaunchTemplateSpotMarketOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {blockDurationMinutes = a} :: LaunchTemplateSpotMarketOptionsRequest)
{-# DEPRECATED ltsmorBlockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead." #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmorInstanceInterruptionBehavior :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Lude.Maybe InstanceInterruptionBehavior)
ltsmorInstanceInterruptionBehavior = Lens.lens (instanceInterruptionBehavior :: LaunchTemplateSpotMarketOptionsRequest -> Lude.Maybe InstanceInterruptionBehavior) (\s a -> s {instanceInterruptionBehavior = a} :: LaunchTemplateSpotMarketOptionsRequest)
{-# DEPRECATED ltsmorInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmorValidUntil :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Lude.Maybe Lude.DateTime)
ltsmorValidUntil = Lens.lens (validUntil :: LaunchTemplateSpotMarketOptionsRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {validUntil = a} :: LaunchTemplateSpotMarketOptionsRequest)
{-# DEPRECATED ltsmorValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | The Spot Instance request type.
--
-- /Note:/ Consider using 'spotInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmorSpotInstanceType :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Lude.Maybe SpotInstanceType)
ltsmorSpotInstanceType = Lens.lens (spotInstanceType :: LaunchTemplateSpotMarketOptionsRequest -> Lude.Maybe SpotInstanceType) (\s a -> s {spotInstanceType = a} :: LaunchTemplateSpotMarketOptionsRequest)
{-# DEPRECATED ltsmorSpotInstanceType "Use generic-lens or generic-optics with 'spotInstanceType' instead." #-}

-- | The maximum hourly price you're willing to pay for the Spot Instances.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsmorMaxPrice :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Lude.Maybe Lude.Text)
ltsmorMaxPrice = Lens.lens (maxPrice :: LaunchTemplateSpotMarketOptionsRequest -> Lude.Maybe Lude.Text) (\s a -> s {maxPrice = a} :: LaunchTemplateSpotMarketOptionsRequest)
{-# DEPRECATED ltsmorMaxPrice "Use generic-lens or generic-optics with 'maxPrice' instead." #-}

instance Lude.ToQuery LaunchTemplateSpotMarketOptionsRequest where
  toQuery LaunchTemplateSpotMarketOptionsRequest' {..} =
    Lude.mconcat
      [ "BlockDurationMinutes" Lude.=: blockDurationMinutes,
        "InstanceInterruptionBehavior"
          Lude.=: instanceInterruptionBehavior,
        "ValidUntil" Lude.=: validUntil,
        "SpotInstanceType" Lude.=: spotInstanceType,
        "MaxPrice" Lude.=: maxPrice
      ]
