-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotMarketOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotMarketOptions
  ( SpotMarketOptions (..),

    -- * Smart constructor
    mkSpotMarketOptions,

    -- * Lenses
    smoBlockDurationMinutes,
    smoInstanceInterruptionBehavior,
    smoValidUntil,
    smoSpotInstanceType,
    smoMaxPrice,
  )
where

import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The options for Spot Instances.
--
-- /See:/ 'mkSpotMarketOptions' smart constructor.
data SpotMarketOptions = SpotMarketOptions'
  { blockDurationMinutes ::
      Lude.Maybe Lude.Int,
    instanceInterruptionBehavior ::
      Lude.Maybe InstanceInterruptionBehavior,
    validUntil :: Lude.Maybe Lude.ISO8601,
    spotInstanceType :: Lude.Maybe SpotInstanceType,
    maxPrice :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotMarketOptions' with the minimum fields required to make a request.
--
-- * 'blockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
-- You can't specify an Availability Zone group or a launch group if you specify a duration.
-- New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
-- * 'instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
-- * 'maxPrice' - The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
-- * 'spotInstanceType' - The Spot Instance request type. For <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances> , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
-- * 'validUntil' - The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). Supported only for persistent requests.
--
--
--     * For a persistent request, the request remains active until the @ValidUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
--
--
--     * For a one-time request, @ValidUntil@ is not supported. The request remains active until all instances launch or you cancel the request.
mkSpotMarketOptions ::
  SpotMarketOptions
mkSpotMarketOptions =
  SpotMarketOptions'
    { blockDurationMinutes = Lude.Nothing,
      instanceInterruptionBehavior = Lude.Nothing,
      validUntil = Lude.Nothing,
      spotInstanceType = Lude.Nothing,
      maxPrice = Lude.Nothing
    }

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
-- You can't specify an Availability Zone group or a launch group if you specify a duration.
-- New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoBlockDurationMinutes :: Lens.Lens' SpotMarketOptions (Lude.Maybe Lude.Int)
smoBlockDurationMinutes = Lens.lens (blockDurationMinutes :: SpotMarketOptions -> Lude.Maybe Lude.Int) (\s a -> s {blockDurationMinutes = a} :: SpotMarketOptions)
{-# DEPRECATED smoBlockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead." #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoInstanceInterruptionBehavior :: Lens.Lens' SpotMarketOptions (Lude.Maybe InstanceInterruptionBehavior)
smoInstanceInterruptionBehavior = Lens.lens (instanceInterruptionBehavior :: SpotMarketOptions -> Lude.Maybe InstanceInterruptionBehavior) (\s a -> s {instanceInterruptionBehavior = a} :: SpotMarketOptions)
{-# DEPRECATED smoInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). Supported only for persistent requests.
--
--
--     * For a persistent request, the request remains active until the @ValidUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
--
--
--     * For a one-time request, @ValidUntil@ is not supported. The request remains active until all instances launch or you cancel the request.
--
--
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoValidUntil :: Lens.Lens' SpotMarketOptions (Lude.Maybe Lude.ISO8601)
smoValidUntil = Lens.lens (validUntil :: SpotMarketOptions -> Lude.Maybe Lude.ISO8601) (\s a -> s {validUntil = a} :: SpotMarketOptions)
{-# DEPRECATED smoValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | The Spot Instance request type. For <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances> , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
--
-- /Note:/ Consider using 'spotInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoSpotInstanceType :: Lens.Lens' SpotMarketOptions (Lude.Maybe SpotInstanceType)
smoSpotInstanceType = Lens.lens (spotInstanceType :: SpotMarketOptions -> Lude.Maybe SpotInstanceType) (\s a -> s {spotInstanceType = a} :: SpotMarketOptions)
{-# DEPRECATED smoSpotInstanceType "Use generic-lens or generic-optics with 'spotInstanceType' instead." #-}

-- | The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoMaxPrice :: Lens.Lens' SpotMarketOptions (Lude.Maybe Lude.Text)
smoMaxPrice = Lens.lens (maxPrice :: SpotMarketOptions -> Lude.Maybe Lude.Text) (\s a -> s {maxPrice = a} :: SpotMarketOptions)
{-# DEPRECATED smoMaxPrice "Use generic-lens or generic-optics with 'maxPrice' instead." #-}

instance Lude.ToQuery SpotMarketOptions where
  toQuery SpotMarketOptions' {..} =
    Lude.mconcat
      [ "BlockDurationMinutes" Lude.=: blockDurationMinutes,
        "InstanceInterruptionBehavior"
          Lude.=: instanceInterruptionBehavior,
        "ValidUntil" Lude.=: validUntil,
        "SpotInstanceType" Lude.=: spotInstanceType,
        "MaxPrice" Lude.=: maxPrice
      ]
