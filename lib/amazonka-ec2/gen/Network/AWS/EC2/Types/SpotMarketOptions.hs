{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotMarketOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotMarketOptions
  ( SpotMarketOptions (..)
  -- * Smart constructor
  , mkSpotMarketOptions
  -- * Lenses
  , smoBlockDurationMinutes
  , smoInstanceInterruptionBehavior
  , smoMaxPrice
  , smoSpotInstanceType
  , smoValidUntil
  ) where

import qualified Network.AWS.EC2.Types.InstanceInterruptionBehavior as Types
import qualified Network.AWS.EC2.Types.SpotInstanceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The options for Spot Instances.
--
-- /See:/ 'mkSpotMarketOptions' smart constructor.
data SpotMarketOptions = SpotMarketOptions'
  { blockDurationMinutes :: Core.Maybe Core.Int
    -- ^ The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
-- You can't specify an Availability Zone group or a launch group if you specify a duration.
-- New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
  , instanceInterruptionBehavior :: Core.Maybe Types.InstanceInterruptionBehavior
    -- ^ The behavior when a Spot Instance is interrupted. The default is @terminate@ .
  , maxPrice :: Core.Maybe Core.Text
    -- ^ The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
  , spotInstanceType :: Core.Maybe Types.SpotInstanceType
    -- ^ The Spot Instance request type. For <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances> , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
  , validUntil :: Core.Maybe Core.UTCTime
    -- ^ The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). Supported only for persistent requests.
--
--
--     * For a persistent request, the request remains active until the @ValidUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
--
--
--     * For a one-time request, @ValidUntil@ is not supported. The request remains active until all instances launch or you cancel the request.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SpotMarketOptions' value with any optional fields omitted.
mkSpotMarketOptions
    :: SpotMarketOptions
mkSpotMarketOptions
  = SpotMarketOptions'{blockDurationMinutes = Core.Nothing,
                       instanceInterruptionBehavior = Core.Nothing,
                       maxPrice = Core.Nothing, spotInstanceType = Core.Nothing,
                       validUntil = Core.Nothing}

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
-- You can't specify an Availability Zone group or a launch group if you specify a duration.
-- New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoBlockDurationMinutes :: Lens.Lens' SpotMarketOptions (Core.Maybe Core.Int)
smoBlockDurationMinutes = Lens.field @"blockDurationMinutes"
{-# INLINEABLE smoBlockDurationMinutes #-}
{-# DEPRECATED blockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead"  #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoInstanceInterruptionBehavior :: Lens.Lens' SpotMarketOptions (Core.Maybe Types.InstanceInterruptionBehavior)
smoInstanceInterruptionBehavior = Lens.field @"instanceInterruptionBehavior"
{-# INLINEABLE smoInstanceInterruptionBehavior #-}
{-# DEPRECATED instanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead"  #-}

-- | The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoMaxPrice :: Lens.Lens' SpotMarketOptions (Core.Maybe Core.Text)
smoMaxPrice = Lens.field @"maxPrice"
{-# INLINEABLE smoMaxPrice #-}
{-# DEPRECATED maxPrice "Use generic-lens or generic-optics with 'maxPrice' instead"  #-}

-- | The Spot Instance request type. For <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances> , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
--
-- /Note:/ Consider using 'spotInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smoSpotInstanceType :: Lens.Lens' SpotMarketOptions (Core.Maybe Types.SpotInstanceType)
smoSpotInstanceType = Lens.field @"spotInstanceType"
{-# INLINEABLE smoSpotInstanceType #-}
{-# DEPRECATED spotInstanceType "Use generic-lens or generic-optics with 'spotInstanceType' instead"  #-}

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
smoValidUntil :: Lens.Lens' SpotMarketOptions (Core.Maybe Core.UTCTime)
smoValidUntil = Lens.field @"validUntil"
{-# INLINEABLE smoValidUntil #-}
{-# DEPRECATED validUntil "Use generic-lens or generic-optics with 'validUntil' instead"  #-}

instance Core.ToQuery SpotMarketOptions where
        toQuery SpotMarketOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "BlockDurationMinutes")
              blockDurationMinutes
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "InstanceInterruptionBehavior")
                instanceInterruptionBehavior
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxPrice") maxPrice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SpotInstanceType")
                spotInstanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ValidUntil") validUntil
