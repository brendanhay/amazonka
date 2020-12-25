{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Offering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Offering
  ( Offering (..),

    -- * Smart constructor
    mkOffering,

    -- * Lenses
    oArn,
    oCurrencyCode,
    oDuration,
    oDurationUnits,
    oFixedPrice,
    oOfferingDescription,
    oOfferingId,
    oOfferingType,
    oRegion,
    oResourceSpecification,
    oUsagePrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.OfferingDurationUnits as Types
import qualified Network.AWS.MediaLive.Types.OfferingType as Types
import qualified Network.AWS.MediaLive.Types.ReservationResourceSpecification as Types
import qualified Network.AWS.Prelude as Core

-- | Reserved resources available for purchase
--
-- /See:/ 'mkOffering' smart constructor.
data Offering = Offering'
  { -- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
    arn :: Core.Maybe Core.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
    currencyCode :: Core.Maybe Core.Text,
    -- | Lease duration, e.g. '12'
    duration :: Core.Maybe Core.Int,
    -- | Units for duration, e.g. 'MONTHS'
    durationUnits :: Core.Maybe Types.OfferingDurationUnits,
    -- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
    fixedPrice :: Core.Maybe Core.Double,
    -- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
    offeringDescription :: Core.Maybe Core.Text,
    -- | Unique offering ID, e.g. '87654321'
    offeringId :: Core.Maybe Core.Text,
    -- | Offering type, e.g. 'NO_UPFRONT'
    offeringType :: Core.Maybe Types.OfferingType,
    -- | AWS region, e.g. 'us-west-2'
    region :: Core.Maybe Core.Text,
    -- | Resource configuration details
    resourceSpecification :: Core.Maybe Types.ReservationResourceSpecification,
    -- | Recurring usage charge for each reserved resource, e.g. '157.0'
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Offering' value with any optional fields omitted.
mkOffering ::
  Offering
mkOffering =
  Offering'
    { arn = Core.Nothing,
      currencyCode = Core.Nothing,
      duration = Core.Nothing,
      durationUnits = Core.Nothing,
      fixedPrice = Core.Nothing,
      offeringDescription = Core.Nothing,
      offeringId = Core.Nothing,
      offeringType = Core.Nothing,
      region = Core.Nothing,
      resourceSpecification = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oArn :: Lens.Lens' Offering (Core.Maybe Core.Text)
oArn = Lens.field @"arn"
{-# DEPRECATED oArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCurrencyCode :: Lens.Lens' Offering (Core.Maybe Core.Text)
oCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED oCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDuration :: Lens.Lens' Offering (Core.Maybe Core.Int)
oDuration = Lens.field @"duration"
{-# DEPRECATED oDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDurationUnits :: Lens.Lens' Offering (Core.Maybe Types.OfferingDurationUnits)
oDurationUnits = Lens.field @"durationUnits"
{-# DEPRECATED oDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oFixedPrice :: Lens.Lens' Offering (Core.Maybe Core.Double)
oFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED oFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOfferingDescription :: Lens.Lens' Offering (Core.Maybe Core.Text)
oOfferingDescription = Lens.field @"offeringDescription"
{-# DEPRECATED oOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOfferingId :: Lens.Lens' Offering (Core.Maybe Core.Text)
oOfferingId = Lens.field @"offeringId"
{-# DEPRECATED oOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOfferingType :: Lens.Lens' Offering (Core.Maybe Types.OfferingType)
oOfferingType = Lens.field @"offeringType"
{-# DEPRECATED oOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oRegion :: Lens.Lens' Offering (Core.Maybe Core.Text)
oRegion = Lens.field @"region"
{-# DEPRECATED oRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oResourceSpecification :: Lens.Lens' Offering (Core.Maybe Types.ReservationResourceSpecification)
oResourceSpecification = Lens.field @"resourceSpecification"
{-# DEPRECATED oResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oUsagePrice :: Lens.Lens' Offering (Core.Maybe Core.Double)
oUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED oUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromJSON Offering where
  parseJSON =
    Core.withObject "Offering" Core.$
      \x ->
        Offering'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "currencyCode")
          Core.<*> (x Core..:? "duration")
          Core.<*> (x Core..:? "durationUnits")
          Core.<*> (x Core..:? "fixedPrice")
          Core.<*> (x Core..:? "offeringDescription")
          Core.<*> (x Core..:? "offeringId")
          Core.<*> (x Core..:? "offeringType")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "resourceSpecification")
          Core.<*> (x Core..:? "usagePrice")
