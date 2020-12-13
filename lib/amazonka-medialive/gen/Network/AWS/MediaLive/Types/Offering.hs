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
    oResourceSpecification,
    oCurrencyCode,
    oARN,
    oOfferingId,
    oRegion,
    oOfferingType,
    oUsagePrice,
    oFixedPrice,
    oDurationUnits,
    oOfferingDescription,
    oDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import qualified Network.AWS.Prelude as Lude

-- | Reserved resources available for purchase
--
-- /See:/ 'mkOffering' smart constructor.
data Offering = Offering'
  { -- | Resource configuration details
    resourceSpecification :: Lude.Maybe ReservationResourceSpecification,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
    currencyCode :: Lude.Maybe Lude.Text,
    -- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
    arn :: Lude.Maybe Lude.Text,
    -- | Unique offering ID, e.g. '87654321'
    offeringId :: Lude.Maybe Lude.Text,
    -- | AWS region, e.g. 'us-west-2'
    region :: Lude.Maybe Lude.Text,
    -- | Offering type, e.g. 'NO_UPFRONT'
    offeringType :: Lude.Maybe OfferingType,
    -- | Recurring usage charge for each reserved resource, e.g. '157.0'
    usagePrice :: Lude.Maybe Lude.Double,
    -- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | Units for duration, e.g. 'MONTHS'
    durationUnits :: Lude.Maybe OfferingDurationUnits,
    -- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
    offeringDescription :: Lude.Maybe Lude.Text,
    -- | Lease duration, e.g. '12'
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Offering' with the minimum fields required to make a request.
--
-- * 'resourceSpecification' - Resource configuration details
-- * 'currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
-- * 'arn' - Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
-- * 'offeringId' - Unique offering ID, e.g. '87654321'
-- * 'region' - AWS region, e.g. 'us-west-2'
-- * 'offeringType' - Offering type, e.g. 'NO_UPFRONT'
-- * 'usagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
-- * 'fixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
-- * 'durationUnits' - Units for duration, e.g. 'MONTHS'
-- * 'offeringDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
-- * 'duration' - Lease duration, e.g. '12'
mkOffering ::
  Offering
mkOffering =
  Offering'
    { resourceSpecification = Lude.Nothing,
      currencyCode = Lude.Nothing,
      arn = Lude.Nothing,
      offeringId = Lude.Nothing,
      region = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      durationUnits = Lude.Nothing,
      offeringDescription = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oResourceSpecification :: Lens.Lens' Offering (Lude.Maybe ReservationResourceSpecification)
oResourceSpecification = Lens.lens (resourceSpecification :: Offering -> Lude.Maybe ReservationResourceSpecification) (\s a -> s {resourceSpecification = a} :: Offering)
{-# DEPRECATED oResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCurrencyCode :: Lens.Lens' Offering (Lude.Maybe Lude.Text)
oCurrencyCode = Lens.lens (currencyCode :: Offering -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: Offering)
{-# DEPRECATED oCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oARN :: Lens.Lens' Offering (Lude.Maybe Lude.Text)
oARN = Lens.lens (arn :: Offering -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Offering)
{-# DEPRECATED oARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOfferingId :: Lens.Lens' Offering (Lude.Maybe Lude.Text)
oOfferingId = Lens.lens (offeringId :: Offering -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: Offering)
{-# DEPRECATED oOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oRegion :: Lens.Lens' Offering (Lude.Maybe Lude.Text)
oRegion = Lens.lens (region :: Offering -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Offering)
{-# DEPRECATED oRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOfferingType :: Lens.Lens' Offering (Lude.Maybe OfferingType)
oOfferingType = Lens.lens (offeringType :: Offering -> Lude.Maybe OfferingType) (\s a -> s {offeringType = a} :: Offering)
{-# DEPRECATED oOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oUsagePrice :: Lens.Lens' Offering (Lude.Maybe Lude.Double)
oUsagePrice = Lens.lens (usagePrice :: Offering -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: Offering)
{-# DEPRECATED oUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oFixedPrice :: Lens.Lens' Offering (Lude.Maybe Lude.Double)
oFixedPrice = Lens.lens (fixedPrice :: Offering -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: Offering)
{-# DEPRECATED oFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDurationUnits :: Lens.Lens' Offering (Lude.Maybe OfferingDurationUnits)
oDurationUnits = Lens.lens (durationUnits :: Offering -> Lude.Maybe OfferingDurationUnits) (\s a -> s {durationUnits = a} :: Offering)
{-# DEPRECATED oDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOfferingDescription :: Lens.Lens' Offering (Lude.Maybe Lude.Text)
oOfferingDescription = Lens.lens (offeringDescription :: Offering -> Lude.Maybe Lude.Text) (\s a -> s {offeringDescription = a} :: Offering)
{-# DEPRECATED oOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDuration :: Lens.Lens' Offering (Lude.Maybe Lude.Int)
oDuration = Lens.lens (duration :: Offering -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: Offering)
{-# DEPRECATED oDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromJSON Offering where
  parseJSON =
    Lude.withObject
      "Offering"
      ( \x ->
          Offering'
            Lude.<$> (x Lude..:? "resourceSpecification")
            Lude.<*> (x Lude..:? "currencyCode")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "offeringId")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "offeringType")
            Lude.<*> (x Lude..:? "usagePrice")
            Lude.<*> (x Lude..:? "fixedPrice")
            Lude.<*> (x Lude..:? "durationUnits")
            Lude.<*> (x Lude..:? "offeringDescription")
            Lude.<*> (x Lude..:? "duration")
      )
