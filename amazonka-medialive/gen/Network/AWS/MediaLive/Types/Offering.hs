{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Offering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Offering where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
import Network.AWS.MediaLive.Types.ReservationResourceSpecification

-- | Reserved resources available for purchase
--
-- /See:/ 'newOffering' smart constructor.
data Offering = Offering'
  { -- | Lease duration, e.g. \'12\'
    duration :: Core.Maybe Core.Int,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Core.Maybe OfferingDurationUnits,
    -- | Unique offering ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
    arn :: Core.Maybe Core.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Core.Maybe Core.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Core.Maybe Core.Text,
    -- | Resource configuration details
    resourceSpecification :: Core.Maybe ReservationResourceSpecification,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Core.Maybe Core.Text,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Core.Maybe Core.Double,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Core.Maybe Core.Double,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Core.Maybe OfferingType,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Offering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'offering_duration' - Lease duration, e.g. \'12\'
--
-- 'durationUnits', 'offering_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'arn', 'offering_arn' - Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
--
-- 'offeringId', 'offering_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'currencyCode', 'offering_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'resourceSpecification', 'offering_resourceSpecification' - Resource configuration details
--
-- 'offeringDescription', 'offering_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'fixedPrice', 'offering_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'usagePrice', 'offering_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'offeringType', 'offering_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'region', 'offering_region' - AWS region, e.g. \'us-west-2\'
newOffering ::
  Offering
newOffering =
  Offering'
    { duration = Core.Nothing,
      durationUnits = Core.Nothing,
      arn = Core.Nothing,
      offeringId = Core.Nothing,
      currencyCode = Core.Nothing,
      resourceSpecification = Core.Nothing,
      offeringDescription = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      region = Core.Nothing
    }

-- | Lease duration, e.g. \'12\'
offering_duration :: Lens.Lens' Offering (Core.Maybe Core.Int)
offering_duration = Lens.lens (\Offering' {duration} -> duration) (\s@Offering' {} a -> s {duration = a} :: Offering)

-- | Units for duration, e.g. \'MONTHS\'
offering_durationUnits :: Lens.Lens' Offering (Core.Maybe OfferingDurationUnits)
offering_durationUnits = Lens.lens (\Offering' {durationUnits} -> durationUnits) (\s@Offering' {} a -> s {durationUnits = a} :: Offering)

-- | Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
offering_arn :: Lens.Lens' Offering (Core.Maybe Core.Text)
offering_arn = Lens.lens (\Offering' {arn} -> arn) (\s@Offering' {} a -> s {arn = a} :: Offering)

-- | Unique offering ID, e.g. \'87654321\'
offering_offeringId :: Lens.Lens' Offering (Core.Maybe Core.Text)
offering_offeringId = Lens.lens (\Offering' {offeringId} -> offeringId) (\s@Offering' {} a -> s {offeringId = a} :: Offering)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
offering_currencyCode :: Lens.Lens' Offering (Core.Maybe Core.Text)
offering_currencyCode = Lens.lens (\Offering' {currencyCode} -> currencyCode) (\s@Offering' {} a -> s {currencyCode = a} :: Offering)

-- | Resource configuration details
offering_resourceSpecification :: Lens.Lens' Offering (Core.Maybe ReservationResourceSpecification)
offering_resourceSpecification = Lens.lens (\Offering' {resourceSpecification} -> resourceSpecification) (\s@Offering' {} a -> s {resourceSpecification = a} :: Offering)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
offering_offeringDescription :: Lens.Lens' Offering (Core.Maybe Core.Text)
offering_offeringDescription = Lens.lens (\Offering' {offeringDescription} -> offeringDescription) (\s@Offering' {} a -> s {offeringDescription = a} :: Offering)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
offering_fixedPrice :: Lens.Lens' Offering (Core.Maybe Core.Double)
offering_fixedPrice = Lens.lens (\Offering' {fixedPrice} -> fixedPrice) (\s@Offering' {} a -> s {fixedPrice = a} :: Offering)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
offering_usagePrice :: Lens.Lens' Offering (Core.Maybe Core.Double)
offering_usagePrice = Lens.lens (\Offering' {usagePrice} -> usagePrice) (\s@Offering' {} a -> s {usagePrice = a} :: Offering)

-- | Offering type, e.g. \'NO_UPFRONT\'
offering_offeringType :: Lens.Lens' Offering (Core.Maybe OfferingType)
offering_offeringType = Lens.lens (\Offering' {offeringType} -> offeringType) (\s@Offering' {} a -> s {offeringType = a} :: Offering)

-- | AWS region, e.g. \'us-west-2\'
offering_region :: Lens.Lens' Offering (Core.Maybe Core.Text)
offering_region = Lens.lens (\Offering' {region} -> region) (\s@Offering' {} a -> s {region = a} :: Offering)

instance Core.FromJSON Offering where
  parseJSON =
    Core.withObject
      "Offering"
      ( \x ->
          Offering'
            Core.<$> (x Core..:? "duration")
            Core.<*> (x Core..:? "durationUnits")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "offeringId")
            Core.<*> (x Core..:? "currencyCode")
            Core.<*> (x Core..:? "resourceSpecification")
            Core.<*> (x Core..:? "offeringDescription")
            Core.<*> (x Core..:? "fixedPrice")
            Core.<*> (x Core..:? "usagePrice")
            Core.<*> (x Core..:? "offeringType")
            Core.<*> (x Core..:? "region")
      )

instance Core.Hashable Offering

instance Core.NFData Offering
