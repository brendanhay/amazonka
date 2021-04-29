{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Reserved resources available for purchase
--
-- /See:/ 'newOffering' smart constructor.
data Offering = Offering'
  { -- | Lease duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Int,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Prelude.Maybe OfferingDurationUnits,
    -- | Unique offering ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
    arn :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Resource configuration details
    resourceSpecification :: Prelude.Maybe ReservationResourceSpecification,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Prelude.Maybe Prelude.Text,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Prelude.Maybe OfferingType,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { duration = Prelude.Nothing,
      durationUnits = Prelude.Nothing,
      arn = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      offeringDescription = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | Lease duration, e.g. \'12\'
offering_duration :: Lens.Lens' Offering (Prelude.Maybe Prelude.Int)
offering_duration = Lens.lens (\Offering' {duration} -> duration) (\s@Offering' {} a -> s {duration = a} :: Offering)

-- | Units for duration, e.g. \'MONTHS\'
offering_durationUnits :: Lens.Lens' Offering (Prelude.Maybe OfferingDurationUnits)
offering_durationUnits = Lens.lens (\Offering' {durationUnits} -> durationUnits) (\s@Offering' {} a -> s {durationUnits = a} :: Offering)

-- | Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
offering_arn :: Lens.Lens' Offering (Prelude.Maybe Prelude.Text)
offering_arn = Lens.lens (\Offering' {arn} -> arn) (\s@Offering' {} a -> s {arn = a} :: Offering)

-- | Unique offering ID, e.g. \'87654321\'
offering_offeringId :: Lens.Lens' Offering (Prelude.Maybe Prelude.Text)
offering_offeringId = Lens.lens (\Offering' {offeringId} -> offeringId) (\s@Offering' {} a -> s {offeringId = a} :: Offering)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
offering_currencyCode :: Lens.Lens' Offering (Prelude.Maybe Prelude.Text)
offering_currencyCode = Lens.lens (\Offering' {currencyCode} -> currencyCode) (\s@Offering' {} a -> s {currencyCode = a} :: Offering)

-- | Resource configuration details
offering_resourceSpecification :: Lens.Lens' Offering (Prelude.Maybe ReservationResourceSpecification)
offering_resourceSpecification = Lens.lens (\Offering' {resourceSpecification} -> resourceSpecification) (\s@Offering' {} a -> s {resourceSpecification = a} :: Offering)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
offering_offeringDescription :: Lens.Lens' Offering (Prelude.Maybe Prelude.Text)
offering_offeringDescription = Lens.lens (\Offering' {offeringDescription} -> offeringDescription) (\s@Offering' {} a -> s {offeringDescription = a} :: Offering)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
offering_fixedPrice :: Lens.Lens' Offering (Prelude.Maybe Prelude.Double)
offering_fixedPrice = Lens.lens (\Offering' {fixedPrice} -> fixedPrice) (\s@Offering' {} a -> s {fixedPrice = a} :: Offering)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
offering_usagePrice :: Lens.Lens' Offering (Prelude.Maybe Prelude.Double)
offering_usagePrice = Lens.lens (\Offering' {usagePrice} -> usagePrice) (\s@Offering' {} a -> s {usagePrice = a} :: Offering)

-- | Offering type, e.g. \'NO_UPFRONT\'
offering_offeringType :: Lens.Lens' Offering (Prelude.Maybe OfferingType)
offering_offeringType = Lens.lens (\Offering' {offeringType} -> offeringType) (\s@Offering' {} a -> s {offeringType = a} :: Offering)

-- | AWS region, e.g. \'us-west-2\'
offering_region :: Lens.Lens' Offering (Prelude.Maybe Prelude.Text)
offering_region = Lens.lens (\Offering' {region} -> region) (\s@Offering' {} a -> s {region = a} :: Offering)

instance Prelude.FromJSON Offering where
  parseJSON =
    Prelude.withObject
      "Offering"
      ( \x ->
          Offering'
            Prelude.<$> (x Prelude..:? "duration")
            Prelude.<*> (x Prelude..:? "durationUnits")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "offeringId")
            Prelude.<*> (x Prelude..:? "currencyCode")
            Prelude.<*> (x Prelude..:? "resourceSpecification")
            Prelude.<*> (x Prelude..:? "offeringDescription")
            Prelude.<*> (x Prelude..:? "fixedPrice")
            Prelude.<*> (x Prelude..:? "usagePrice")
            Prelude.<*> (x Prelude..:? "offeringType")
            Prelude.<*> (x Prelude..:? "region")
      )

instance Prelude.Hashable Offering

instance Prelude.NFData Offering
