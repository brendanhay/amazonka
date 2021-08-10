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
-- Module      : Network.AWS.MediaLive.Types.Reservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Reservation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import Network.AWS.MediaLive.Types.ReservationState
import qualified Network.AWS.Prelude as Prelude

-- | Reserved resources available to use
--
-- /See:/ 'newReservation' smart constructor.
data Reservation = Reservation'
  { -- | Reservation UTC end date and time in ISO-8601 format, e.g.
    -- \'2019-03-01T00:00:00\'
    end :: Prelude.Maybe Prelude.Text,
    -- | Lease duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Int,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Prelude.Maybe OfferingDurationUnits,
    -- | Unique reservation ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
    arn :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Resource configuration details
    resourceSpecification :: Prelude.Maybe ReservationResourceSpecification,
    -- | Current state of reservation, e.g. \'ACTIVE\'
    state :: Prelude.Maybe ReservationState,
    -- | User specified reservation name
    name :: Prelude.Maybe Prelude.Text,
    -- | A collection of key-value pairs
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Prelude.Maybe Prelude.Text,
    -- | Number of reserved resources
    count :: Prelude.Maybe Prelude.Int,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Prelude.Maybe OfferingType,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Prelude.Maybe Prelude.Text,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g.
    -- \'2018-03-01T00:00:00\'
    start :: Prelude.Maybe Prelude.Text,
    -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Reservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'reservation_end' - Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
--
-- 'duration', 'reservation_duration' - Lease duration, e.g. \'12\'
--
-- 'durationUnits', 'reservation_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'arn', 'reservation_arn' - Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
--
-- 'offeringId', 'reservation_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'currencyCode', 'reservation_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'resourceSpecification', 'reservation_resourceSpecification' - Resource configuration details
--
-- 'state', 'reservation_state' - Current state of reservation, e.g. \'ACTIVE\'
--
-- 'name', 'reservation_name' - User specified reservation name
--
-- 'tags', 'reservation_tags' - A collection of key-value pairs
--
-- 'offeringDescription', 'reservation_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'count', 'reservation_count' - Number of reserved resources
--
-- 'fixedPrice', 'reservation_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'usagePrice', 'reservation_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'offeringType', 'reservation_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'region', 'reservation_region' - AWS region, e.g. \'us-west-2\'
--
-- 'start', 'reservation_start' - Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
--
-- 'reservationId', 'reservation_reservationId' - Unique reservation ID, e.g. \'1234567\'
newReservation ::
  Reservation
newReservation =
  Reservation'
    { end = Prelude.Nothing,
      duration = Prelude.Nothing,
      durationUnits = Prelude.Nothing,
      arn = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      offeringDescription = Prelude.Nothing,
      count = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      region = Prelude.Nothing,
      start = Prelude.Nothing,
      reservationId = Prelude.Nothing
    }

-- | Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
reservation_end :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_end = Lens.lens (\Reservation' {end} -> end) (\s@Reservation' {} a -> s {end = a} :: Reservation)

-- | Lease duration, e.g. \'12\'
reservation_duration :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Int)
reservation_duration = Lens.lens (\Reservation' {duration} -> duration) (\s@Reservation' {} a -> s {duration = a} :: Reservation)

-- | Units for duration, e.g. \'MONTHS\'
reservation_durationUnits :: Lens.Lens' Reservation (Prelude.Maybe OfferingDurationUnits)
reservation_durationUnits = Lens.lens (\Reservation' {durationUnits} -> durationUnits) (\s@Reservation' {} a -> s {durationUnits = a} :: Reservation)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
reservation_arn :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_arn = Lens.lens (\Reservation' {arn} -> arn) (\s@Reservation' {} a -> s {arn = a} :: Reservation)

-- | Unique offering ID, e.g. \'87654321\'
reservation_offeringId :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_offeringId = Lens.lens (\Reservation' {offeringId} -> offeringId) (\s@Reservation' {} a -> s {offeringId = a} :: Reservation)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
reservation_currencyCode :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_currencyCode = Lens.lens (\Reservation' {currencyCode} -> currencyCode) (\s@Reservation' {} a -> s {currencyCode = a} :: Reservation)

-- | Resource configuration details
reservation_resourceSpecification :: Lens.Lens' Reservation (Prelude.Maybe ReservationResourceSpecification)
reservation_resourceSpecification = Lens.lens (\Reservation' {resourceSpecification} -> resourceSpecification) (\s@Reservation' {} a -> s {resourceSpecification = a} :: Reservation)

-- | Current state of reservation, e.g. \'ACTIVE\'
reservation_state :: Lens.Lens' Reservation (Prelude.Maybe ReservationState)
reservation_state = Lens.lens (\Reservation' {state} -> state) (\s@Reservation' {} a -> s {state = a} :: Reservation)

-- | User specified reservation name
reservation_name :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_name = Lens.lens (\Reservation' {name} -> name) (\s@Reservation' {} a -> s {name = a} :: Reservation)

-- | A collection of key-value pairs
reservation_tags :: Lens.Lens' Reservation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
reservation_tags = Lens.lens (\Reservation' {tags} -> tags) (\s@Reservation' {} a -> s {tags = a} :: Reservation) Prelude.. Lens.mapping Lens._Coerce

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
reservation_offeringDescription :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_offeringDescription = Lens.lens (\Reservation' {offeringDescription} -> offeringDescription) (\s@Reservation' {} a -> s {offeringDescription = a} :: Reservation)

-- | Number of reserved resources
reservation_count :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Int)
reservation_count = Lens.lens (\Reservation' {count} -> count) (\s@Reservation' {} a -> s {count = a} :: Reservation)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
reservation_fixedPrice :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Double)
reservation_fixedPrice = Lens.lens (\Reservation' {fixedPrice} -> fixedPrice) (\s@Reservation' {} a -> s {fixedPrice = a} :: Reservation)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
reservation_usagePrice :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Double)
reservation_usagePrice = Lens.lens (\Reservation' {usagePrice} -> usagePrice) (\s@Reservation' {} a -> s {usagePrice = a} :: Reservation)

-- | Offering type, e.g. \'NO_UPFRONT\'
reservation_offeringType :: Lens.Lens' Reservation (Prelude.Maybe OfferingType)
reservation_offeringType = Lens.lens (\Reservation' {offeringType} -> offeringType) (\s@Reservation' {} a -> s {offeringType = a} :: Reservation)

-- | AWS region, e.g. \'us-west-2\'
reservation_region :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_region = Lens.lens (\Reservation' {region} -> region) (\s@Reservation' {} a -> s {region = a} :: Reservation)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
reservation_start :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_start = Lens.lens (\Reservation' {start} -> start) (\s@Reservation' {} a -> s {start = a} :: Reservation)

-- | Unique reservation ID, e.g. \'1234567\'
reservation_reservationId :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_reservationId = Lens.lens (\Reservation' {reservationId} -> reservationId) (\s@Reservation' {} a -> s {reservationId = a} :: Reservation)

instance Core.FromJSON Reservation where
  parseJSON =
    Core.withObject
      "Reservation"
      ( \x ->
          Reservation'
            Prelude.<$> (x Core..:? "end")
            Prelude.<*> (x Core..:? "duration")
            Prelude.<*> (x Core..:? "durationUnits")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "offeringId")
            Prelude.<*> (x Core..:? "currencyCode")
            Prelude.<*> (x Core..:? "resourceSpecification")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "offeringDescription")
            Prelude.<*> (x Core..:? "count")
            Prelude.<*> (x Core..:? "fixedPrice")
            Prelude.<*> (x Core..:? "usagePrice")
            Prelude.<*> (x Core..:? "offeringType")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "start")
            Prelude.<*> (x Core..:? "reservationId")
      )

instance Prelude.Hashable Reservation

instance Prelude.NFData Reservation
