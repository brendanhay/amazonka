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
-- Module      : Amazonka.MediaLive.Types.Reservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Reservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.OfferingDurationUnits
import Amazonka.MediaLive.Types.OfferingType
import Amazonka.MediaLive.Types.ReservationResourceSpecification
import Amazonka.MediaLive.Types.ReservationState
import qualified Amazonka.Prelude as Prelude

-- | Reserved resources available to use
--
-- /See:/ 'newReservation' smart constructor.
data Reservation = Reservation'
  { -- | Current state of reservation, e.g. \'ACTIVE\'
    state :: Prelude.Maybe ReservationState,
    -- | Resource configuration details
    resourceSpecification :: Prelude.Maybe ReservationResourceSpecification,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Unique reservation ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
    arn :: Prelude.Maybe Prelude.Text,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g.
    -- \'2018-03-01T00:00:00\'
    start :: Prelude.Maybe Prelude.Text,
    -- | Number of reserved resources
    count :: Prelude.Maybe Prelude.Int,
    -- | Reservation UTC end date and time in ISO-8601 format, e.g.
    -- \'2019-03-01T00:00:00\'
    end :: Prelude.Maybe Prelude.Text,
    -- | User specified reservation name
    name :: Prelude.Maybe Prelude.Text,
    -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Prelude.Maybe Prelude.Text,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Prelude.Maybe OfferingType,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Prelude.Maybe OfferingDurationUnits,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Prelude.Maybe Prelude.Text,
    -- | Lease duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Int,
    -- | A collection of key-value pairs
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'state', 'reservation_state' - Current state of reservation, e.g. \'ACTIVE\'
--
-- 'resourceSpecification', 'reservation_resourceSpecification' - Resource configuration details
--
-- 'currencyCode', 'reservation_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'arn', 'reservation_arn' - Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
--
-- 'start', 'reservation_start' - Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
--
-- 'count', 'reservation_count' - Number of reserved resources
--
-- 'end', 'reservation_end' - Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
--
-- 'name', 'reservation_name' - User specified reservation name
--
-- 'reservationId', 'reservation_reservationId' - Unique reservation ID, e.g. \'1234567\'
--
-- 'offeringId', 'reservation_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'region', 'reservation_region' - AWS region, e.g. \'us-west-2\'
--
-- 'offeringType', 'reservation_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'usagePrice', 'reservation_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'fixedPrice', 'reservation_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'durationUnits', 'reservation_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'offeringDescription', 'reservation_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'duration', 'reservation_duration' - Lease duration, e.g. \'12\'
--
-- 'tags', 'reservation_tags' - A collection of key-value pairs
newReservation ::
  Reservation
newReservation =
  Reservation'
    { state = Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      arn = Prelude.Nothing,
      start = Prelude.Nothing,
      count = Prelude.Nothing,
      end = Prelude.Nothing,
      name = Prelude.Nothing,
      reservationId = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      region = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      durationUnits = Prelude.Nothing,
      offeringDescription = Prelude.Nothing,
      duration = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Current state of reservation, e.g. \'ACTIVE\'
reservation_state :: Lens.Lens' Reservation (Prelude.Maybe ReservationState)
reservation_state = Lens.lens (\Reservation' {state} -> state) (\s@Reservation' {} a -> s {state = a} :: Reservation)

-- | Resource configuration details
reservation_resourceSpecification :: Lens.Lens' Reservation (Prelude.Maybe ReservationResourceSpecification)
reservation_resourceSpecification = Lens.lens (\Reservation' {resourceSpecification} -> resourceSpecification) (\s@Reservation' {} a -> s {resourceSpecification = a} :: Reservation)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
reservation_currencyCode :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_currencyCode = Lens.lens (\Reservation' {currencyCode} -> currencyCode) (\s@Reservation' {} a -> s {currencyCode = a} :: Reservation)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
reservation_arn :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_arn = Lens.lens (\Reservation' {arn} -> arn) (\s@Reservation' {} a -> s {arn = a} :: Reservation)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
reservation_start :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_start = Lens.lens (\Reservation' {start} -> start) (\s@Reservation' {} a -> s {start = a} :: Reservation)

-- | Number of reserved resources
reservation_count :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Int)
reservation_count = Lens.lens (\Reservation' {count} -> count) (\s@Reservation' {} a -> s {count = a} :: Reservation)

-- | Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
reservation_end :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_end = Lens.lens (\Reservation' {end} -> end) (\s@Reservation' {} a -> s {end = a} :: Reservation)

-- | User specified reservation name
reservation_name :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_name = Lens.lens (\Reservation' {name} -> name) (\s@Reservation' {} a -> s {name = a} :: Reservation)

-- | Unique reservation ID, e.g. \'1234567\'
reservation_reservationId :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_reservationId = Lens.lens (\Reservation' {reservationId} -> reservationId) (\s@Reservation' {} a -> s {reservationId = a} :: Reservation)

-- | Unique offering ID, e.g. \'87654321\'
reservation_offeringId :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_offeringId = Lens.lens (\Reservation' {offeringId} -> offeringId) (\s@Reservation' {} a -> s {offeringId = a} :: Reservation)

-- | AWS region, e.g. \'us-west-2\'
reservation_region :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_region = Lens.lens (\Reservation' {region} -> region) (\s@Reservation' {} a -> s {region = a} :: Reservation)

-- | Offering type, e.g. \'NO_UPFRONT\'
reservation_offeringType :: Lens.Lens' Reservation (Prelude.Maybe OfferingType)
reservation_offeringType = Lens.lens (\Reservation' {offeringType} -> offeringType) (\s@Reservation' {} a -> s {offeringType = a} :: Reservation)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
reservation_usagePrice :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Double)
reservation_usagePrice = Lens.lens (\Reservation' {usagePrice} -> usagePrice) (\s@Reservation' {} a -> s {usagePrice = a} :: Reservation)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
reservation_fixedPrice :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Double)
reservation_fixedPrice = Lens.lens (\Reservation' {fixedPrice} -> fixedPrice) (\s@Reservation' {} a -> s {fixedPrice = a} :: Reservation)

-- | Units for duration, e.g. \'MONTHS\'
reservation_durationUnits :: Lens.Lens' Reservation (Prelude.Maybe OfferingDurationUnits)
reservation_durationUnits = Lens.lens (\Reservation' {durationUnits} -> durationUnits) (\s@Reservation' {} a -> s {durationUnits = a} :: Reservation)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
reservation_offeringDescription :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_offeringDescription = Lens.lens (\Reservation' {offeringDescription} -> offeringDescription) (\s@Reservation' {} a -> s {offeringDescription = a} :: Reservation)

-- | Lease duration, e.g. \'12\'
reservation_duration :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Int)
reservation_duration = Lens.lens (\Reservation' {duration} -> duration) (\s@Reservation' {} a -> s {duration = a} :: Reservation)

-- | A collection of key-value pairs
reservation_tags :: Lens.Lens' Reservation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
reservation_tags = Lens.lens (\Reservation' {tags} -> tags) (\s@Reservation' {} a -> s {tags = a} :: Reservation) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Reservation where
  parseJSON =
    Core.withObject
      "Reservation"
      ( \x ->
          Reservation'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "resourceSpecification")
            Prelude.<*> (x Core..:? "currencyCode")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "start")
            Prelude.<*> (x Core..:? "count")
            Prelude.<*> (x Core..:? "end")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "reservationId")
            Prelude.<*> (x Core..:? "offeringId")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "offeringType")
            Prelude.<*> (x Core..:? "usagePrice")
            Prelude.<*> (x Core..:? "fixedPrice")
            Prelude.<*> (x Core..:? "durationUnits")
            Prelude.<*> (x Core..:? "offeringDescription")
            Prelude.<*> (x Core..:? "duration")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Reservation where
  hashWithSalt _salt Reservation' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` resourceSpecification
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reservationId
      `Prelude.hashWithSalt` offeringId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` usagePrice
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` durationUnits
      `Prelude.hashWithSalt` offeringDescription
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Reservation where
  rnf Reservation' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf resourceSpecification
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reservationId
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf usagePrice
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf durationUnits
      `Prelude.seq` Prelude.rnf offeringDescription
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf tags
