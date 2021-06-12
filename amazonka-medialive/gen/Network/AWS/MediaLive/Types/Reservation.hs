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

-- | Reserved resources available to use
--
-- /See:/ 'newReservation' smart constructor.
data Reservation = Reservation'
  { -- | Reservation UTC end date and time in ISO-8601 format, e.g.
    -- \'2019-03-01T00:00:00\'
    end :: Core.Maybe Core.Text,
    -- | Lease duration, e.g. \'12\'
    duration :: Core.Maybe Core.Int,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Core.Maybe OfferingDurationUnits,
    -- | Unique reservation ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
    arn :: Core.Maybe Core.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Core.Maybe Core.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Core.Maybe Core.Text,
    -- | Resource configuration details
    resourceSpecification :: Core.Maybe ReservationResourceSpecification,
    -- | Current state of reservation, e.g. \'ACTIVE\'
    state :: Core.Maybe ReservationState,
    -- | User specified reservation name
    name :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Core.Maybe Core.Text,
    -- | Number of reserved resources
    count :: Core.Maybe Core.Int,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Core.Maybe Core.Double,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Core.Maybe Core.Double,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Core.Maybe OfferingType,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Core.Maybe Core.Text,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g.
    -- \'2018-03-01T00:00:00\'
    start :: Core.Maybe Core.Text,
    -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { end = Core.Nothing,
      duration = Core.Nothing,
      durationUnits = Core.Nothing,
      arn = Core.Nothing,
      offeringId = Core.Nothing,
      currencyCode = Core.Nothing,
      resourceSpecification = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      offeringDescription = Core.Nothing,
      count = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      region = Core.Nothing,
      start = Core.Nothing,
      reservationId = Core.Nothing
    }

-- | Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
reservation_end :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_end = Lens.lens (\Reservation' {end} -> end) (\s@Reservation' {} a -> s {end = a} :: Reservation)

-- | Lease duration, e.g. \'12\'
reservation_duration :: Lens.Lens' Reservation (Core.Maybe Core.Int)
reservation_duration = Lens.lens (\Reservation' {duration} -> duration) (\s@Reservation' {} a -> s {duration = a} :: Reservation)

-- | Units for duration, e.g. \'MONTHS\'
reservation_durationUnits :: Lens.Lens' Reservation (Core.Maybe OfferingDurationUnits)
reservation_durationUnits = Lens.lens (\Reservation' {durationUnits} -> durationUnits) (\s@Reservation' {} a -> s {durationUnits = a} :: Reservation)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
reservation_arn :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_arn = Lens.lens (\Reservation' {arn} -> arn) (\s@Reservation' {} a -> s {arn = a} :: Reservation)

-- | Unique offering ID, e.g. \'87654321\'
reservation_offeringId :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_offeringId = Lens.lens (\Reservation' {offeringId} -> offeringId) (\s@Reservation' {} a -> s {offeringId = a} :: Reservation)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
reservation_currencyCode :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_currencyCode = Lens.lens (\Reservation' {currencyCode} -> currencyCode) (\s@Reservation' {} a -> s {currencyCode = a} :: Reservation)

-- | Resource configuration details
reservation_resourceSpecification :: Lens.Lens' Reservation (Core.Maybe ReservationResourceSpecification)
reservation_resourceSpecification = Lens.lens (\Reservation' {resourceSpecification} -> resourceSpecification) (\s@Reservation' {} a -> s {resourceSpecification = a} :: Reservation)

-- | Current state of reservation, e.g. \'ACTIVE\'
reservation_state :: Lens.Lens' Reservation (Core.Maybe ReservationState)
reservation_state = Lens.lens (\Reservation' {state} -> state) (\s@Reservation' {} a -> s {state = a} :: Reservation)

-- | User specified reservation name
reservation_name :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_name = Lens.lens (\Reservation' {name} -> name) (\s@Reservation' {} a -> s {name = a} :: Reservation)

-- | A collection of key-value pairs
reservation_tags :: Lens.Lens' Reservation (Core.Maybe (Core.HashMap Core.Text Core.Text))
reservation_tags = Lens.lens (\Reservation' {tags} -> tags) (\s@Reservation' {} a -> s {tags = a} :: Reservation) Core.. Lens.mapping Lens._Coerce

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
reservation_offeringDescription :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_offeringDescription = Lens.lens (\Reservation' {offeringDescription} -> offeringDescription) (\s@Reservation' {} a -> s {offeringDescription = a} :: Reservation)

-- | Number of reserved resources
reservation_count :: Lens.Lens' Reservation (Core.Maybe Core.Int)
reservation_count = Lens.lens (\Reservation' {count} -> count) (\s@Reservation' {} a -> s {count = a} :: Reservation)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
reservation_fixedPrice :: Lens.Lens' Reservation (Core.Maybe Core.Double)
reservation_fixedPrice = Lens.lens (\Reservation' {fixedPrice} -> fixedPrice) (\s@Reservation' {} a -> s {fixedPrice = a} :: Reservation)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
reservation_usagePrice :: Lens.Lens' Reservation (Core.Maybe Core.Double)
reservation_usagePrice = Lens.lens (\Reservation' {usagePrice} -> usagePrice) (\s@Reservation' {} a -> s {usagePrice = a} :: Reservation)

-- | Offering type, e.g. \'NO_UPFRONT\'
reservation_offeringType :: Lens.Lens' Reservation (Core.Maybe OfferingType)
reservation_offeringType = Lens.lens (\Reservation' {offeringType} -> offeringType) (\s@Reservation' {} a -> s {offeringType = a} :: Reservation)

-- | AWS region, e.g. \'us-west-2\'
reservation_region :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_region = Lens.lens (\Reservation' {region} -> region) (\s@Reservation' {} a -> s {region = a} :: Reservation)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
reservation_start :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_start = Lens.lens (\Reservation' {start} -> start) (\s@Reservation' {} a -> s {start = a} :: Reservation)

-- | Unique reservation ID, e.g. \'1234567\'
reservation_reservationId :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_reservationId = Lens.lens (\Reservation' {reservationId} -> reservationId) (\s@Reservation' {} a -> s {reservationId = a} :: Reservation)

instance Core.FromJSON Reservation where
  parseJSON =
    Core.withObject
      "Reservation"
      ( \x ->
          Reservation'
            Core.<$> (x Core..:? "end")
            Core.<*> (x Core..:? "duration")
            Core.<*> (x Core..:? "durationUnits")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "offeringId")
            Core.<*> (x Core..:? "currencyCode")
            Core.<*> (x Core..:? "resourceSpecification")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "offeringDescription")
            Core.<*> (x Core..:? "count")
            Core.<*> (x Core..:? "fixedPrice")
            Core.<*> (x Core..:? "usagePrice")
            Core.<*> (x Core..:? "offeringType")
            Core.<*> (x Core..:? "region")
            Core.<*> (x Core..:? "start")
            Core.<*> (x Core..:? "reservationId")
      )

instance Core.Hashable Reservation

instance Core.NFData Reservation
