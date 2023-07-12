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
-- Module      : Amazonka.MediaConnect.Types.Reservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Reservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.DurationUnits
import Amazonka.MediaConnect.Types.PriceUnits
import Amazonka.MediaConnect.Types.ReservationState
import Amazonka.MediaConnect.Types.ResourceSpecification
import qualified Amazonka.Prelude as Prelude

-- | A pricing agreement for a discounted rate for a specific outbound
-- bandwidth that your MediaConnect account will use each month over a
-- specific time period. The discounted rate in the reservation applies to
-- outbound bandwidth for all flows from your account until your account
-- reaches the amount of bandwidth in your reservation. If you use more
-- outbound bandwidth than the agreed upon amount in a single month, the
-- overage is charged at the on-demand rate.
--
-- /See:/ 'newReservation' smart constructor.
data Reservation = Reservation'
  { -- | The type of currency that is used for billing. The currencyCode used for
    -- your reservation is US dollars.
    currencyCode :: Prelude.Text,
    -- | The status of your reservation.
    reservationState :: ReservationState,
    -- | The Amazon Resource Name (ARN) that MediaConnect assigns to the
    -- offering.
    offeringArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that MediaConnect assigns to the
    -- reservation when you purchase an offering.
    reservationArn :: Prelude.Text,
    -- | The day and time that the reservation becomes active. You set this value
    -- when you purchase the offering.
    start :: Prelude.Text,
    -- | A description of the offering. MediaConnect defines this value in the
    -- offering.
    offeringDescription :: Prelude.Text,
    -- | The name that you assigned to the reservation when you purchased the
    -- offering.
    reservationName :: Prelude.Text,
    -- | The day and time that this reservation expires. This value is calculated
    -- based on the start date and time that you set and the offering\'s
    -- duration.
    end :: Prelude.Text,
    -- | The length of time that this reservation is active. MediaConnect defines
    -- this value in the offering.
    duration :: Prelude.Int,
    -- | The unit of measurement for the duration of the reservation.
    -- MediaConnect defines this value in the offering.
    durationUnits :: DurationUnits,
    -- | The cost of a single unit. This value, in combination with priceUnits,
    -- makes up the rate. MediaConnect defines this value in the offering.
    pricePerUnit :: Prelude.Text,
    -- | A definition of the amount of outbound bandwidth that you would be
    -- reserving if you purchase the offering. MediaConnect defines the values
    -- that make up the resourceSpecification in the offering.
    resourceSpecification :: ResourceSpecification,
    -- | The unit of measurement that is used for billing. This value, in
    -- combination with pricePerUnit, makes up the rate. MediaConnect defines
    -- this value in the offering.
    priceUnits :: PriceUnits
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
-- 'currencyCode', 'reservation_currencyCode' - The type of currency that is used for billing. The currencyCode used for
-- your reservation is US dollars.
--
-- 'reservationState', 'reservation_reservationState' - The status of your reservation.
--
-- 'offeringArn', 'reservation_offeringArn' - The Amazon Resource Name (ARN) that MediaConnect assigns to the
-- offering.
--
-- 'reservationArn', 'reservation_reservationArn' - The Amazon Resource Name (ARN) that MediaConnect assigns to the
-- reservation when you purchase an offering.
--
-- 'start', 'reservation_start' - The day and time that the reservation becomes active. You set this value
-- when you purchase the offering.
--
-- 'offeringDescription', 'reservation_offeringDescription' - A description of the offering. MediaConnect defines this value in the
-- offering.
--
-- 'reservationName', 'reservation_reservationName' - The name that you assigned to the reservation when you purchased the
-- offering.
--
-- 'end', 'reservation_end' - The day and time that this reservation expires. This value is calculated
-- based on the start date and time that you set and the offering\'s
-- duration.
--
-- 'duration', 'reservation_duration' - The length of time that this reservation is active. MediaConnect defines
-- this value in the offering.
--
-- 'durationUnits', 'reservation_durationUnits' - The unit of measurement for the duration of the reservation.
-- MediaConnect defines this value in the offering.
--
-- 'pricePerUnit', 'reservation_pricePerUnit' - The cost of a single unit. This value, in combination with priceUnits,
-- makes up the rate. MediaConnect defines this value in the offering.
--
-- 'resourceSpecification', 'reservation_resourceSpecification' - A definition of the amount of outbound bandwidth that you would be
-- reserving if you purchase the offering. MediaConnect defines the values
-- that make up the resourceSpecification in the offering.
--
-- 'priceUnits', 'reservation_priceUnits' - The unit of measurement that is used for billing. This value, in
-- combination with pricePerUnit, makes up the rate. MediaConnect defines
-- this value in the offering.
newReservation ::
  -- | 'currencyCode'
  Prelude.Text ->
  -- | 'reservationState'
  ReservationState ->
  -- | 'offeringArn'
  Prelude.Text ->
  -- | 'reservationArn'
  Prelude.Text ->
  -- | 'start'
  Prelude.Text ->
  -- | 'offeringDescription'
  Prelude.Text ->
  -- | 'reservationName'
  Prelude.Text ->
  -- | 'end'
  Prelude.Text ->
  -- | 'duration'
  Prelude.Int ->
  -- | 'durationUnits'
  DurationUnits ->
  -- | 'pricePerUnit'
  Prelude.Text ->
  -- | 'resourceSpecification'
  ResourceSpecification ->
  -- | 'priceUnits'
  PriceUnits ->
  Reservation
newReservation
  pCurrencyCode_
  pReservationState_
  pOfferingArn_
  pReservationArn_
  pStart_
  pOfferingDescription_
  pReservationName_
  pEnd_
  pDuration_
  pDurationUnits_
  pPricePerUnit_
  pResourceSpecification_
  pPriceUnits_ =
    Reservation'
      { currencyCode = pCurrencyCode_,
        reservationState = pReservationState_,
        offeringArn = pOfferingArn_,
        reservationArn = pReservationArn_,
        start = pStart_,
        offeringDescription = pOfferingDescription_,
        reservationName = pReservationName_,
        end = pEnd_,
        duration = pDuration_,
        durationUnits = pDurationUnits_,
        pricePerUnit = pPricePerUnit_,
        resourceSpecification = pResourceSpecification_,
        priceUnits = pPriceUnits_
      }

-- | The type of currency that is used for billing. The currencyCode used for
-- your reservation is US dollars.
reservation_currencyCode :: Lens.Lens' Reservation Prelude.Text
reservation_currencyCode = Lens.lens (\Reservation' {currencyCode} -> currencyCode) (\s@Reservation' {} a -> s {currencyCode = a} :: Reservation)

-- | The status of your reservation.
reservation_reservationState :: Lens.Lens' Reservation ReservationState
reservation_reservationState = Lens.lens (\Reservation' {reservationState} -> reservationState) (\s@Reservation' {} a -> s {reservationState = a} :: Reservation)

-- | The Amazon Resource Name (ARN) that MediaConnect assigns to the
-- offering.
reservation_offeringArn :: Lens.Lens' Reservation Prelude.Text
reservation_offeringArn = Lens.lens (\Reservation' {offeringArn} -> offeringArn) (\s@Reservation' {} a -> s {offeringArn = a} :: Reservation)

-- | The Amazon Resource Name (ARN) that MediaConnect assigns to the
-- reservation when you purchase an offering.
reservation_reservationArn :: Lens.Lens' Reservation Prelude.Text
reservation_reservationArn = Lens.lens (\Reservation' {reservationArn} -> reservationArn) (\s@Reservation' {} a -> s {reservationArn = a} :: Reservation)

-- | The day and time that the reservation becomes active. You set this value
-- when you purchase the offering.
reservation_start :: Lens.Lens' Reservation Prelude.Text
reservation_start = Lens.lens (\Reservation' {start} -> start) (\s@Reservation' {} a -> s {start = a} :: Reservation)

-- | A description of the offering. MediaConnect defines this value in the
-- offering.
reservation_offeringDescription :: Lens.Lens' Reservation Prelude.Text
reservation_offeringDescription = Lens.lens (\Reservation' {offeringDescription} -> offeringDescription) (\s@Reservation' {} a -> s {offeringDescription = a} :: Reservation)

-- | The name that you assigned to the reservation when you purchased the
-- offering.
reservation_reservationName :: Lens.Lens' Reservation Prelude.Text
reservation_reservationName = Lens.lens (\Reservation' {reservationName} -> reservationName) (\s@Reservation' {} a -> s {reservationName = a} :: Reservation)

-- | The day and time that this reservation expires. This value is calculated
-- based on the start date and time that you set and the offering\'s
-- duration.
reservation_end :: Lens.Lens' Reservation Prelude.Text
reservation_end = Lens.lens (\Reservation' {end} -> end) (\s@Reservation' {} a -> s {end = a} :: Reservation)

-- | The length of time that this reservation is active. MediaConnect defines
-- this value in the offering.
reservation_duration :: Lens.Lens' Reservation Prelude.Int
reservation_duration = Lens.lens (\Reservation' {duration} -> duration) (\s@Reservation' {} a -> s {duration = a} :: Reservation)

-- | The unit of measurement for the duration of the reservation.
-- MediaConnect defines this value in the offering.
reservation_durationUnits :: Lens.Lens' Reservation DurationUnits
reservation_durationUnits = Lens.lens (\Reservation' {durationUnits} -> durationUnits) (\s@Reservation' {} a -> s {durationUnits = a} :: Reservation)

-- | The cost of a single unit. This value, in combination with priceUnits,
-- makes up the rate. MediaConnect defines this value in the offering.
reservation_pricePerUnit :: Lens.Lens' Reservation Prelude.Text
reservation_pricePerUnit = Lens.lens (\Reservation' {pricePerUnit} -> pricePerUnit) (\s@Reservation' {} a -> s {pricePerUnit = a} :: Reservation)

-- | A definition of the amount of outbound bandwidth that you would be
-- reserving if you purchase the offering. MediaConnect defines the values
-- that make up the resourceSpecification in the offering.
reservation_resourceSpecification :: Lens.Lens' Reservation ResourceSpecification
reservation_resourceSpecification = Lens.lens (\Reservation' {resourceSpecification} -> resourceSpecification) (\s@Reservation' {} a -> s {resourceSpecification = a} :: Reservation)

-- | The unit of measurement that is used for billing. This value, in
-- combination with pricePerUnit, makes up the rate. MediaConnect defines
-- this value in the offering.
reservation_priceUnits :: Lens.Lens' Reservation PriceUnits
reservation_priceUnits = Lens.lens (\Reservation' {priceUnits} -> priceUnits) (\s@Reservation' {} a -> s {priceUnits = a} :: Reservation)

instance Data.FromJSON Reservation where
  parseJSON =
    Data.withObject
      "Reservation"
      ( \x ->
          Reservation'
            Prelude.<$> (x Data..: "currencyCode")
            Prelude.<*> (x Data..: "reservationState")
            Prelude.<*> (x Data..: "offeringArn")
            Prelude.<*> (x Data..: "reservationArn")
            Prelude.<*> (x Data..: "start")
            Prelude.<*> (x Data..: "offeringDescription")
            Prelude.<*> (x Data..: "reservationName")
            Prelude.<*> (x Data..: "end")
            Prelude.<*> (x Data..: "duration")
            Prelude.<*> (x Data..: "durationUnits")
            Prelude.<*> (x Data..: "pricePerUnit")
            Prelude.<*> (x Data..: "resourceSpecification")
            Prelude.<*> (x Data..: "priceUnits")
      )

instance Prelude.Hashable Reservation where
  hashWithSalt _salt Reservation' {..} =
    _salt
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` reservationState
      `Prelude.hashWithSalt` offeringArn
      `Prelude.hashWithSalt` reservationArn
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` offeringDescription
      `Prelude.hashWithSalt` reservationName
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` durationUnits
      `Prelude.hashWithSalt` pricePerUnit
      `Prelude.hashWithSalt` resourceSpecification
      `Prelude.hashWithSalt` priceUnits

instance Prelude.NFData Reservation where
  rnf Reservation' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf reservationState
      `Prelude.seq` Prelude.rnf offeringArn
      `Prelude.seq` Prelude.rnf reservationArn
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf offeringDescription
      `Prelude.seq` Prelude.rnf reservationName
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf durationUnits
      `Prelude.seq` Prelude.rnf pricePerUnit
      `Prelude.seq` Prelude.rnf resourceSpecification
      `Prelude.seq` Prelude.rnf priceUnits
