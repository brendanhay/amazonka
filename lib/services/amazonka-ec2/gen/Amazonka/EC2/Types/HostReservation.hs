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
-- Module      : Amazonka.EC2.Types.HostReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.HostReservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CurrencyCodeValues
import Amazonka.EC2.Types.PaymentOption
import Amazonka.EC2.Types.ReservationState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Details about the Dedicated Host Reservation and associated Dedicated
-- Hosts.
--
-- /See:/ 'newHostReservation' smart constructor.
data HostReservation = HostReservation'
  { -- | The number of Dedicated Hosts the reservation is associated with.
    count :: Prelude.Maybe Prelude.Int,
    -- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are
    -- specified. At this time, the only supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The length of the reservation\'s term, specified in seconds. Can be
    -- @31536000 (1 year)@ | @94608000 (3 years)@.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The date and time that the reservation ends.
    end :: Prelude.Maybe Data.ISO8601,
    -- | The IDs of the Dedicated Hosts associated with the reservation.
    hostIdSet :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the reservation that specifies the associated Dedicated Hosts.
    hostReservationId :: Prelude.Maybe Prelude.Text,
    -- | The hourly price of the reservation.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The instance family of the Dedicated Host Reservation. The instance
    -- family on the Dedicated Host must be the same in order for it to benefit
    -- from the reservation.
    instanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The ID of the reservation. This remains the same regardless of which
    -- Dedicated Hosts are associated with it.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The payment option selected for this reservation.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | The date and time that the reservation started.
    start :: Prelude.Maybe Data.ISO8601,
    -- | The state of the reservation.
    state :: Prelude.Maybe ReservationState,
    -- | Any tags assigned to the Dedicated Host Reservation.
    tags :: Prelude.Maybe [Tag],
    -- | The upfront price of the reservation.
    upfrontPrice :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'hostReservation_count' - The number of Dedicated Hosts the reservation is associated with.
--
-- 'currencyCode', 'hostReservation_currencyCode' - The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
--
-- 'duration', 'hostReservation_duration' - The length of the reservation\'s term, specified in seconds. Can be
-- @31536000 (1 year)@ | @94608000 (3 years)@.
--
-- 'end', 'hostReservation_end' - The date and time that the reservation ends.
--
-- 'hostIdSet', 'hostReservation_hostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
--
-- 'hostReservationId', 'hostReservation_hostReservationId' - The ID of the reservation that specifies the associated Dedicated Hosts.
--
-- 'hourlyPrice', 'hostReservation_hourlyPrice' - The hourly price of the reservation.
--
-- 'instanceFamily', 'hostReservation_instanceFamily' - The instance family of the Dedicated Host Reservation. The instance
-- family on the Dedicated Host must be the same in order for it to benefit
-- from the reservation.
--
-- 'offeringId', 'hostReservation_offeringId' - The ID of the reservation. This remains the same regardless of which
-- Dedicated Hosts are associated with it.
--
-- 'paymentOption', 'hostReservation_paymentOption' - The payment option selected for this reservation.
--
-- 'start', 'hostReservation_start' - The date and time that the reservation started.
--
-- 'state', 'hostReservation_state' - The state of the reservation.
--
-- 'tags', 'hostReservation_tags' - Any tags assigned to the Dedicated Host Reservation.
--
-- 'upfrontPrice', 'hostReservation_upfrontPrice' - The upfront price of the reservation.
newHostReservation ::
  HostReservation
newHostReservation =
  HostReservation'
    { count = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      duration = Prelude.Nothing,
      end = Prelude.Nothing,
      hostIdSet = Prelude.Nothing,
      hostReservationId = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      instanceFamily = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      start = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      upfrontPrice = Prelude.Nothing
    }

-- | The number of Dedicated Hosts the reservation is associated with.
hostReservation_count :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.Int)
hostReservation_count = Lens.lens (\HostReservation' {count} -> count) (\s@HostReservation' {} a -> s {count = a} :: HostReservation)

-- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
hostReservation_currencyCode :: Lens.Lens' HostReservation (Prelude.Maybe CurrencyCodeValues)
hostReservation_currencyCode = Lens.lens (\HostReservation' {currencyCode} -> currencyCode) (\s@HostReservation' {} a -> s {currencyCode = a} :: HostReservation)

-- | The length of the reservation\'s term, specified in seconds. Can be
-- @31536000 (1 year)@ | @94608000 (3 years)@.
hostReservation_duration :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.Int)
hostReservation_duration = Lens.lens (\HostReservation' {duration} -> duration) (\s@HostReservation' {} a -> s {duration = a} :: HostReservation)

-- | The date and time that the reservation ends.
hostReservation_end :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.UTCTime)
hostReservation_end = Lens.lens (\HostReservation' {end} -> end) (\s@HostReservation' {} a -> s {end = a} :: HostReservation) Prelude.. Lens.mapping Data._Time

-- | The IDs of the Dedicated Hosts associated with the reservation.
hostReservation_hostIdSet :: Lens.Lens' HostReservation (Prelude.Maybe [Prelude.Text])
hostReservation_hostIdSet = Lens.lens (\HostReservation' {hostIdSet} -> hostIdSet) (\s@HostReservation' {} a -> s {hostIdSet = a} :: HostReservation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the reservation that specifies the associated Dedicated Hosts.
hostReservation_hostReservationId :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.Text)
hostReservation_hostReservationId = Lens.lens (\HostReservation' {hostReservationId} -> hostReservationId) (\s@HostReservation' {} a -> s {hostReservationId = a} :: HostReservation)

-- | The hourly price of the reservation.
hostReservation_hourlyPrice :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.Text)
hostReservation_hourlyPrice = Lens.lens (\HostReservation' {hourlyPrice} -> hourlyPrice) (\s@HostReservation' {} a -> s {hourlyPrice = a} :: HostReservation)

-- | The instance family of the Dedicated Host Reservation. The instance
-- family on the Dedicated Host must be the same in order for it to benefit
-- from the reservation.
hostReservation_instanceFamily :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.Text)
hostReservation_instanceFamily = Lens.lens (\HostReservation' {instanceFamily} -> instanceFamily) (\s@HostReservation' {} a -> s {instanceFamily = a} :: HostReservation)

-- | The ID of the reservation. This remains the same regardless of which
-- Dedicated Hosts are associated with it.
hostReservation_offeringId :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.Text)
hostReservation_offeringId = Lens.lens (\HostReservation' {offeringId} -> offeringId) (\s@HostReservation' {} a -> s {offeringId = a} :: HostReservation)

-- | The payment option selected for this reservation.
hostReservation_paymentOption :: Lens.Lens' HostReservation (Prelude.Maybe PaymentOption)
hostReservation_paymentOption = Lens.lens (\HostReservation' {paymentOption} -> paymentOption) (\s@HostReservation' {} a -> s {paymentOption = a} :: HostReservation)

-- | The date and time that the reservation started.
hostReservation_start :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.UTCTime)
hostReservation_start = Lens.lens (\HostReservation' {start} -> start) (\s@HostReservation' {} a -> s {start = a} :: HostReservation) Prelude.. Lens.mapping Data._Time

-- | The state of the reservation.
hostReservation_state :: Lens.Lens' HostReservation (Prelude.Maybe ReservationState)
hostReservation_state = Lens.lens (\HostReservation' {state} -> state) (\s@HostReservation' {} a -> s {state = a} :: HostReservation)

-- | Any tags assigned to the Dedicated Host Reservation.
hostReservation_tags :: Lens.Lens' HostReservation (Prelude.Maybe [Tag])
hostReservation_tags = Lens.lens (\HostReservation' {tags} -> tags) (\s@HostReservation' {} a -> s {tags = a} :: HostReservation) Prelude.. Lens.mapping Lens.coerced

-- | The upfront price of the reservation.
hostReservation_upfrontPrice :: Lens.Lens' HostReservation (Prelude.Maybe Prelude.Text)
hostReservation_upfrontPrice = Lens.lens (\HostReservation' {upfrontPrice} -> upfrontPrice) (\s@HostReservation' {} a -> s {upfrontPrice = a} :: HostReservation)

instance Data.FromXML HostReservation where
  parseXML x =
    HostReservation'
      Prelude.<$> (x Data..@? "count")
      Prelude.<*> (x Data..@? "currencyCode")
      Prelude.<*> (x Data..@? "duration")
      Prelude.<*> (x Data..@? "end")
      Prelude.<*> ( x
                      Data..@? "hostIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "hostReservationId")
      Prelude.<*> (x Data..@? "hourlyPrice")
      Prelude.<*> (x Data..@? "instanceFamily")
      Prelude.<*> (x Data..@? "offeringId")
      Prelude.<*> (x Data..@? "paymentOption")
      Prelude.<*> (x Data..@? "start")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "upfrontPrice")

instance Prelude.Hashable HostReservation where
  hashWithSalt _salt HostReservation' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` hostIdSet
      `Prelude.hashWithSalt` hostReservationId
      `Prelude.hashWithSalt` hourlyPrice
      `Prelude.hashWithSalt` instanceFamily
      `Prelude.hashWithSalt` offeringId
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` upfrontPrice

instance Prelude.NFData HostReservation where
  rnf HostReservation' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf hostIdSet
      `Prelude.seq` Prelude.rnf hostReservationId
      `Prelude.seq` Prelude.rnf hourlyPrice
      `Prelude.seq` Prelude.rnf instanceFamily
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf upfrontPrice
