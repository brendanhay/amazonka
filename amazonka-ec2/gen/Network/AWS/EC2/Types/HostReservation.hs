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
-- Module      : Network.AWS.EC2.Types.HostReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostReservation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import Network.AWS.EC2.Types.ReservationState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Details about the Dedicated Host Reservation and associated Dedicated
-- Hosts.
--
-- /See:/ 'newHostReservation' smart constructor.
data HostReservation = HostReservation'
  { -- | The instance family of the Dedicated Host Reservation. The instance
    -- family on the Dedicated Host must be the same in order for it to benefit
    -- from the reservation.
    instanceFamily :: Core.Maybe Core.Text,
    -- | The date and time that the reservation ends.
    end :: Core.Maybe Core.ISO8601,
    -- | The IDs of the Dedicated Hosts associated with the reservation.
    hostIdSet :: Core.Maybe [Core.Text],
    -- | The upfront price of the reservation.
    upfrontPrice :: Core.Maybe Core.Text,
    -- | The payment option selected for this reservation.
    paymentOption :: Core.Maybe PaymentOption,
    -- | The length of the reservation\'s term, specified in seconds. Can be
    -- @31536000 (1 year)@ | @94608000 (3 years)@.
    duration :: Core.Maybe Core.Int,
    -- | The ID of the reservation. This remains the same regardless of which
    -- Dedicated Hosts are associated with it.
    offeringId :: Core.Maybe Core.Text,
    -- | The ID of the reservation that specifies the associated Dedicated Hosts.
    hostReservationId :: Core.Maybe Core.Text,
    -- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are
    -- specified. At this time, the only supported currency is @USD@.
    currencyCode :: Core.Maybe CurrencyCodeValues,
    -- | The state of the reservation.
    state :: Core.Maybe ReservationState,
    -- | Any tags assigned to the Dedicated Host Reservation.
    tags :: Core.Maybe [Tag],
    -- | The number of Dedicated Hosts the reservation is associated with.
    count :: Core.Maybe Core.Int,
    -- | The hourly price of the reservation.
    hourlyPrice :: Core.Maybe Core.Text,
    -- | The date and time that the reservation started.
    start :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HostReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'hostReservation_instanceFamily' - The instance family of the Dedicated Host Reservation. The instance
-- family on the Dedicated Host must be the same in order for it to benefit
-- from the reservation.
--
-- 'end', 'hostReservation_end' - The date and time that the reservation ends.
--
-- 'hostIdSet', 'hostReservation_hostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
--
-- 'upfrontPrice', 'hostReservation_upfrontPrice' - The upfront price of the reservation.
--
-- 'paymentOption', 'hostReservation_paymentOption' - The payment option selected for this reservation.
--
-- 'duration', 'hostReservation_duration' - The length of the reservation\'s term, specified in seconds. Can be
-- @31536000 (1 year)@ | @94608000 (3 years)@.
--
-- 'offeringId', 'hostReservation_offeringId' - The ID of the reservation. This remains the same regardless of which
-- Dedicated Hosts are associated with it.
--
-- 'hostReservationId', 'hostReservation_hostReservationId' - The ID of the reservation that specifies the associated Dedicated Hosts.
--
-- 'currencyCode', 'hostReservation_currencyCode' - The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
--
-- 'state', 'hostReservation_state' - The state of the reservation.
--
-- 'tags', 'hostReservation_tags' - Any tags assigned to the Dedicated Host Reservation.
--
-- 'count', 'hostReservation_count' - The number of Dedicated Hosts the reservation is associated with.
--
-- 'hourlyPrice', 'hostReservation_hourlyPrice' - The hourly price of the reservation.
--
-- 'start', 'hostReservation_start' - The date and time that the reservation started.
newHostReservation ::
  HostReservation
newHostReservation =
  HostReservation'
    { instanceFamily = Core.Nothing,
      end = Core.Nothing,
      hostIdSet = Core.Nothing,
      upfrontPrice = Core.Nothing,
      paymentOption = Core.Nothing,
      duration = Core.Nothing,
      offeringId = Core.Nothing,
      hostReservationId = Core.Nothing,
      currencyCode = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      count = Core.Nothing,
      hourlyPrice = Core.Nothing,
      start = Core.Nothing
    }

-- | The instance family of the Dedicated Host Reservation. The instance
-- family on the Dedicated Host must be the same in order for it to benefit
-- from the reservation.
hostReservation_instanceFamily :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hostReservation_instanceFamily = Lens.lens (\HostReservation' {instanceFamily} -> instanceFamily) (\s@HostReservation' {} a -> s {instanceFamily = a} :: HostReservation)

-- | The date and time that the reservation ends.
hostReservation_end :: Lens.Lens' HostReservation (Core.Maybe Core.UTCTime)
hostReservation_end = Lens.lens (\HostReservation' {end} -> end) (\s@HostReservation' {} a -> s {end = a} :: HostReservation) Core.. Lens.mapping Core._Time

-- | The IDs of the Dedicated Hosts associated with the reservation.
hostReservation_hostIdSet :: Lens.Lens' HostReservation (Core.Maybe [Core.Text])
hostReservation_hostIdSet = Lens.lens (\HostReservation' {hostIdSet} -> hostIdSet) (\s@HostReservation' {} a -> s {hostIdSet = a} :: HostReservation) Core.. Lens.mapping Lens._Coerce

-- | The upfront price of the reservation.
hostReservation_upfrontPrice :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hostReservation_upfrontPrice = Lens.lens (\HostReservation' {upfrontPrice} -> upfrontPrice) (\s@HostReservation' {} a -> s {upfrontPrice = a} :: HostReservation)

-- | The payment option selected for this reservation.
hostReservation_paymentOption :: Lens.Lens' HostReservation (Core.Maybe PaymentOption)
hostReservation_paymentOption = Lens.lens (\HostReservation' {paymentOption} -> paymentOption) (\s@HostReservation' {} a -> s {paymentOption = a} :: HostReservation)

-- | The length of the reservation\'s term, specified in seconds. Can be
-- @31536000 (1 year)@ | @94608000 (3 years)@.
hostReservation_duration :: Lens.Lens' HostReservation (Core.Maybe Core.Int)
hostReservation_duration = Lens.lens (\HostReservation' {duration} -> duration) (\s@HostReservation' {} a -> s {duration = a} :: HostReservation)

-- | The ID of the reservation. This remains the same regardless of which
-- Dedicated Hosts are associated with it.
hostReservation_offeringId :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hostReservation_offeringId = Lens.lens (\HostReservation' {offeringId} -> offeringId) (\s@HostReservation' {} a -> s {offeringId = a} :: HostReservation)

-- | The ID of the reservation that specifies the associated Dedicated Hosts.
hostReservation_hostReservationId :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hostReservation_hostReservationId = Lens.lens (\HostReservation' {hostReservationId} -> hostReservationId) (\s@HostReservation' {} a -> s {hostReservationId = a} :: HostReservation)

-- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
hostReservation_currencyCode :: Lens.Lens' HostReservation (Core.Maybe CurrencyCodeValues)
hostReservation_currencyCode = Lens.lens (\HostReservation' {currencyCode} -> currencyCode) (\s@HostReservation' {} a -> s {currencyCode = a} :: HostReservation)

-- | The state of the reservation.
hostReservation_state :: Lens.Lens' HostReservation (Core.Maybe ReservationState)
hostReservation_state = Lens.lens (\HostReservation' {state} -> state) (\s@HostReservation' {} a -> s {state = a} :: HostReservation)

-- | Any tags assigned to the Dedicated Host Reservation.
hostReservation_tags :: Lens.Lens' HostReservation (Core.Maybe [Tag])
hostReservation_tags = Lens.lens (\HostReservation' {tags} -> tags) (\s@HostReservation' {} a -> s {tags = a} :: HostReservation) Core.. Lens.mapping Lens._Coerce

-- | The number of Dedicated Hosts the reservation is associated with.
hostReservation_count :: Lens.Lens' HostReservation (Core.Maybe Core.Int)
hostReservation_count = Lens.lens (\HostReservation' {count} -> count) (\s@HostReservation' {} a -> s {count = a} :: HostReservation)

-- | The hourly price of the reservation.
hostReservation_hourlyPrice :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hostReservation_hourlyPrice = Lens.lens (\HostReservation' {hourlyPrice} -> hourlyPrice) (\s@HostReservation' {} a -> s {hourlyPrice = a} :: HostReservation)

-- | The date and time that the reservation started.
hostReservation_start :: Lens.Lens' HostReservation (Core.Maybe Core.UTCTime)
hostReservation_start = Lens.lens (\HostReservation' {start} -> start) (\s@HostReservation' {} a -> s {start = a} :: HostReservation) Core.. Lens.mapping Core._Time

instance Core.FromXML HostReservation where
  parseXML x =
    HostReservation'
      Core.<$> (x Core..@? "instanceFamily")
      Core.<*> (x Core..@? "end")
      Core.<*> ( x Core..@? "hostIdSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "upfrontPrice")
      Core.<*> (x Core..@? "paymentOption")
      Core.<*> (x Core..@? "duration")
      Core.<*> (x Core..@? "offeringId")
      Core.<*> (x Core..@? "hostReservationId")
      Core.<*> (x Core..@? "currencyCode")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "count")
      Core.<*> (x Core..@? "hourlyPrice")
      Core.<*> (x Core..@? "start")

instance Core.Hashable HostReservation

instance Core.NFData HostReservation
