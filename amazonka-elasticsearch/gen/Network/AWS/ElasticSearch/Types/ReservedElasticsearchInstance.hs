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
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Network.AWS.Lens as Lens

-- | Details of a reserved Elasticsearch instance.
--
-- /See:/ 'newReservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { -- | The customer-specified identifier to track this reservation.
    reservationName :: Core.Maybe Core.Text,
    -- | The payment option as defined in the reserved Elasticsearch instance
    -- offering.
    paymentOption :: Core.Maybe ReservedElasticsearchInstancePaymentOption,
    -- | The number of Elasticsearch instances that have been reserved.
    elasticsearchInstanceCount :: Core.Maybe Core.Int,
    -- | The duration, in seconds, for which the Elasticsearch instance is
    -- reserved.
    duration :: Core.Maybe Core.Int,
    -- | The time the reservation started.
    startTime :: Core.Maybe Core.POSIX,
    -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Core.Maybe Core.Text,
    -- | The Elasticsearch instance type offered by the reserved instance
    -- offering.
    elasticsearchInstanceType :: Core.Maybe ESPartitionInstanceType,
    -- | The state of the reserved Elasticsearch instance.
    state :: Core.Maybe Core.Text,
    -- | The upfront fixed charge you will paid to purchase the specific reserved
    -- Elasticsearch instance offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The unique identifier for the reservation.
    reservedElasticsearchInstanceId :: Core.Maybe Core.Text,
    -- | The offering identifier.
    reservedElasticsearchInstanceOfferingId :: Core.Maybe Core.Text,
    -- | The rate you are charged for each hour for the domain that is using this
    -- reserved instance.
    usagePrice :: Core.Maybe Core.Double,
    -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Core.Maybe [RecurringCharge]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedElasticsearchInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationName', 'reservedElasticsearchInstance_reservationName' - The customer-specified identifier to track this reservation.
--
-- 'paymentOption', 'reservedElasticsearchInstance_paymentOption' - The payment option as defined in the reserved Elasticsearch instance
-- offering.
--
-- 'elasticsearchInstanceCount', 'reservedElasticsearchInstance_elasticsearchInstanceCount' - The number of Elasticsearch instances that have been reserved.
--
-- 'duration', 'reservedElasticsearchInstance_duration' - The duration, in seconds, for which the Elasticsearch instance is
-- reserved.
--
-- 'startTime', 'reservedElasticsearchInstance_startTime' - The time the reservation started.
--
-- 'currencyCode', 'reservedElasticsearchInstance_currencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- 'elasticsearchInstanceType', 'reservedElasticsearchInstance_elasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance
-- offering.
--
-- 'state', 'reservedElasticsearchInstance_state' - The state of the reserved Elasticsearch instance.
--
-- 'fixedPrice', 'reservedElasticsearchInstance_fixedPrice' - The upfront fixed charge you will paid to purchase the specific reserved
-- Elasticsearch instance offering.
--
-- 'reservedElasticsearchInstanceId', 'reservedElasticsearchInstance_reservedElasticsearchInstanceId' - The unique identifier for the reservation.
--
-- 'reservedElasticsearchInstanceOfferingId', 'reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId' - The offering identifier.
--
-- 'usagePrice', 'reservedElasticsearchInstance_usagePrice' - The rate you are charged for each hour for the domain that is using this
-- reserved instance.
--
-- 'recurringCharges', 'reservedElasticsearchInstance_recurringCharges' - The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
newReservedElasticsearchInstance ::
  ReservedElasticsearchInstance
newReservedElasticsearchInstance =
  ReservedElasticsearchInstance'
    { reservationName =
        Core.Nothing,
      paymentOption = Core.Nothing,
      elasticsearchInstanceCount = Core.Nothing,
      duration = Core.Nothing,
      startTime = Core.Nothing,
      currencyCode = Core.Nothing,
      elasticsearchInstanceType = Core.Nothing,
      state = Core.Nothing,
      fixedPrice = Core.Nothing,
      reservedElasticsearchInstanceId =
        Core.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Core.Nothing,
      usagePrice = Core.Nothing,
      recurringCharges = Core.Nothing
    }

-- | The customer-specified identifier to track this reservation.
reservedElasticsearchInstance_reservationName :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reservedElasticsearchInstance_reservationName = Lens.lens (\ReservedElasticsearchInstance' {reservationName} -> reservationName) (\s@ReservedElasticsearchInstance' {} a -> s {reservationName = a} :: ReservedElasticsearchInstance)

-- | The payment option as defined in the reserved Elasticsearch instance
-- offering.
reservedElasticsearchInstance_paymentOption :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe ReservedElasticsearchInstancePaymentOption)
reservedElasticsearchInstance_paymentOption = Lens.lens (\ReservedElasticsearchInstance' {paymentOption} -> paymentOption) (\s@ReservedElasticsearchInstance' {} a -> s {paymentOption = a} :: ReservedElasticsearchInstance)

-- | The number of Elasticsearch instances that have been reserved.
reservedElasticsearchInstance_elasticsearchInstanceCount :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Int)
reservedElasticsearchInstance_elasticsearchInstanceCount = Lens.lens (\ReservedElasticsearchInstance' {elasticsearchInstanceCount} -> elasticsearchInstanceCount) (\s@ReservedElasticsearchInstance' {} a -> s {elasticsearchInstanceCount = a} :: ReservedElasticsearchInstance)

-- | The duration, in seconds, for which the Elasticsearch instance is
-- reserved.
reservedElasticsearchInstance_duration :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Int)
reservedElasticsearchInstance_duration = Lens.lens (\ReservedElasticsearchInstance' {duration} -> duration) (\s@ReservedElasticsearchInstance' {} a -> s {duration = a} :: ReservedElasticsearchInstance)

-- | The time the reservation started.
reservedElasticsearchInstance_startTime :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.UTCTime)
reservedElasticsearchInstance_startTime = Lens.lens (\ReservedElasticsearchInstance' {startTime} -> startTime) (\s@ReservedElasticsearchInstance' {} a -> s {startTime = a} :: ReservedElasticsearchInstance) Core.. Lens.mapping Core._Time

-- | The currency code for the reserved Elasticsearch instance offering.
reservedElasticsearchInstance_currencyCode :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reservedElasticsearchInstance_currencyCode = Lens.lens (\ReservedElasticsearchInstance' {currencyCode} -> currencyCode) (\s@ReservedElasticsearchInstance' {} a -> s {currencyCode = a} :: ReservedElasticsearchInstance)

-- | The Elasticsearch instance type offered by the reserved instance
-- offering.
reservedElasticsearchInstance_elasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe ESPartitionInstanceType)
reservedElasticsearchInstance_elasticsearchInstanceType = Lens.lens (\ReservedElasticsearchInstance' {elasticsearchInstanceType} -> elasticsearchInstanceType) (\s@ReservedElasticsearchInstance' {} a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstance)

-- | The state of the reserved Elasticsearch instance.
reservedElasticsearchInstance_state :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reservedElasticsearchInstance_state = Lens.lens (\ReservedElasticsearchInstance' {state} -> state) (\s@ReservedElasticsearchInstance' {} a -> s {state = a} :: ReservedElasticsearchInstance)

-- | The upfront fixed charge you will paid to purchase the specific reserved
-- Elasticsearch instance offering.
reservedElasticsearchInstance_fixedPrice :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Double)
reservedElasticsearchInstance_fixedPrice = Lens.lens (\ReservedElasticsearchInstance' {fixedPrice} -> fixedPrice) (\s@ReservedElasticsearchInstance' {} a -> s {fixedPrice = a} :: ReservedElasticsearchInstance)

-- | The unique identifier for the reservation.
reservedElasticsearchInstance_reservedElasticsearchInstanceId :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reservedElasticsearchInstance_reservedElasticsearchInstanceId = Lens.lens (\ReservedElasticsearchInstance' {reservedElasticsearchInstanceId} -> reservedElasticsearchInstanceId) (\s@ReservedElasticsearchInstance' {} a -> s {reservedElasticsearchInstanceId = a} :: ReservedElasticsearchInstance)

-- | The offering identifier.
reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId = Lens.lens (\ReservedElasticsearchInstance' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@ReservedElasticsearchInstance' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstance)

-- | The rate you are charged for each hour for the domain that is using this
-- reserved instance.
reservedElasticsearchInstance_usagePrice :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Double)
reservedElasticsearchInstance_usagePrice = Lens.lens (\ReservedElasticsearchInstance' {usagePrice} -> usagePrice) (\s@ReservedElasticsearchInstance' {} a -> s {usagePrice = a} :: ReservedElasticsearchInstance)

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedElasticsearchInstance_recurringCharges :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe [RecurringCharge])
reservedElasticsearchInstance_recurringCharges = Lens.lens (\ReservedElasticsearchInstance' {recurringCharges} -> recurringCharges) (\s@ReservedElasticsearchInstance' {} a -> s {recurringCharges = a} :: ReservedElasticsearchInstance) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ReservedElasticsearchInstance where
  parseJSON =
    Core.withObject
      "ReservedElasticsearchInstance"
      ( \x ->
          ReservedElasticsearchInstance'
            Core.<$> (x Core..:? "ReservationName")
            Core.<*> (x Core..:? "PaymentOption")
            Core.<*> (x Core..:? "ElasticsearchInstanceCount")
            Core.<*> (x Core..:? "Duration")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "CurrencyCode")
            Core.<*> (x Core..:? "ElasticsearchInstanceType")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "FixedPrice")
            Core.<*> (x Core..:? "ReservedElasticsearchInstanceId")
            Core.<*> ( x
                         Core..:? "ReservedElasticsearchInstanceOfferingId"
                     )
            Core.<*> (x Core..:? "UsagePrice")
            Core.<*> (x Core..:? "RecurringCharges" Core..!= Core.mempty)
      )

instance Core.Hashable ReservedElasticsearchInstance

instance Core.NFData ReservedElasticsearchInstance
