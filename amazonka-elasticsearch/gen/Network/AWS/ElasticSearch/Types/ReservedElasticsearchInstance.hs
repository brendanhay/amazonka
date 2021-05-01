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
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance where

import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of a reserved Elasticsearch instance.
--
-- /See:/ 'newReservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { -- | The customer-specified identifier to track this reservation.
    reservationName :: Prelude.Maybe Prelude.Text,
    -- | The payment option as defined in the reserved Elasticsearch instance
    -- offering.
    paymentOption :: Prelude.Maybe ReservedElasticsearchInstancePaymentOption,
    -- | The number of Elasticsearch instances that have been reserved.
    elasticsearchInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The duration, in seconds, for which the Elasticsearch instance is
    -- reserved.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The time the reservation started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch instance type offered by the reserved instance
    -- offering.
    elasticsearchInstanceType :: Prelude.Maybe ESPartitionInstanceType,
    -- | The state of the reserved Elasticsearch instance.
    state :: Prelude.Maybe Prelude.Text,
    -- | The upfront fixed charge you will paid to purchase the specific reserved
    -- Elasticsearch instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The unique identifier for the reservation.
    reservedElasticsearchInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier.
    reservedElasticsearchInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The rate you are charged for each hour for the domain that is using this
    -- reserved instance.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      elasticsearchInstanceCount = Prelude.Nothing,
      duration = Prelude.Nothing,
      startTime = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      elasticsearchInstanceType = Prelude.Nothing,
      state = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      reservedElasticsearchInstanceId =
        Prelude.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      recurringCharges = Prelude.Nothing
    }

-- | The customer-specified identifier to track this reservation.
reservedElasticsearchInstance_reservationName :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_reservationName = Lens.lens (\ReservedElasticsearchInstance' {reservationName} -> reservationName) (\s@ReservedElasticsearchInstance' {} a -> s {reservationName = a} :: ReservedElasticsearchInstance)

-- | The payment option as defined in the reserved Elasticsearch instance
-- offering.
reservedElasticsearchInstance_paymentOption :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe ReservedElasticsearchInstancePaymentOption)
reservedElasticsearchInstance_paymentOption = Lens.lens (\ReservedElasticsearchInstance' {paymentOption} -> paymentOption) (\s@ReservedElasticsearchInstance' {} a -> s {paymentOption = a} :: ReservedElasticsearchInstance)

-- | The number of Elasticsearch instances that have been reserved.
reservedElasticsearchInstance_elasticsearchInstanceCount :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Int)
reservedElasticsearchInstance_elasticsearchInstanceCount = Lens.lens (\ReservedElasticsearchInstance' {elasticsearchInstanceCount} -> elasticsearchInstanceCount) (\s@ReservedElasticsearchInstance' {} a -> s {elasticsearchInstanceCount = a} :: ReservedElasticsearchInstance)

-- | The duration, in seconds, for which the Elasticsearch instance is
-- reserved.
reservedElasticsearchInstance_duration :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Int)
reservedElasticsearchInstance_duration = Lens.lens (\ReservedElasticsearchInstance' {duration} -> duration) (\s@ReservedElasticsearchInstance' {} a -> s {duration = a} :: ReservedElasticsearchInstance)

-- | The time the reservation started.
reservedElasticsearchInstance_startTime :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.UTCTime)
reservedElasticsearchInstance_startTime = Lens.lens (\ReservedElasticsearchInstance' {startTime} -> startTime) (\s@ReservedElasticsearchInstance' {} a -> s {startTime = a} :: ReservedElasticsearchInstance) Prelude.. Lens.mapping Prelude._Time

-- | The currency code for the reserved Elasticsearch instance offering.
reservedElasticsearchInstance_currencyCode :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_currencyCode = Lens.lens (\ReservedElasticsearchInstance' {currencyCode} -> currencyCode) (\s@ReservedElasticsearchInstance' {} a -> s {currencyCode = a} :: ReservedElasticsearchInstance)

-- | The Elasticsearch instance type offered by the reserved instance
-- offering.
reservedElasticsearchInstance_elasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe ESPartitionInstanceType)
reservedElasticsearchInstance_elasticsearchInstanceType = Lens.lens (\ReservedElasticsearchInstance' {elasticsearchInstanceType} -> elasticsearchInstanceType) (\s@ReservedElasticsearchInstance' {} a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstance)

-- | The state of the reserved Elasticsearch instance.
reservedElasticsearchInstance_state :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_state = Lens.lens (\ReservedElasticsearchInstance' {state} -> state) (\s@ReservedElasticsearchInstance' {} a -> s {state = a} :: ReservedElasticsearchInstance)

-- | The upfront fixed charge you will paid to purchase the specific reserved
-- Elasticsearch instance offering.
reservedElasticsearchInstance_fixedPrice :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstance_fixedPrice = Lens.lens (\ReservedElasticsearchInstance' {fixedPrice} -> fixedPrice) (\s@ReservedElasticsearchInstance' {} a -> s {fixedPrice = a} :: ReservedElasticsearchInstance)

-- | The unique identifier for the reservation.
reservedElasticsearchInstance_reservedElasticsearchInstanceId :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_reservedElasticsearchInstanceId = Lens.lens (\ReservedElasticsearchInstance' {reservedElasticsearchInstanceId} -> reservedElasticsearchInstanceId) (\s@ReservedElasticsearchInstance' {} a -> s {reservedElasticsearchInstanceId = a} :: ReservedElasticsearchInstance)

-- | The offering identifier.
reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId = Lens.lens (\ReservedElasticsearchInstance' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@ReservedElasticsearchInstance' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstance)

-- | The rate you are charged for each hour for the domain that is using this
-- reserved instance.
reservedElasticsearchInstance_usagePrice :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstance_usagePrice = Lens.lens (\ReservedElasticsearchInstance' {usagePrice} -> usagePrice) (\s@ReservedElasticsearchInstance' {} a -> s {usagePrice = a} :: ReservedElasticsearchInstance)

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedElasticsearchInstance_recurringCharges :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe [RecurringCharge])
reservedElasticsearchInstance_recurringCharges = Lens.lens (\ReservedElasticsearchInstance' {recurringCharges} -> recurringCharges) (\s@ReservedElasticsearchInstance' {} a -> s {recurringCharges = a} :: ReservedElasticsearchInstance) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    ReservedElasticsearchInstance
  where
  parseJSON =
    Prelude.withObject
      "ReservedElasticsearchInstance"
      ( \x ->
          ReservedElasticsearchInstance'
            Prelude.<$> (x Prelude..:? "ReservationName")
            Prelude.<*> (x Prelude..:? "PaymentOption")
            Prelude.<*> (x Prelude..:? "ElasticsearchInstanceCount")
            Prelude.<*> (x Prelude..:? "Duration")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "CurrencyCode")
            Prelude.<*> (x Prelude..:? "ElasticsearchInstanceType")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "FixedPrice")
            Prelude.<*> (x Prelude..:? "ReservedElasticsearchInstanceId")
            Prelude.<*> ( x
                            Prelude..:? "ReservedElasticsearchInstanceOfferingId"
                        )
            Prelude.<*> (x Prelude..:? "UsagePrice")
            Prelude.<*> ( x Prelude..:? "RecurringCharges"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ReservedElasticsearchInstance

instance Prelude.NFData ReservedElasticsearchInstance
