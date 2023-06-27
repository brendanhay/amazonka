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
-- Module      : Amazonka.ElasticSearch.Types.ReservedElasticsearchInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ReservedElasticsearchInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.ESPartitionInstanceType
import Amazonka.ElasticSearch.Types.RecurringCharge
import Amazonka.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Details of a reserved Elasticsearch instance.
--
-- /See:/ 'newReservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The duration, in seconds, for which the Elasticsearch instance is
    -- reserved.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The number of Elasticsearch instances that have been reserved.
    elasticsearchInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The Elasticsearch instance type offered by the reserved instance
    -- offering.
    elasticsearchInstanceType :: Prelude.Maybe ESPartitionInstanceType,
    -- | The upfront fixed charge you will paid to purchase the specific reserved
    -- Elasticsearch instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The payment option as defined in the reserved Elasticsearch instance
    -- offering.
    paymentOption :: Prelude.Maybe ReservedElasticsearchInstancePaymentOption,
    -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The customer-specified identifier to track this reservation.
    reservationName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the reservation.
    reservedElasticsearchInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier.
    reservedElasticsearchInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The time the reservation started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the reserved Elasticsearch instance.
    state :: Prelude.Maybe Prelude.Text,
    -- | The rate you are charged for each hour for the domain that is using this
    -- reserved instance.
    usagePrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedElasticsearchInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'reservedElasticsearchInstance_currencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- 'duration', 'reservedElasticsearchInstance_duration' - The duration, in seconds, for which the Elasticsearch instance is
-- reserved.
--
-- 'elasticsearchInstanceCount', 'reservedElasticsearchInstance_elasticsearchInstanceCount' - The number of Elasticsearch instances that have been reserved.
--
-- 'elasticsearchInstanceType', 'reservedElasticsearchInstance_elasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance
-- offering.
--
-- 'fixedPrice', 'reservedElasticsearchInstance_fixedPrice' - The upfront fixed charge you will paid to purchase the specific reserved
-- Elasticsearch instance offering.
--
-- 'paymentOption', 'reservedElasticsearchInstance_paymentOption' - The payment option as defined in the reserved Elasticsearch instance
-- offering.
--
-- 'recurringCharges', 'reservedElasticsearchInstance_recurringCharges' - The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
--
-- 'reservationName', 'reservedElasticsearchInstance_reservationName' - The customer-specified identifier to track this reservation.
--
-- 'reservedElasticsearchInstanceId', 'reservedElasticsearchInstance_reservedElasticsearchInstanceId' - The unique identifier for the reservation.
--
-- 'reservedElasticsearchInstanceOfferingId', 'reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId' - The offering identifier.
--
-- 'startTime', 'reservedElasticsearchInstance_startTime' - The time the reservation started.
--
-- 'state', 'reservedElasticsearchInstance_state' - The state of the reserved Elasticsearch instance.
--
-- 'usagePrice', 'reservedElasticsearchInstance_usagePrice' - The rate you are charged for each hour for the domain that is using this
-- reserved instance.
newReservedElasticsearchInstance ::
  ReservedElasticsearchInstance
newReservedElasticsearchInstance =
  ReservedElasticsearchInstance'
    { currencyCode =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      elasticsearchInstanceCount = Prelude.Nothing,
      elasticsearchInstanceType = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      reservationName = Prelude.Nothing,
      reservedElasticsearchInstanceId =
        Prelude.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The currency code for the reserved Elasticsearch instance offering.
reservedElasticsearchInstance_currencyCode :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_currencyCode = Lens.lens (\ReservedElasticsearchInstance' {currencyCode} -> currencyCode) (\s@ReservedElasticsearchInstance' {} a -> s {currencyCode = a} :: ReservedElasticsearchInstance)

-- | The duration, in seconds, for which the Elasticsearch instance is
-- reserved.
reservedElasticsearchInstance_duration :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Int)
reservedElasticsearchInstance_duration = Lens.lens (\ReservedElasticsearchInstance' {duration} -> duration) (\s@ReservedElasticsearchInstance' {} a -> s {duration = a} :: ReservedElasticsearchInstance)

-- | The number of Elasticsearch instances that have been reserved.
reservedElasticsearchInstance_elasticsearchInstanceCount :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Int)
reservedElasticsearchInstance_elasticsearchInstanceCount = Lens.lens (\ReservedElasticsearchInstance' {elasticsearchInstanceCount} -> elasticsearchInstanceCount) (\s@ReservedElasticsearchInstance' {} a -> s {elasticsearchInstanceCount = a} :: ReservedElasticsearchInstance)

-- | The Elasticsearch instance type offered by the reserved instance
-- offering.
reservedElasticsearchInstance_elasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe ESPartitionInstanceType)
reservedElasticsearchInstance_elasticsearchInstanceType = Lens.lens (\ReservedElasticsearchInstance' {elasticsearchInstanceType} -> elasticsearchInstanceType) (\s@ReservedElasticsearchInstance' {} a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstance)

-- | The upfront fixed charge you will paid to purchase the specific reserved
-- Elasticsearch instance offering.
reservedElasticsearchInstance_fixedPrice :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstance_fixedPrice = Lens.lens (\ReservedElasticsearchInstance' {fixedPrice} -> fixedPrice) (\s@ReservedElasticsearchInstance' {} a -> s {fixedPrice = a} :: ReservedElasticsearchInstance)

-- | The payment option as defined in the reserved Elasticsearch instance
-- offering.
reservedElasticsearchInstance_paymentOption :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe ReservedElasticsearchInstancePaymentOption)
reservedElasticsearchInstance_paymentOption = Lens.lens (\ReservedElasticsearchInstance' {paymentOption} -> paymentOption) (\s@ReservedElasticsearchInstance' {} a -> s {paymentOption = a} :: ReservedElasticsearchInstance)

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedElasticsearchInstance_recurringCharges :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe [RecurringCharge])
reservedElasticsearchInstance_recurringCharges = Lens.lens (\ReservedElasticsearchInstance' {recurringCharges} -> recurringCharges) (\s@ReservedElasticsearchInstance' {} a -> s {recurringCharges = a} :: ReservedElasticsearchInstance) Prelude.. Lens.mapping Lens.coerced

-- | The customer-specified identifier to track this reservation.
reservedElasticsearchInstance_reservationName :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_reservationName = Lens.lens (\ReservedElasticsearchInstance' {reservationName} -> reservationName) (\s@ReservedElasticsearchInstance' {} a -> s {reservationName = a} :: ReservedElasticsearchInstance)

-- | The unique identifier for the reservation.
reservedElasticsearchInstance_reservedElasticsearchInstanceId :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_reservedElasticsearchInstanceId = Lens.lens (\ReservedElasticsearchInstance' {reservedElasticsearchInstanceId} -> reservedElasticsearchInstanceId) (\s@ReservedElasticsearchInstance' {} a -> s {reservedElasticsearchInstanceId = a} :: ReservedElasticsearchInstance)

-- | The offering identifier.
reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId = Lens.lens (\ReservedElasticsearchInstance' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@ReservedElasticsearchInstance' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstance)

-- | The time the reservation started.
reservedElasticsearchInstance_startTime :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.UTCTime)
reservedElasticsearchInstance_startTime = Lens.lens (\ReservedElasticsearchInstance' {startTime} -> startTime) (\s@ReservedElasticsearchInstance' {} a -> s {startTime = a} :: ReservedElasticsearchInstance) Prelude.. Lens.mapping Data._Time

-- | The state of the reserved Elasticsearch instance.
reservedElasticsearchInstance_state :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstance_state = Lens.lens (\ReservedElasticsearchInstance' {state} -> state) (\s@ReservedElasticsearchInstance' {} a -> s {state = a} :: ReservedElasticsearchInstance)

-- | The rate you are charged for each hour for the domain that is using this
-- reserved instance.
reservedElasticsearchInstance_usagePrice :: Lens.Lens' ReservedElasticsearchInstance (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstance_usagePrice = Lens.lens (\ReservedElasticsearchInstance' {usagePrice} -> usagePrice) (\s@ReservedElasticsearchInstance' {} a -> s {usagePrice = a} :: ReservedElasticsearchInstance)

instance Data.FromJSON ReservedElasticsearchInstance where
  parseJSON =
    Data.withObject
      "ReservedElasticsearchInstance"
      ( \x ->
          ReservedElasticsearchInstance'
            Prelude.<$> (x Data..:? "CurrencyCode")
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "ElasticsearchInstanceCount")
            Prelude.<*> (x Data..:? "ElasticsearchInstanceType")
            Prelude.<*> (x Data..:? "FixedPrice")
            Prelude.<*> (x Data..:? "PaymentOption")
            Prelude.<*> ( x
                            Data..:? "RecurringCharges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReservationName")
            Prelude.<*> (x Data..:? "ReservedElasticsearchInstanceId")
            Prelude.<*> ( x
                            Data..:? "ReservedElasticsearchInstanceOfferingId"
                        )
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "UsagePrice")
      )

instance
  Prelude.Hashable
    ReservedElasticsearchInstance
  where
  hashWithSalt _salt ReservedElasticsearchInstance' {..} =
    _salt
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` elasticsearchInstanceCount
      `Prelude.hashWithSalt` elasticsearchInstanceType
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` reservationName
      `Prelude.hashWithSalt` reservedElasticsearchInstanceId
      `Prelude.hashWithSalt` reservedElasticsearchInstanceOfferingId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedElasticsearchInstance where
  rnf ReservedElasticsearchInstance' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf elasticsearchInstanceCount
      `Prelude.seq` Prelude.rnf elasticsearchInstanceType
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf reservationName
      `Prelude.seq` Prelude.rnf reservedElasticsearchInstanceId
      `Prelude.seq` Prelude.rnf
        reservedElasticsearchInstanceOfferingId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf usagePrice
