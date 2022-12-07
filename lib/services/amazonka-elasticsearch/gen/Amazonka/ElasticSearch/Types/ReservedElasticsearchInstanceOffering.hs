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
-- Module      : Amazonka.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ReservedElasticsearchInstanceOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.ESPartitionInstanceType
import Amazonka.ElasticSearch.Types.RecurringCharge
import Amazonka.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Details of a reserved Elasticsearch instance offering.
--
-- /See:/ 'newReservedElasticsearchInstanceOffering' smart constructor.
data ReservedElasticsearchInstanceOffering = ReservedElasticsearchInstanceOffering'
  { -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The Elasticsearch instance type offered by the reserved instance
    -- offering.
    elasticsearchInstanceType :: Prelude.Maybe ESPartitionInstanceType,
    -- | The duration, in seconds, for which the offering will reserve the
    -- Elasticsearch instance.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch reserved instance offering identifier.
    reservedElasticsearchInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The upfront fixed charge you will pay to purchase the specific reserved
    -- Elasticsearch instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Payment option for the reserved Elasticsearch instance offering
    paymentOption :: Prelude.Maybe ReservedElasticsearchInstancePaymentOption,
    -- | The rate you are charged for each hour the domain that is using the
    -- offering is running.
    usagePrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedElasticsearchInstanceOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recurringCharges', 'reservedElasticsearchInstanceOffering_recurringCharges' - The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
--
-- 'elasticsearchInstanceType', 'reservedElasticsearchInstanceOffering_elasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance
-- offering.
--
-- 'duration', 'reservedElasticsearchInstanceOffering_duration' - The duration, in seconds, for which the offering will reserve the
-- Elasticsearch instance.
--
-- 'currencyCode', 'reservedElasticsearchInstanceOffering_currencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- 'reservedElasticsearchInstanceOfferingId', 'reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId' - The Elasticsearch reserved instance offering identifier.
--
-- 'fixedPrice', 'reservedElasticsearchInstanceOffering_fixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved
-- Elasticsearch instance offering.
--
-- 'paymentOption', 'reservedElasticsearchInstanceOffering_paymentOption' - Payment option for the reserved Elasticsearch instance offering
--
-- 'usagePrice', 'reservedElasticsearchInstanceOffering_usagePrice' - The rate you are charged for each hour the domain that is using the
-- offering is running.
newReservedElasticsearchInstanceOffering ::
  ReservedElasticsearchInstanceOffering
newReservedElasticsearchInstanceOffering =
  ReservedElasticsearchInstanceOffering'
    { recurringCharges =
        Prelude.Nothing,
      elasticsearchInstanceType =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedElasticsearchInstanceOffering_recurringCharges :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe [RecurringCharge])
reservedElasticsearchInstanceOffering_recurringCharges = Lens.lens (\ReservedElasticsearchInstanceOffering' {recurringCharges} -> recurringCharges) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {recurringCharges = a} :: ReservedElasticsearchInstanceOffering) Prelude.. Lens.mapping Lens.coerced

-- | The Elasticsearch instance type offered by the reserved instance
-- offering.
reservedElasticsearchInstanceOffering_elasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe ESPartitionInstanceType)
reservedElasticsearchInstanceOffering_elasticsearchInstanceType = Lens.lens (\ReservedElasticsearchInstanceOffering' {elasticsearchInstanceType} -> elasticsearchInstanceType) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstanceOffering)

-- | The duration, in seconds, for which the offering will reserve the
-- Elasticsearch instance.
reservedElasticsearchInstanceOffering_duration :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Int)
reservedElasticsearchInstanceOffering_duration = Lens.lens (\ReservedElasticsearchInstanceOffering' {duration} -> duration) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {duration = a} :: ReservedElasticsearchInstanceOffering)

-- | The currency code for the reserved Elasticsearch instance offering.
reservedElasticsearchInstanceOffering_currencyCode :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstanceOffering_currencyCode = Lens.lens (\ReservedElasticsearchInstanceOffering' {currencyCode} -> currencyCode) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {currencyCode = a} :: ReservedElasticsearchInstanceOffering)

-- | The Elasticsearch reserved instance offering identifier.
reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId = Lens.lens (\ReservedElasticsearchInstanceOffering' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstanceOffering)

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- Elasticsearch instance offering.
reservedElasticsearchInstanceOffering_fixedPrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstanceOffering_fixedPrice = Lens.lens (\ReservedElasticsearchInstanceOffering' {fixedPrice} -> fixedPrice) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {fixedPrice = a} :: ReservedElasticsearchInstanceOffering)

-- | Payment option for the reserved Elasticsearch instance offering
reservedElasticsearchInstanceOffering_paymentOption :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe ReservedElasticsearchInstancePaymentOption)
reservedElasticsearchInstanceOffering_paymentOption = Lens.lens (\ReservedElasticsearchInstanceOffering' {paymentOption} -> paymentOption) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {paymentOption = a} :: ReservedElasticsearchInstanceOffering)

-- | The rate you are charged for each hour the domain that is using the
-- offering is running.
reservedElasticsearchInstanceOffering_usagePrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstanceOffering_usagePrice = Lens.lens (\ReservedElasticsearchInstanceOffering' {usagePrice} -> usagePrice) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {usagePrice = a} :: ReservedElasticsearchInstanceOffering)

instance
  Data.FromJSON
    ReservedElasticsearchInstanceOffering
  where
  parseJSON =
    Data.withObject
      "ReservedElasticsearchInstanceOffering"
      ( \x ->
          ReservedElasticsearchInstanceOffering'
            Prelude.<$> ( x Data..:? "RecurringCharges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ElasticsearchInstanceType")
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "CurrencyCode")
            Prelude.<*> ( x
                            Data..:? "ReservedElasticsearchInstanceOfferingId"
                        )
            Prelude.<*> (x Data..:? "FixedPrice")
            Prelude.<*> (x Data..:? "PaymentOption")
            Prelude.<*> (x Data..:? "UsagePrice")
      )

instance
  Prelude.Hashable
    ReservedElasticsearchInstanceOffering
  where
  hashWithSalt
    _salt
    ReservedElasticsearchInstanceOffering' {..} =
      _salt `Prelude.hashWithSalt` recurringCharges
        `Prelude.hashWithSalt` elasticsearchInstanceType
        `Prelude.hashWithSalt` duration
        `Prelude.hashWithSalt` currencyCode
        `Prelude.hashWithSalt` reservedElasticsearchInstanceOfferingId
        `Prelude.hashWithSalt` fixedPrice
        `Prelude.hashWithSalt` paymentOption
        `Prelude.hashWithSalt` usagePrice

instance
  Prelude.NFData
    ReservedElasticsearchInstanceOffering
  where
  rnf ReservedElasticsearchInstanceOffering' {..} =
    Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf elasticsearchInstanceType
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf reservedElasticsearchInstanceOfferingId
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf usagePrice
