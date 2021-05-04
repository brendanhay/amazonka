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
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering where

import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of a reserved Elasticsearch instance offering.
--
-- /See:/ 'newReservedElasticsearchInstanceOffering' smart constructor.
data ReservedElasticsearchInstanceOffering = ReservedElasticsearchInstanceOffering'
  { -- | Payment option for the reserved Elasticsearch instance offering
    paymentOption :: Prelude.Maybe ReservedElasticsearchInstancePaymentOption,
    -- | The duration, in seconds, for which the offering will reserve the
    -- Elasticsearch instance.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch instance type offered by the reserved instance
    -- offering.
    elasticsearchInstanceType :: Prelude.Maybe ESPartitionInstanceType,
    -- | The upfront fixed charge you will pay to purchase the specific reserved
    -- Elasticsearch instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The Elasticsearch reserved instance offering identifier.
    reservedElasticsearchInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The rate you are charged for each hour the domain that is using the
    -- offering is running.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservedElasticsearchInstanceOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paymentOption', 'reservedElasticsearchInstanceOffering_paymentOption' - Payment option for the reserved Elasticsearch instance offering
--
-- 'duration', 'reservedElasticsearchInstanceOffering_duration' - The duration, in seconds, for which the offering will reserve the
-- Elasticsearch instance.
--
-- 'currencyCode', 'reservedElasticsearchInstanceOffering_currencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- 'elasticsearchInstanceType', 'reservedElasticsearchInstanceOffering_elasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance
-- offering.
--
-- 'fixedPrice', 'reservedElasticsearchInstanceOffering_fixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved
-- Elasticsearch instance offering.
--
-- 'reservedElasticsearchInstanceOfferingId', 'reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId' - The Elasticsearch reserved instance offering identifier.
--
-- 'usagePrice', 'reservedElasticsearchInstanceOffering_usagePrice' - The rate you are charged for each hour the domain that is using the
-- offering is running.
--
-- 'recurringCharges', 'reservedElasticsearchInstanceOffering_recurringCharges' - The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
newReservedElasticsearchInstanceOffering ::
  ReservedElasticsearchInstanceOffering
newReservedElasticsearchInstanceOffering =
  ReservedElasticsearchInstanceOffering'
    { paymentOption =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      elasticsearchInstanceType =
        Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      recurringCharges = Prelude.Nothing
    }

-- | Payment option for the reserved Elasticsearch instance offering
reservedElasticsearchInstanceOffering_paymentOption :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe ReservedElasticsearchInstancePaymentOption)
reservedElasticsearchInstanceOffering_paymentOption = Lens.lens (\ReservedElasticsearchInstanceOffering' {paymentOption} -> paymentOption) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {paymentOption = a} :: ReservedElasticsearchInstanceOffering)

-- | The duration, in seconds, for which the offering will reserve the
-- Elasticsearch instance.
reservedElasticsearchInstanceOffering_duration :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Int)
reservedElasticsearchInstanceOffering_duration = Lens.lens (\ReservedElasticsearchInstanceOffering' {duration} -> duration) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {duration = a} :: ReservedElasticsearchInstanceOffering)

-- | The currency code for the reserved Elasticsearch instance offering.
reservedElasticsearchInstanceOffering_currencyCode :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstanceOffering_currencyCode = Lens.lens (\ReservedElasticsearchInstanceOffering' {currencyCode} -> currencyCode) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {currencyCode = a} :: ReservedElasticsearchInstanceOffering)

-- | The Elasticsearch instance type offered by the reserved instance
-- offering.
reservedElasticsearchInstanceOffering_elasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe ESPartitionInstanceType)
reservedElasticsearchInstanceOffering_elasticsearchInstanceType = Lens.lens (\ReservedElasticsearchInstanceOffering' {elasticsearchInstanceType} -> elasticsearchInstanceType) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstanceOffering)

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- Elasticsearch instance offering.
reservedElasticsearchInstanceOffering_fixedPrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstanceOffering_fixedPrice = Lens.lens (\ReservedElasticsearchInstanceOffering' {fixedPrice} -> fixedPrice) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {fixedPrice = a} :: ReservedElasticsearchInstanceOffering)

-- | The Elasticsearch reserved instance offering identifier.
reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Text)
reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId = Lens.lens (\ReservedElasticsearchInstanceOffering' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstanceOffering)

-- | The rate you are charged for each hour the domain that is using the
-- offering is running.
reservedElasticsearchInstanceOffering_usagePrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Double)
reservedElasticsearchInstanceOffering_usagePrice = Lens.lens (\ReservedElasticsearchInstanceOffering' {usagePrice} -> usagePrice) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {usagePrice = a} :: ReservedElasticsearchInstanceOffering)

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedElasticsearchInstanceOffering_recurringCharges :: Lens.Lens' ReservedElasticsearchInstanceOffering (Prelude.Maybe [RecurringCharge])
reservedElasticsearchInstanceOffering_recurringCharges = Lens.lens (\ReservedElasticsearchInstanceOffering' {recurringCharges} -> recurringCharges) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {recurringCharges = a} :: ReservedElasticsearchInstanceOffering) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    ReservedElasticsearchInstanceOffering
  where
  parseJSON =
    Prelude.withObject
      "ReservedElasticsearchInstanceOffering"
      ( \x ->
          ReservedElasticsearchInstanceOffering'
            Prelude.<$> (x Prelude..:? "PaymentOption")
            Prelude.<*> (x Prelude..:? "Duration")
            Prelude.<*> (x Prelude..:? "CurrencyCode")
            Prelude.<*> (x Prelude..:? "ElasticsearchInstanceType")
            Prelude.<*> (x Prelude..:? "FixedPrice")
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
    ReservedElasticsearchInstanceOffering

instance
  Prelude.NFData
    ReservedElasticsearchInstanceOffering
