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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Network.AWS.Lens as Lens

-- | Details of a reserved Elasticsearch instance offering.
--
-- /See:/ 'newReservedElasticsearchInstanceOffering' smart constructor.
data ReservedElasticsearchInstanceOffering = ReservedElasticsearchInstanceOffering'
  { -- | Payment option for the reserved Elasticsearch instance offering
    paymentOption :: Core.Maybe ReservedElasticsearchInstancePaymentOption,
    -- | The duration, in seconds, for which the offering will reserve the
    -- Elasticsearch instance.
    duration :: Core.Maybe Core.Int,
    -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Core.Maybe Core.Text,
    -- | The Elasticsearch instance type offered by the reserved instance
    -- offering.
    elasticsearchInstanceType :: Core.Maybe ESPartitionInstanceType,
    -- | The upfront fixed charge you will pay to purchase the specific reserved
    -- Elasticsearch instance offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The Elasticsearch reserved instance offering identifier.
    reservedElasticsearchInstanceOfferingId :: Core.Maybe Core.Text,
    -- | The rate you are charged for each hour the domain that is using the
    -- offering is running.
    usagePrice :: Core.Maybe Core.Double,
    -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Core.Maybe [RecurringCharge]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      duration = Core.Nothing,
      currencyCode = Core.Nothing,
      elasticsearchInstanceType =
        Core.Nothing,
      fixedPrice = Core.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Core.Nothing,
      usagePrice = Core.Nothing,
      recurringCharges = Core.Nothing
    }

-- | Payment option for the reserved Elasticsearch instance offering
reservedElasticsearchInstanceOffering_paymentOption :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe ReservedElasticsearchInstancePaymentOption)
reservedElasticsearchInstanceOffering_paymentOption = Lens.lens (\ReservedElasticsearchInstanceOffering' {paymentOption} -> paymentOption) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {paymentOption = a} :: ReservedElasticsearchInstanceOffering)

-- | The duration, in seconds, for which the offering will reserve the
-- Elasticsearch instance.
reservedElasticsearchInstanceOffering_duration :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Int)
reservedElasticsearchInstanceOffering_duration = Lens.lens (\ReservedElasticsearchInstanceOffering' {duration} -> duration) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {duration = a} :: ReservedElasticsearchInstanceOffering)

-- | The currency code for the reserved Elasticsearch instance offering.
reservedElasticsearchInstanceOffering_currencyCode :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Text)
reservedElasticsearchInstanceOffering_currencyCode = Lens.lens (\ReservedElasticsearchInstanceOffering' {currencyCode} -> currencyCode) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {currencyCode = a} :: ReservedElasticsearchInstanceOffering)

-- | The Elasticsearch instance type offered by the reserved instance
-- offering.
reservedElasticsearchInstanceOffering_elasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe ESPartitionInstanceType)
reservedElasticsearchInstanceOffering_elasticsearchInstanceType = Lens.lens (\ReservedElasticsearchInstanceOffering' {elasticsearchInstanceType} -> elasticsearchInstanceType) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstanceOffering)

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- Elasticsearch instance offering.
reservedElasticsearchInstanceOffering_fixedPrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Double)
reservedElasticsearchInstanceOffering_fixedPrice = Lens.lens (\ReservedElasticsearchInstanceOffering' {fixedPrice} -> fixedPrice) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {fixedPrice = a} :: ReservedElasticsearchInstanceOffering)

-- | The Elasticsearch reserved instance offering identifier.
reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Text)
reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId = Lens.lens (\ReservedElasticsearchInstanceOffering' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstanceOffering)

-- | The rate you are charged for each hour the domain that is using the
-- offering is running.
reservedElasticsearchInstanceOffering_usagePrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Double)
reservedElasticsearchInstanceOffering_usagePrice = Lens.lens (\ReservedElasticsearchInstanceOffering' {usagePrice} -> usagePrice) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {usagePrice = a} :: ReservedElasticsearchInstanceOffering)

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedElasticsearchInstanceOffering_recurringCharges :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe [RecurringCharge])
reservedElasticsearchInstanceOffering_recurringCharges = Lens.lens (\ReservedElasticsearchInstanceOffering' {recurringCharges} -> recurringCharges) (\s@ReservedElasticsearchInstanceOffering' {} a -> s {recurringCharges = a} :: ReservedElasticsearchInstanceOffering) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    ReservedElasticsearchInstanceOffering
  where
  parseJSON =
    Core.withObject
      "ReservedElasticsearchInstanceOffering"
      ( \x ->
          ReservedElasticsearchInstanceOffering'
            Core.<$> (x Core..:? "PaymentOption")
            Core.<*> (x Core..:? "Duration")
            Core.<*> (x Core..:? "CurrencyCode")
            Core.<*> (x Core..:? "ElasticsearchInstanceType")
            Core.<*> (x Core..:? "FixedPrice")
            Core.<*> ( x
                         Core..:? "ReservedElasticsearchInstanceOfferingId"
                     )
            Core.<*> (x Core..:? "UsagePrice")
            Core.<*> (x Core..:? "RecurringCharges" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    ReservedElasticsearchInstanceOffering

instance
  Core.NFData
    ReservedElasticsearchInstanceOffering
