{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
  ( ReservedElasticsearchInstanceOffering (..),

    -- * Smart constructor
    mkReservedElasticsearchInstanceOffering,

    -- * Lenses
    reioCurrencyCode,
    reioDuration,
    reioElasticsearchInstanceType,
    reioFixedPrice,
    reioPaymentOption,
    reioRecurringCharges,
    reioReservedElasticsearchInstanceOfferingId,
    reioUsagePrice,
  )
where

import qualified Network.AWS.ElasticSearch.Types.ESPartitionInstanceType as Types
import qualified Network.AWS.ElasticSearch.Types.RecurringCharge as Types
import qualified Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOfferingId as Types
import qualified Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption as Types
import qualified Network.AWS.ElasticSearch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of a reserved Elasticsearch instance offering.
--
-- /See:/ 'mkReservedElasticsearchInstanceOffering' smart constructor.
data ReservedElasticsearchInstanceOffering = ReservedElasticsearchInstanceOffering'
  { -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Core.Maybe Types.String,
    -- | The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
    duration :: Core.Maybe Core.Int,
    -- | The Elasticsearch instance type offered by the reserved instance offering.
    elasticsearchInstanceType :: Core.Maybe Types.ESPartitionInstanceType,
    -- | The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | Payment option for the reserved Elasticsearch instance offering
    paymentOption :: Core.Maybe Types.ReservedElasticsearchInstancePaymentOption,
    -- | The charge to your account regardless of whether you are creating any domains using the instance offering.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The Elasticsearch reserved instance offering identifier.
    reservedElasticsearchInstanceOfferingId :: Core.Maybe Types.ReservedElasticsearchInstanceOfferingId,
    -- | The rate you are charged for each hour the domain that is using the offering is running.
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedElasticsearchInstanceOffering' value with any optional fields omitted.
mkReservedElasticsearchInstanceOffering ::
  ReservedElasticsearchInstanceOffering
mkReservedElasticsearchInstanceOffering =
  ReservedElasticsearchInstanceOffering'
    { currencyCode =
        Core.Nothing,
      duration = Core.Nothing,
      elasticsearchInstanceType = Core.Nothing,
      fixedPrice = Core.Nothing,
      paymentOption = Core.Nothing,
      recurringCharges = Core.Nothing,
      reservedElasticsearchInstanceOfferingId = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | The currency code for the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioCurrencyCode :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Types.String)
reioCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED reioCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioDuration :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Int)
reioDuration = Lens.field @"duration"
{-# DEPRECATED reioDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The Elasticsearch instance type offered by the reserved instance offering.
--
-- /Note:/ Consider using 'elasticsearchInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioElasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Types.ESPartitionInstanceType)
reioElasticsearchInstanceType = Lens.field @"elasticsearchInstanceType"
{-# DEPRECATED reioElasticsearchInstanceType "Use generic-lens or generic-optics with 'elasticsearchInstanceType' instead." #-}

-- | The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioFixedPrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Double)
reioFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED reioFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Payment option for the reserved Elasticsearch instance offering
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioPaymentOption :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Types.ReservedElasticsearchInstancePaymentOption)
reioPaymentOption = Lens.field @"paymentOption"
{-# DEPRECATED reioPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioRecurringCharges :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe [Types.RecurringCharge])
reioRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED reioRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The Elasticsearch reserved instance offering identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioReservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Types.ReservedElasticsearchInstanceOfferingId)
reioReservedElasticsearchInstanceOfferingId = Lens.field @"reservedElasticsearchInstanceOfferingId"
{-# DEPRECATED reioReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

-- | The rate you are charged for each hour the domain that is using the offering is running.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioUsagePrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Core.Maybe Core.Double)
reioUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED reioUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromJSON ReservedElasticsearchInstanceOffering where
  parseJSON =
    Core.withObject "ReservedElasticsearchInstanceOffering" Core.$
      \x ->
        ReservedElasticsearchInstanceOffering'
          Core.<$> (x Core..:? "CurrencyCode")
          Core.<*> (x Core..:? "Duration")
          Core.<*> (x Core..:? "ElasticsearchInstanceType")
          Core.<*> (x Core..:? "FixedPrice")
          Core.<*> (x Core..:? "PaymentOption")
          Core.<*> (x Core..:? "RecurringCharges")
          Core.<*> (x Core..:? "ReservedElasticsearchInstanceOfferingId")
          Core.<*> (x Core..:? "UsagePrice")
