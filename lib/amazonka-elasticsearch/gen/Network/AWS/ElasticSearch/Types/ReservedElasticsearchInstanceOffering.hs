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
    reioReservedElasticsearchInstanceOfferingId,
    reioElasticsearchInstanceType,
    reioRecurringCharges,
    reioUsagePrice,
    reioFixedPrice,
    reioDuration,
    reioPaymentOption,
  )
where

import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of a reserved Elasticsearch instance offering.
--
-- /See:/ 'mkReservedElasticsearchInstanceOffering' smart constructor.
data ReservedElasticsearchInstanceOffering = ReservedElasticsearchInstanceOffering'
  { -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Lude.Maybe Lude.Text,
    -- | The Elasticsearch reserved instance offering identifier.
    reservedElasticsearchInstanceOfferingId :: Lude.Maybe Lude.Text,
    -- | The Elasticsearch instance type offered by the reserved instance offering.
    elasticsearchInstanceType :: Lude.Maybe ESPartitionInstanceType,
    -- | The charge to your account regardless of whether you are creating any domains using the instance offering.
    recurringCharges :: Lude.Maybe [RecurringCharge],
    -- | The rate you are charged for each hour the domain that is using the offering is running.
    usagePrice :: Lude.Maybe Lude.Double,
    -- | The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
    duration :: Lude.Maybe Lude.Int,
    -- | Payment option for the reserved Elasticsearch instance offering
    paymentOption :: Lude.Maybe ReservedElasticsearchInstancePaymentOption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedElasticsearchInstanceOffering' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency code for the reserved Elasticsearch instance offering.
-- * 'reservedElasticsearchInstanceOfferingId' - The Elasticsearch reserved instance offering identifier.
-- * 'elasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance offering.
-- * 'recurringCharges' - The charge to your account regardless of whether you are creating any domains using the instance offering.
-- * 'usagePrice' - The rate you are charged for each hour the domain that is using the offering is running.
-- * 'fixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
-- * 'duration' - The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
-- * 'paymentOption' - Payment option for the reserved Elasticsearch instance offering
mkReservedElasticsearchInstanceOffering ::
  ReservedElasticsearchInstanceOffering
mkReservedElasticsearchInstanceOffering =
  ReservedElasticsearchInstanceOffering'
    { currencyCode =
        Lude.Nothing,
      reservedElasticsearchInstanceOfferingId = Lude.Nothing,
      elasticsearchInstanceType = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing,
      paymentOption = Lude.Nothing
    }

-- | The currency code for the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioCurrencyCode :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe Lude.Text)
reioCurrencyCode = Lens.lens (currencyCode :: ReservedElasticsearchInstanceOffering -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The Elasticsearch reserved instance offering identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioReservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe Lude.Text)
reioReservedElasticsearchInstanceOfferingId = Lens.lens (reservedElasticsearchInstanceOfferingId :: ReservedElasticsearchInstanceOffering -> Lude.Maybe Lude.Text) (\s a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

-- | The Elasticsearch instance type offered by the reserved instance offering.
--
-- /Note:/ Consider using 'elasticsearchInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioElasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe ESPartitionInstanceType)
reioElasticsearchInstanceType = Lens.lens (elasticsearchInstanceType :: ReservedElasticsearchInstanceOffering -> Lude.Maybe ESPartitionInstanceType) (\s a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioElasticsearchInstanceType "Use generic-lens or generic-optics with 'elasticsearchInstanceType' instead." #-}

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioRecurringCharges :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe [RecurringCharge])
reioRecurringCharges = Lens.lens (recurringCharges :: ReservedElasticsearchInstanceOffering -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The rate you are charged for each hour the domain that is using the offering is running.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioUsagePrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe Lude.Double)
reioUsagePrice = Lens.lens (usagePrice :: ReservedElasticsearchInstanceOffering -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioFixedPrice :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe Lude.Double)
reioFixedPrice = Lens.lens (fixedPrice :: ReservedElasticsearchInstanceOffering -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioDuration :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe Lude.Int)
reioDuration = Lens.lens (duration :: ReservedElasticsearchInstanceOffering -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Payment option for the reserved Elasticsearch instance offering
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reioPaymentOption :: Lens.Lens' ReservedElasticsearchInstanceOffering (Lude.Maybe ReservedElasticsearchInstancePaymentOption)
reioPaymentOption = Lens.lens (paymentOption :: ReservedElasticsearchInstanceOffering -> Lude.Maybe ReservedElasticsearchInstancePaymentOption) (\s a -> s {paymentOption = a} :: ReservedElasticsearchInstanceOffering)
{-# DEPRECATED reioPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.FromJSON ReservedElasticsearchInstanceOffering where
  parseJSON =
    Lude.withObject
      "ReservedElasticsearchInstanceOffering"
      ( \x ->
          ReservedElasticsearchInstanceOffering'
            Lude.<$> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "ReservedElasticsearchInstanceOfferingId")
            Lude.<*> (x Lude..:? "ElasticsearchInstanceType")
            Lude.<*> (x Lude..:? "RecurringCharges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UsagePrice")
            Lude.<*> (x Lude..:? "FixedPrice")
            Lude.<*> (x Lude..:? "Duration")
            Lude.<*> (x Lude..:? "PaymentOption")
      )
