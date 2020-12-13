{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
  ( ReservedElasticsearchInstance (..),

    -- * Smart constructor
    mkReservedElasticsearchInstance,

    -- * Lenses
    reiState,
    reiCurrencyCode,
    reiStartTime,
    reiReservedElasticsearchInstanceOfferingId,
    reiReservedElasticsearchInstanceId,
    reiElasticsearchInstanceCount,
    reiReservationName,
    reiElasticsearchInstanceType,
    reiRecurringCharges,
    reiUsagePrice,
    reiFixedPrice,
    reiDuration,
    reiPaymentOption,
  )
where

import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of a reserved Elasticsearch instance.
--
-- /See:/ 'mkReservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { -- | The state of the reserved Elasticsearch instance.
    state :: Lude.Maybe Lude.Text,
    -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Lude.Maybe Lude.Text,
    -- | The time the reservation started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The offering identifier.
    reservedElasticsearchInstanceOfferingId :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the reservation.
    reservedElasticsearchInstanceId :: Lude.Maybe Lude.Text,
    -- | The number of Elasticsearch instances that have been reserved.
    elasticsearchInstanceCount :: Lude.Maybe Lude.Int,
    -- | The customer-specified identifier to track this reservation.
    reservationName :: Lude.Maybe Lude.Text,
    -- | The Elasticsearch instance type offered by the reserved instance offering.
    elasticsearchInstanceType :: Lude.Maybe ESPartitionInstanceType,
    -- | The charge to your account regardless of whether you are creating any domains using the instance offering.
    recurringCharges :: Lude.Maybe [RecurringCharge],
    -- | The rate you are charged for each hour for the domain that is using this reserved instance.
    usagePrice :: Lude.Maybe Lude.Double,
    -- | The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | The duration, in seconds, for which the Elasticsearch instance is reserved.
    duration :: Lude.Maybe Lude.Int,
    -- | The payment option as defined in the reserved Elasticsearch instance offering.
    paymentOption :: Lude.Maybe ReservedElasticsearchInstancePaymentOption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedElasticsearchInstance' with the minimum fields required to make a request.
--
-- * 'state' - The state of the reserved Elasticsearch instance.
-- * 'currencyCode' - The currency code for the reserved Elasticsearch instance offering.
-- * 'startTime' - The time the reservation started.
-- * 'reservedElasticsearchInstanceOfferingId' - The offering identifier.
-- * 'reservedElasticsearchInstanceId' - The unique identifier for the reservation.
-- * 'elasticsearchInstanceCount' - The number of Elasticsearch instances that have been reserved.
-- * 'reservationName' - The customer-specified identifier to track this reservation.
-- * 'elasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance offering.
-- * 'recurringCharges' - The charge to your account regardless of whether you are creating any domains using the instance offering.
-- * 'usagePrice' - The rate you are charged for each hour for the domain that is using this reserved instance.
-- * 'fixedPrice' - The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
-- * 'duration' - The duration, in seconds, for which the Elasticsearch instance is reserved.
-- * 'paymentOption' - The payment option as defined in the reserved Elasticsearch instance offering.
mkReservedElasticsearchInstance ::
  ReservedElasticsearchInstance
mkReservedElasticsearchInstance =
  ReservedElasticsearchInstance'
    { state = Lude.Nothing,
      currencyCode = Lude.Nothing,
      startTime = Lude.Nothing,
      reservedElasticsearchInstanceOfferingId = Lude.Nothing,
      reservedElasticsearchInstanceId = Lude.Nothing,
      elasticsearchInstanceCount = Lude.Nothing,
      reservationName = Lude.Nothing,
      elasticsearchInstanceType = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing,
      paymentOption = Lude.Nothing
    }

-- | The state of the reserved Elasticsearch instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiState :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Text)
reiState = Lens.lens (state :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The currency code for the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiCurrencyCode :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Text)
reiCurrencyCode = Lens.lens (currencyCode :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiStartTime :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Timestamp)
reiStartTime = Lens.lens (startTime :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Text)
reiReservedElasticsearchInstanceOfferingId = Lens.lens (reservedElasticsearchInstanceOfferingId :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Text) (\s a -> s {reservedElasticsearchInstanceOfferingId = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservedElasticsearchInstanceId :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Text)
reiReservedElasticsearchInstanceId = Lens.lens (reservedElasticsearchInstanceId :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Text) (\s a -> s {reservedElasticsearchInstanceId = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiReservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead." #-}

-- | The number of Elasticsearch instances that have been reserved.
--
-- /Note:/ Consider using 'elasticsearchInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticsearchInstanceCount :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Int)
reiElasticsearchInstanceCount = Lens.lens (elasticsearchInstanceCount :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Int) (\s a -> s {elasticsearchInstanceCount = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiElasticsearchInstanceCount "Use generic-lens or generic-optics with 'elasticsearchInstanceCount' instead." #-}

-- | The customer-specified identifier to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservationName :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Text)
reiReservationName = Lens.lens (reservationName :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Text) (\s a -> s {reservationName = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiReservationName "Use generic-lens or generic-optics with 'reservationName' instead." #-}

-- | The Elasticsearch instance type offered by the reserved instance offering.
--
-- /Note:/ Consider using 'elasticsearchInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe ESPartitionInstanceType)
reiElasticsearchInstanceType = Lens.lens (elasticsearchInstanceType :: ReservedElasticsearchInstance -> Lude.Maybe ESPartitionInstanceType) (\s a -> s {elasticsearchInstanceType = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiElasticsearchInstanceType "Use generic-lens or generic-optics with 'elasticsearchInstanceType' instead." #-}

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiRecurringCharges :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe [RecurringCharge])
reiRecurringCharges = Lens.lens (recurringCharges :: ReservedElasticsearchInstance -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The rate you are charged for each hour for the domain that is using this reserved instance.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiUsagePrice :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Double)
reiUsagePrice = Lens.lens (usagePrice :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiFixedPrice :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Double)
reiFixedPrice = Lens.lens (fixedPrice :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration, in seconds, for which the Elasticsearch instance is reserved.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiDuration :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe Lude.Int)
reiDuration = Lens.lens (duration :: ReservedElasticsearchInstance -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The payment option as defined in the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiPaymentOption :: Lens.Lens' ReservedElasticsearchInstance (Lude.Maybe ReservedElasticsearchInstancePaymentOption)
reiPaymentOption = Lens.lens (paymentOption :: ReservedElasticsearchInstance -> Lude.Maybe ReservedElasticsearchInstancePaymentOption) (\s a -> s {paymentOption = a} :: ReservedElasticsearchInstance)
{-# DEPRECATED reiPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.FromJSON ReservedElasticsearchInstance where
  parseJSON =
    Lude.withObject
      "ReservedElasticsearchInstance"
      ( \x ->
          ReservedElasticsearchInstance'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "ReservedElasticsearchInstanceOfferingId")
            Lude.<*> (x Lude..:? "ReservedElasticsearchInstanceId")
            Lude.<*> (x Lude..:? "ElasticsearchInstanceCount")
            Lude.<*> (x Lude..:? "ReservationName")
            Lude.<*> (x Lude..:? "ElasticsearchInstanceType")
            Lude.<*> (x Lude..:? "RecurringCharges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UsagePrice")
            Lude.<*> (x Lude..:? "FixedPrice")
            Lude.<*> (x Lude..:? "Duration")
            Lude.<*> (x Lude..:? "PaymentOption")
      )
