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
    reiCurrencyCode,
    reiDuration,
    reiElasticsearchInstanceCount,
    reiElasticsearchInstanceType,
    reiFixedPrice,
    reiPaymentOption,
    reiRecurringCharges,
    reiReservationName,
    reiReservedElasticsearchInstanceId,
    reiReservedElasticsearchInstanceOfferingId,
    reiStartTime,
    reiState,
    reiUsagePrice,
  )
where

import qualified Network.AWS.ElasticSearch.Types.ESPartitionInstanceType as Types
import qualified Network.AWS.ElasticSearch.Types.RecurringCharge as Types
import qualified Network.AWS.ElasticSearch.Types.ReservationToken as Types
import qualified Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceId as Types
import qualified Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption as Types
import qualified Network.AWS.ElasticSearch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of a reserved Elasticsearch instance.
--
-- /See:/ 'mkReservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { -- | The currency code for the reserved Elasticsearch instance offering.
    currencyCode :: Core.Maybe Types.String,
    -- | The duration, in seconds, for which the Elasticsearch instance is reserved.
    duration :: Core.Maybe Core.Int,
    -- | The number of Elasticsearch instances that have been reserved.
    elasticsearchInstanceCount :: Core.Maybe Core.Int,
    -- | The Elasticsearch instance type offered by the reserved instance offering.
    elasticsearchInstanceType :: Core.Maybe Types.ESPartitionInstanceType,
    -- | The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The payment option as defined in the reserved Elasticsearch instance offering.
    paymentOption :: Core.Maybe Types.ReservedElasticsearchInstancePaymentOption,
    -- | The charge to your account regardless of whether you are creating any domains using the instance offering.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The customer-specified identifier to track this reservation.
    reservationName :: Core.Maybe Types.ReservationToken,
    -- | The unique identifier for the reservation.
    reservedElasticsearchInstanceId :: Core.Maybe Types.ReservedElasticsearchInstanceId,
    -- | The offering identifier.
    reservedElasticsearchInstanceOfferingId :: Core.Maybe Types.String,
    -- | The time the reservation started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The state of the reserved Elasticsearch instance.
    state :: Core.Maybe Types.String,
    -- | The rate you are charged for each hour for the domain that is using this reserved instance.
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReservedElasticsearchInstance' value with any optional fields omitted.
mkReservedElasticsearchInstance ::
  ReservedElasticsearchInstance
mkReservedElasticsearchInstance =
  ReservedElasticsearchInstance'
    { currencyCode = Core.Nothing,
      duration = Core.Nothing,
      elasticsearchInstanceCount = Core.Nothing,
      elasticsearchInstanceType = Core.Nothing,
      fixedPrice = Core.Nothing,
      paymentOption = Core.Nothing,
      recurringCharges = Core.Nothing,
      reservationName = Core.Nothing,
      reservedElasticsearchInstanceId = Core.Nothing,
      reservedElasticsearchInstanceOfferingId = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | The currency code for the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiCurrencyCode :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.String)
reiCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED reiCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The duration, in seconds, for which the Elasticsearch instance is reserved.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiDuration :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Int)
reiDuration = Lens.field @"duration"
{-# DEPRECATED reiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The number of Elasticsearch instances that have been reserved.
--
-- /Note:/ Consider using 'elasticsearchInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticsearchInstanceCount :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Int)
reiElasticsearchInstanceCount = Lens.field @"elasticsearchInstanceCount"
{-# DEPRECATED reiElasticsearchInstanceCount "Use generic-lens or generic-optics with 'elasticsearchInstanceCount' instead." #-}

-- | The Elasticsearch instance type offered by the reserved instance offering.
--
-- /Note:/ Consider using 'elasticsearchInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ESPartitionInstanceType)
reiElasticsearchInstanceType = Lens.field @"elasticsearchInstanceType"
{-# DEPRECATED reiElasticsearchInstanceType "Use generic-lens or generic-optics with 'elasticsearchInstanceType' instead." #-}

-- | The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiFixedPrice :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Double)
reiFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED reiFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The payment option as defined in the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiPaymentOption :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ReservedElasticsearchInstancePaymentOption)
reiPaymentOption = Lens.field @"paymentOption"
{-# DEPRECATED reiPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiRecurringCharges :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe [Types.RecurringCharge])
reiRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED reiRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The customer-specified identifier to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservationName :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ReservationToken)
reiReservationName = Lens.field @"reservationName"
{-# DEPRECATED reiReservationName "Use generic-lens or generic-optics with 'reservationName' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservedElasticsearchInstanceId :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ReservedElasticsearchInstanceId)
reiReservedElasticsearchInstanceId = Lens.field @"reservedElasticsearchInstanceId"
{-# DEPRECATED reiReservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.String)
reiReservedElasticsearchInstanceOfferingId = Lens.field @"reservedElasticsearchInstanceOfferingId"
{-# DEPRECATED reiReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiStartTime :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.NominalDiffTime)
reiStartTime = Lens.field @"startTime"
{-# DEPRECATED reiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The state of the reserved Elasticsearch instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiState :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.String)
reiState = Lens.field @"state"
{-# DEPRECATED reiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The rate you are charged for each hour for the domain that is using this reserved instance.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiUsagePrice :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Double)
reiUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED reiUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromJSON ReservedElasticsearchInstance where
  parseJSON =
    Core.withObject "ReservedElasticsearchInstance" Core.$
      \x ->
        ReservedElasticsearchInstance'
          Core.<$> (x Core..:? "CurrencyCode")
          Core.<*> (x Core..:? "Duration")
          Core.<*> (x Core..:? "ElasticsearchInstanceCount")
          Core.<*> (x Core..:? "ElasticsearchInstanceType")
          Core.<*> (x Core..:? "FixedPrice")
          Core.<*> (x Core..:? "PaymentOption")
          Core.<*> (x Core..:? "RecurringCharges")
          Core.<*> (x Core..:? "ReservationName")
          Core.<*> (x Core..:? "ReservedElasticsearchInstanceId")
          Core.<*> (x Core..:? "ReservedElasticsearchInstanceOfferingId")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "UsagePrice")
