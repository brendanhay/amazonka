{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
  ( ReservedElasticsearchInstance (..)
  -- * Smart constructor
  , mkReservedElasticsearchInstance
  -- * Lenses
  , reiCurrencyCode
  , reiDuration
  , reiElasticsearchInstanceCount
  , reiElasticsearchInstanceType
  , reiFixedPrice
  , reiPaymentOption
  , reiRecurringCharges
  , reiReservationName
  , reiReservedElasticsearchInstanceId
  , reiReservedElasticsearchInstanceOfferingId
  , reiStartTime
  , reiState
  , reiUsagePrice
  ) where

import qualified Network.AWS.ElasticSearch.Types.ESPartitionInstanceType as Types
import qualified Network.AWS.ElasticSearch.Types.RecurringCharge as Types
import qualified Network.AWS.ElasticSearch.Types.ReservationToken as Types
import qualified Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceId as Types
import qualified Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of a reserved Elasticsearch instance.
--
-- /See:/ 'mkReservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { currencyCode :: Core.Maybe Core.Text
    -- ^ The currency code for the reserved Elasticsearch instance offering.
  , duration :: Core.Maybe Core.Int
    -- ^ The duration, in seconds, for which the Elasticsearch instance is reserved.
  , elasticsearchInstanceCount :: Core.Maybe Core.Int
    -- ^ The number of Elasticsearch instances that have been reserved.
  , elasticsearchInstanceType :: Core.Maybe Types.ESPartitionInstanceType
    -- ^ The Elasticsearch instance type offered by the reserved instance offering.
  , fixedPrice :: Core.Maybe Core.Double
    -- ^ The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering. 
  , paymentOption :: Core.Maybe Types.ReservedElasticsearchInstancePaymentOption
    -- ^ The payment option as defined in the reserved Elasticsearch instance offering.
  , recurringCharges :: Core.Maybe [Types.RecurringCharge]
    -- ^ The charge to your account regardless of whether you are creating any domains using the instance offering.
  , reservationName :: Core.Maybe Types.ReservationToken
    -- ^ The customer-specified identifier to track this reservation.
  , reservedElasticsearchInstanceId :: Core.Maybe Types.ReservedElasticsearchInstanceId
    -- ^ The unique identifier for the reservation.
  , reservedElasticsearchInstanceOfferingId :: Core.Maybe Core.Text
    -- ^ The offering identifier.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the reservation started.
  , state :: Core.Maybe Core.Text
    -- ^ The state of the reserved Elasticsearch instance.
  , usagePrice :: Core.Maybe Core.Double
    -- ^ The rate you are charged for each hour for the domain that is using this reserved instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReservedElasticsearchInstance' value with any optional fields omitted.
mkReservedElasticsearchInstance
    :: ReservedElasticsearchInstance
mkReservedElasticsearchInstance
  = ReservedElasticsearchInstance'{currencyCode = Core.Nothing,
                                   duration = Core.Nothing,
                                   elasticsearchInstanceCount = Core.Nothing,
                                   elasticsearchInstanceType = Core.Nothing,
                                   fixedPrice = Core.Nothing, paymentOption = Core.Nothing,
                                   recurringCharges = Core.Nothing, reservationName = Core.Nothing,
                                   reservedElasticsearchInstanceId = Core.Nothing,
                                   reservedElasticsearchInstanceOfferingId = Core.Nothing,
                                   startTime = Core.Nothing, state = Core.Nothing,
                                   usagePrice = Core.Nothing}

-- | The currency code for the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiCurrencyCode :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reiCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE reiCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The duration, in seconds, for which the Elasticsearch instance is reserved.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiDuration :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Int)
reiDuration = Lens.field @"duration"
{-# INLINEABLE reiDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The number of Elasticsearch instances that have been reserved.
--
-- /Note:/ Consider using 'elasticsearchInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticsearchInstanceCount :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Int)
reiElasticsearchInstanceCount = Lens.field @"elasticsearchInstanceCount"
{-# INLINEABLE reiElasticsearchInstanceCount #-}
{-# DEPRECATED elasticsearchInstanceCount "Use generic-lens or generic-optics with 'elasticsearchInstanceCount' instead"  #-}

-- | The Elasticsearch instance type offered by the reserved instance offering.
--
-- /Note:/ Consider using 'elasticsearchInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticsearchInstanceType :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ESPartitionInstanceType)
reiElasticsearchInstanceType = Lens.field @"elasticsearchInstanceType"
{-# INLINEABLE reiElasticsearchInstanceType #-}
{-# DEPRECATED elasticsearchInstanceType "Use generic-lens or generic-optics with 'elasticsearchInstanceType' instead"  #-}

-- | The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering. 
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiFixedPrice :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Double)
reiFixedPrice = Lens.field @"fixedPrice"
{-# INLINEABLE reiFixedPrice #-}
{-# DEPRECATED fixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead"  #-}

-- | The payment option as defined in the reserved Elasticsearch instance offering.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiPaymentOption :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ReservedElasticsearchInstancePaymentOption)
reiPaymentOption = Lens.field @"paymentOption"
{-# INLINEABLE reiPaymentOption #-}
{-# DEPRECATED paymentOption "Use generic-lens or generic-optics with 'paymentOption' instead"  #-}

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiRecurringCharges :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe [Types.RecurringCharge])
reiRecurringCharges = Lens.field @"recurringCharges"
{-# INLINEABLE reiRecurringCharges #-}
{-# DEPRECATED recurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead"  #-}

-- | The customer-specified identifier to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservationName :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ReservationToken)
reiReservationName = Lens.field @"reservationName"
{-# INLINEABLE reiReservationName #-}
{-# DEPRECATED reservationName "Use generic-lens or generic-optics with 'reservationName' instead"  #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservedElasticsearchInstanceId :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Types.ReservedElasticsearchInstanceId)
reiReservedElasticsearchInstanceId = Lens.field @"reservedElasticsearchInstanceId"
{-# INLINEABLE reiReservedElasticsearchInstanceId #-}
{-# DEPRECATED reservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead"  #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiReservedElasticsearchInstanceOfferingId :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reiReservedElasticsearchInstanceOfferingId = Lens.field @"reservedElasticsearchInstanceOfferingId"
{-# INLINEABLE reiReservedElasticsearchInstanceOfferingId #-}
{-# DEPRECATED reservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead"  #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiStartTime :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.NominalDiffTime)
reiStartTime = Lens.field @"startTime"
{-# INLINEABLE reiStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The state of the reserved Elasticsearch instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiState :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Text)
reiState = Lens.field @"state"
{-# INLINEABLE reiState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The rate you are charged for each hour for the domain that is using this reserved instance.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiUsagePrice :: Lens.Lens' ReservedElasticsearchInstance (Core.Maybe Core.Double)
reiUsagePrice = Lens.field @"usagePrice"
{-# INLINEABLE reiUsagePrice #-}
{-# DEPRECATED usagePrice "Use generic-lens or generic-optics with 'usagePrice' instead"  #-}

instance Core.FromJSON ReservedElasticsearchInstance where
        parseJSON
          = Core.withObject "ReservedElasticsearchInstance" Core.$
              \ x ->
                ReservedElasticsearchInstance' Core.<$>
                  (x Core..:? "CurrencyCode") Core.<*> x Core..:? "Duration" Core.<*>
                    x Core..:? "ElasticsearchInstanceCount"
                    Core.<*> x Core..:? "ElasticsearchInstanceType"
                    Core.<*> x Core..:? "FixedPrice"
                    Core.<*> x Core..:? "PaymentOption"
                    Core.<*> x Core..:? "RecurringCharges"
                    Core.<*> x Core..:? "ReservationName"
                    Core.<*> x Core..:? "ReservedElasticsearchInstanceId"
                    Core.<*> x Core..:? "ReservedElasticsearchInstanceOfferingId"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "UsagePrice"
