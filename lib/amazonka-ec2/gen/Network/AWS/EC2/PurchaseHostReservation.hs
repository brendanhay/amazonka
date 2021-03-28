{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.PurchaseHostReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchase a reservation with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation. This action results in the specified reservation being purchased and charged to your account.
module Network.AWS.EC2.PurchaseHostReservation
    (
    -- * Creating a request
      PurchaseHostReservation (..)
    , mkPurchaseHostReservation
    -- ** Request lenses
    , phrHostIdSet
    , phrOfferingId
    , phrClientToken
    , phrCurrencyCode
    , phrLimitPrice
    , phrTagSpecifications

    -- * Destructuring the response
    , PurchaseHostReservationResponse (..)
    , mkPurchaseHostReservationResponse
    -- ** Response lenses
    , phrrrsClientToken
    , phrrrsCurrencyCode
    , phrrrsPurchase
    , phrrrsTotalHourlyPrice
    , phrrrsTotalUpfrontPrice
    , phrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPurchaseHostReservation' smart constructor.
data PurchaseHostReservation = PurchaseHostReservation'
  { hostIdSet :: [Types.DedicatedHostId]
    -- ^ The IDs of the Dedicated Hosts with which the reservation will be associated.
  , offeringId :: Types.OfferingId
    -- ^ The ID of the offering.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , currencyCode :: Core.Maybe Types.CurrencyCodeValues
    -- ^ The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
  , limitPrice :: Core.Maybe Core.Text
    -- ^ The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request fails. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the Dedicated Host Reservation during purchase.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseHostReservation' value with any optional fields omitted.
mkPurchaseHostReservation
    :: Types.OfferingId -- ^ 'offeringId'
    -> PurchaseHostReservation
mkPurchaseHostReservation offeringId
  = PurchaseHostReservation'{hostIdSet = Core.mempty, offeringId,
                             clientToken = Core.Nothing, currencyCode = Core.Nothing,
                             limitPrice = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The IDs of the Dedicated Hosts with which the reservation will be associated.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrHostIdSet :: Lens.Lens' PurchaseHostReservation [Types.DedicatedHostId]
phrHostIdSet = Lens.field @"hostIdSet"
{-# INLINEABLE phrHostIdSet #-}
{-# DEPRECATED hostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead"  #-}

-- | The ID of the offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrOfferingId :: Lens.Lens' PurchaseHostReservation Types.OfferingId
phrOfferingId = Lens.field @"offeringId"
{-# INLINEABLE phrOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrClientToken :: Lens.Lens' PurchaseHostReservation (Core.Maybe Core.Text)
phrClientToken = Lens.field @"clientToken"
{-# INLINEABLE phrClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrCurrencyCode :: Lens.Lens' PurchaseHostReservation (Core.Maybe Types.CurrencyCodeValues)
phrCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE phrCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request fails. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
--
-- /Note:/ Consider using 'limitPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrLimitPrice :: Lens.Lens' PurchaseHostReservation (Core.Maybe Core.Text)
phrLimitPrice = Lens.field @"limitPrice"
{-# INLINEABLE phrLimitPrice #-}
{-# DEPRECATED limitPrice "Use generic-lens or generic-optics with 'limitPrice' instead"  #-}

-- | The tags to apply to the Dedicated Host Reservation during purchase.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrTagSpecifications :: Lens.Lens' PurchaseHostReservation (Core.Maybe [Types.TagSpecification])
phrTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE phrTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery PurchaseHostReservation where
        toQuery PurchaseHostReservation{..}
          = Core.toQueryPair "Action"
              ("PurchaseHostReservation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "HostIdSet" hostIdSet
              Core.<> Core.toQueryPair "OfferingId" offeringId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CurrencyCode")
                currencyCode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LimitPrice") limitPrice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders PurchaseHostReservation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PurchaseHostReservation where
        type Rs PurchaseHostReservation = PurchaseHostReservationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 PurchaseHostReservationResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*> x Core..@? "currencyCode"
                     Core.<*> x Core..@? "purchase" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "totalHourlyPrice"
                     Core.<*> x Core..@? "totalUpfrontPrice"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPurchaseHostReservationResponse' smart constructor.
data PurchaseHostReservationResponse = PurchaseHostReservationResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , currencyCode :: Core.Maybe Types.CurrencyCodeValues
    -- ^ The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
  , purchase :: Core.Maybe [Types.Purchase]
    -- ^ Describes the details of the purchase.
  , totalHourlyPrice :: Core.Maybe Core.Text
    -- ^ The total hourly price of the reservation calculated per hour.
  , totalUpfrontPrice :: Core.Maybe Core.Text
    -- ^ The total amount charged to your account when you purchase the reservation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseHostReservationResponse' value with any optional fields omitted.
mkPurchaseHostReservationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PurchaseHostReservationResponse
mkPurchaseHostReservationResponse responseStatus
  = PurchaseHostReservationResponse'{clientToken = Core.Nothing,
                                     currencyCode = Core.Nothing, purchase = Core.Nothing,
                                     totalHourlyPrice = Core.Nothing,
                                     totalUpfrontPrice = Core.Nothing, responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsClientToken :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Core.Text)
phrrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE phrrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsCurrencyCode :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Types.CurrencyCodeValues)
phrrrsCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE phrrrsCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | Describes the details of the purchase.
--
-- /Note:/ Consider using 'purchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsPurchase :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe [Types.Purchase])
phrrrsPurchase = Lens.field @"purchase"
{-# INLINEABLE phrrrsPurchase #-}
{-# DEPRECATED purchase "Use generic-lens or generic-optics with 'purchase' instead"  #-}

-- | The total hourly price of the reservation calculated per hour.
--
-- /Note:/ Consider using 'totalHourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsTotalHourlyPrice :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Core.Text)
phrrrsTotalHourlyPrice = Lens.field @"totalHourlyPrice"
{-# INLINEABLE phrrrsTotalHourlyPrice #-}
{-# DEPRECATED totalHourlyPrice "Use generic-lens or generic-optics with 'totalHourlyPrice' instead"  #-}

-- | The total amount charged to your account when you purchase the reservation.
--
-- /Note:/ Consider using 'totalUpfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsTotalUpfrontPrice :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Core.Text)
phrrrsTotalUpfrontPrice = Lens.field @"totalUpfrontPrice"
{-# INLINEABLE phrrrsTotalUpfrontPrice #-}
{-# DEPRECATED totalUpfrontPrice "Use generic-lens or generic-optics with 'totalUpfrontPrice' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsResponseStatus :: Lens.Lens' PurchaseHostReservationResponse Core.Int
phrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE phrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
