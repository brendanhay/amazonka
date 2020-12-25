{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PurchaseHostReservation (..),
    mkPurchaseHostReservation,

    -- ** Request lenses
    phrHostIdSet,
    phrOfferingId,
    phrClientToken,
    phrCurrencyCode,
    phrLimitPrice,
    phrTagSpecifications,

    -- * Destructuring the response
    PurchaseHostReservationResponse (..),
    mkPurchaseHostReservationResponse,

    -- ** Response lenses
    phrrrsClientToken,
    phrrrsCurrencyCode,
    phrrrsPurchase,
    phrrrsTotalHourlyPrice,
    phrrrsTotalUpfrontPrice,
    phrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPurchaseHostReservation' smart constructor.
data PurchaseHostReservation = PurchaseHostReservation'
  { -- | The IDs of the Dedicated Hosts with which the reservation will be associated.
    hostIdSet :: [Types.DedicatedHostId],
    -- | The ID of the offering.
    offeringId :: Types.OfferingId,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Core.Maybe Types.CurrencyCodeValues,
    -- | The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request fails. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
    limitPrice :: Core.Maybe Types.String,
    -- | The tags to apply to the Dedicated Host Reservation during purchase.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseHostReservation' value with any optional fields omitted.
mkPurchaseHostReservation ::
  -- | 'offeringId'
  Types.OfferingId ->
  PurchaseHostReservation
mkPurchaseHostReservation offeringId =
  PurchaseHostReservation'
    { hostIdSet = Core.mempty,
      offeringId,
      clientToken = Core.Nothing,
      currencyCode = Core.Nothing,
      limitPrice = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The IDs of the Dedicated Hosts with which the reservation will be associated.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrHostIdSet :: Lens.Lens' PurchaseHostReservation [Types.DedicatedHostId]
phrHostIdSet = Lens.field @"hostIdSet"
{-# DEPRECATED phrHostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead." #-}

-- | The ID of the offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrOfferingId :: Lens.Lens' PurchaseHostReservation Types.OfferingId
phrOfferingId = Lens.field @"offeringId"
{-# DEPRECATED phrOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrClientToken :: Lens.Lens' PurchaseHostReservation (Core.Maybe Types.String)
phrClientToken = Lens.field @"clientToken"
{-# DEPRECATED phrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrCurrencyCode :: Lens.Lens' PurchaseHostReservation (Core.Maybe Types.CurrencyCodeValues)
phrCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED phrCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request fails. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
--
-- /Note:/ Consider using 'limitPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrLimitPrice :: Lens.Lens' PurchaseHostReservation (Core.Maybe Types.String)
phrLimitPrice = Lens.field @"limitPrice"
{-# DEPRECATED phrLimitPrice "Use generic-lens or generic-optics with 'limitPrice' instead." #-}

-- | The tags to apply to the Dedicated Host Reservation during purchase.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrTagSpecifications :: Lens.Lens' PurchaseHostReservation (Core.Maybe [Types.TagSpecification])
phrTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED phrTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest PurchaseHostReservation where
  type Rs PurchaseHostReservation = PurchaseHostReservationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PurchaseHostReservation")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "HostIdSet" hostIdSet)
                Core.<> (Core.toQueryValue "OfferingId" offeringId)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "CurrencyCode" Core.<$> currencyCode)
                Core.<> (Core.toQueryValue "LimitPrice" Core.<$> limitPrice)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          PurchaseHostReservationResponse'
            Core.<$> (x Core..@? "clientToken")
            Core.<*> (x Core..@? "currencyCode")
            Core.<*> (x Core..@? "purchase" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "totalHourlyPrice")
            Core.<*> (x Core..@? "totalUpfrontPrice")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPurchaseHostReservationResponse' smart constructor.
data PurchaseHostReservationResponse = PurchaseHostReservationResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Core.Maybe Types.CurrencyCodeValues,
    -- | Describes the details of the purchase.
    purchase :: Core.Maybe [Types.Purchase],
    -- | The total hourly price of the reservation calculated per hour.
    totalHourlyPrice :: Core.Maybe Types.String,
    -- | The total amount charged to your account when you purchase the reservation.
    totalUpfrontPrice :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseHostReservationResponse' value with any optional fields omitted.
mkPurchaseHostReservationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PurchaseHostReservationResponse
mkPurchaseHostReservationResponse responseStatus =
  PurchaseHostReservationResponse'
    { clientToken = Core.Nothing,
      currencyCode = Core.Nothing,
      purchase = Core.Nothing,
      totalHourlyPrice = Core.Nothing,
      totalUpfrontPrice = Core.Nothing,
      responseStatus
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsClientToken :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Types.String)
phrrrsClientToken = Lens.field @"clientToken"
{-# DEPRECATED phrrrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsCurrencyCode :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Types.CurrencyCodeValues)
phrrrsCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED phrrrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Describes the details of the purchase.
--
-- /Note:/ Consider using 'purchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsPurchase :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe [Types.Purchase])
phrrrsPurchase = Lens.field @"purchase"
{-# DEPRECATED phrrrsPurchase "Use generic-lens or generic-optics with 'purchase' instead." #-}

-- | The total hourly price of the reservation calculated per hour.
--
-- /Note:/ Consider using 'totalHourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsTotalHourlyPrice :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Types.String)
phrrrsTotalHourlyPrice = Lens.field @"totalHourlyPrice"
{-# DEPRECATED phrrrsTotalHourlyPrice "Use generic-lens or generic-optics with 'totalHourlyPrice' instead." #-}

-- | The total amount charged to your account when you purchase the reservation.
--
-- /Note:/ Consider using 'totalUpfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsTotalUpfrontPrice :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Types.String)
phrrrsTotalUpfrontPrice = Lens.field @"totalUpfrontPrice"
{-# DEPRECATED phrrrsTotalUpfrontPrice "Use generic-lens or generic-optics with 'totalUpfrontPrice' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrrsResponseStatus :: Lens.Lens' PurchaseHostReservationResponse Core.Int
phrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED phrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
