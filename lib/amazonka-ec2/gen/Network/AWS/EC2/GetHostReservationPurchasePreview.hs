{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetHostReservationPurchasePreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Preview a reservation purchase with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation.
--
-- This is a preview of the 'PurchaseHostReservation' action and does not result in the offering being purchased.
module Network.AWS.EC2.GetHostReservationPurchasePreview
  ( -- * Creating a request
    GetHostReservationPurchasePreview (..),
    mkGetHostReservationPurchasePreview,

    -- ** Request lenses
    ghrppHostIdSet,
    ghrppOfferingId,

    -- * Destructuring the response
    GetHostReservationPurchasePreviewResponse (..),
    mkGetHostReservationPurchasePreviewResponse,

    -- ** Response lenses
    ghrpprrsCurrencyCode,
    ghrpprrsPurchase,
    ghrpprrsTotalHourlyPrice,
    ghrpprrsTotalUpfrontPrice,
    ghrpprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetHostReservationPurchasePreview' smart constructor.
data GetHostReservationPurchasePreview = GetHostReservationPurchasePreview'
  { -- | The IDs of the Dedicated Hosts with which the reservation is associated.
    hostIdSet :: [Types.DedicatedHostId],
    -- | The offering ID of the reservation.
    offeringId :: Types.OfferingId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostReservationPurchasePreview' value with any optional fields omitted.
mkGetHostReservationPurchasePreview ::
  -- | 'offeringId'
  Types.OfferingId ->
  GetHostReservationPurchasePreview
mkGetHostReservationPurchasePreview offeringId =
  GetHostReservationPurchasePreview'
    { hostIdSet = Core.mempty,
      offeringId
    }

-- | The IDs of the Dedicated Hosts with which the reservation is associated.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrppHostIdSet :: Lens.Lens' GetHostReservationPurchasePreview [Types.DedicatedHostId]
ghrppHostIdSet = Lens.field @"hostIdSet"
{-# DEPRECATED ghrppHostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead." #-}

-- | The offering ID of the reservation.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrppOfferingId :: Lens.Lens' GetHostReservationPurchasePreview Types.OfferingId
ghrppOfferingId = Lens.field @"offeringId"
{-# DEPRECATED ghrppOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

instance Core.AWSRequest GetHostReservationPurchasePreview where
  type
    Rs GetHostReservationPurchasePreview =
      GetHostReservationPurchasePreviewResponse
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
            ( Core.pure ("Action", "GetHostReservationPurchasePreview")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "HostIdSet" hostIdSet)
                Core.<> (Core.toQueryValue "OfferingId" offeringId)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostReservationPurchasePreviewResponse'
            Core.<$> (x Core..@? "currencyCode")
            Core.<*> (x Core..@? "purchase" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "totalHourlyPrice")
            Core.<*> (x Core..@? "totalUpfrontPrice")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetHostReservationPurchasePreviewResponse' smart constructor.
data GetHostReservationPurchasePreviewResponse = GetHostReservationPurchasePreviewResponse'
  { -- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Core.Maybe Types.CurrencyCodeValues,
    -- | The purchase information of the Dedicated Host reservation and the Dedicated Hosts associated with it.
    purchase :: Core.Maybe [Types.Purchase],
    -- | The potential total hourly price of the reservation per hour.
    totalHourlyPrice :: Core.Maybe Types.String,
    -- | The potential total upfront price. This is billed immediately.
    totalUpfrontPrice :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostReservationPurchasePreviewResponse' value with any optional fields omitted.
mkGetHostReservationPurchasePreviewResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetHostReservationPurchasePreviewResponse
mkGetHostReservationPurchasePreviewResponse responseStatus =
  GetHostReservationPurchasePreviewResponse'
    { currencyCode =
        Core.Nothing,
      purchase = Core.Nothing,
      totalHourlyPrice = Core.Nothing,
      totalUpfrontPrice = Core.Nothing,
      responseStatus
    }

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprrsCurrencyCode :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe Types.CurrencyCodeValues)
ghrpprrsCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED ghrpprrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The purchase information of the Dedicated Host reservation and the Dedicated Hosts associated with it.
--
-- /Note:/ Consider using 'purchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprrsPurchase :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe [Types.Purchase])
ghrpprrsPurchase = Lens.field @"purchase"
{-# DEPRECATED ghrpprrsPurchase "Use generic-lens or generic-optics with 'purchase' instead." #-}

-- | The potential total hourly price of the reservation per hour.
--
-- /Note:/ Consider using 'totalHourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprrsTotalHourlyPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe Types.String)
ghrpprrsTotalHourlyPrice = Lens.field @"totalHourlyPrice"
{-# DEPRECATED ghrpprrsTotalHourlyPrice "Use generic-lens or generic-optics with 'totalHourlyPrice' instead." #-}

-- | The potential total upfront price. This is billed immediately.
--
-- /Note:/ Consider using 'totalUpfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprrsTotalUpfrontPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe Types.String)
ghrpprrsTotalUpfrontPrice = Lens.field @"totalUpfrontPrice"
{-# DEPRECATED ghrpprrsTotalUpfrontPrice "Use generic-lens or generic-optics with 'totalUpfrontPrice' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprrsResponseStatus :: Lens.Lens' GetHostReservationPurchasePreviewResponse Core.Int
ghrpprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ghrpprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
