{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved Elasticsearch instances.
module Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
  ( -- * Creating a request
    PurchaseReservedElasticsearchInstanceOffering (..),
    mkPurchaseReservedElasticsearchInstanceOffering,

    -- ** Request lenses
    preioReservedElasticsearchInstanceOfferingId,
    preioReservationName,
    preioInstanceCount,

    -- * Destructuring the response
    PurchaseReservedElasticsearchInstanceOfferingResponse (..),
    mkPurchaseReservedElasticsearchInstanceOfferingResponse,

    -- ** Response lenses
    preiorrsReservationName,
    preiorrsReservedElasticsearchInstanceId,
    preiorrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to @PurchaseReservedElasticsearchInstanceOffering@
--
-- /See:/ 'mkPurchaseReservedElasticsearchInstanceOffering' smart constructor.
data PurchaseReservedElasticsearchInstanceOffering = PurchaseReservedElasticsearchInstanceOffering'
  { -- | The ID of the reserved Elasticsearch instance offering to purchase.
    reservedElasticsearchInstanceOfferingId :: Types.ReservedElasticsearchInstanceOfferingId,
    -- | A customer-specified identifier to track this reservation.
    reservationName :: Types.ReservationToken,
    -- | The number of Elasticsearch instances to reserve.
    instanceCount :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedElasticsearchInstanceOffering' value with any optional fields omitted.
mkPurchaseReservedElasticsearchInstanceOffering ::
  -- | 'reservedElasticsearchInstanceOfferingId'
  Types.ReservedElasticsearchInstanceOfferingId ->
  -- | 'reservationName'
  Types.ReservationToken ->
  PurchaseReservedElasticsearchInstanceOffering
mkPurchaseReservedElasticsearchInstanceOffering
  reservedElasticsearchInstanceOfferingId
  reservationName =
    PurchaseReservedElasticsearchInstanceOffering'
      { reservedElasticsearchInstanceOfferingId,
        reservationName,
        instanceCount = Core.Nothing
      }

-- | The ID of the reserved Elasticsearch instance offering to purchase.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioReservedElasticsearchInstanceOfferingId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Types.ReservedElasticsearchInstanceOfferingId
preioReservedElasticsearchInstanceOfferingId = Lens.field @"reservedElasticsearchInstanceOfferingId"
{-# DEPRECATED preioReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

-- | A customer-specified identifier to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioReservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Types.ReservationToken
preioReservationName = Lens.field @"reservationName"
{-# DEPRECATED preioReservationName "Use generic-lens or generic-optics with 'reservationName' instead." #-}

-- | The number of Elasticsearch instances to reserve.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioInstanceCount :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering (Core.Maybe Core.Natural)
preioInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED preioInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

instance
  Core.FromJSON
    PurchaseReservedElasticsearchInstanceOffering
  where
  toJSON PurchaseReservedElasticsearchInstanceOffering {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReservedElasticsearchInstanceOfferingId"
                  Core..= reservedElasticsearchInstanceOfferingId
              ),
            Core.Just ("ReservationName" Core..= reservationName),
            ("InstanceCount" Core..=) Core.<$> instanceCount
          ]
      )

instance
  Core.AWSRequest
    PurchaseReservedElasticsearchInstanceOffering
  where
  type
    Rs PurchaseReservedElasticsearchInstanceOffering =
      PurchaseReservedElasticsearchInstanceOfferingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/2015-01-01/es/purchaseReservedInstanceOffering",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseReservedElasticsearchInstanceOfferingResponse'
            Core.<$> (x Core..:? "ReservationName")
            Core.<*> (x Core..:? "ReservedElasticsearchInstanceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @PurchaseReservedElasticsearchInstanceOffering@ operation.
--
-- /See:/ 'mkPurchaseReservedElasticsearchInstanceOfferingResponse' smart constructor.
data PurchaseReservedElasticsearchInstanceOfferingResponse = PurchaseReservedElasticsearchInstanceOfferingResponse'
  { -- | The customer-specified identifier used to track this reservation.
    reservationName :: Core.Maybe Types.ReservationToken,
    -- | Details of the reserved Elasticsearch instance which was purchased.
    reservedElasticsearchInstanceId :: Core.Maybe Types.ReservedElasticsearchInstanceId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedElasticsearchInstanceOfferingResponse' value with any optional fields omitted.
mkPurchaseReservedElasticsearchInstanceOfferingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PurchaseReservedElasticsearchInstanceOfferingResponse
mkPurchaseReservedElasticsearchInstanceOfferingResponse
  responseStatus =
    PurchaseReservedElasticsearchInstanceOfferingResponse'
      { reservationName =
          Core.Nothing,
        reservedElasticsearchInstanceId =
          Core.Nothing,
        responseStatus
      }

-- | The customer-specified identifier used to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorrsReservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Core.Maybe Types.ReservationToken)
preiorrsReservationName = Lens.field @"reservationName"
{-# DEPRECATED preiorrsReservationName "Use generic-lens or generic-optics with 'reservationName' instead." #-}

-- | Details of the reserved Elasticsearch instance which was purchased.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorrsReservedElasticsearchInstanceId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Core.Maybe Types.ReservedElasticsearchInstanceId)
preiorrsReservedElasticsearchInstanceId = Lens.field @"reservedElasticsearchInstanceId"
{-# DEPRECATED preiorrsReservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorrsResponseStatus :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse Core.Int
preiorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED preiorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
