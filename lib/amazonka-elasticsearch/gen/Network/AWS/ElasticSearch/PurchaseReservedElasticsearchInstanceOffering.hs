{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PurchaseReservedElasticsearchInstanceOffering (..)
    , mkPurchaseReservedElasticsearchInstanceOffering
    -- ** Request lenses
    , preioReservedElasticsearchInstanceOfferingId
    , preioReservationName
    , preioInstanceCount

    -- * Destructuring the response
    , PurchaseReservedElasticsearchInstanceOfferingResponse (..)
    , mkPurchaseReservedElasticsearchInstanceOfferingResponse
    -- ** Response lenses
    , preiorrsReservationName
    , preiorrsReservedElasticsearchInstanceId
    , preiorrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to @PurchaseReservedElasticsearchInstanceOffering@ 
--
-- /See:/ 'mkPurchaseReservedElasticsearchInstanceOffering' smart constructor.
data PurchaseReservedElasticsearchInstanceOffering = PurchaseReservedElasticsearchInstanceOffering'
  { reservedElasticsearchInstanceOfferingId :: Types.ReservedElasticsearchInstanceOfferingId
    -- ^ The ID of the reserved Elasticsearch instance offering to purchase.
  , reservationName :: Types.ReservationToken
    -- ^ A customer-specified identifier to track this reservation.
  , instanceCount :: Core.Maybe Core.Natural
    -- ^ The number of Elasticsearch instances to reserve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedElasticsearchInstanceOffering' value with any optional fields omitted.
mkPurchaseReservedElasticsearchInstanceOffering
    :: Types.ReservedElasticsearchInstanceOfferingId -- ^ 'reservedElasticsearchInstanceOfferingId'
    -> Types.ReservationToken -- ^ 'reservationName'
    -> PurchaseReservedElasticsearchInstanceOffering
mkPurchaseReservedElasticsearchInstanceOffering
  reservedElasticsearchInstanceOfferingId reservationName
  = PurchaseReservedElasticsearchInstanceOffering'{reservedElasticsearchInstanceOfferingId,
                                                   reservationName, instanceCount = Core.Nothing}

-- | The ID of the reserved Elasticsearch instance offering to purchase.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioReservedElasticsearchInstanceOfferingId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Types.ReservedElasticsearchInstanceOfferingId
preioReservedElasticsearchInstanceOfferingId = Lens.field @"reservedElasticsearchInstanceOfferingId"
{-# INLINEABLE preioReservedElasticsearchInstanceOfferingId #-}
{-# DEPRECATED reservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead"  #-}

-- | A customer-specified identifier to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioReservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Types.ReservationToken
preioReservationName = Lens.field @"reservationName"
{-# INLINEABLE preioReservationName #-}
{-# DEPRECATED reservationName "Use generic-lens or generic-optics with 'reservationName' instead"  #-}

-- | The number of Elasticsearch instances to reserve.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioInstanceCount :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering (Core.Maybe Core.Natural)
preioInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE preioInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

instance Core.ToQuery PurchaseReservedElasticsearchInstanceOffering
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           PurchaseReservedElasticsearchInstanceOffering
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON
           PurchaseReservedElasticsearchInstanceOffering
         where
        toJSON PurchaseReservedElasticsearchInstanceOffering{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReservedElasticsearchInstanceOfferingId" Core..=
                       reservedElasticsearchInstanceOfferingId),
                  Core.Just ("ReservationName" Core..= reservationName),
                  ("InstanceCount" Core..=) Core.<$> instanceCount])

instance Core.AWSRequest
           PurchaseReservedElasticsearchInstanceOffering
         where
        type Rs PurchaseReservedElasticsearchInstanceOffering =
             PurchaseReservedElasticsearchInstanceOfferingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-01-01/es/purchaseReservedInstanceOffering",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PurchaseReservedElasticsearchInstanceOfferingResponse' Core.<$>
                   (x Core..:? "ReservationName") Core.<*>
                     x Core..:? "ReservedElasticsearchInstanceId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @PurchaseReservedElasticsearchInstanceOffering@ operation.
--
-- /See:/ 'mkPurchaseReservedElasticsearchInstanceOfferingResponse' smart constructor.
data PurchaseReservedElasticsearchInstanceOfferingResponse = PurchaseReservedElasticsearchInstanceOfferingResponse'
  { reservationName :: Core.Maybe Types.ReservationToken
    -- ^ The customer-specified identifier used to track this reservation.
  , reservedElasticsearchInstanceId :: Core.Maybe Types.ReservedElasticsearchInstanceId
    -- ^ Details of the reserved Elasticsearch instance which was purchased.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedElasticsearchInstanceOfferingResponse' value with any optional fields omitted.
mkPurchaseReservedElasticsearchInstanceOfferingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PurchaseReservedElasticsearchInstanceOfferingResponse
mkPurchaseReservedElasticsearchInstanceOfferingResponse
  responseStatus
  = PurchaseReservedElasticsearchInstanceOfferingResponse'{reservationName
                                                             = Core.Nothing,
                                                           reservedElasticsearchInstanceId =
                                                             Core.Nothing,
                                                           responseStatus}

-- | The customer-specified identifier used to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorrsReservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Core.Maybe Types.ReservationToken)
preiorrsReservationName = Lens.field @"reservationName"
{-# INLINEABLE preiorrsReservationName #-}
{-# DEPRECATED reservationName "Use generic-lens or generic-optics with 'reservationName' instead"  #-}

-- | Details of the reserved Elasticsearch instance which was purchased.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorrsReservedElasticsearchInstanceId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Core.Maybe Types.ReservedElasticsearchInstanceId)
preiorrsReservedElasticsearchInstanceId = Lens.field @"reservedElasticsearchInstanceId"
{-# INLINEABLE preiorrsReservedElasticsearchInstanceId #-}
{-# DEPRECATED reservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorrsResponseStatus :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse Core.Int
preiorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE preiorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
