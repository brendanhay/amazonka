{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a reserved DB instance offering.
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
    (
    -- * Creating a request
      PurchaseReservedDBInstancesOffering (..)
    , mkPurchaseReservedDBInstancesOffering
    -- ** Request lenses
    , prdbioReservedDBInstancesOfferingId
    , prdbioDBInstanceCount
    , prdbioReservedDBInstanceId
    , prdbioTags

    -- * Destructuring the response
    , PurchaseReservedDBInstancesOfferingResponse (..)
    , mkPurchaseReservedDBInstancesOfferingResponse
    -- ** Response lenses
    , prdbiorrsReservedDBInstance
    , prdbiorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkPurchaseReservedDBInstancesOffering' smart constructor.
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering'
  { reservedDBInstancesOfferingId :: Core.Text
    -- ^ The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
  , dBInstanceCount :: Core.Maybe Core.Int
    -- ^ The number of instances to reserve.
--
-- Default: @1@ 
  , reservedDBInstanceId :: Core.Maybe Core.Text
    -- ^ Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
  , tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedDBInstancesOffering' value with any optional fields omitted.
mkPurchaseReservedDBInstancesOffering
    :: Core.Text -- ^ 'reservedDBInstancesOfferingId'
    -> PurchaseReservedDBInstancesOffering
mkPurchaseReservedDBInstancesOffering reservedDBInstancesOfferingId
  = PurchaseReservedDBInstancesOffering'{reservedDBInstancesOfferingId,
                                         dBInstanceCount = Core.Nothing,
                                         reservedDBInstanceId = Core.Nothing, tags = Core.Nothing}

-- | The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdbioReservedDBInstancesOfferingId :: Lens.Lens' PurchaseReservedDBInstancesOffering Core.Text
prdbioReservedDBInstancesOfferingId = Lens.field @"reservedDBInstancesOfferingId"
{-# INLINEABLE prdbioReservedDBInstancesOfferingId #-}
{-# DEPRECATED reservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead"  #-}

-- | The number of instances to reserve.
--
-- Default: @1@ 
--
-- /Note:/ Consider using 'dBInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdbioDBInstanceCount :: Lens.Lens' PurchaseReservedDBInstancesOffering (Core.Maybe Core.Int)
prdbioDBInstanceCount = Lens.field @"dBInstanceCount"
{-# INLINEABLE prdbioDBInstanceCount #-}
{-# DEPRECATED dBInstanceCount "Use generic-lens or generic-optics with 'dBInstanceCount' instead"  #-}

-- | Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
--
-- /Note:/ Consider using 'reservedDBInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdbioReservedDBInstanceId :: Lens.Lens' PurchaseReservedDBInstancesOffering (Core.Maybe Core.Text)
prdbioReservedDBInstanceId = Lens.field @"reservedDBInstanceId"
{-# INLINEABLE prdbioReservedDBInstanceId #-}
{-# DEPRECATED reservedDBInstanceId "Use generic-lens or generic-optics with 'reservedDBInstanceId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdbioTags :: Lens.Lens' PurchaseReservedDBInstancesOffering (Core.Maybe [Types.Tag])
prdbioTags = Lens.field @"tags"
{-# INLINEABLE prdbioTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery PurchaseReservedDBInstancesOffering where
        toQuery PurchaseReservedDBInstancesOffering{..}
          = Core.toQueryPair "Action"
              ("PurchaseReservedDBInstancesOffering" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "ReservedDBInstancesOfferingId"
                reservedDBInstancesOfferingId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceCount")
                dBInstanceCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReservedDBInstanceId")
                reservedDBInstanceId
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders PurchaseReservedDBInstancesOffering where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PurchaseReservedDBInstancesOffering where
        type Rs PurchaseReservedDBInstancesOffering =
             PurchaseReservedDBInstancesOfferingResponse
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
          = Response.receiveXMLWrapper
              "PurchaseReservedDBInstancesOfferingResult"
              (\ s h x ->
                 PurchaseReservedDBInstancesOfferingResponse' Core.<$>
                   (x Core..@? "ReservedDBInstance") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPurchaseReservedDBInstancesOfferingResponse' smart constructor.
data PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse'
  { reservedDBInstance :: Core.Maybe Types.ReservedDBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PurchaseReservedDBInstancesOfferingResponse' value with any optional fields omitted.
mkPurchaseReservedDBInstancesOfferingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PurchaseReservedDBInstancesOfferingResponse
mkPurchaseReservedDBInstancesOfferingResponse responseStatus
  = PurchaseReservedDBInstancesOfferingResponse'{reservedDBInstance =
                                                   Core.Nothing,
                                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservedDBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdbiorrsReservedDBInstance :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse (Core.Maybe Types.ReservedDBInstance)
prdbiorrsReservedDBInstance = Lens.field @"reservedDBInstance"
{-# INLINEABLE prdbiorrsReservedDBInstance #-}
{-# DEPRECATED reservedDBInstance "Use generic-lens or generic-optics with 'reservedDBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdbiorrsResponseStatus :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse Core.Int
prdbiorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prdbiorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
