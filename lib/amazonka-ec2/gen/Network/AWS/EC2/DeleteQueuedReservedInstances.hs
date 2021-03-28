{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteQueuedReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queued purchases for the specified Reserved Instances.
module Network.AWS.EC2.DeleteQueuedReservedInstances
    (
    -- * Creating a request
      DeleteQueuedReservedInstances (..)
    , mkDeleteQueuedReservedInstances
    -- ** Request lenses
    , dqriReservedInstancesIds
    , dqriDryRun

    -- * Destructuring the response
    , DeleteQueuedReservedInstancesResponse (..)
    , mkDeleteQueuedReservedInstancesResponse
    -- ** Response lenses
    , dqrirrsFailedQueuedPurchaseDeletions
    , dqrirrsSuccessfulQueuedPurchaseDeletions
    , dqrirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteQueuedReservedInstances' smart constructor.
data DeleteQueuedReservedInstances = DeleteQueuedReservedInstances'
  { reservedInstancesIds :: Core.NonEmpty Types.ReservationId
    -- ^ The IDs of the Reserved Instances.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueuedReservedInstances' value with any optional fields omitted.
mkDeleteQueuedReservedInstances
    :: Core.NonEmpty Types.ReservationId -- ^ 'reservedInstancesIds'
    -> DeleteQueuedReservedInstances
mkDeleteQueuedReservedInstances reservedInstancesIds
  = DeleteQueuedReservedInstances'{reservedInstancesIds,
                                   dryRun = Core.Nothing}

-- | The IDs of the Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqriReservedInstancesIds :: Lens.Lens' DeleteQueuedReservedInstances (Core.NonEmpty Types.ReservationId)
dqriReservedInstancesIds = Lens.field @"reservedInstancesIds"
{-# INLINEABLE dqriReservedInstancesIds #-}
{-# DEPRECATED reservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqriDryRun :: Lens.Lens' DeleteQueuedReservedInstances (Core.Maybe Core.Bool)
dqriDryRun = Lens.field @"dryRun"
{-# INLINEABLE dqriDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteQueuedReservedInstances where
        toQuery DeleteQueuedReservedInstances{..}
          = Core.toQueryPair "Action"
              ("DeleteQueuedReservedInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "ReservedInstancesId" reservedInstancesIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteQueuedReservedInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteQueuedReservedInstances where
        type Rs DeleteQueuedReservedInstances =
             DeleteQueuedReservedInstancesResponse
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
                 DeleteQueuedReservedInstancesResponse' Core.<$>
                   (x Core..@? "failedQueuedPurchaseDeletionSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "successfulQueuedPurchaseDeletionSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteQueuedReservedInstancesResponse' smart constructor.
data DeleteQueuedReservedInstancesResponse = DeleteQueuedReservedInstancesResponse'
  { failedQueuedPurchaseDeletions :: Core.Maybe [Types.FailedQueuedPurchaseDeletion]
    -- ^ Information about the queued purchases that could not be deleted.
  , successfulQueuedPurchaseDeletions :: Core.Maybe [Types.SuccessfulQueuedPurchaseDeletion]
    -- ^ Information about the queued purchases that were successfully deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueuedReservedInstancesResponse' value with any optional fields omitted.
mkDeleteQueuedReservedInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteQueuedReservedInstancesResponse
mkDeleteQueuedReservedInstancesResponse responseStatus
  = DeleteQueuedReservedInstancesResponse'{failedQueuedPurchaseDeletions
                                             = Core.Nothing,
                                           successfulQueuedPurchaseDeletions = Core.Nothing,
                                           responseStatus}

-- | Information about the queued purchases that could not be deleted.
--
-- /Note:/ Consider using 'failedQueuedPurchaseDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirrsFailedQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Core.Maybe [Types.FailedQueuedPurchaseDeletion])
dqrirrsFailedQueuedPurchaseDeletions = Lens.field @"failedQueuedPurchaseDeletions"
{-# INLINEABLE dqrirrsFailedQueuedPurchaseDeletions #-}
{-# DEPRECATED failedQueuedPurchaseDeletions "Use generic-lens or generic-optics with 'failedQueuedPurchaseDeletions' instead"  #-}

-- | Information about the queued purchases that were successfully deleted.
--
-- /Note:/ Consider using 'successfulQueuedPurchaseDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirrsSuccessfulQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Core.Maybe [Types.SuccessfulQueuedPurchaseDeletion])
dqrirrsSuccessfulQueuedPurchaseDeletions = Lens.field @"successfulQueuedPurchaseDeletions"
{-# INLINEABLE dqrirrsSuccessfulQueuedPurchaseDeletions #-}
{-# DEPRECATED successfulQueuedPurchaseDeletions "Use generic-lens or generic-optics with 'successfulQueuedPurchaseDeletions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirrsResponseStatus :: Lens.Lens' DeleteQueuedReservedInstancesResponse Core.Int
dqrirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dqrirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
