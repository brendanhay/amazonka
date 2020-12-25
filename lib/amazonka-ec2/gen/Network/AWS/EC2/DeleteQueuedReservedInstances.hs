{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteQueuedReservedInstances (..),
    mkDeleteQueuedReservedInstances,

    -- ** Request lenses
    dqriReservedInstancesIds,
    dqriDryRun,

    -- * Destructuring the response
    DeleteQueuedReservedInstancesResponse (..),
    mkDeleteQueuedReservedInstancesResponse,

    -- ** Response lenses
    dqrirrsFailedQueuedPurchaseDeletions,
    dqrirrsSuccessfulQueuedPurchaseDeletions,
    dqrirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteQueuedReservedInstances' smart constructor.
data DeleteQueuedReservedInstances = DeleteQueuedReservedInstances'
  { -- | The IDs of the Reserved Instances.
    reservedInstancesIds :: Core.NonEmpty Types.ReservationId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueuedReservedInstances' value with any optional fields omitted.
mkDeleteQueuedReservedInstances ::
  -- | 'reservedInstancesIds'
  Core.NonEmpty Types.ReservationId ->
  DeleteQueuedReservedInstances
mkDeleteQueuedReservedInstances reservedInstancesIds =
  DeleteQueuedReservedInstances'
    { reservedInstancesIds,
      dryRun = Core.Nothing
    }

-- | The IDs of the Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqriReservedInstancesIds :: Lens.Lens' DeleteQueuedReservedInstances (Core.NonEmpty Types.ReservationId)
dqriReservedInstancesIds = Lens.field @"reservedInstancesIds"
{-# DEPRECATED dqriReservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqriDryRun :: Lens.Lens' DeleteQueuedReservedInstances (Core.Maybe Core.Bool)
dqriDryRun = Lens.field @"dryRun"
{-# DEPRECATED dqriDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteQueuedReservedInstances where
  type
    Rs DeleteQueuedReservedInstances =
      DeleteQueuedReservedInstancesResponse
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
            ( Core.pure ("Action", "DeleteQueuedReservedInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "ReservedInstancesId" reservedInstancesIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteQueuedReservedInstancesResponse'
            Core.<$> ( x Core..@? "failedQueuedPurchaseDeletionSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> ( x Core..@? "successfulQueuedPurchaseDeletionSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteQueuedReservedInstancesResponse' smart constructor.
data DeleteQueuedReservedInstancesResponse = DeleteQueuedReservedInstancesResponse'
  { -- | Information about the queued purchases that could not be deleted.
    failedQueuedPurchaseDeletions :: Core.Maybe [Types.FailedQueuedPurchaseDeletion],
    -- | Information about the queued purchases that were successfully deleted.
    successfulQueuedPurchaseDeletions :: Core.Maybe [Types.SuccessfulQueuedPurchaseDeletion],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueuedReservedInstancesResponse' value with any optional fields omitted.
mkDeleteQueuedReservedInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteQueuedReservedInstancesResponse
mkDeleteQueuedReservedInstancesResponse responseStatus =
  DeleteQueuedReservedInstancesResponse'
    { failedQueuedPurchaseDeletions =
        Core.Nothing,
      successfulQueuedPurchaseDeletions = Core.Nothing,
      responseStatus
    }

-- | Information about the queued purchases that could not be deleted.
--
-- /Note:/ Consider using 'failedQueuedPurchaseDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirrsFailedQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Core.Maybe [Types.FailedQueuedPurchaseDeletion])
dqrirrsFailedQueuedPurchaseDeletions = Lens.field @"failedQueuedPurchaseDeletions"
{-# DEPRECATED dqrirrsFailedQueuedPurchaseDeletions "Use generic-lens or generic-optics with 'failedQueuedPurchaseDeletions' instead." #-}

-- | Information about the queued purchases that were successfully deleted.
--
-- /Note:/ Consider using 'successfulQueuedPurchaseDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirrsSuccessfulQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Core.Maybe [Types.SuccessfulQueuedPurchaseDeletion])
dqrirrsSuccessfulQueuedPurchaseDeletions = Lens.field @"successfulQueuedPurchaseDeletions"
{-# DEPRECATED dqrirrsSuccessfulQueuedPurchaseDeletions "Use generic-lens or generic-optics with 'successfulQueuedPurchaseDeletions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrirrsResponseStatus :: Lens.Lens' DeleteQueuedReservedInstancesResponse Core.Int
dqrirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dqrirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
