{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeletePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the publishing definition with the specified @destinationId@ .
module Network.AWS.GuardDuty.DeletePublishingDestination
    (
    -- * Creating a request
      DeletePublishingDestination (..)
    , mkDeletePublishingDestination
    -- ** Request lenses
    , dpdDetectorId
    , dpdDestinationId

    -- * Destructuring the response
    , DeletePublishingDestinationResponse (..)
    , mkDeletePublishingDestinationResponse
    -- ** Response lenses
    , dpdrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePublishingDestination' smart constructor.
data DeletePublishingDestination = DeletePublishingDestination'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector associated with the publishing destination to delete.
  , destinationId :: Core.Text
    -- ^ The ID of the publishing destination to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePublishingDestination' value with any optional fields omitted.
mkDeletePublishingDestination
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'destinationId'
    -> DeletePublishingDestination
mkDeletePublishingDestination detectorId destinationId
  = DeletePublishingDestination'{detectorId, destinationId}

-- | The unique ID of the detector associated with the publishing destination to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDetectorId :: Lens.Lens' DeletePublishingDestination Types.DetectorId
dpdDetectorId = Lens.field @"detectorId"
{-# INLINEABLE dpdDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The ID of the publishing destination to delete.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDestinationId :: Lens.Lens' DeletePublishingDestination Core.Text
dpdDestinationId = Lens.field @"destinationId"
{-# INLINEABLE dpdDestinationId #-}
{-# DEPRECATED destinationId "Use generic-lens or generic-optics with 'destinationId' instead"  #-}

instance Core.ToQuery DeletePublishingDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePublishingDestination where
        toHeaders DeletePublishingDestination{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeletePublishingDestination where
        type Rs DeletePublishingDestination =
             DeletePublishingDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/publishingDestination/"
                             Core.<> Core.toText destinationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeletePublishingDestinationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePublishingDestinationResponse' smart constructor.
newtype DeletePublishingDestinationResponse = DeletePublishingDestinationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePublishingDestinationResponse' value with any optional fields omitted.
mkDeletePublishingDestinationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeletePublishingDestinationResponse
mkDeletePublishingDestinationResponse responseStatus
  = DeletePublishingDestinationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrrsResponseStatus :: Lens.Lens' DeletePublishingDestinationResponse Core.Int
dpdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
