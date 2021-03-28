{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteInstanceSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific snapshot of a virtual private server (or /instance/ ).
--
-- The @delete instance snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by @instance snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteInstanceSnapshot
    (
    -- * Creating a request
      DeleteInstanceSnapshot (..)
    , mkDeleteInstanceSnapshot
    -- ** Request lenses
    , disInstanceSnapshotName

    -- * Destructuring the response
    , DeleteInstanceSnapshotResponse (..)
    , mkDeleteInstanceSnapshotResponse
    -- ** Response lenses
    , disrrsOperations
    , disrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstanceSnapshot' smart constructor.
newtype DeleteInstanceSnapshot = DeleteInstanceSnapshot'
  { instanceSnapshotName :: Types.ResourceName
    -- ^ The name of the snapshot to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstanceSnapshot' value with any optional fields omitted.
mkDeleteInstanceSnapshot
    :: Types.ResourceName -- ^ 'instanceSnapshotName'
    -> DeleteInstanceSnapshot
mkDeleteInstanceSnapshot instanceSnapshotName
  = DeleteInstanceSnapshot'{instanceSnapshotName}

-- | The name of the snapshot to delete.
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disInstanceSnapshotName :: Lens.Lens' DeleteInstanceSnapshot Types.ResourceName
disInstanceSnapshotName = Lens.field @"instanceSnapshotName"
{-# INLINEABLE disInstanceSnapshotName #-}
{-# DEPRECATED instanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead"  #-}

instance Core.ToQuery DeleteInstanceSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInstanceSnapshot where
        toHeaders DeleteInstanceSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteInstanceSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteInstanceSnapshot where
        toJSON DeleteInstanceSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("instanceSnapshotName" Core..= instanceSnapshotName)])

instance Core.AWSRequest DeleteInstanceSnapshot where
        type Rs DeleteInstanceSnapshot = DeleteInstanceSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteInstanceSnapshotResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteInstanceSnapshotResponse' smart constructor.
data DeleteInstanceSnapshotResponse = DeleteInstanceSnapshotResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteInstanceSnapshotResponse' value with any optional fields omitted.
mkDeleteInstanceSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteInstanceSnapshotResponse
mkDeleteInstanceSnapshotResponse responseStatus
  = DeleteInstanceSnapshotResponse'{operations = Core.Nothing,
                                    responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrrsOperations :: Lens.Lens' DeleteInstanceSnapshotResponse (Core.Maybe [Types.Operation])
disrrsOperations = Lens.field @"operations"
{-# INLINEABLE disrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrrsResponseStatus :: Lens.Lens' DeleteInstanceSnapshotResponse Core.Int
disrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE disrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
