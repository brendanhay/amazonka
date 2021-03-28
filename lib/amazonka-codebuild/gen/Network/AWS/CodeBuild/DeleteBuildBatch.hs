{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch build.
module Network.AWS.CodeBuild.DeleteBuildBatch
    (
    -- * Creating a request
      DeleteBuildBatch (..)
    , mkDeleteBuildBatch
    -- ** Request lenses
    , dbbId

    -- * Destructuring the response
    , DeleteBuildBatchResponse (..)
    , mkDeleteBuildBatchResponse
    -- ** Response lenses
    , dbbrrsBuildsDeleted
    , dbbrrsBuildsNotDeleted
    , dbbrrsStatusCode
    , dbbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBuildBatch' smart constructor.
newtype DeleteBuildBatch = DeleteBuildBatch'
  { id :: Types.Id
    -- ^ The identifier of the batch build to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBuildBatch' value with any optional fields omitted.
mkDeleteBuildBatch
    :: Types.Id -- ^ 'id'
    -> DeleteBuildBatch
mkDeleteBuildBatch id = DeleteBuildBatch'{id}

-- | The identifier of the batch build to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbId :: Lens.Lens' DeleteBuildBatch Types.Id
dbbId = Lens.field @"id"
{-# INLINEABLE dbbId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteBuildBatch where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBuildBatch where
        toHeaders DeleteBuildBatch{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.DeleteBuildBatch")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteBuildBatch where
        toJSON DeleteBuildBatch{..}
          = Core.object (Core.catMaybes [Core.Just ("id" Core..= id)])

instance Core.AWSRequest DeleteBuildBatch where
        type Rs DeleteBuildBatch = DeleteBuildBatchResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteBuildBatchResponse' Core.<$>
                   (x Core..:? "buildsDeleted") Core.<*> x Core..:? "buildsNotDeleted"
                     Core.<*> x Core..:? "statusCode"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBuildBatchResponse' smart constructor.
data DeleteBuildBatchResponse = DeleteBuildBatchResponse'
  { buildsDeleted :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ An array of strings that contain the identifiers of the builds that were deleted.
  , buildsNotDeleted :: Core.Maybe [Types.BuildNotDeleted]
    -- ^ An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
  , statusCode :: Core.Maybe Core.Text
    -- ^ The status code.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBuildBatchResponse' value with any optional fields omitted.
mkDeleteBuildBatchResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteBuildBatchResponse
mkDeleteBuildBatchResponse responseStatus
  = DeleteBuildBatchResponse'{buildsDeleted = Core.Nothing,
                              buildsNotDeleted = Core.Nothing, statusCode = Core.Nothing,
                              responseStatus}

-- | An array of strings that contain the identifiers of the builds that were deleted.
--
-- /Note:/ Consider using 'buildsDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsBuildsDeleted :: Lens.Lens' DeleteBuildBatchResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
dbbrrsBuildsDeleted = Lens.field @"buildsDeleted"
{-# INLINEABLE dbbrrsBuildsDeleted #-}
{-# DEPRECATED buildsDeleted "Use generic-lens or generic-optics with 'buildsDeleted' instead"  #-}

-- | An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
--
-- /Note:/ Consider using 'buildsNotDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsBuildsNotDeleted :: Lens.Lens' DeleteBuildBatchResponse (Core.Maybe [Types.BuildNotDeleted])
dbbrrsBuildsNotDeleted = Lens.field @"buildsNotDeleted"
{-# INLINEABLE dbbrrsBuildsNotDeleted #-}
{-# DEPRECATED buildsNotDeleted "Use generic-lens or generic-optics with 'buildsNotDeleted' instead"  #-}

-- | The status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsStatusCode :: Lens.Lens' DeleteBuildBatchResponse (Core.Maybe Core.Text)
dbbrrsStatusCode = Lens.field @"statusCode"
{-# INLINEABLE dbbrrsStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbbrrsResponseStatus :: Lens.Lens' DeleteBuildBatchResponse Core.Int
dbbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
