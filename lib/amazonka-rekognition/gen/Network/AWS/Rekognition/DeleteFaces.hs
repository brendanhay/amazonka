{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes faces from a collection. You specify a collection ID and an array of face IDs to remove from the collection.
--
-- This operation requires permissions to perform the @rekognition:DeleteFaces@ action.
module Network.AWS.Rekognition.DeleteFaces
    (
    -- * Creating a request
      DeleteFaces (..)
    , mkDeleteFaces
    -- ** Request lenses
    , dfCollectionId
    , dfFaceIds

    -- * Destructuring the response
    , DeleteFacesResponse (..)
    , mkDeleteFacesResponse
    -- ** Response lenses
    , dfrfrsDeletedFaces
    , dfrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFaces' smart constructor.
data DeleteFaces = DeleteFaces'
  { collectionId :: Types.CollectionId
    -- ^ Collection from which to remove the specific faces.
  , faceIds :: Core.NonEmpty Types.FaceId
    -- ^ An array of face IDs to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFaces' value with any optional fields omitted.
mkDeleteFaces
    :: Types.CollectionId -- ^ 'collectionId'
    -> Core.NonEmpty Types.FaceId -- ^ 'faceIds'
    -> DeleteFaces
mkDeleteFaces collectionId faceIds
  = DeleteFaces'{collectionId, faceIds}

-- | Collection from which to remove the specific faces.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCollectionId :: Lens.Lens' DeleteFaces Types.CollectionId
dfCollectionId = Lens.field @"collectionId"
{-# INLINEABLE dfCollectionId #-}
{-# DEPRECATED collectionId "Use generic-lens or generic-optics with 'collectionId' instead"  #-}

-- | An array of face IDs to delete.
--
-- /Note:/ Consider using 'faceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFaceIds :: Lens.Lens' DeleteFaces (Core.NonEmpty Types.FaceId)
dfFaceIds = Lens.field @"faceIds"
{-# INLINEABLE dfFaceIds #-}
{-# DEPRECATED faceIds "Use generic-lens or generic-optics with 'faceIds' instead"  #-}

instance Core.ToQuery DeleteFaces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFaces where
        toHeaders DeleteFaces{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.DeleteFaces")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteFaces where
        toJSON DeleteFaces{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CollectionId" Core..= collectionId),
                  Core.Just ("FaceIds" Core..= faceIds)])

instance Core.AWSRequest DeleteFaces where
        type Rs DeleteFaces = DeleteFacesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteFacesResponse' Core.<$>
                   (x Core..:? "DeletedFaces") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFacesResponse' smart constructor.
data DeleteFacesResponse = DeleteFacesResponse'
  { deletedFaces :: Core.Maybe (Core.NonEmpty Types.FaceId)
    -- ^ An array of strings (face IDs) of the faces that were deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFacesResponse' value with any optional fields omitted.
mkDeleteFacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFacesResponse
mkDeleteFacesResponse responseStatus
  = DeleteFacesResponse'{deletedFaces = Core.Nothing, responseStatus}

-- | An array of strings (face IDs) of the faces that were deleted.
--
-- /Note:/ Consider using 'deletedFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsDeletedFaces :: Lens.Lens' DeleteFacesResponse (Core.Maybe (Core.NonEmpty Types.FaceId))
dfrfrsDeletedFaces = Lens.field @"deletedFaces"
{-# INLINEABLE dfrfrsDeletedFaces #-}
{-# DEPRECATED deletedFaces "Use generic-lens or generic-optics with 'deletedFaces' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrfrsResponseStatus :: Lens.Lens' DeleteFacesResponse Core.Int
dfrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
