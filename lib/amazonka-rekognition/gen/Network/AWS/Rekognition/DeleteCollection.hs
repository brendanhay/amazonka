{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified collection. Note that this operation removes all faces in the collection. For an example, see 'delete-collection-procedure' .
--
-- This operation requires permissions to perform the @rekognition:DeleteCollection@ action.
module Network.AWS.Rekognition.DeleteCollection
    (
    -- * Creating a request
      DeleteCollection (..)
    , mkDeleteCollection
    -- ** Request lenses
    , dcCollectionId

    -- * Destructuring the response
    , DeleteCollectionResponse (..)
    , mkDeleteCollectionResponse
    -- ** Response lenses
    , dcrrsStatusCode
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCollection' smart constructor.
newtype DeleteCollection = DeleteCollection'
  { collectionId :: Types.CollectionId
    -- ^ ID of the collection to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCollection' value with any optional fields omitted.
mkDeleteCollection
    :: Types.CollectionId -- ^ 'collectionId'
    -> DeleteCollection
mkDeleteCollection collectionId = DeleteCollection'{collectionId}

-- | ID of the collection to delete.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCollectionId :: Lens.Lens' DeleteCollection Types.CollectionId
dcCollectionId = Lens.field @"collectionId"
{-# INLINEABLE dcCollectionId #-}
{-# DEPRECATED collectionId "Use generic-lens or generic-optics with 'collectionId' instead"  #-}

instance Core.ToQuery DeleteCollection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCollection where
        toHeaders DeleteCollection{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.DeleteCollection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCollection where
        toJSON DeleteCollection{..}
          = Core.object
              (Core.catMaybes [Core.Just ("CollectionId" Core..= collectionId)])

instance Core.AWSRequest DeleteCollection where
        type Rs DeleteCollection = DeleteCollectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteCollectionResponse' Core.<$>
                   (x Core..:? "StatusCode") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCollectionResponse' smart constructor.
data DeleteCollectionResponse = DeleteCollectionResponse'
  { statusCode :: Core.Maybe Core.Natural
    -- ^ HTTP status code that indicates the result of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCollectionResponse' value with any optional fields omitted.
mkDeleteCollectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCollectionResponse
mkDeleteCollectionResponse responseStatus
  = DeleteCollectionResponse'{statusCode = Core.Nothing,
                              responseStatus}

-- | HTTP status code that indicates the result of the operation.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsStatusCode :: Lens.Lens' DeleteCollectionResponse (Core.Maybe Core.Natural)
dcrrsStatusCode = Lens.field @"statusCode"
{-# INLINEABLE dcrrsStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteCollectionResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
