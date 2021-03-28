{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.ListFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata for faces in the specified collection. This metadata includes information such as the bounding box coordinates, the confidence (that the bounding box contains a face), and face ID. For an example, see Listing Faces in a Collection in the Amazon Rekognition Developer Guide.
--
-- This operation requires permissions to perform the @rekognition:ListFaces@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListFaces
    (
    -- * Creating a request
      ListFaces (..)
    , mkListFaces
    -- ** Request lenses
    , lfCollectionId
    , lfMaxResults
    , lfNextToken

    -- * Destructuring the response
    , ListFacesResponse (..)
    , mkListFacesResponse
    -- ** Response lenses
    , lfrrsFaceModelVersion
    , lfrrsFaces
    , lfrrsNextToken
    , lfrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFaces' smart constructor.
data ListFaces = ListFaces'
  { collectionId :: Types.CollectionId
    -- ^ ID of the collection from which to list the faces.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of faces to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFaces' value with any optional fields omitted.
mkListFaces
    :: Types.CollectionId -- ^ 'collectionId'
    -> ListFaces
mkListFaces collectionId
  = ListFaces'{collectionId, maxResults = Core.Nothing,
               nextToken = Core.Nothing}

-- | ID of the collection from which to list the faces.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfCollectionId :: Lens.Lens' ListFaces Types.CollectionId
lfCollectionId = Lens.field @"collectionId"
{-# INLINEABLE lfCollectionId #-}
{-# DEPRECATED collectionId "Use generic-lens or generic-optics with 'collectionId' instead"  #-}

-- | Maximum number of faces to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFaces (Core.Maybe Core.Natural)
lfMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lfMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFaces (Core.Maybe Types.PaginationToken)
lfNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListFaces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListFaces where
        toHeaders ListFaces{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.ListFaces")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListFaces where
        toJSON ListFaces{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CollectionId" Core..= collectionId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListFaces where
        type Rs ListFaces = ListFacesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFacesResponse' Core.<$>
                   (x Core..:? "FaceModelVersion") Core.<*> x Core..:? "Faces"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFaces where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"faces" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListFacesResponse' smart constructor.
data ListFacesResponse = ListFacesResponse'
  { faceModelVersion :: Core.Maybe Core.Text
    -- ^ Version number of the face detection model associated with the input collection (@CollectionId@ ).
  , faces :: Core.Maybe [Types.Face]
    -- ^ An array of @Face@ objects. 
  , nextToken :: Core.Maybe Core.Text
    -- ^ If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFacesResponse' value with any optional fields omitted.
mkListFacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFacesResponse
mkListFacesResponse responseStatus
  = ListFacesResponse'{faceModelVersion = Core.Nothing,
                       faces = Core.Nothing, nextToken = Core.Nothing, responseStatus}

-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFaceModelVersion :: Lens.Lens' ListFacesResponse (Core.Maybe Core.Text)
lfrrsFaceModelVersion = Lens.field @"faceModelVersion"
{-# INLINEABLE lfrrsFaceModelVersion #-}
{-# DEPRECATED faceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead"  #-}

-- | An array of @Face@ objects. 
--
-- /Note:/ Consider using 'faces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFaces :: Lens.Lens' ListFacesResponse (Core.Maybe [Types.Face])
lfrrsFaces = Lens.field @"faces"
{-# INLINEABLE lfrrsFaces #-}
{-# DEPRECATED faces "Use generic-lens or generic-optics with 'faces' instead"  #-}

-- | If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsNextToken :: Lens.Lens' ListFacesResponse (Core.Maybe Core.Text)
lfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsResponseStatus :: Lens.Lens' ListFacesResponse Core.Int
lfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
