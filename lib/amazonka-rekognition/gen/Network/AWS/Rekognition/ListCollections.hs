{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.ListCollections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of collection IDs in your account. If the result is truncated, the response also provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
--
-- For an example, see Listing Collections in the Amazon Rekognition Developer Guide.
-- This operation requires permissions to perform the @rekognition:ListCollections@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListCollections
  ( -- * Creating a request
    ListCollections (..),
    mkListCollections,

    -- ** Request lenses
    lcMaxResults,
    lcNextToken,

    -- * Destructuring the response
    ListCollectionsResponse (..),
    mkListCollectionsResponse,

    -- ** Response lenses
    lcrrsCollectionIds,
    lcrrsFaceModelVersions,
    lcrrsNextToken,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCollections' smart constructor.
data ListCollections = ListCollections'
  { -- | Maximum number of collection IDs to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | Pagination token from the previous response.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCollections' value with any optional fields omitted.
mkListCollections ::
  ListCollections
mkListCollections =
  ListCollections'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Maximum number of collection IDs to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListCollections (Core.Maybe Core.Natural)
lcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination token from the previous response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCollections (Core.Maybe Types.PaginationToken)
lcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListCollections where
  toJSON ListCollections {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListCollections where
  type Rs ListCollections = ListCollectionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.ListCollections")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCollectionsResponse'
            Core.<$> (x Core..:? "CollectionIds")
            Core.<*> (x Core..:? "FaceModelVersions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCollections where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"collectionIds" Core.. Lens._Just) =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"faceModelVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCollectionsResponse' smart constructor.
data ListCollectionsResponse = ListCollectionsResponse'
  { -- | An array of collection IDs.
    collectionIds :: Core.Maybe [Types.CollectionId],
    -- | Version numbers of the face detection models associated with the collections in the array @CollectionIds@ . For example, the value of @FaceModelVersions[2]@ is the version number for the face detection model used by the collection in @CollectionId[2]@ .
    faceModelVersions :: Core.Maybe [Types.String],
    -- | If the result is truncated, the response provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCollectionsResponse' value with any optional fields omitted.
mkListCollectionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCollectionsResponse
mkListCollectionsResponse responseStatus =
  ListCollectionsResponse'
    { collectionIds = Core.Nothing,
      faceModelVersions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of collection IDs.
--
-- /Note:/ Consider using 'collectionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsCollectionIds :: Lens.Lens' ListCollectionsResponse (Core.Maybe [Types.CollectionId])
lcrrsCollectionIds = Lens.field @"collectionIds"
{-# DEPRECATED lcrrsCollectionIds "Use generic-lens or generic-optics with 'collectionIds' instead." #-}

-- | Version numbers of the face detection models associated with the collections in the array @CollectionIds@ . For example, the value of @FaceModelVersions[2]@ is the version number for the face detection model used by the collection in @CollectionId[2]@ .
--
-- /Note:/ Consider using 'faceModelVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsFaceModelVersions :: Lens.Lens' ListCollectionsResponse (Core.Maybe [Types.String])
lcrrsFaceModelVersions = Lens.field @"faceModelVersions"
{-# DEPRECATED lcrrsFaceModelVersions "Use generic-lens or generic-optics with 'faceModelVersions' instead." #-}

-- | If the result is truncated, the response provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListCollectionsResponse (Core.Maybe Types.NextToken)
lcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListCollectionsResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
