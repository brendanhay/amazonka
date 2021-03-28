{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListImageVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a specified image and their properties. The list can be filtered by creation time or modified time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImageVersions
    (
    -- * Creating a request
      ListImageVersions (..)
    , mkListImageVersions
    -- ** Request lenses
    , livImageName
    , livCreationTimeAfter
    , livCreationTimeBefore
    , livLastModifiedTimeAfter
    , livLastModifiedTimeBefore
    , livMaxResults
    , livNextToken
    , livSortBy
    , livSortOrder

    -- * Destructuring the response
    , ListImageVersionsResponse (..)
    , mkListImageVersionsResponse
    -- ** Response lenses
    , livrrsImageVersions
    , livrrsNextToken
    , livrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListImageVersions' smart constructor.
data ListImageVersions = ListImageVersions'
  { imageName :: Types.ImageName
    -- ^ The name of the image to list the versions of.
  , creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only versions created on or after the specified time.
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only versions created on or before the specified time.
  , lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only versions modified on or after the specified time.
  , lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only versions modified on or before the specified time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of versions to return in the response. The default value is 10. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous call to @ListImageVersions@ didn't return the full set of versions, the call returns a token for getting the next set of versions.
  , sortBy :: Core.Maybe Types.ImageVersionSortBy
    -- ^ The property used to sort results. The default value is @CREATION_TIME@ .
  , sortOrder :: Core.Maybe Types.ImageVersionSortOrder
    -- ^ The sort order. The default value is @DESCENDING@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListImageVersions' value with any optional fields omitted.
mkListImageVersions
    :: Types.ImageName -- ^ 'imageName'
    -> ListImageVersions
mkListImageVersions imageName
  = ListImageVersions'{imageName, creationTimeAfter = Core.Nothing,
                       creationTimeBefore = Core.Nothing,
                       lastModifiedTimeAfter = Core.Nothing,
                       lastModifiedTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                       nextToken = Core.Nothing, sortBy = Core.Nothing,
                       sortOrder = Core.Nothing}

-- | The name of the image to list the versions of.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livImageName :: Lens.Lens' ListImageVersions Types.ImageName
livImageName = Lens.field @"imageName"
{-# INLINEABLE livImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

-- | A filter that returns only versions created on or after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livCreationTimeAfter :: Lens.Lens' ListImageVersions (Core.Maybe Core.NominalDiffTime)
livCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE livCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only versions created on or before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livCreationTimeBefore :: Lens.Lens' ListImageVersions (Core.Maybe Core.NominalDiffTime)
livCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE livCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A filter that returns only versions modified on or after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livLastModifiedTimeAfter :: Lens.Lens' ListImageVersions (Core.Maybe Core.NominalDiffTime)
livLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# INLINEABLE livLastModifiedTimeAfter #-}
{-# DEPRECATED lastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead"  #-}

-- | A filter that returns only versions modified on or before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livLastModifiedTimeBefore :: Lens.Lens' ListImageVersions (Core.Maybe Core.NominalDiffTime)
livLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# INLINEABLE livLastModifiedTimeBefore #-}
{-# DEPRECATED lastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead"  #-}

-- | The maximum number of versions to return in the response. The default value is 10. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livMaxResults :: Lens.Lens' ListImageVersions (Core.Maybe Core.Natural)
livMaxResults = Lens.field @"maxResults"
{-# INLINEABLE livMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous call to @ListImageVersions@ didn't return the full set of versions, the call returns a token for getting the next set of versions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livNextToken :: Lens.Lens' ListImageVersions (Core.Maybe Types.NextToken)
livNextToken = Lens.field @"nextToken"
{-# INLINEABLE livNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The property used to sort results. The default value is @CREATION_TIME@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livSortBy :: Lens.Lens' ListImageVersions (Core.Maybe Types.ImageVersionSortBy)
livSortBy = Lens.field @"sortBy"
{-# INLINEABLE livSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order. The default value is @DESCENDING@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livSortOrder :: Lens.Lens' ListImageVersions (Core.Maybe Types.ImageVersionSortOrder)
livSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE livSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListImageVersions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListImageVersions where
        toHeaders ListImageVersions{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListImageVersions") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListImageVersions where
        toJSON ListImageVersions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ImageName" Core..= imageName),
                  ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
                  ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListImageVersions where
        type Rs ListImageVersions = ListImageVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListImageVersionsResponse' Core.<$>
                   (x Core..:? "ImageVersions") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListImageVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"imageVersions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListImageVersionsResponse' smart constructor.
data ListImageVersionsResponse = ListImageVersionsResponse'
  { imageVersions :: Core.Maybe [Types.ImageVersion]
    -- ^ A list of versions and their properties.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token for getting the next set of versions, if there are any.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListImageVersionsResponse' value with any optional fields omitted.
mkListImageVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListImageVersionsResponse
mkListImageVersionsResponse responseStatus
  = ListImageVersionsResponse'{imageVersions = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | A list of versions and their properties.
--
-- /Note:/ Consider using 'imageVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livrrsImageVersions :: Lens.Lens' ListImageVersionsResponse (Core.Maybe [Types.ImageVersion])
livrrsImageVersions = Lens.field @"imageVersions"
{-# INLINEABLE livrrsImageVersions #-}
{-# DEPRECATED imageVersions "Use generic-lens or generic-optics with 'imageVersions' instead"  #-}

-- | A token for getting the next set of versions, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livrrsNextToken :: Lens.Lens' ListImageVersionsResponse (Core.Maybe Types.NextToken)
livrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE livrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
livrrsResponseStatus :: Lens.Lens' ListImageVersionsResponse Core.Int
livrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE livrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
