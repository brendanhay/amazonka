{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists versions for the specified application.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
    (
    -- * Creating a request
      ListApplicationVersions (..)
    , mkListApplicationVersions
    -- ** Request lenses
    , lavApplicationId
    , lavMaxItems
    , lavNextToken

    -- * Destructuring the response
    , ListApplicationVersionsResponse (..)
    , mkListApplicationVersionsResponse
    -- ** Response lenses
    , lavrrsNextToken
    , lavrrsVersions
    , lavrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkListApplicationVersions' smart constructor.
data ListApplicationVersions = ListApplicationVersions'
  { applicationId :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ The total number of items to return.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to specify where to start paginating.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplicationVersions' value with any optional fields omitted.
mkListApplicationVersions
    :: Core.Text -- ^ 'applicationId'
    -> ListApplicationVersions
mkListApplicationVersions applicationId
  = ListApplicationVersions'{applicationId, maxItems = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavApplicationId :: Lens.Lens' ListApplicationVersions Core.Text
lavApplicationId = Lens.field @"applicationId"
{-# INLINEABLE lavApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The total number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavMaxItems :: Lens.Lens' ListApplicationVersions (Core.Maybe Core.Natural)
lavMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lavMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A token to specify where to start paginating.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavNextToken :: Lens.Lens' ListApplicationVersions (Core.Maybe Core.Text)
lavNextToken = Lens.field @"nextToken"
{-# INLINEABLE lavNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListApplicationVersions where
        toQuery ListApplicationVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListApplicationVersions where
        toHeaders ListApplicationVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListApplicationVersions where
        type Rs ListApplicationVersions = ListApplicationVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/applications/" Core.<> Core.toText applicationId Core.<>
                             "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListApplicationVersionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListApplicationVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListApplicationVersionsResponse' smart constructor.
data ListApplicationVersionsResponse = ListApplicationVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  , versions :: Core.Maybe [Types.VersionSummary]
    -- ^ An array of version summaries for the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplicationVersionsResponse' value with any optional fields omitted.
mkListApplicationVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListApplicationVersionsResponse
mkListApplicationVersionsResponse responseStatus
  = ListApplicationVersionsResponse'{nextToken = Core.Nothing,
                                     versions = Core.Nothing, responseStatus}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsNextToken :: Lens.Lens' ListApplicationVersionsResponse (Core.Maybe Core.Text)
lavrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lavrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of version summaries for the application.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsVersions :: Lens.Lens' ListApplicationVersionsResponse (Core.Maybe [Types.VersionSummary])
lavrrsVersions = Lens.field @"versions"
{-# INLINEABLE lavrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsResponseStatus :: Lens.Lens' ListApplicationVersionsResponse Core.Int
lavrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lavrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
