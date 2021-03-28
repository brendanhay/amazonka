{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists applications owned by the requester.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplications
    (
    -- * Creating a request
      ListApplications (..)
    , mkListApplications
    -- ** Request lenses
    , laMaxItems
    , laNextToken

    -- * Destructuring the response
    , ListApplicationsResponse (..)
    , mkListApplicationsResponse
    -- ** Response lenses
    , larrsApplications
    , larrsNextToken
    , larrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkListApplications' smart constructor.
data ListApplications = ListApplications'
  { maxItems :: Core.Maybe Core.Natural
    -- ^ The total number of items to return.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to specify where to start paginating.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplications' value with any optional fields omitted.
mkListApplications
    :: ListApplications
mkListApplications
  = ListApplications'{maxItems = Core.Nothing,
                      nextToken = Core.Nothing}

-- | The total number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxItems :: Lens.Lens' ListApplications (Core.Maybe Core.Natural)
laMaxItems = Lens.field @"maxItems"
{-# INLINEABLE laMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A token to specify where to start paginating.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApplications (Core.Maybe Core.Text)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListApplications where
        toQuery ListApplications{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListApplications where
        toHeaders ListApplications{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListApplications where
        type Rs ListApplications = ListApplicationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/applications",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListApplicationsResponse' Core.<$>
                   (x Core..:? "applications") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListApplications where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"applications" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { applications :: Core.Maybe [Types.ApplicationSummary]
    -- ^ An array of application summaries.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplicationsResponse' value with any optional fields omitted.
mkListApplicationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListApplicationsResponse
mkListApplicationsResponse responseStatus
  = ListApplicationsResponse'{applications = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | An array of application summaries.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsApplications :: Lens.Lens' ListApplicationsResponse (Core.Maybe [Types.ApplicationSummary])
larrsApplications = Lens.field @"applications"
{-# INLINEABLE larrsApplications #-}
{-# DEPRECATED applications "Use generic-lens or generic-optics with 'applications' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListApplicationsResponse (Core.Maybe Core.Text)
larrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListApplicationsResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
