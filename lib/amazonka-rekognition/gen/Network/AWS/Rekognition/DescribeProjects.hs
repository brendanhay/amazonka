{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and gets information about your Amazon Rekognition Custom Labels projects.
--
-- This operation requires permissions to perform the @rekognition:DescribeProjects@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.DescribeProjects
    (
    -- * Creating a request
      DescribeProjects (..)
    , mkDescribeProjects
    -- ** Request lenses
    , dpMaxResults
    , dpNextToken

    -- * Destructuring the response
    , DescribeProjectsResponse (..)
    , mkDescribeProjectsResponse
    -- ** Response lenses
    , dprfrsNextToken
    , dprfrsProjectDescriptions
    , dprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProjects' smart constructor.
data DescribeProjects = DescribeProjects'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProjects' value with any optional fields omitted.
mkDescribeProjects
    :: DescribeProjects
mkDescribeProjects
  = DescribeProjects'{maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribeProjects (Core.Maybe Core.Natural)
dpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribeProjects (Core.Maybe Types.NextToken)
dpNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeProjects where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProjects where
        toHeaders DescribeProjects{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.DescribeProjects")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeProjects where
        toJSON DescribeProjects{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeProjects where
        type Rs DescribeProjects = DescribeProjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProjectsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "ProjectDescriptions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeProjects where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"projectDescriptions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeProjectsResponse' smart constructor.
data DescribeProjectsResponse = DescribeProjectsResponse'
  { nextToken :: Core.Maybe Types.ExtendedPaginationToken
    -- ^ If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results. 
  , projectDescriptions :: Core.Maybe [Types.ProjectDescription]
    -- ^ A list of project descriptions. The list is sorted by the date and time the projects are created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeProjectsResponse' value with any optional fields omitted.
mkDescribeProjectsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeProjectsResponse
mkDescribeProjectsResponse responseStatus
  = DescribeProjectsResponse'{nextToken = Core.Nothing,
                              projectDescriptions = Core.Nothing, responseStatus}

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsNextToken :: Lens.Lens' DescribeProjectsResponse (Core.Maybe Types.ExtendedPaginationToken)
dprfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dprfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of project descriptions. The list is sorted by the date and time the projects are created.
--
-- /Note:/ Consider using 'projectDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsProjectDescriptions :: Lens.Lens' DescribeProjectsResponse (Core.Maybe [Types.ProjectDescription])
dprfrsProjectDescriptions = Lens.field @"projectDescriptions"
{-# INLINEABLE dprfrsProjectDescriptions #-}
{-# DEPRECATED projectDescriptions "Use generic-lens or generic-optics with 'projectDescriptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsResponseStatus :: Lens.Lens' DescribeProjectsResponse Core.Int
dprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
