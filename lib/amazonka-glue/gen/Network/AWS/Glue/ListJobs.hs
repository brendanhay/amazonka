{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all job resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListJobs
    (
    -- * Creating a request
      ListJobs (..)
    , mkListJobs
    -- ** Request lenses
    , ljMaxResults
    , ljNextToken
    , ljTags

    -- * Destructuring the response
    , ListJobsResponse (..)
    , mkListJobsResponse
    -- ** Response lenses
    , ljrrsJobNames
    , ljrrsNextToken
    , ljrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of a list to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if this is a continuation request.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ Specifies to return only these tagged resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobs' value with any optional fields omitted.
mkListJobs
    :: ListJobs
mkListJobs
  = ListJobs'{maxResults = Core.Nothing, nextToken = Core.Nothing,
              tags = Core.Nothing}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Core.Maybe Core.Natural)
ljMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ljMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Core.Maybe Types.NextToken)
ljNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljTags :: Lens.Lens' ListJobs (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ljTags = Lens.field @"tags"
{-# INLINEABLE ljTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery ListJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListJobs where
        toHeaders ListJobs{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ListJobs") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListJobs where
        toJSON ListJobs{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListJobsResponse' Core.<$>
                   (x Core..:? "JobNames") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { jobNames :: Core.Maybe [Types.NameString]
    -- ^ The names of all jobs in the account, or the jobs with the specified tags.
  , nextToken :: Core.Maybe Types.GenericString
    -- ^ A continuation token, if the returned list does not contain the last metric available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobsResponse' value with any optional fields omitted.
mkListJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListJobsResponse
mkListJobsResponse responseStatus
  = ListJobsResponse'{jobNames = Core.Nothing,
                      nextToken = Core.Nothing, responseStatus}

-- | The names of all jobs in the account, or the jobs with the specified tags.
--
-- /Note:/ Consider using 'jobNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJobNames :: Lens.Lens' ListJobsResponse (Core.Maybe [Types.NameString])
ljrrsJobNames = Lens.field @"jobNames"
{-# INLINEABLE ljrrsJobNames #-}
{-# DEPRECATED jobNames "Use generic-lens or generic-optics with 'jobNames' instead"  #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsNextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Types.GenericString)
ljrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsResponseStatus :: Lens.Lens' ListJobsResponse Core.Int
ljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
