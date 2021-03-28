{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items that have tags as specified by the key-value pairs, name and value, passed to the optional parameter @filters@ .
--
-- There are three valid tag filter names:
--
--     * tagKey
--
--
--     * tagValue
--
--
--     * configurationId
--
--
-- Also, all configuration items associated with your user account that have tags can be listed if you call @DescribeTags@ as is without passing any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeTags
    (
    -- * Creating a request
      DescribeTags (..)
    , mkDescribeTags
    -- ** Request lenses
    , dtFilters
    , dtMaxResults
    , dtNextToken

    -- * Destructuring the response
    , DescribeTagsResponse (..)
    , mkDescribeTagsResponse
    -- ** Response lenses
    , dtrrsNextToken
    , dtrrsTags
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { filters :: Core.Maybe [Types.TagFilter]
    -- ^ You can filter the list using a /key/ -/value/ format. You can separate these items by using logical operators. Allowed filters include @tagKey@ , @tagValue@ , and @configurationId@ . 
  , maxResults :: Core.Maybe Core.Int
    -- ^ The total number of items to return in a single page of output. The maximum value is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTags' value with any optional fields omitted.
mkDescribeTags
    :: DescribeTags
mkDescribeTags
  = DescribeTags'{filters = Core.Nothing, maxResults = Core.Nothing,
                  nextToken = Core.Nothing}

-- | You can filter the list using a /key/ -/value/ format. You can separate these items by using logical operators. Allowed filters include @tagKey@ , @tagValue@ , and @configurationId@ . 
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtFilters :: Lens.Lens' DescribeTags (Core.Maybe [Types.TagFilter])
dtFilters = Lens.field @"filters"
{-# INLINEABLE dtFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The total number of items to return in a single page of output. The maximum value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMaxResults :: Lens.Lens' DescribeTags (Core.Maybe Core.Int)
dtMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dtMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtNextToken :: Lens.Lens' DescribeTags (Core.Maybe Types.NextToken)
dtNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTags where
        toHeaders DescribeTags{..}
          = Core.pure
              ("X-Amz-Target", "AWSPoseidonService_V2015_11_01.DescribeTags")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTags where
        toJSON DescribeTags{..}
          = Core.object
              (Core.catMaybes
                 [("filters" Core..=) Core.<$> filters,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTagsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTags where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tags" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The call returns a token. Use this token to get the next set of results.
  , tags :: Core.Maybe [Types.ConfigurationTag]
    -- ^ Depending on the input, this is a list of configuration items tagged with a specific tag, or a list of tags for a specific configuration item.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTagsResponse' value with any optional fields omitted.
mkDescribeTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTagsResponse
mkDescribeTagsResponse responseStatus
  = DescribeTagsResponse'{nextToken = Core.Nothing,
                          tags = Core.Nothing, responseStatus}

-- | The call returns a token. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsNextToken :: Lens.Lens' DescribeTagsResponse (Core.Maybe Types.NextToken)
dtrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Depending on the input, this is a list of configuration items tagged with a specific tag, or a list of tags for a specific configuration item.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTags :: Lens.Lens' DescribeTagsResponse (Core.Maybe [Types.ConfigurationTag])
dtrrsTags = Lens.field @"tags"
{-# INLINEABLE dtrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTagsResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
