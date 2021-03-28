{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListTaskDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definitions that are registered to your account. You can filter the results by family name with the @familyPrefix@ parameter or by status with the @status@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTaskDefinitions
    (
    -- * Creating a request
      ListTaskDefinitions (..)
    , mkListTaskDefinitions
    -- ** Request lenses
    , ltdFamilyPrefix
    , ltdMaxResults
    , ltdNextToken
    , ltdSort
    , ltdStatus

    -- * Destructuring the response
    , ListTaskDefinitionsResponse (..)
    , mkListTaskDefinitionsResponse
    -- ** Response lenses
    , ltdrrsNextToken
    , ltdrrsTaskDefinitionArns
    , ltdrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTaskDefinitions' smart constructor.
data ListTaskDefinitions = ListTaskDefinitions'
  { familyPrefix :: Core.Maybe Core.Text
    -- ^ The full family name with which to filter the @ListTaskDefinitions@ results. Specifying a @familyPrefix@ limits the listed task definitions to task definition revisions that belong to that family.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of task definition results returned by @ListTaskDefinitions@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value returned from a @ListTaskDefinitions@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
  , sort :: Core.Maybe Types.SortOrder
    -- ^ The order in which to sort the results. Valid values are @ASC@ and @DESC@ . By default (@ASC@ ), task definitions are listed lexicographically by family name and in ascending numerical order by revision so that the newest task definitions in a family are listed last. Setting this parameter to @DESC@ reverses the sort order on family name and revision so that the newest task definitions in a family are listed first.
  , status :: Core.Maybe Types.TaskDefinitionStatus
    -- ^ The task definition status with which to filter the @ListTaskDefinitions@ results. By default, only @ACTIVE@ task definitions are listed. By setting this parameter to @INACTIVE@ , you can view task definitions that are @INACTIVE@ as long as an active task or service still references them. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTaskDefinitions' value with any optional fields omitted.
mkListTaskDefinitions
    :: ListTaskDefinitions
mkListTaskDefinitions
  = ListTaskDefinitions'{familyPrefix = Core.Nothing,
                         maxResults = Core.Nothing, nextToken = Core.Nothing,
                         sort = Core.Nothing, status = Core.Nothing}

-- | The full family name with which to filter the @ListTaskDefinitions@ results. Specifying a @familyPrefix@ limits the listed task definitions to task definition revisions that belong to that family.
--
-- /Note:/ Consider using 'familyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdFamilyPrefix :: Lens.Lens' ListTaskDefinitions (Core.Maybe Core.Text)
ltdFamilyPrefix = Lens.field @"familyPrefix"
{-# INLINEABLE ltdFamilyPrefix #-}
{-# DEPRECATED familyPrefix "Use generic-lens or generic-optics with 'familyPrefix' instead"  #-}

-- | The maximum number of task definition results returned by @ListTaskDefinitions@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdMaxResults :: Lens.Lens' ListTaskDefinitions (Core.Maybe Core.Int)
ltdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a @ListTaskDefinitions@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdNextToken :: Lens.Lens' ListTaskDefinitions (Core.Maybe Core.Text)
ltdNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The order in which to sort the results. Valid values are @ASC@ and @DESC@ . By default (@ASC@ ), task definitions are listed lexicographically by family name and in ascending numerical order by revision so that the newest task definitions in a family are listed last. Setting this parameter to @DESC@ reverses the sort order on family name and revision so that the newest task definitions in a family are listed first.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdSort :: Lens.Lens' ListTaskDefinitions (Core.Maybe Types.SortOrder)
ltdSort = Lens.field @"sort"
{-# INLINEABLE ltdSort #-}
{-# DEPRECATED sort "Use generic-lens or generic-optics with 'sort' instead"  #-}

-- | The task definition status with which to filter the @ListTaskDefinitions@ results. By default, only @ACTIVE@ task definitions are listed. By setting this parameter to @INACTIVE@ , you can view task definitions that are @INACTIVE@ as long as an active task or service still references them. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdStatus :: Lens.Lens' ListTaskDefinitions (Core.Maybe Types.TaskDefinitionStatus)
ltdStatus = Lens.field @"status"
{-# INLINEABLE ltdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListTaskDefinitions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTaskDefinitions where
        toHeaders ListTaskDefinitions{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.ListTaskDefinitions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTaskDefinitions where
        toJSON ListTaskDefinitions{..}
          = Core.object
              (Core.catMaybes
                 [("familyPrefix" Core..=) Core.<$> familyPrefix,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("sort" Core..=) Core.<$> sort,
                  ("status" Core..=) Core.<$> status])

instance Core.AWSRequest ListTaskDefinitions where
        type Rs ListTaskDefinitions = ListTaskDefinitionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTaskDefinitionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "taskDefinitionArns"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTaskDefinitions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"taskDefinitionArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTaskDefinitionsResponse' smart constructor.
data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value to include in a future @ListTaskDefinitions@ request. When the results of a @ListTaskDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , taskDefinitionArns :: Core.Maybe [Core.Text]
    -- ^ The list of task definition Amazon Resource Name (ARN) entries for the @ListTaskDefinitions@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTaskDefinitionsResponse' value with any optional fields omitted.
mkListTaskDefinitionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTaskDefinitionsResponse
mkListTaskDefinitionsResponse responseStatus
  = ListTaskDefinitionsResponse'{nextToken = Core.Nothing,
                                 taskDefinitionArns = Core.Nothing, responseStatus}

-- | The @nextToken@ value to include in a future @ListTaskDefinitions@ request. When the results of a @ListTaskDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdrrsNextToken :: Lens.Lens' ListTaskDefinitionsResponse (Core.Maybe Core.Text)
ltdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of task definition Amazon Resource Name (ARN) entries for the @ListTaskDefinitions@ request.
--
-- /Note:/ Consider using 'taskDefinitionArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdrrsTaskDefinitionArns :: Lens.Lens' ListTaskDefinitionsResponse (Core.Maybe [Core.Text])
ltdrrsTaskDefinitionArns = Lens.field @"taskDefinitionArns"
{-# INLINEABLE ltdrrsTaskDefinitionArns #-}
{-# DEPRECATED taskDefinitionArns "Use generic-lens or generic-optics with 'taskDefinitionArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdrrsResponseStatus :: Lens.Lens' ListTaskDefinitionsResponse Core.Int
ltdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
