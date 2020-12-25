{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter group descriptions. If a parameter group name is specified, the list will contain only the descriptions for that group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeParameterGroups
  ( -- * Creating a request
    DescribeParameterGroups (..),
    mkDescribeParameterGroups,

    -- ** Request lenses
    dpgMaxResults,
    dpgNextToken,
    dpgParameterGroupNames,

    -- * Destructuring the response
    DescribeParameterGroupsResponse (..),
    mkDescribeParameterGroupsResponse,

    -- ** Response lenses
    dpgrfrsNextToken,
    dpgrfrsParameterGroups,
    dpgrfrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeParameterGroups' smart constructor.
data DescribeParameterGroups = DescribeParameterGroups'
  { -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Core.Maybe Core.Int,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Core.Maybe Types.String,
    -- | The names of the parameter groups.
    parameterGroupNames :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeParameterGroups' value with any optional fields omitted.
mkDescribeParameterGroups ::
  DescribeParameterGroups
mkDescribeParameterGroups =
  DescribeParameterGroups'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      parameterGroupNames = Core.Nothing
    }

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgMaxResults :: Lens.Lens' DescribeParameterGroups (Core.Maybe Core.Int)
dpgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgNextToken :: Lens.Lens' DescribeParameterGroups (Core.Maybe Types.String)
dpgNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of the parameter groups.
--
-- /Note:/ Consider using 'parameterGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgParameterGroupNames :: Lens.Lens' DescribeParameterGroups (Core.Maybe [Types.String])
dpgParameterGroupNames = Lens.field @"parameterGroupNames"
{-# DEPRECATED dpgParameterGroupNames "Use generic-lens or generic-optics with 'parameterGroupNames' instead." #-}

instance Core.FromJSON DescribeParameterGroups where
  toJSON DescribeParameterGroups {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ParameterGroupNames" Core..=) Core.<$> parameterGroupNames
          ]
      )

instance Core.AWSRequest DescribeParameterGroups where
  type Rs DescribeParameterGroups = DescribeParameterGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.DescribeParameterGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeParameterGroupsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ParameterGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeParameterGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"parameterGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeParameterGroupsResponse' smart constructor.
data DescribeParameterGroupsResponse = DescribeParameterGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Types.String,
    -- | An array of parameter groups. Each element in the array represents one parameter group.
    parameterGroups :: Core.Maybe [Types.ParameterGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeParameterGroupsResponse' value with any optional fields omitted.
mkDescribeParameterGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeParameterGroupsResponse
mkDescribeParameterGroupsResponse responseStatus =
  DescribeParameterGroupsResponse'
    { nextToken = Core.Nothing,
      parameterGroups = Core.Nothing,
      responseStatus
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrfrsNextToken :: Lens.Lens' DescribeParameterGroupsResponse (Core.Maybe Types.String)
dpgrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpgrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of parameter groups. Each element in the array represents one parameter group.
--
-- /Note:/ Consider using 'parameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrfrsParameterGroups :: Lens.Lens' DescribeParameterGroupsResponse (Core.Maybe [Types.ParameterGroup])
dpgrfrsParameterGroups = Lens.field @"parameterGroups"
{-# DEPRECATED dpgrfrsParameterGroups "Use generic-lens or generic-optics with 'parameterGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrfrsResponseStatus :: Lens.Lens' DescribeParameterGroupsResponse Core.Int
dpgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
