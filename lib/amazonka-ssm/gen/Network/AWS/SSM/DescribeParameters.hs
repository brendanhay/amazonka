{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeParameters
  ( -- * Creating a request
    DescribeParameters (..),
    mkDescribeParameters,

    -- ** Request lenses
    dpFilters,
    dpMaxResults,
    dpNextToken,
    dpParameterFilters,

    -- * Destructuring the response
    DescribeParametersResponse (..),
    mkDescribeParametersResponse,

    -- ** Response lenses
    dprrsNextToken,
    dprrsParameters,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { -- | This data type is deprecated. Instead, use @ParameterFilters@ .
    filters :: Core.Maybe [Types.ParametersFilter],
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | Filters to limit the request results.
    parameterFilters :: Core.Maybe [Types.ParameterStringFilter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeParameters' value with any optional fields omitted.
mkDescribeParameters ::
  DescribeParameters
mkDescribeParameters =
  DescribeParameters'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      parameterFilters = Core.Nothing
    }

-- | This data type is deprecated. Instead, use @ParameterFilters@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFilters :: Lens.Lens' DescribeParameters (Core.Maybe [Types.ParametersFilter])
dpFilters = Lens.field @"filters"
{-# DEPRECATED dpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribeParameters (Core.Maybe Core.Natural)
dpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribeParameters (Core.Maybe Types.NextToken)
dpNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters to limit the request results.
--
-- /Note:/ Consider using 'parameterFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpParameterFilters :: Lens.Lens' DescribeParameters (Core.Maybe [Types.ParameterStringFilter])
dpParameterFilters = Lens.field @"parameterFilters"
{-# DEPRECATED dpParameterFilters "Use generic-lens or generic-optics with 'parameterFilters' instead." #-}

instance Core.FromJSON DescribeParameters where
  toJSON DescribeParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ParameterFilters" Core..=) Core.<$> parameterFilters
          ]
      )

instance Core.AWSRequest DescribeParameters where
  type Rs DescribeParameters = DescribeParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribeParameters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeParametersResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Parameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeParameters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeParametersResponse' smart constructor.
data DescribeParametersResponse = DescribeParametersResponse'
  { -- | The token to use when requesting the next set of items.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Parameters returned by the request.
    parameters :: Core.Maybe [Types.ParameterMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeParametersResponse' value with any optional fields omitted.
mkDescribeParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeParametersResponse
mkDescribeParametersResponse responseStatus =
  DescribeParametersResponse'
    { nextToken = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsNextToken :: Lens.Lens' DescribeParametersResponse (Core.Maybe Types.NextToken)
dprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Parameters returned by the request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsParameters :: Lens.Lens' DescribeParametersResponse (Core.Maybe [Types.ParameterMetadata])
dprrsParameters = Lens.field @"parameters"
{-# DEPRECATED dprrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribeParametersResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
