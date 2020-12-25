{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular parameter group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeParameters
  ( -- * Creating a request
    DescribeParameters (..),
    mkDescribeParameters,

    -- ** Request lenses
    dpParameterGroupName,
    dpMaxResults,
    dpNextToken,
    dpSource,

    -- * Destructuring the response
    DescribeParametersResponse (..),
    mkDescribeParametersResponse,

    -- ** Response lenses
    dprrsNextToken,
    dprrsParameters,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { -- | The name of the parameter group.
    parameterGroupName :: Types.String,
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Core.Maybe Core.Int,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Core.Maybe Types.String,
    -- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
    source :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeParameters' value with any optional fields omitted.
mkDescribeParameters ::
  -- | 'parameterGroupName'
  Types.String ->
  DescribeParameters
mkDescribeParameters parameterGroupName =
  DescribeParameters'
    { parameterGroupName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      source = Core.Nothing
    }

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpParameterGroupName :: Lens.Lens' DescribeParameters Types.String
dpParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED dpParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribeParameters (Core.Maybe Core.Int)
dpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribeParameters (Core.Maybe Types.String)
dpNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSource :: Lens.Lens' DescribeParameters (Core.Maybe Types.String)
dpSource = Lens.field @"source"
{-# DEPRECATED dpSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON DescribeParameters where
  toJSON DescribeParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParameterGroupName" Core..= parameterGroupName),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Source" Core..=) Core.<$> source
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
          Core.pure ("X-Amz-Target", "AmazonDAXV3.DescribeParameters")
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
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of parameters within a parameter group. Each element in the list represents one parameter.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

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

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsNextToken :: Lens.Lens' DescribeParametersResponse (Core.Maybe Types.NextToken)
dprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parameters within a parameter group. Each element in the list represents one parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsParameters :: Lens.Lens' DescribeParametersResponse (Core.Maybe [Types.Parameter])
dprrsParameters = Lens.field @"parameters"
{-# DEPRECATED dprrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribeParametersResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
