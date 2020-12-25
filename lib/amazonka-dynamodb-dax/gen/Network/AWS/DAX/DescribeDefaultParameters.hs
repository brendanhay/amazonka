{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeDefaultParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default system parameter information for the DAX caching software.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeDefaultParameters
  ( -- * Creating a request
    DescribeDefaultParameters (..),
    mkDescribeDefaultParameters,

    -- ** Request lenses
    ddpMaxResults,
    ddpNextToken,

    -- * Destructuring the response
    DescribeDefaultParametersResponse (..),
    mkDescribeDefaultParametersResponse,

    -- ** Response lenses
    ddprrsNextToken,
    ddprrsParameters,
    ddprrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDefaultParameters' smart constructor.
data DescribeDefaultParameters = DescribeDefaultParameters'
  { -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Core.Maybe Core.Int,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDefaultParameters' value with any optional fields omitted.
mkDescribeDefaultParameters ::
  DescribeDefaultParameters
mkDescribeDefaultParameters =
  DescribeDefaultParameters'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpMaxResults :: Lens.Lens' DescribeDefaultParameters (Core.Maybe Core.Int)
ddpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ddpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpNextToken :: Lens.Lens' DescribeDefaultParameters (Core.Maybe Types.NextToken)
ddpNextToken = Lens.field @"nextToken"
{-# DEPRECATED ddpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeDefaultParameters where
  toJSON DescribeDefaultParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeDefaultParameters where
  type
    Rs DescribeDefaultParameters =
      DescribeDefaultParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.DescribeDefaultParameters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDefaultParametersResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Parameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDefaultParameters where
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

-- | /See:/ 'mkDescribeDefaultParametersResponse' smart constructor.
data DescribeDefaultParametersResponse = DescribeDefaultParametersResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Types.String,
    -- | A list of parameters. Each element in the list represents one parameter.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDefaultParametersResponse' value with any optional fields omitted.
mkDescribeDefaultParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDefaultParametersResponse
mkDescribeDefaultParametersResponse responseStatus =
  DescribeDefaultParametersResponse'
    { nextToken = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsNextToken :: Lens.Lens' DescribeDefaultParametersResponse (Core.Maybe Types.String)
ddprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ddprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parameters. Each element in the list represents one parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsParameters :: Lens.Lens' DescribeDefaultParametersResponse (Core.Maybe [Types.Parameter])
ddprrsParameters = Lens.field @"parameters"
{-# DEPRECATED ddprrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsResponseStatus :: Lens.Lens' DescribeDefaultParametersResponse Core.Int
ddprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
