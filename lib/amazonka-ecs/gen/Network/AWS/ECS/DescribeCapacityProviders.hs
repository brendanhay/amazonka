{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeCapacityProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your capacity providers.
module Network.AWS.ECS.DescribeCapacityProviders
  ( -- * Creating a request
    DescribeCapacityProviders (..),
    mkDescribeCapacityProviders,

    -- ** Request lenses
    dcpCapacityProviders,
    dcpInclude,
    dcpMaxResults,
    dcpNextToken,

    -- * Destructuring the response
    DescribeCapacityProvidersResponse (..),
    mkDescribeCapacityProvidersResponse,

    -- ** Response lenses
    dcprrsCapacityProviders,
    dcprrsFailures,
    dcprrsNextToken,
    dcprrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCapacityProviders' smart constructor.
data DescribeCapacityProviders = DescribeCapacityProviders'
  { -- | The short name or full Amazon Resource Name (ARN) of one or more capacity providers. Up to @100@ capacity providers can be described in an action.
    capacityProviders :: Core.Maybe [Types.String],
    -- | Specifies whether or not you want to see the resource tags for the capacity provider. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
    include :: Core.Maybe [Types.CapacityProviderField],
    -- | The maximum number of account setting results returned by @DescribeCapacityProviders@ in paginated output. When this parameter is used, @DescribeCapacityProviders@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeCapacityProviders@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The @nextToken@ value returned from a previous paginated @DescribeCapacityProviders@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCapacityProviders' value with any optional fields omitted.
mkDescribeCapacityProviders ::
  DescribeCapacityProviders
mkDescribeCapacityProviders =
  DescribeCapacityProviders'
    { capacityProviders = Core.Nothing,
      include = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of one or more capacity providers. Up to @100@ capacity providers can be described in an action.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpCapacityProviders :: Lens.Lens' DescribeCapacityProviders (Core.Maybe [Types.String])
dcpCapacityProviders = Lens.field @"capacityProviders"
{-# DEPRECATED dcpCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | Specifies whether or not you want to see the resource tags for the capacity provider. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpInclude :: Lens.Lens' DescribeCapacityProviders (Core.Maybe [Types.CapacityProviderField])
dcpInclude = Lens.field @"include"
{-# DEPRECATED dcpInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The maximum number of account setting results returned by @DescribeCapacityProviders@ in paginated output. When this parameter is used, @DescribeCapacityProviders@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeCapacityProviders@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMaxResults :: Lens.Lens' DescribeCapacityProviders (Core.Maybe Core.Int)
dcpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dcpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeCapacityProviders@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeCapacityProviders (Core.Maybe Types.NextToken)
dcpNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeCapacityProviders where
  toJSON DescribeCapacityProviders {..} =
    Core.object
      ( Core.catMaybes
          [ ("capacityProviders" Core..=) Core.<$> capacityProviders,
            ("include" Core..=) Core.<$> include,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeCapacityProviders where
  type
    Rs DescribeCapacityProviders =
      DescribeCapacityProvidersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DescribeCapacityProviders"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCapacityProvidersResponse'
            Core.<$> (x Core..:? "capacityProviders")
            Core.<*> (x Core..:? "failures")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeCapacityProvidersResponse' smart constructor.
data DescribeCapacityProvidersResponse = DescribeCapacityProvidersResponse'
  { -- | The list of capacity providers.
    capacityProviders :: Core.Maybe [Types.CapacityProvider],
    -- | Any failures associated with the call.
    failures :: Core.Maybe [Types.Failure],
    -- | The @nextToken@ value to include in a future @DescribeCapacityProviders@ request. When the results of a @DescribeCapacityProviders@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCapacityProvidersResponse' value with any optional fields omitted.
mkDescribeCapacityProvidersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCapacityProvidersResponse
mkDescribeCapacityProvidersResponse responseStatus =
  DescribeCapacityProvidersResponse'
    { capacityProviders =
        Core.Nothing,
      failures = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of capacity providers.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsCapacityProviders :: Lens.Lens' DescribeCapacityProvidersResponse (Core.Maybe [Types.CapacityProvider])
dcprrsCapacityProviders = Lens.field @"capacityProviders"
{-# DEPRECATED dcprrsCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsFailures :: Lens.Lens' DescribeCapacityProvidersResponse (Core.Maybe [Types.Failure])
dcprrsFailures = Lens.field @"failures"
{-# DEPRECATED dcprrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeCapacityProviders@ request. When the results of a @DescribeCapacityProviders@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsNextToken :: Lens.Lens' DescribeCapacityProvidersResponse (Core.Maybe Types.String)
dcprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeCapacityProvidersResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
