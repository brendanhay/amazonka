{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeCapacityProviders (..)
    , mkDescribeCapacityProviders
    -- ** Request lenses
    , dcpCapacityProviders
    , dcpInclude
    , dcpMaxResults
    , dcpNextToken

    -- * Destructuring the response
    , DescribeCapacityProvidersResponse (..)
    , mkDescribeCapacityProvidersResponse
    -- ** Response lenses
    , dcprrsCapacityProviders
    , dcprrsFailures
    , dcprrsNextToken
    , dcprrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCapacityProviders' smart constructor.
data DescribeCapacityProviders = DescribeCapacityProviders'
  { capacityProviders :: Core.Maybe [Core.Text]
    -- ^ The short name or full Amazon Resource Name (ARN) of one or more capacity providers. Up to @100@ capacity providers can be described in an action.
  , include :: Core.Maybe [Types.CapacityProviderField]
    -- ^ Specifies whether or not you want to see the resource tags for the capacity provider. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of account setting results returned by @DescribeCapacityProviders@ in paginated output. When this parameter is used, @DescribeCapacityProviders@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeCapacityProviders@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@ value if applicable.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value returned from a previous paginated @DescribeCapacityProviders@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCapacityProviders' value with any optional fields omitted.
mkDescribeCapacityProviders
    :: DescribeCapacityProviders
mkDescribeCapacityProviders
  = DescribeCapacityProviders'{capacityProviders = Core.Nothing,
                               include = Core.Nothing, maxResults = Core.Nothing,
                               nextToken = Core.Nothing}

-- | The short name or full Amazon Resource Name (ARN) of one or more capacity providers. Up to @100@ capacity providers can be described in an action.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpCapacityProviders :: Lens.Lens' DescribeCapacityProviders (Core.Maybe [Core.Text])
dcpCapacityProviders = Lens.field @"capacityProviders"
{-# INLINEABLE dcpCapacityProviders #-}
{-# DEPRECATED capacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead"  #-}

-- | Specifies whether or not you want to see the resource tags for the capacity provider. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpInclude :: Lens.Lens' DescribeCapacityProviders (Core.Maybe [Types.CapacityProviderField])
dcpInclude = Lens.field @"include"
{-# INLINEABLE dcpInclude #-}
{-# DEPRECATED include "Use generic-lens or generic-optics with 'include' instead"  #-}

-- | The maximum number of account setting results returned by @DescribeCapacityProviders@ in paginated output. When this parameter is used, @DescribeCapacityProviders@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeCapacityProviders@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMaxResults :: Lens.Lens' DescribeCapacityProviders (Core.Maybe Core.Int)
dcpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeCapacityProviders@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeCapacityProviders (Core.Maybe Core.Text)
dcpNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeCapacityProviders where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCapacityProviders where
        toHeaders DescribeCapacityProviders{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DescribeCapacityProviders")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCapacityProviders where
        toJSON DescribeCapacityProviders{..}
          = Core.object
              (Core.catMaybes
                 [("capacityProviders" Core..=) Core.<$> capacityProviders,
                  ("include" Core..=) Core.<$> include,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeCapacityProviders where
        type Rs DescribeCapacityProviders =
             DescribeCapacityProvidersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCapacityProvidersResponse' Core.<$>
                   (x Core..:? "capacityProviders") Core.<*> x Core..:? "failures"
                     Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCapacityProvidersResponse' smart constructor.
data DescribeCapacityProvidersResponse = DescribeCapacityProvidersResponse'
  { capacityProviders :: Core.Maybe [Types.CapacityProvider]
    -- ^ The list of capacity providers.
  , failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value to include in a future @DescribeCapacityProviders@ request. When the results of a @DescribeCapacityProviders@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCapacityProvidersResponse' value with any optional fields omitted.
mkDescribeCapacityProvidersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCapacityProvidersResponse
mkDescribeCapacityProvidersResponse responseStatus
  = DescribeCapacityProvidersResponse'{capacityProviders =
                                         Core.Nothing,
                                       failures = Core.Nothing, nextToken = Core.Nothing,
                                       responseStatus}

-- | The list of capacity providers.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsCapacityProviders :: Lens.Lens' DescribeCapacityProvidersResponse (Core.Maybe [Types.CapacityProvider])
dcprrsCapacityProviders = Lens.field @"capacityProviders"
{-# INLINEABLE dcprrsCapacityProviders #-}
{-# DEPRECATED capacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead"  #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsFailures :: Lens.Lens' DescribeCapacityProvidersResponse (Core.Maybe [Types.Failure])
dcprrsFailures = Lens.field @"failures"
{-# INLINEABLE dcprrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The @nextToken@ value to include in a future @DescribeCapacityProviders@ request. When the results of a @DescribeCapacityProviders@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsNextToken :: Lens.Lens' DescribeCapacityProvidersResponse (Core.Maybe Core.Text)
dcprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeCapacityProvidersResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
