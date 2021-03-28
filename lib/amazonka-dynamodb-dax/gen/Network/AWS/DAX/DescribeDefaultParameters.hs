{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeDefaultParameters (..)
    , mkDescribeDefaultParameters
    -- ** Request lenses
    , ddpMaxResults
    , ddpNextToken

    -- * Destructuring the response
    , DescribeDefaultParametersResponse (..)
    , mkDescribeDefaultParametersResponse
    -- ** Response lenses
    , ddprrsNextToken
    , ddprrsParameters
    , ddprrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDefaultParameters' smart constructor.
data DescribeDefaultParameters = DescribeDefaultParameters'
  { maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDefaultParameters' value with any optional fields omitted.
mkDescribeDefaultParameters
    :: DescribeDefaultParameters
mkDescribeDefaultParameters
  = DescribeDefaultParameters'{maxResults = Core.Nothing,
                               nextToken = Core.Nothing}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpMaxResults :: Lens.Lens' DescribeDefaultParameters (Core.Maybe Core.Int)
ddpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ddpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpNextToken :: Lens.Lens' DescribeDefaultParameters (Core.Maybe Core.Text)
ddpNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeDefaultParameters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDefaultParameters where
        toHeaders DescribeDefaultParameters{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDAXV3.DescribeDefaultParameters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDefaultParameters where
        toJSON DescribeDefaultParameters{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeDefaultParameters where
        type Rs DescribeDefaultParameters =
             DescribeDefaultParametersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDefaultParametersResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Parameters" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDefaultParameters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeDefaultParametersResponse' smart constructor.
data DescribeDefaultParametersResponse = DescribeDefaultParametersResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of parameters. Each element in the list represents one parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDefaultParametersResponse' value with any optional fields omitted.
mkDescribeDefaultParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDefaultParametersResponse
mkDescribeDefaultParametersResponse responseStatus
  = DescribeDefaultParametersResponse'{nextToken = Core.Nothing,
                                       parameters = Core.Nothing, responseStatus}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsNextToken :: Lens.Lens' DescribeDefaultParametersResponse (Core.Maybe Core.Text)
ddprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of parameters. Each element in the list represents one parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsParameters :: Lens.Lens' DescribeDefaultParametersResponse (Core.Maybe [Types.Parameter])
ddprrsParameters = Lens.field @"parameters"
{-# INLINEABLE ddprrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsResponseStatus :: Lens.Lens' DescribeDefaultParametersResponse Core.Int
ddprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
