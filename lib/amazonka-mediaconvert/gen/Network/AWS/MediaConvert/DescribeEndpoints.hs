{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DescribeEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send an request with an empty body to the regional API endpoint to get your account API endpoint.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.DescribeEndpoints
    (
    -- * Creating a request
      DescribeEndpoints (..)
    , mkDescribeEndpoints
    -- ** Request lenses
    , deMaxResults
    , deMode
    , deNextToken

    -- * Destructuring the response
    , DescribeEndpointsResponse (..)
    , mkDescribeEndpointsResponse
    -- ** Response lenses
    , derrsEndpoints
    , derrsNextToken
    , derrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | DescribeEndpointsRequest
--
-- /See:/ 'mkDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { maxResults :: Core.Maybe Core.Int
    -- ^ Optional. Max number of endpoints, up to twenty, that will be returned at one time.
  , mode :: Core.Maybe Types.DescribeEndpointsMode
    -- ^ Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Use this string, provided with the response to a previous request, to request the next batch of endpoints.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpoints' value with any optional fields omitted.
mkDescribeEndpoints
    :: DescribeEndpoints
mkDescribeEndpoints
  = DescribeEndpoints'{maxResults = Core.Nothing,
                       mode = Core.Nothing, nextToken = Core.Nothing}

-- | Optional. Max number of endpoints, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxResults :: Lens.Lens' DescribeEndpoints (Core.Maybe Core.Int)
deMaxResults = Lens.field @"maxResults"
{-# INLINEABLE deMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMode :: Lens.Lens' DescribeEndpoints (Core.Maybe Types.DescribeEndpointsMode)
deMode = Lens.field @"mode"
{-# INLINEABLE deMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of endpoints.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEndpoints (Core.Maybe Core.Text)
deNextToken = Lens.field @"nextToken"
{-# INLINEABLE deNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeEndpoints where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEndpoints where
        toHeaders DescribeEndpoints{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEndpoints where
        toJSON DescribeEndpoints{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("mode" Core..=) Core.<$> mode,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeEndpoints where
        type Rs DescribeEndpoints = DescribeEndpointsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2017-08-29/endpoints",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEndpointsResponse' Core.<$>
                   (x Core..:? "endpoints") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEndpoints where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"endpoints" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { endpoints :: Core.Maybe [Types.Endpoint]
    -- ^ List of endpoints
  , nextToken :: Core.Maybe Core.Text
    -- ^ Use this string to request the next batch of endpoints.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpointsResponse' value with any optional fields omitted.
mkDescribeEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEndpointsResponse
mkDescribeEndpointsResponse responseStatus
  = DescribeEndpointsResponse'{endpoints = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | List of endpoints
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEndpoints :: Lens.Lens' DescribeEndpointsResponse (Core.Maybe [Types.Endpoint])
derrsEndpoints = Lens.field @"endpoints"
{-# INLINEABLE derrsEndpoints #-}
{-# DEPRECATED endpoints "Use generic-lens or generic-optics with 'endpoints' instead"  #-}

-- | Use this string to request the next batch of endpoints.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsNextToken :: Lens.Lens' DescribeEndpointsResponse (Core.Maybe Core.Text)
derrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE derrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeEndpointsResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
