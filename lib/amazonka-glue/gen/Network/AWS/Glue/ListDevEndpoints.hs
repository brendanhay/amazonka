{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListDevEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all @DevEndpoint@ resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListDevEndpoints
    (
    -- * Creating a request
      ListDevEndpoints (..)
    , mkListDevEndpoints
    -- ** Request lenses
    , ldeMaxResults
    , ldeNextToken
    , ldeTags

    -- * Destructuring the response
    , ListDevEndpointsResponse (..)
    , mkListDevEndpointsResponse
    -- ** Response lenses
    , lderrsDevEndpointNames
    , lderrsNextToken
    , lderrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDevEndpoints' smart constructor.
data ListDevEndpoints = ListDevEndpoints'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of a list to return.
  , nextToken :: Core.Maybe Types.GenericString
    -- ^ A continuation token, if this is a continuation request.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ Specifies to return only these tagged resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDevEndpoints' value with any optional fields omitted.
mkListDevEndpoints
    :: ListDevEndpoints
mkListDevEndpoints
  = ListDevEndpoints'{maxResults = Core.Nothing,
                      nextToken = Core.Nothing, tags = Core.Nothing}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeMaxResults :: Lens.Lens' ListDevEndpoints (Core.Maybe Core.Natural)
ldeMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldeMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeNextToken :: Lens.Lens' ListDevEndpoints (Core.Maybe Types.GenericString)
ldeNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldeNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeTags :: Lens.Lens' ListDevEndpoints (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ldeTags = Lens.field @"tags"
{-# INLINEABLE ldeTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery ListDevEndpoints where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDevEndpoints where
        toHeaders ListDevEndpoints{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ListDevEndpoints") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDevEndpoints where
        toJSON ListDevEndpoints{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest ListDevEndpoints where
        type Rs ListDevEndpoints = ListDevEndpointsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDevEndpointsResponse' Core.<$>
                   (x Core..:? "DevEndpointNames") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListDevEndpointsResponse' smart constructor.
data ListDevEndpointsResponse = ListDevEndpointsResponse'
  { devEndpointNames :: Core.Maybe [Types.NameString]
    -- ^ The names of all the @DevEndpoint@ s in the account, or the @DevEndpoint@ s with the specified tags.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if the returned list does not contain the last metric available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDevEndpointsResponse' value with any optional fields omitted.
mkListDevEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDevEndpointsResponse
mkListDevEndpointsResponse responseStatus
  = ListDevEndpointsResponse'{devEndpointNames = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | The names of all the @DevEndpoint@ s in the account, or the @DevEndpoint@ s with the specified tags.
--
-- /Note:/ Consider using 'devEndpointNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lderrsDevEndpointNames :: Lens.Lens' ListDevEndpointsResponse (Core.Maybe [Types.NameString])
lderrsDevEndpointNames = Lens.field @"devEndpointNames"
{-# INLINEABLE lderrsDevEndpointNames #-}
{-# DEPRECATED devEndpointNames "Use generic-lens or generic-optics with 'devEndpointNames' instead"  #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lderrsNextToken :: Lens.Lens' ListDevEndpointsResponse (Core.Maybe Types.NextToken)
lderrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lderrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lderrsResponseStatus :: Lens.Lens' ListDevEndpointsResponse Core.Int
lderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
