{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.ListContainers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of all containers in AWS Elemental MediaStore. 
--
-- You can query to receive all the containers in one response. Or you can include the @MaxResults@ parameter to receive a limited number of containers in each response. In this case, the response includes a token. To get the next set of containers, send the command again, this time with the @NextToken@ parameter (with the returned token as its value). The next set of responses appears, with a token if there are still more containers to receive. 
-- See also 'DescribeContainer' , which gets the properties of one container. 
--
-- This operation returns paginated results.
module Network.AWS.MediaStore.ListContainers
    (
    -- * Creating a request
      ListContainers (..)
    , mkListContainers
    -- ** Request lenses
    , lcMaxResults
    , lcNextToken

    -- * Destructuring the response
    , ListContainersResponse (..)
    , mkListContainersResponse
    -- ** Response lenses
    , lcrrsContainers
    , lcrrsNextToken
    , lcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListContainers' smart constructor.
data ListContainers = ListContainers'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Enter the maximum number of containers in the response. Use from 1 to 255 characters. 
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Only if you used @MaxResults@ in the first command, enter the token (which was included in the previous response) to obtain the next set of containers. This token is included in a response only if there actually are more containers to list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContainers' value with any optional fields omitted.
mkListContainers
    :: ListContainers
mkListContainers
  = ListContainers'{maxResults = Core.Nothing,
                    nextToken = Core.Nothing}

-- | Enter the maximum number of containers in the response. Use from 1 to 255 characters. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListContainers (Core.Maybe Core.Natural)
lcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Only if you used @MaxResults@ in the first command, enter the token (which was included in the previous response) to obtain the next set of containers. This token is included in a response only if there actually are more containers to list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListContainers (Core.Maybe Types.PaginationToken)
lcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListContainers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListContainers where
        toHeaders ListContainers{..}
          = Core.pure ("X-Amz-Target", "MediaStore_20170901.ListContainers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListContainers where
        toJSON ListContainers{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListContainers where
        type Rs ListContainers = ListContainersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListContainersResponse' Core.<$>
                   (x Core..:? "Containers" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListContainers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"containers") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListContainersResponse' smart constructor.
data ListContainersResponse = ListContainersResponse'
  { containers :: [Types.Container]
    -- ^ The names of the containers.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ @NextToken@ is the token to use in the next call to @ListContainers@ . This token is returned only if you included the @MaxResults@ tag in the original command, and only if there are still containers to return. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListContainersResponse' value with any optional fields omitted.
mkListContainersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListContainersResponse
mkListContainersResponse responseStatus
  = ListContainersResponse'{containers = Core.mempty,
                            nextToken = Core.Nothing, responseStatus}

-- | The names of the containers.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsContainers :: Lens.Lens' ListContainersResponse [Types.Container]
lcrrsContainers = Lens.field @"containers"
{-# INLINEABLE lcrrsContainers #-}
{-# DEPRECATED containers "Use generic-lens or generic-optics with 'containers' instead"  #-}

-- | @NextToken@ is the token to use in the next call to @ListContainers@ . This token is returned only if you included the @MaxResults@ tag in the original command, and only if there are still containers to return. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListContainersResponse (Core.Maybe Types.PaginationToken)
lcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListContainersResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
