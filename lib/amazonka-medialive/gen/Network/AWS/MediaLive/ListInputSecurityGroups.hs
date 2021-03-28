{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a list of Input Security Groups for an account
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputSecurityGroups
    (
    -- * Creating a request
      ListInputSecurityGroups (..)
    , mkListInputSecurityGroups
    -- ** Request lenses
    , lisgMaxResults
    , lisgNextToken

    -- * Destructuring the response
    , ListInputSecurityGroupsResponse (..)
    , mkListInputSecurityGroupsResponse
    -- ** Response lenses
    , lisgrrsInputSecurityGroups
    , lisgrrsNextToken
    , lisgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputSecurityGroupsRequest
--
-- /See:/ 'mkListInputSecurityGroups' smart constructor.
data ListInputSecurityGroups = ListInputSecurityGroups'
  { maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputSecurityGroups' value with any optional fields omitted.
mkListInputSecurityGroups
    :: ListInputSecurityGroups
mkListInputSecurityGroups
  = ListInputSecurityGroups'{maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgMaxResults :: Lens.Lens' ListInputSecurityGroups (Core.Maybe Core.Natural)
lisgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lisgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgNextToken :: Lens.Lens' ListInputSecurityGroups (Core.Maybe Core.Text)
lisgNextToken = Lens.field @"nextToken"
{-# INLINEABLE lisgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListInputSecurityGroups where
        toQuery ListInputSecurityGroups{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListInputSecurityGroups where
        toHeaders ListInputSecurityGroups{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListInputSecurityGroups where
        type Rs ListInputSecurityGroups = ListInputSecurityGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/prod/inputSecurityGroups",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInputSecurityGroupsResponse' Core.<$>
                   (x Core..:? "inputSecurityGroups") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInputSecurityGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"inputSecurityGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Placeholder documentation for ListInputSecurityGroupsResponse
--
-- /See:/ 'mkListInputSecurityGroupsResponse' smart constructor.
data ListInputSecurityGroupsResponse = ListInputSecurityGroupsResponse'
  { inputSecurityGroups :: Core.Maybe [Types.InputSecurityGroup]
    -- ^ List of input security groups
  , nextToken :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputSecurityGroupsResponse' value with any optional fields omitted.
mkListInputSecurityGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInputSecurityGroupsResponse
mkListInputSecurityGroupsResponse responseStatus
  = ListInputSecurityGroupsResponse'{inputSecurityGroups =
                                       Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | List of input security groups
--
-- /Note:/ Consider using 'inputSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgrrsInputSecurityGroups :: Lens.Lens' ListInputSecurityGroupsResponse (Core.Maybe [Types.InputSecurityGroup])
lisgrrsInputSecurityGroups = Lens.field @"inputSecurityGroups"
{-# INLINEABLE lisgrrsInputSecurityGroups #-}
{-# DEPRECATED inputSecurityGroups "Use generic-lens or generic-optics with 'inputSecurityGroups' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgrrsNextToken :: Lens.Lens' ListInputSecurityGroupsResponse (Core.Maybe Core.Text)
lisgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lisgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgrrsResponseStatus :: Lens.Lens' ListInputSecurityGroupsResponse Core.Int
lisgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lisgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
