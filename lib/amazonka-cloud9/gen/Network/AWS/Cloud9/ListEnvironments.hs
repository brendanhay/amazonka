{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.ListEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of AWS Cloud9 development environment identifiers.
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.ListEnvironments
    (
    -- * Creating a request
      ListEnvironments (..)
    , mkListEnvironments
    -- ** Request lenses
    , leMaxResults
    , leNextToken

    -- * Destructuring the response
    , ListEnvironmentsResponse (..)
    , mkListEnvironmentsResponse
    -- ** Response lenses
    , lerrsEnvironmentIds
    , lerrsNextToken
    , lerrsResponseStatus
    ) where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEnvironments' smart constructor.
data ListEnvironments = ListEnvironments'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of environments to get identifiers for.
  , nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEnvironments' value with any optional fields omitted.
mkListEnvironments
    :: ListEnvironments
mkListEnvironments
  = ListEnvironments'{maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | The maximum number of environments to get identifiers for.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListEnvironments (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# INLINEABLE leMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListEnvironments (Core.Maybe Core.Text)
leNextToken = Lens.field @"nextToken"
{-# INLINEABLE leNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListEnvironments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListEnvironments where
        toHeaders ListEnvironments{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCloud9WorkspaceManagementService.ListEnvironments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListEnvironments where
        toJSON ListEnvironments{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListEnvironments where
        type Rs ListEnvironments = ListEnvironmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListEnvironmentsResponse' Core.<$>
                   (x Core..:? "environmentIds") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListEnvironments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"environmentIds" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListEnvironmentsResponse' smart constructor.
data ListEnvironmentsResponse = ListEnvironmentsResponse'
  { environmentIds :: Core.Maybe [Types.EnvironmentId]
    -- ^ The list of environment identifiers.
  , nextToken :: Core.Maybe Core.Text
    -- ^ If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEnvironmentsResponse' value with any optional fields omitted.
mkListEnvironmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListEnvironmentsResponse
mkListEnvironmentsResponse responseStatus
  = ListEnvironmentsResponse'{environmentIds = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | The list of environment identifiers.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsEnvironmentIds :: Lens.Lens' ListEnvironmentsResponse (Core.Maybe [Types.EnvironmentId])
lerrsEnvironmentIds = Lens.field @"environmentIds"
{-# INLINEABLE lerrsEnvironmentIds #-}
{-# DEPRECATED environmentIds "Use generic-lens or generic-optics with 'environmentIds' instead"  #-}

-- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListEnvironmentsResponse (Core.Maybe Core.Text)
lerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListEnvironmentsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
