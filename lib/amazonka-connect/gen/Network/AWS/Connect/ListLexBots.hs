{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListLexBots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the Amazon Lex bots currently associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLexBots
    (
    -- * Creating a request
      ListLexBots (..)
    , mkListLexBots
    -- ** Request lenses
    , llbInstanceId
    , llbMaxResults
    , llbNextToken

    -- * Destructuring the response
    , ListLexBotsResponse (..)
    , mkListLexBotsResponse
    -- ** Response lenses
    , llbrrsLexBots
    , llbrrsNextToken
    , llbrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLexBots' smart constructor.
data ListLexBots = ListLexBots'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLexBots' value with any optional fields omitted.
mkListLexBots
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListLexBots
mkListLexBots instanceId
  = ListLexBots'{instanceId, maxResults = Core.Nothing,
                 nextToken = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbInstanceId :: Lens.Lens' ListLexBots Types.InstanceId
llbInstanceId = Lens.field @"instanceId"
{-# INLINEABLE llbInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbMaxResults :: Lens.Lens' ListLexBots (Core.Maybe Core.Natural)
llbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE llbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbNextToken :: Lens.Lens' ListLexBots (Core.Maybe Types.NextToken)
llbNextToken = Lens.field @"nextToken"
{-# INLINEABLE llbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListLexBots where
        toQuery ListLexBots{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListLexBots where
        toHeaders ListLexBots{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListLexBots where
        type Rs ListLexBots = ListLexBotsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<> "/lex-bots",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListLexBotsResponse' Core.<$>
                   (x Core..:? "LexBots") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListLexBots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"lexBots" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListLexBotsResponse' smart constructor.
data ListLexBotsResponse = ListLexBotsResponse'
  { lexBots :: Core.Maybe [Types.LexBot]
    -- ^ The the names and regions of the Amazon Lex bots associated with the specified instance.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLexBotsResponse' value with any optional fields omitted.
mkListLexBotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListLexBotsResponse
mkListLexBotsResponse responseStatus
  = ListLexBotsResponse'{lexBots = Core.Nothing,
                         nextToken = Core.Nothing, responseStatus}

-- | The the names and regions of the Amazon Lex bots associated with the specified instance.
--
-- /Note:/ Consider using 'lexBots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrrsLexBots :: Lens.Lens' ListLexBotsResponse (Core.Maybe [Types.LexBot])
llbrrsLexBots = Lens.field @"lexBots"
{-# INLINEABLE llbrrsLexBots #-}
{-# DEPRECATED lexBots "Use generic-lens or generic-optics with 'lexBots' instead"  #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrrsNextToken :: Lens.Lens' ListLexBotsResponse (Core.Maybe Types.NextToken)
llbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE llbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrrsResponseStatus :: Lens.Lens' ListLexBotsResponse Core.Int
llbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE llbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
