{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester's topics. Each call returns a limited list of topics, up to 100. If there are more topics, a @NextToken@ is also returned. Use the @NextToken@ parameter in a new @ListTopics@ call to get further results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListTopics
    (
    -- * Creating a request
      ListTopics (..)
    , mkListTopics
    -- ** Request lenses
    , ltNextToken

    -- * Destructuring the response
    , ListTopicsResponse (..)
    , mkListTopicsResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsTopics
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | /See:/ 'mkListTopics' smart constructor.
newtype ListTopics = ListTopics'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ Token returned by the previous @ListTopics@ request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTopics' value with any optional fields omitted.
mkListTopics
    :: ListTopics
mkListTopics = ListTopics'{nextToken = Core.Nothing}

-- | Token returned by the previous @ListTopics@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTopics (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTopics where
        toQuery ListTopics{..}
          = Core.toQueryPair "Action" ("ListTopics" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListTopics where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTopics where
        type Rs ListTopics = ListTopicsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListTopicsResult"
              (\ s h x ->
                 ListTopicsResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "Topics" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTopics where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"topics" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Response for ListTopics action.
--
-- /See:/ 'mkListTopicsResponse' smart constructor.
data ListTopicsResponse = ListTopicsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ Token to pass along to the next @ListTopics@ request. This element is returned if there are additional topics to retrieve.
  , topics :: Core.Maybe [Types.Topic]
    -- ^ A list of topic ARNs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTopicsResponse' value with any optional fields omitted.
mkListTopicsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTopicsResponse
mkListTopicsResponse responseStatus
  = ListTopicsResponse'{nextToken = Core.Nothing,
                        topics = Core.Nothing, responseStatus}

-- | Token to pass along to the next @ListTopics@ request. This element is returned if there are additional topics to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTopicsResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of topic ARNs.
--
-- /Note:/ Consider using 'topics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTopics :: Lens.Lens' ListTopicsResponse (Core.Maybe [Types.Topic])
ltrrsTopics = Lens.field @"topics"
{-# INLINEABLE ltrrsTopics #-}
{-# DEPRECATED topics "Use generic-lens or generic-optics with 'topics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTopicsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
