{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Amazon EventBridge rules. You can either list all the rules or you can provide a prefix to match to the rule names.
--
-- ListRules does not list the targets of a rule. To see the targets associated with a rule, use 'ListTargetsByRule' .
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListRules
    (
    -- * Creating a request
      ListRules (..)
    , mkListRules
    -- ** Request lenses
    , lrEventBusName
    , lrLimit
    , lrNamePrefix
    , lrNextToken

    -- * Destructuring the response
    , ListRulesResponse (..)
    , mkListRulesResponse
    -- ** Response lenses
    , lrrrsNextToken
    , lrrrsRules
    , lrrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRules' smart constructor.
data ListRules = ListRules'
  { eventBusName :: Core.Maybe Types.EventBusNameOrArn
    -- ^ The name or ARN of the event bus to list the rules for. If you omit this, the default event bus is used.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return.
  , namePrefix :: Core.Maybe Types.RuleName
    -- ^ The prefix matching the rule name.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned by a previous call to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRules' value with any optional fields omitted.
mkListRules
    :: ListRules
mkListRules
  = ListRules'{eventBusName = Core.Nothing, limit = Core.Nothing,
               namePrefix = Core.Nothing, nextToken = Core.Nothing}

-- | The name or ARN of the event bus to list the rules for. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrEventBusName :: Lens.Lens' ListRules (Core.Maybe Types.EventBusNameOrArn)
lrEventBusName = Lens.field @"eventBusName"
{-# INLINEABLE lrEventBusName #-}
{-# DEPRECATED eventBusName "Use generic-lens or generic-optics with 'eventBusName' instead"  #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLimit :: Lens.Lens' ListRules (Core.Maybe Core.Natural)
lrLimit = Lens.field @"limit"
{-# INLINEABLE lrLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The prefix matching the rule name.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNamePrefix :: Lens.Lens' ListRules (Core.Maybe Types.RuleName)
lrNamePrefix = Lens.field @"namePrefix"
{-# INLINEABLE lrNamePrefix #-}
{-# DEPRECATED namePrefix "Use generic-lens or generic-optics with 'namePrefix' instead"  #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRules (Core.Maybe Types.NextToken)
lrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListRules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListRules where
        toHeaders ListRules{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.ListRules") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListRules where
        toJSON ListRules{..}
          = Core.object
              (Core.catMaybes
                 [("EventBusName" Core..=) Core.<$> eventBusName,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NamePrefix" Core..=) Core.<$> namePrefix,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListRules where
        type Rs ListRules = ListRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRulesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Rules" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListRules where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"rules" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
  , rules :: Core.Maybe [Types.Rule]
    -- ^ The rules that match the specified criteria.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRulesResponse' value with any optional fields omitted.
mkListRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRulesResponse
mkListRulesResponse responseStatus
  = ListRulesResponse'{nextToken = Core.Nothing,
                       rules = Core.Nothing, responseStatus}

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListRulesResponse (Core.Maybe Types.NextToken)
lrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The rules that match the specified criteria.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsRules :: Lens.Lens' ListRulesResponse (Core.Maybe [Types.Rule])
lrrrsRules = Lens.field @"rules"
{-# INLINEABLE lrrrsRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListRulesResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
