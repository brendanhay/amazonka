{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListRuleGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleGroup' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListRuleGroups
    (
    -- * Creating a request
      ListRuleGroups (..)
    , mkListRuleGroups
    -- ** Request lenses
    , lrgLimit
    , lrgNextMarker

    -- * Destructuring the response
    , ListRuleGroupsResponse (..)
    , mkListRuleGroupsResponse
    -- ** Response lenses
    , lrgrrsNextMarker
    , lrgrrsRuleGroups
    , lrgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListRuleGroups' smart constructor.
data ListRuleGroups = ListRuleGroups'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of @RuleGroups@ that you want AWS WAF to return for this request. If you have more @RuleGroups@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RuleGroups@ .
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more @RuleGroups@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RuleGroups@ . For the second and subsequent @ListRuleGroups@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RuleGroups@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRuleGroups' value with any optional fields omitted.
mkListRuleGroups
    :: ListRuleGroups
mkListRuleGroups
  = ListRuleGroups'{limit = Core.Nothing, nextMarker = Core.Nothing}

-- | Specifies the number of @RuleGroups@ that you want AWS WAF to return for this request. If you have more @RuleGroups@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RuleGroups@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgLimit :: Lens.Lens' ListRuleGroups (Core.Maybe Core.Natural)
lrgLimit = Lens.field @"limit"
{-# INLINEABLE lrgLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more @RuleGroups@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RuleGroups@ . For the second and subsequent @ListRuleGroups@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RuleGroups@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgNextMarker :: Lens.Lens' ListRuleGroups (Core.Maybe Types.NextMarker)
lrgNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lrgNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListRuleGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListRuleGroups where
        toHeaders ListRuleGroups{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.ListRuleGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListRuleGroups where
        toJSON ListRuleGroups{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest ListRuleGroups where
        type Rs ListRuleGroups = ListRuleGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRuleGroupsResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "RuleGroups" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListRuleGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"ruleGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextMarker" Lens..~
                   rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListRuleGroupsResponse' smart constructor.
data ListRuleGroupsResponse = ListRuleGroupsResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more @RuleGroups@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RuleGroups@ , submit another @ListRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , ruleGroups :: Core.Maybe [Types.RuleGroupSummary]
    -- ^ An array of 'RuleGroup' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRuleGroupsResponse' value with any optional fields omitted.
mkListRuleGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRuleGroupsResponse
mkListRuleGroupsResponse responseStatus
  = ListRuleGroupsResponse'{nextMarker = Core.Nothing,
                            ruleGroups = Core.Nothing, responseStatus}

-- | If you have more @RuleGroups@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RuleGroups@ , submit another @ListRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrrsNextMarker :: Lens.Lens' ListRuleGroupsResponse (Core.Maybe Types.NextMarker)
lrgrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lrgrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | An array of 'RuleGroup' objects.
--
-- /Note:/ Consider using 'ruleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrrsRuleGroups :: Lens.Lens' ListRuleGroupsResponse (Core.Maybe [Types.RuleGroupSummary])
lrgrrsRuleGroups = Lens.field @"ruleGroups"
{-# INLINEABLE lrgrrsRuleGroups #-}
{-# DEPRECATED ruleGroups "Use generic-lens or generic-optics with 'ruleGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrrsResponseStatus :: Lens.Lens' ListRuleGroupsResponse Core.Int
lrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
