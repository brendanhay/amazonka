{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListSubscribedRuleGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleGroup' objects that you are subscribed to.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListSubscribedRuleGroups
    (
    -- * Creating a request
      ListSubscribedRuleGroups (..)
    , mkListSubscribedRuleGroups
    -- ** Request lenses
    , lsrgLimit
    , lsrgNextMarker

    -- * Destructuring the response
    , ListSubscribedRuleGroupsResponse (..)
    , mkListSubscribedRuleGroupsResponse
    -- ** Response lenses
    , lsrgrrsNextMarker
    , lsrgrrsRuleGroups
    , lsrgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListSubscribedRuleGroups' smart constructor.
data ListSubscribedRuleGroups = ListSubscribedRuleGroups'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of subscribed rule groups that you want AWS WAF to return for this request. If you have more objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more @ByteMatchSets@ subscribed rule groups than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of subscribed rule groups. For the second and subsequent @ListSubscribedRuleGroupsRequest@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of subscribed rule groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscribedRuleGroups' value with any optional fields omitted.
mkListSubscribedRuleGroups
    :: ListSubscribedRuleGroups
mkListSubscribedRuleGroups
  = ListSubscribedRuleGroups'{limit = Core.Nothing,
                              nextMarker = Core.Nothing}

-- | Specifies the number of subscribed rule groups that you want AWS WAF to return for this request. If you have more objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgLimit :: Lens.Lens' ListSubscribedRuleGroups (Core.Maybe Core.Natural)
lsrgLimit = Lens.field @"limit"
{-# INLINEABLE lsrgLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ subscribed rule groups than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of subscribed rule groups. For the second and subsequent @ListSubscribedRuleGroupsRequest@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of subscribed rule groups.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgNextMarker :: Lens.Lens' ListSubscribedRuleGroups (Core.Maybe Types.NextMarker)
lsrgNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lsrgNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListSubscribedRuleGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSubscribedRuleGroups where
        toHeaders ListSubscribedRuleGroups{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.ListSubscribedRuleGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSubscribedRuleGroups where
        toJSON ListSubscribedRuleGroups{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest ListSubscribedRuleGroups where
        type Rs ListSubscribedRuleGroups = ListSubscribedRuleGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSubscribedRuleGroupsResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "RuleGroups" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSubscribedRuleGroups where
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

-- | /See:/ 'mkListSubscribedRuleGroupsResponse' smart constructor.
data ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more objects, submit another @ListSubscribedRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , ruleGroups :: Core.Maybe [Types.SubscribedRuleGroupSummary]
    -- ^ An array of 'RuleGroup' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscribedRuleGroupsResponse' value with any optional fields omitted.
mkListSubscribedRuleGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSubscribedRuleGroupsResponse
mkListSubscribedRuleGroupsResponse responseStatus
  = ListSubscribedRuleGroupsResponse'{nextMarker = Core.Nothing,
                                      ruleGroups = Core.Nothing, responseStatus}

-- | If you have more objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more objects, submit another @ListSubscribedRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsNextMarker :: Lens.Lens' ListSubscribedRuleGroupsResponse (Core.Maybe Types.NextMarker)
lsrgrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lsrgrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | An array of 'RuleGroup' objects.
--
-- /Note:/ Consider using 'ruleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsRuleGroups :: Lens.Lens' ListSubscribedRuleGroupsResponse (Core.Maybe [Types.SubscribedRuleGroupSummary])
lsrgrrsRuleGroups = Lens.field @"ruleGroups"
{-# INLINEABLE lsrgrrsRuleGroups #-}
{-# DEPRECATED ruleGroups "Use generic-lens or generic-optics with 'ruleGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsResponseStatus :: Lens.Lens' ListSubscribedRuleGroupsResponse Core.Int
lsrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
