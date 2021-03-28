{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListActivatedRulesInRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'ActivatedRule' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListActivatedRulesInRuleGroup
    (
    -- * Creating a request
      ListActivatedRulesInRuleGroup (..)
    , mkListActivatedRulesInRuleGroup
    -- ** Request lenses
    , larirgLimit
    , larirgNextMarker
    , larirgRuleGroupId

    -- * Destructuring the response
    , ListActivatedRulesInRuleGroupResponse (..)
    , mkListActivatedRulesInRuleGroupResponse
    -- ** Response lenses
    , larirgrrsActivatedRules
    , larirgrrsNextMarker
    , larirgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListActivatedRulesInRuleGroup' smart constructor.
data ListActivatedRulesInRuleGroup = ListActivatedRulesInRuleGroup'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of @ActivatedRules@ that you want AWS WAF to return for this request. If you have more @ActivatedRules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ActivatedRules@ .
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more @ActivatedRules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ActivatedRules@ . For the second and subsequent @ListActivatedRulesInRuleGroup@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ActivatedRules@ .
  , ruleGroupId :: Core.Maybe Types.ResourceId
    -- ^ The @RuleGroupId@ of the 'RuleGroup' for which you want to get a list of 'ActivatedRule' objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListActivatedRulesInRuleGroup' value with any optional fields omitted.
mkListActivatedRulesInRuleGroup
    :: ListActivatedRulesInRuleGroup
mkListActivatedRulesInRuleGroup
  = ListActivatedRulesInRuleGroup'{limit = Core.Nothing,
                                   nextMarker = Core.Nothing, ruleGroupId = Core.Nothing}

-- | Specifies the number of @ActivatedRules@ that you want AWS WAF to return for this request. If you have more @ActivatedRules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ActivatedRules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgLimit :: Lens.Lens' ListActivatedRulesInRuleGroup (Core.Maybe Core.Natural)
larirgLimit = Lens.field @"limit"
{-# INLINEABLE larirgLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more @ActivatedRules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ActivatedRules@ . For the second and subsequent @ListActivatedRulesInRuleGroup@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ActivatedRules@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgNextMarker :: Lens.Lens' ListActivatedRulesInRuleGroup (Core.Maybe Types.NextMarker)
larirgNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE larirgNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The @RuleGroupId@ of the 'RuleGroup' for which you want to get a list of 'ActivatedRule' objects.
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgRuleGroupId :: Lens.Lens' ListActivatedRulesInRuleGroup (Core.Maybe Types.ResourceId)
larirgRuleGroupId = Lens.field @"ruleGroupId"
{-# INLINEABLE larirgRuleGroupId #-}
{-# DEPRECATED ruleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead"  #-}

instance Core.ToQuery ListActivatedRulesInRuleGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListActivatedRulesInRuleGroup where
        toHeaders ListActivatedRulesInRuleGroup{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.ListActivatedRulesInRuleGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListActivatedRulesInRuleGroup where
        toJSON ListActivatedRulesInRuleGroup{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker,
                  ("RuleGroupId" Core..=) Core.<$> ruleGroupId])

instance Core.AWSRequest ListActivatedRulesInRuleGroup where
        type Rs ListActivatedRulesInRuleGroup =
             ListActivatedRulesInRuleGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListActivatedRulesInRuleGroupResponse' Core.<$>
                   (x Core..:? "ActivatedRules") Core.<*> x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListActivatedRulesInRuleGroup where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"activatedRules" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextMarker" Lens..~
                   rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListActivatedRulesInRuleGroupResponse' smart constructor.
data ListActivatedRulesInRuleGroupResponse = ListActivatedRulesInRuleGroupResponse'
  { activatedRules :: Core.Maybe [Types.ActivatedRule]
    -- ^ An array of @ActivatedRules@ objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more @ActivatedRules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ActivatedRules@ , submit another @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListActivatedRulesInRuleGroupResponse' value with any optional fields omitted.
mkListActivatedRulesInRuleGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListActivatedRulesInRuleGroupResponse
mkListActivatedRulesInRuleGroupResponse responseStatus
  = ListActivatedRulesInRuleGroupResponse'{activatedRules =
                                             Core.Nothing,
                                           nextMarker = Core.Nothing, responseStatus}

-- | An array of @ActivatedRules@ objects.
--
-- /Note:/ Consider using 'activatedRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgrrsActivatedRules :: Lens.Lens' ListActivatedRulesInRuleGroupResponse (Core.Maybe [Types.ActivatedRule])
larirgrrsActivatedRules = Lens.field @"activatedRules"
{-# INLINEABLE larirgrrsActivatedRules #-}
{-# DEPRECATED activatedRules "Use generic-lens or generic-optics with 'activatedRules' instead"  #-}

-- | If you have more @ActivatedRules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ActivatedRules@ , submit another @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgrrsNextMarker :: Lens.Lens' ListActivatedRulesInRuleGroupResponse (Core.Maybe Types.NextMarker)
larirgrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE larirgrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larirgrrsResponseStatus :: Lens.Lens' ListActivatedRulesInRuleGroupResponse Core.Int
larirgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larirgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
