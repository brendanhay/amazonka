{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListTargetsByRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets assigned to the specified rule.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListTargetsByRule
    (
    -- * Creating a request
      ListTargetsByRule (..)
    , mkListTargetsByRule
    -- ** Request lenses
    , ltbrRule
    , ltbrEventBusName
    , ltbrLimit
    , ltbrNextToken

    -- * Destructuring the response
    , ListTargetsByRuleResponse (..)
    , mkListTargetsByRuleResponse
    -- ** Response lenses
    , ltbrrrsNextToken
    , ltbrrrsTargets
    , ltbrrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTargetsByRule' smart constructor.
data ListTargetsByRule = ListTargetsByRule'
  { rule :: Types.RuleName
    -- ^ The name of the rule.
  , eventBusName :: Core.Maybe Types.EventBusNameOrArn
    -- ^ The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned by a previous call to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsByRule' value with any optional fields omitted.
mkListTargetsByRule
    :: Types.RuleName -- ^ 'rule'
    -> ListTargetsByRule
mkListTargetsByRule rule
  = ListTargetsByRule'{rule, eventBusName = Core.Nothing,
                       limit = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrRule :: Lens.Lens' ListTargetsByRule Types.RuleName
ltbrRule = Lens.field @"rule"
{-# INLINEABLE ltbrRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrEventBusName :: Lens.Lens' ListTargetsByRule (Core.Maybe Types.EventBusNameOrArn)
ltbrEventBusName = Lens.field @"eventBusName"
{-# INLINEABLE ltbrEventBusName #-}
{-# DEPRECATED eventBusName "Use generic-lens or generic-optics with 'eventBusName' instead"  #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrLimit :: Lens.Lens' ListTargetsByRule (Core.Maybe Core.Natural)
ltbrLimit = Lens.field @"limit"
{-# INLINEABLE ltbrLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrNextToken :: Lens.Lens' ListTargetsByRule (Core.Maybe Types.NextToken)
ltbrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltbrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTargetsByRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTargetsByRule where
        toHeaders ListTargetsByRule{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.ListTargetsByRule") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTargetsByRule where
        toJSON ListTargetsByRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Rule" Core..= rule),
                  ("EventBusName" Core..=) Core.<$> eventBusName,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTargetsByRule where
        type Rs ListTargetsByRule = ListTargetsByRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTargetsByRuleResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Targets" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTargetsByRule where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"targets" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTargetsByRuleResponse' smart constructor.
data ListTargetsByRuleResponse = ListTargetsByRuleResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
  , targets :: Core.Maybe (Core.NonEmpty Types.Target)
    -- ^ The targets assigned to the rule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsByRuleResponse' value with any optional fields omitted.
mkListTargetsByRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTargetsByRuleResponse
mkListTargetsByRuleResponse responseStatus
  = ListTargetsByRuleResponse'{nextToken = Core.Nothing,
                               targets = Core.Nothing, responseStatus}

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrrrsNextToken :: Lens.Lens' ListTargetsByRuleResponse (Core.Maybe Types.NextToken)
ltbrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltbrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The targets assigned to the rule.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrrrsTargets :: Lens.Lens' ListTargetsByRuleResponse (Core.Maybe (Core.NonEmpty Types.Target))
ltbrrrsTargets = Lens.field @"targets"
{-# INLINEABLE ltbrrrsTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbrrrsResponseStatus :: Lens.Lens' ListTargetsByRuleResponse Core.Int
ltbrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltbrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
