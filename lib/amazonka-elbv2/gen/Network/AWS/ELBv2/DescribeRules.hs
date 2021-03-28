{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rules or the rules for the specified listener. You must specify either a listener or one or more rules.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeRules
    (
    -- * Creating a request
      DescribeRules (..)
    , mkDescribeRules
    -- ** Request lenses
    , drListenerArn
    , drMarker
    , drPageSize
    , drRuleArns

    -- * Destructuring the response
    , DescribeRulesResponse (..)
    , mkDescribeRulesResponse
    -- ** Response lenses
    , drrfrsNextMarker
    , drrfrsRules
    , drrfrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRules' smart constructor.
data DescribeRules = DescribeRules'
  { listenerArn :: Core.Maybe Types.ListenerArn
    -- ^ The Amazon Resource Name (ARN) of the listener.
  , marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with this call.
  , ruleArns :: Core.Maybe [Types.RuleArn]
    -- ^ The Amazon Resource Names (ARN) of the rules.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRules' value with any optional fields omitted.
mkDescribeRules
    :: DescribeRules
mkDescribeRules
  = DescribeRules'{listenerArn = Core.Nothing, marker = Core.Nothing,
                   pageSize = Core.Nothing, ruleArns = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drListenerArn :: Lens.Lens' DescribeRules (Core.Maybe Types.ListenerArn)
drListenerArn = Lens.field @"listenerArn"
{-# INLINEABLE drListenerArn #-}
{-# DEPRECATED listenerArn "Use generic-lens or generic-optics with 'listenerArn' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMarker :: Lens.Lens' DescribeRules (Core.Maybe Types.Marker)
drMarker = Lens.field @"marker"
{-# INLINEABLE drMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drPageSize :: Lens.Lens' DescribeRules (Core.Maybe Core.Natural)
drPageSize = Lens.field @"pageSize"
{-# INLINEABLE drPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The Amazon Resource Names (ARN) of the rules.
--
-- /Note:/ Consider using 'ruleArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRuleArns :: Lens.Lens' DescribeRules (Core.Maybe [Types.RuleArn])
drRuleArns = Lens.field @"ruleArns"
{-# INLINEABLE drRuleArns #-}
{-# DEPRECATED ruleArns "Use generic-lens or generic-optics with 'ruleArns' instead"  #-}

instance Core.ToQuery DescribeRules where
        toQuery DescribeRules{..}
          = Core.toQueryPair "Action" ("DescribeRules" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ListenerArn") listenerArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PageSize") pageSize
              Core.<>
              Core.toQueryPair "RuleArns"
                (Core.maybe Core.mempty (Core.toQueryList "member") ruleArns)

instance Core.ToHeaders DescribeRules where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeRules where
        type Rs DescribeRules = DescribeRulesResponse
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
          = Response.receiveXMLWrapper "DescribeRulesResult"
              (\ s h x ->
                 DescribeRulesResponse' Core.<$>
                   (x Core..@? "NextMarker") Core.<*>
                     x Core..@? "Rules" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeRules where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"rules" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkDescribeRulesResponse' smart constructor.
data DescribeRulesResponse = DescribeRulesResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
  , rules :: Core.Maybe [Types.Rule]
    -- ^ Information about the rules.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRulesResponse' value with any optional fields omitted.
mkDescribeRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRulesResponse
mkDescribeRulesResponse responseStatus
  = DescribeRulesResponse'{nextMarker = Core.Nothing,
                           rules = Core.Nothing, responseStatus}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsNextMarker :: Lens.Lens' DescribeRulesResponse (Core.Maybe Types.NextMarker)
drrfrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE drrfrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | Information about the rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsRules :: Lens.Lens' DescribeRulesResponse (Core.Maybe [Types.Rule])
drrfrsRules = Lens.field @"rules"
{-# INLINEABLE drrfrsRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsResponseStatus :: Lens.Lens' DescribeRulesResponse Core.Int
drrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
