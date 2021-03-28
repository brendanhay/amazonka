{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified Contributor Insights rules. When rules are disabled, they do not analyze log groups and do not incur costs.
module Network.AWS.CloudWatch.DisableInsightRules
    (
    -- * Creating a request
      DisableInsightRules (..)
    , mkDisableInsightRules
    -- ** Request lenses
    , dirRuleNames

    -- * Destructuring the response
    , DisableInsightRulesResponse (..)
    , mkDisableInsightRulesResponse
    -- ** Response lenses
    , dirrrsFailures
    , dirrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableInsightRules' smart constructor.
newtype DisableInsightRules = DisableInsightRules'
  { ruleNames :: [Types.InsightRuleName]
    -- ^ An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableInsightRules' value with any optional fields omitted.
mkDisableInsightRules
    :: DisableInsightRules
mkDisableInsightRules
  = DisableInsightRules'{ruleNames = Core.mempty}

-- | An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirRuleNames :: Lens.Lens' DisableInsightRules [Types.InsightRuleName]
dirRuleNames = Lens.field @"ruleNames"
{-# INLINEABLE dirRuleNames #-}
{-# DEPRECATED ruleNames "Use generic-lens or generic-optics with 'ruleNames' instead"  #-}

instance Core.ToQuery DisableInsightRules where
        toQuery DisableInsightRules{..}
          = Core.toQueryPair "Action" ("DisableInsightRules" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "RuleNames" (Core.toQueryList "member" ruleNames)

instance Core.ToHeaders DisableInsightRules where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableInsightRules where
        type Rs DisableInsightRules = DisableInsightRulesResponse
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
          = Response.receiveXMLWrapper "DisableInsightRulesResult"
              (\ s h x ->
                 DisableInsightRulesResponse' Core.<$>
                   (x Core..@? "Failures" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableInsightRulesResponse' smart constructor.
data DisableInsightRulesResponse = DisableInsightRulesResponse'
  { failures :: Core.Maybe [Types.PartialFailure]
    -- ^ An array listing the rules that could not be disabled. You cannot disable built-in rules.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableInsightRulesResponse' value with any optional fields omitted.
mkDisableInsightRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableInsightRulesResponse
mkDisableInsightRulesResponse responseStatus
  = DisableInsightRulesResponse'{failures = Core.Nothing,
                                 responseStatus}

-- | An array listing the rules that could not be disabled. You cannot disable built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrrsFailures :: Lens.Lens' DisableInsightRulesResponse (Core.Maybe [Types.PartialFailure])
dirrrsFailures = Lens.field @"failures"
{-# INLINEABLE dirrrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrrsResponseStatus :: Lens.Lens' DisableInsightRulesResponse Core.Int
dirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
