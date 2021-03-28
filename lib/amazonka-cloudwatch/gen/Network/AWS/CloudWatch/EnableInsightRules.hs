{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.EnableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified Contributor Insights rules. When rules are enabled, they immediately begin analyzing log data.
module Network.AWS.CloudWatch.EnableInsightRules
    (
    -- * Creating a request
      EnableInsightRules (..)
    , mkEnableInsightRules
    -- ** Request lenses
    , eirRuleNames

    -- * Destructuring the response
    , EnableInsightRulesResponse (..)
    , mkEnableInsightRulesResponse
    -- ** Response lenses
    , eirrrsFailures
    , eirrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableInsightRules' smart constructor.
newtype EnableInsightRules = EnableInsightRules'
  { ruleNames :: [Types.InsightRuleName]
    -- ^ An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableInsightRules' value with any optional fields omitted.
mkEnableInsightRules
    :: EnableInsightRules
mkEnableInsightRules = EnableInsightRules'{ruleNames = Core.mempty}

-- | An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirRuleNames :: Lens.Lens' EnableInsightRules [Types.InsightRuleName]
eirRuleNames = Lens.field @"ruleNames"
{-# INLINEABLE eirRuleNames #-}
{-# DEPRECATED ruleNames "Use generic-lens or generic-optics with 'ruleNames' instead"  #-}

instance Core.ToQuery EnableInsightRules where
        toQuery EnableInsightRules{..}
          = Core.toQueryPair "Action" ("EnableInsightRules" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "RuleNames" (Core.toQueryList "member" ruleNames)

instance Core.ToHeaders EnableInsightRules where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableInsightRules where
        type Rs EnableInsightRules = EnableInsightRulesResponse
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
          = Response.receiveXMLWrapper "EnableInsightRulesResult"
              (\ s h x ->
                 EnableInsightRulesResponse' Core.<$>
                   (x Core..@? "Failures" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableInsightRulesResponse' smart constructor.
data EnableInsightRulesResponse = EnableInsightRulesResponse'
  { failures :: Core.Maybe [Types.PartialFailure]
    -- ^ An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableInsightRulesResponse' value with any optional fields omitted.
mkEnableInsightRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableInsightRulesResponse
mkEnableInsightRulesResponse responseStatus
  = EnableInsightRulesResponse'{failures = Core.Nothing,
                                responseStatus}

-- | An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrrsFailures :: Lens.Lens' EnableInsightRulesResponse (Core.Maybe [Types.PartialFailure])
eirrrsFailures = Lens.field @"failures"
{-# INLINEABLE eirrrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrrsResponseStatus :: Lens.Lens' EnableInsightRulesResponse Core.Int
eirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
