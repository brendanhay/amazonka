{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the rule.
module Network.AWS.IoT.GetTopicRule
    (
    -- * Creating a request
      GetTopicRule (..)
    , mkGetTopicRule
    -- ** Request lenses
    , gtrRuleName

    -- * Destructuring the response
    , GetTopicRuleResponse (..)
    , mkGetTopicRuleResponse
    -- ** Response lenses
    , gtrrrsRule
    , gtrrrsRuleArn
    , gtrrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetTopicRule operation.
--
-- /See:/ 'mkGetTopicRule' smart constructor.
newtype GetTopicRule = GetTopicRule'
  { ruleName :: Types.RuleName
    -- ^ The name of the rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTopicRule' value with any optional fields omitted.
mkGetTopicRule
    :: Types.RuleName -- ^ 'ruleName'
    -> GetTopicRule
mkGetTopicRule ruleName = GetTopicRule'{ruleName}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrRuleName :: Lens.Lens' GetTopicRule Types.RuleName
gtrRuleName = Lens.field @"ruleName"
{-# INLINEABLE gtrRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.ToQuery GetTopicRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTopicRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTopicRule where
        type Rs GetTopicRule = GetTopicRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/rules/" Core.<> Core.toText ruleName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTopicRuleResponse' Core.<$>
                   (x Core..:? "rule") Core.<*> x Core..:? "ruleArn" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the GetTopicRule operation.
--
-- /See:/ 'mkGetTopicRuleResponse' smart constructor.
data GetTopicRuleResponse = GetTopicRuleResponse'
  { rule :: Core.Maybe Types.TopicRule
    -- ^ The rule.
  , ruleArn :: Core.Maybe Types.RuleArn
    -- ^ The rule ARN.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTopicRuleResponse' value with any optional fields omitted.
mkGetTopicRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTopicRuleResponse
mkGetTopicRuleResponse responseStatus
  = GetTopicRuleResponse'{rule = Core.Nothing,
                          ruleArn = Core.Nothing, responseStatus}

-- | The rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrrsRule :: Lens.Lens' GetTopicRuleResponse (Core.Maybe Types.TopicRule)
gtrrrsRule = Lens.field @"rule"
{-# INLINEABLE gtrrrsRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

-- | The rule ARN.
--
-- /Note:/ Consider using 'ruleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrrsRuleArn :: Lens.Lens' GetTopicRuleResponse (Core.Maybe Types.RuleArn)
gtrrrsRuleArn = Lens.field @"ruleArn"
{-# INLINEABLE gtrrrsRuleArn #-}
{-# DEPRECATED ruleArn "Use generic-lens or generic-optics with 'ruleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrrsResponseStatus :: Lens.Lens' GetTopicRuleResponse Core.Int
gtrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
