{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RateBasedRule' that is specified by the @RuleId@ that you included in the @GetRateBasedRule@ request.
module Network.AWS.WAF.GetRateBasedRule
    (
    -- * Creating a request
      GetRateBasedRule (..)
    , mkGetRateBasedRule
    -- ** Request lenses
    , grbrRuleId

    -- * Destructuring the response
    , GetRateBasedRuleResponse (..)
    , mkGetRateBasedRuleResponse
    -- ** Response lenses
    , grbrrrsRule
    , grbrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetRateBasedRule' smart constructor.
newtype GetRateBasedRule = GetRateBasedRule'
  { ruleId :: Types.ResourceId
    -- ^ The @RuleId@ of the 'RateBasedRule' that you want to get. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRateBasedRule' value with any optional fields omitted.
mkGetRateBasedRule
    :: Types.ResourceId -- ^ 'ruleId'
    -> GetRateBasedRule
mkGetRateBasedRule ruleId = GetRateBasedRule'{ruleId}

-- | The @RuleId@ of the 'RateBasedRule' that you want to get. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrRuleId :: Lens.Lens' GetRateBasedRule Types.ResourceId
grbrRuleId = Lens.field @"ruleId"
{-# INLINEABLE grbrRuleId #-}
{-# DEPRECATED ruleId "Use generic-lens or generic-optics with 'ruleId' instead"  #-}

instance Core.ToQuery GetRateBasedRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRateBasedRule where
        toHeaders GetRateBasedRule{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetRateBasedRule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRateBasedRule where
        toJSON GetRateBasedRule{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RuleId" Core..= ruleId)])

instance Core.AWSRequest GetRateBasedRule where
        type Rs GetRateBasedRule = GetRateBasedRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRateBasedRuleResponse' Core.<$>
                   (x Core..:? "Rule") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRateBasedRuleResponse' smart constructor.
data GetRateBasedRuleResponse = GetRateBasedRuleResponse'
  { rule :: Core.Maybe Types.RateBasedRule
    -- ^ Information about the 'RateBasedRule' that you specified in the @GetRateBasedRule@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRateBasedRuleResponse' value with any optional fields omitted.
mkGetRateBasedRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRateBasedRuleResponse
mkGetRateBasedRuleResponse responseStatus
  = GetRateBasedRuleResponse'{rule = Core.Nothing, responseStatus}

-- | Information about the 'RateBasedRule' that you specified in the @GetRateBasedRule@ request.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrrrsRule :: Lens.Lens' GetRateBasedRuleResponse (Core.Maybe Types.RateBasedRule)
grbrrrsRule = Lens.field @"rule"
{-# INLINEABLE grbrrrsRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrrrsResponseStatus :: Lens.Lens' GetRateBasedRuleResponse Core.Int
grbrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grbrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
