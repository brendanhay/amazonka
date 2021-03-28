{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'Rule' that is specified by the @RuleId@ that you included in the @GetRule@ request.
module Network.AWS.WAF.GetRule
    (
    -- * Creating a request
      GetRule (..)
    , mkGetRule
    -- ** Request lenses
    , grRuleId

    -- * Destructuring the response
    , GetRuleResponse (..)
    , mkGetRuleResponse
    -- ** Response lenses
    , grrrsRule
    , grrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetRule' smart constructor.
newtype GetRule = GetRule'
  { ruleId :: Types.ResourceId
    -- ^ The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRule' value with any optional fields omitted.
mkGetRule
    :: Types.ResourceId -- ^ 'ruleId'
    -> GetRule
mkGetRule ruleId = GetRule'{ruleId}

-- | The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRuleId :: Lens.Lens' GetRule Types.ResourceId
grRuleId = Lens.field @"ruleId"
{-# INLINEABLE grRuleId #-}
{-# DEPRECATED ruleId "Use generic-lens or generic-optics with 'ruleId' instead"  #-}

instance Core.ToQuery GetRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRule where
        toHeaders GetRule{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetRule") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRule where
        toJSON GetRule{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RuleId" Core..= ruleId)])

instance Core.AWSRequest GetRule where
        type Rs GetRule = GetRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRuleResponse' Core.<$>
                   (x Core..:? "Rule") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { rule :: Core.Maybe Types.Rule
    -- ^ Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:
--
--
--     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@ 
--
--
--     * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@ 
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRuleResponse' value with any optional fields omitted.
mkGetRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRuleResponse
mkGetRuleResponse responseStatus
  = GetRuleResponse'{rule = Core.Nothing, responseStatus}

-- | Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:
--
--
--     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@ 
--
--
--     * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@ 
--
--
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRule :: Lens.Lens' GetRuleResponse (Core.Maybe Types.Rule)
grrrsRule = Lens.field @"rule"
{-# INLINEABLE grrrsRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRuleResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
