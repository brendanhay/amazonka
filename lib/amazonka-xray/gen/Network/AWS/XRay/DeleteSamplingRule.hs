{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.DeleteSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a sampling rule.
module Network.AWS.XRay.DeleteSamplingRule
    (
    -- * Creating a request
      DeleteSamplingRule (..)
    , mkDeleteSamplingRule
    -- ** Request lenses
    , dsrRuleARN
    , dsrRuleName

    -- * Destructuring the response
    , DeleteSamplingRuleResponse (..)
    , mkDeleteSamplingRuleResponse
    -- ** Response lenses
    , dsrrrsSamplingRuleRecord
    , dsrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkDeleteSamplingRule' smart constructor.
data DeleteSamplingRule = DeleteSamplingRule'
  { ruleARN :: Core.Maybe Core.Text
    -- ^ The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
  , ruleName :: Core.Maybe Core.Text
    -- ^ The name of the sampling rule. Specify a rule by either name or ARN, but not both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSamplingRule' value with any optional fields omitted.
mkDeleteSamplingRule
    :: DeleteSamplingRule
mkDeleteSamplingRule
  = DeleteSamplingRule'{ruleARN = Core.Nothing,
                        ruleName = Core.Nothing}

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRuleARN :: Lens.Lens' DeleteSamplingRule (Core.Maybe Core.Text)
dsrRuleARN = Lens.field @"ruleARN"
{-# INLINEABLE dsrRuleARN #-}
{-# DEPRECATED ruleARN "Use generic-lens or generic-optics with 'ruleARN' instead"  #-}

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRuleName :: Lens.Lens' DeleteSamplingRule (Core.Maybe Core.Text)
dsrRuleName = Lens.field @"ruleName"
{-# INLINEABLE dsrRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.ToQuery DeleteSamplingRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSamplingRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DeleteSamplingRule where
        toJSON DeleteSamplingRule{..}
          = Core.object
              (Core.catMaybes
                 [("RuleARN" Core..=) Core.<$> ruleARN,
                  ("RuleName" Core..=) Core.<$> ruleName])

instance Core.AWSRequest DeleteSamplingRule where
        type Rs DeleteSamplingRule = DeleteSamplingRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/DeleteSamplingRule",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSamplingRuleResponse' Core.<$>
                   (x Core..:? "SamplingRuleRecord") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSamplingRuleResponse' smart constructor.
data DeleteSamplingRuleResponse = DeleteSamplingRuleResponse'
  { samplingRuleRecord :: Core.Maybe Types.SamplingRuleRecord
    -- ^ The deleted rule definition and metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteSamplingRuleResponse' value with any optional fields omitted.
mkDeleteSamplingRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSamplingRuleResponse
mkDeleteSamplingRuleResponse responseStatus
  = DeleteSamplingRuleResponse'{samplingRuleRecord = Core.Nothing,
                                responseStatus}

-- | The deleted rule definition and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsSamplingRuleRecord :: Lens.Lens' DeleteSamplingRuleResponse (Core.Maybe Types.SamplingRuleRecord)
dsrrrsSamplingRuleRecord = Lens.field @"samplingRuleRecord"
{-# INLINEABLE dsrrrsSamplingRuleRecord #-}
{-# DEPRECATED samplingRuleRecord "Use generic-lens or generic-optics with 'samplingRuleRecord' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsResponseStatus :: Lens.Lens' DeleteSamplingRuleResponse Core.Int
dsrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
