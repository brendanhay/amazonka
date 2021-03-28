{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateRateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'Predicate' objects in a rule and updates the @RateLimit@ in the rule. 
--
-- Each @Predicate@ object identifies a predicate, such as a 'ByteMatchSet' or an 'IPSet' , that specifies the web requests that you want to block or count. The @RateLimit@ specifies the number of requests every five minutes that triggers the rule.
-- If you add more than one predicate to a @RateBasedRule@ , a request must match all the predicates and exceed the @RateLimit@ to be counted or blocked. For example, suppose you add the following to a @RateBasedRule@ :
--
--     * An @IPSet@ that matches the IP address @192.0.2.44/32@ 
--
--
--     * A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
--
-- Further, you specify a @RateLimit@ of 1,000.
-- You then add the @RateBasedRule@ to a @WebACL@ and specify that you want to block requests that satisfy the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header in the request must contain the value @BadBot@ . Further, requests that match these two conditions much be received at a rate of more than 1,000 every five minutes. If the rate drops below this limit, AWS WAF no longer blocks the requests.
-- As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a @RateBasedRule@ :
--
--     * A @ByteMatchSet@ with @FieldToMatch@ of @URI@ 
--
--
--     * A @PositionalConstraint@ of @STARTS_WITH@ 
--
--
--     * A @TargetString@ of @login@ 
--
--
-- Further, you specify a @RateLimit@ of 1,000.
-- By adding this @RateBasedRule@ to a @WebACL@ , you could limit requests to your login page without affecting the rest of your site.
module Network.AWS.WAF.UpdateRateBasedRule
    (
    -- * Creating a request
      UpdateRateBasedRule (..)
    , mkUpdateRateBasedRule
    -- ** Request lenses
    , urbrRuleId
    , urbrChangeToken
    , urbrUpdates
    , urbrRateLimit

    -- * Destructuring the response
    , UpdateRateBasedRuleResponse (..)
    , mkUpdateRateBasedRuleResponse
    -- ** Response lenses
    , urbrrrsChangeToken
    , urbrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkUpdateRateBasedRule' smart constructor.
data UpdateRateBasedRule = UpdateRateBasedRule'
  { ruleId :: Types.ResourceId
    -- ^ The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is returned by @CreateRateBasedRule@ and by 'ListRateBasedRules' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , updates :: [Types.RuleUpdate]
    -- ^ An array of @RuleUpdate@ objects that you want to insert into or delete from a 'RateBasedRule' . 
  , rateLimit :: Core.Natural
    -- ^ The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRateBasedRule' value with any optional fields omitted.
mkUpdateRateBasedRule
    :: Types.ResourceId -- ^ 'ruleId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> Core.Natural -- ^ 'rateLimit'
    -> UpdateRateBasedRule
mkUpdateRateBasedRule ruleId changeToken rateLimit
  = UpdateRateBasedRule'{ruleId, changeToken, updates = Core.mempty,
                         rateLimit}

-- | The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is returned by @CreateRateBasedRule@ and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrRuleId :: Lens.Lens' UpdateRateBasedRule Types.ResourceId
urbrRuleId = Lens.field @"ruleId"
{-# INLINEABLE urbrRuleId #-}
{-# DEPRECATED ruleId "Use generic-lens or generic-optics with 'ruleId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrChangeToken :: Lens.Lens' UpdateRateBasedRule Types.ChangeToken
urbrChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urbrChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'RateBasedRule' . 
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrUpdates :: Lens.Lens' UpdateRateBasedRule [Types.RuleUpdate]
urbrUpdates = Lens.field @"updates"
{-# INLINEABLE urbrUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

-- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrRateLimit :: Lens.Lens' UpdateRateBasedRule Core.Natural
urbrRateLimit = Lens.field @"rateLimit"
{-# INLINEABLE urbrRateLimit #-}
{-# DEPRECATED rateLimit "Use generic-lens or generic-optics with 'rateLimit' instead"  #-}

instance Core.ToQuery UpdateRateBasedRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRateBasedRule where
        toHeaders UpdateRateBasedRule{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.UpdateRateBasedRule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRateBasedRule where
        toJSON UpdateRateBasedRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RuleId" Core..= ruleId),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  Core.Just ("Updates" Core..= updates),
                  Core.Just ("RateLimit" Core..= rateLimit)])

instance Core.AWSRequest UpdateRateBasedRule where
        type Rs UpdateRateBasedRule = UpdateRateBasedRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateRateBasedRuleResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRateBasedRuleResponse' smart constructor.
data UpdateRateBasedRuleResponse = UpdateRateBasedRuleResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRateBasedRuleResponse' value with any optional fields omitted.
mkUpdateRateBasedRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRateBasedRuleResponse
mkUpdateRateBasedRuleResponse responseStatus
  = UpdateRateBasedRuleResponse'{changeToken = Core.Nothing,
                                 responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrrrsChangeToken :: Lens.Lens' UpdateRateBasedRuleResponse (Core.Maybe Types.ChangeToken)
urbrrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urbrrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrrrsResponseStatus :: Lens.Lens' UpdateRateBasedRuleResponse Core.Int
urbrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urbrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
