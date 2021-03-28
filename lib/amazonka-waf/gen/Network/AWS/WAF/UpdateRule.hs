{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'Predicate' objects in a @Rule@ . Each @Predicate@ object identifies a predicate, such as a 'ByteMatchSet' or an 'IPSet' , that specifies the web requests that you want to allow, block, or count. If you add more than one predicate to a @Rule@ , a request must match all of the specifications to be allowed, blocked, or counted. For example, suppose that you add the following to a @Rule@ : 
--
--
--     * A @ByteMatchSet@ that matches the value @BadBot@ in the @User-Agent@ header
--
--
--     * An @IPSet@ that matches the IP address @192.0.2.44@ 
--
--
-- You then add the @Rule@ to a @WebACL@ and specify that you want to block requests that satisfy the @Rule@ . For a request to be blocked, the @User-Agent@ header in the request must contain the value @BadBot@ /and/ the request must originate from the IP address 192.0.2.44.
-- To create and configure a @Rule@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in the @Rule@ .
--
--
--     * Create the @Rule@ . See 'CreateRule' .
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRule' request.
--
--
--     * Submit an @UpdateRule@ request to add predicates to the @Rule@ .
--
--
--     * Create and update a @WebACL@ that contains the @Rule@ . See 'CreateWebACL' .
--
--
-- If you want to replace one @ByteMatchSet@ or @IPSet@ with another, you delete the existing one and add the new one.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateRule
    (
    -- * Creating a request
      UpdateRule (..)
    , mkUpdateRule
    -- ** Request lenses
    , urRuleId
    , urChangeToken
    , urUpdates

    -- * Destructuring the response
    , UpdateRuleResponse (..)
    , mkUpdateRuleResponse
    -- ** Response lenses
    , urrrsChangeToken
    , urrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkUpdateRule' smart constructor.
data UpdateRule = UpdateRule'
  { ruleId :: Types.ResourceId
    -- ^ The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned by @CreateRule@ and by 'ListRules' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , updates :: [Types.RuleUpdate]
    -- ^ An array of @RuleUpdate@ objects that you want to insert into or delete from a 'Rule' . For more information, see the applicable data types:
--
--
--     * 'RuleUpdate' : Contains @Action@ and @Predicate@ 
--
--
--     * 'Predicate' : Contains @DataId@ , @Negated@ , and @Type@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRule' value with any optional fields omitted.
mkUpdateRule
    :: Types.ResourceId -- ^ 'ruleId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> UpdateRule
mkUpdateRule ruleId changeToken
  = UpdateRule'{ruleId, changeToken, updates = Core.mempty}

-- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned by @CreateRule@ and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRuleId :: Lens.Lens' UpdateRule Types.ResourceId
urRuleId = Lens.field @"ruleId"
{-# INLINEABLE urRuleId #-}
{-# DEPRECATED ruleId "Use generic-lens or generic-optics with 'ruleId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urChangeToken :: Lens.Lens' UpdateRule Types.ChangeToken
urChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'Rule' . For more information, see the applicable data types:
--
--
--     * 'RuleUpdate' : Contains @Action@ and @Predicate@ 
--
--
--     * 'Predicate' : Contains @DataId@ , @Negated@ , and @Type@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urUpdates :: Lens.Lens' UpdateRule [Types.RuleUpdate]
urUpdates = Lens.field @"updates"
{-# INLINEABLE urUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

instance Core.ToQuery UpdateRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRule where
        toHeaders UpdateRule{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.UpdateRule") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRule where
        toJSON UpdateRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RuleId" Core..= ruleId),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  Core.Just ("Updates" Core..= updates)])

instance Core.AWSRequest UpdateRule where
        type Rs UpdateRule = UpdateRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateRuleResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRuleResponse' value with any optional fields omitted.
mkUpdateRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRuleResponse
mkUpdateRuleResponse responseStatus
  = UpdateRuleResponse'{changeToken = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsChangeToken :: Lens.Lens' UpdateRuleResponse (Core.Maybe Types.ChangeToken)
urrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE urrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateRuleResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
