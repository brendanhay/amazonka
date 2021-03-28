{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RuleGroup' . You can't delete a @RuleGroup@ if it's still used in any @WebACL@ objects or if it still includes any rules.
--
-- If you just want to remove a @RuleGroup@ from a @WebACL@ , use 'UpdateWebACL' .
-- To permanently delete a @RuleGroup@ from AWS WAF, perform the following steps:
--
--     * Update the @RuleGroup@ to remove rules, if any. For more information, see 'UpdateRuleGroup' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRuleGroup@ request.
--
--
--     * Submit a @DeleteRuleGroup@ request.
--
--
module Network.AWS.WAFRegional.DeleteRuleGroup
    (
    -- * Creating a request
      DeleteRuleGroup (..)
    , mkDeleteRuleGroup
    -- ** Request lenses
    , drgRuleGroupId
    , drgChangeToken

    -- * Destructuring the response
    , DeleteRuleGroupResponse (..)
    , mkDeleteRuleGroupResponse
    -- ** Response lenses
    , drgrrsChangeToken
    , drgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeleteRuleGroup' smart constructor.
data DeleteRuleGroup = DeleteRuleGroup'
  { ruleGroupId :: Types.ResourceId
    -- ^ The @RuleGroupId@ of the 'RuleGroup' that you want to delete. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRuleGroup' value with any optional fields omitted.
mkDeleteRuleGroup
    :: Types.ResourceId -- ^ 'ruleGroupId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteRuleGroup
mkDeleteRuleGroup ruleGroupId changeToken
  = DeleteRuleGroup'{ruleGroupId, changeToken}

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to delete. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgRuleGroupId :: Lens.Lens' DeleteRuleGroup Types.ResourceId
drgRuleGroupId = Lens.field @"ruleGroupId"
{-# INLINEABLE drgRuleGroupId #-}
{-# DEPRECATED ruleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgChangeToken :: Lens.Lens' DeleteRuleGroup Types.ChangeToken
drgChangeToken = Lens.field @"changeToken"
{-# INLINEABLE drgChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteRuleGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRuleGroup where
        toHeaders DeleteRuleGroup{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.DeleteRuleGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRuleGroup where
        toJSON DeleteRuleGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RuleGroupId" Core..= ruleGroupId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteRuleGroup where
        type Rs DeleteRuleGroup = DeleteRuleGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteRuleGroupResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRuleGroupResponse' smart constructor.
data DeleteRuleGroupResponse = DeleteRuleGroupResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRuleGroupResponse' value with any optional fields omitted.
mkDeleteRuleGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRuleGroupResponse
mkDeleteRuleGroupResponse responseStatus
  = DeleteRuleGroupResponse'{changeToken = Core.Nothing,
                             responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsChangeToken :: Lens.Lens' DeleteRuleGroupResponse (Core.Maybe Types.ChangeToken)
drgrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE drgrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsResponseStatus :: Lens.Lens' DeleteRuleGroupResponse Core.Int
drgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
