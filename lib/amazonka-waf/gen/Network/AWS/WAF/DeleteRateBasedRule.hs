{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteRateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RateBasedRule' . You can't delete a rule if it's still used in any @WebACL@ objects or if it still includes any predicates, such as @ByteMatchSet@ objects.
--
-- If you just want to remove a rule from a @WebACL@ , use 'UpdateWebACL' .
-- To permanently delete a @RateBasedRule@ from AWS WAF, perform the following steps:
--
--     * Update the @RateBasedRule@ to remove predicates, if any. For more information, see 'UpdateRateBasedRule' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRateBasedRule@ request.
--
--
--     * Submit a @DeleteRateBasedRule@ request.
module Network.AWS.WAF.DeleteRateBasedRule
  ( -- * Creating a request
    DeleteRateBasedRule (..),
    mkDeleteRateBasedRule,

    -- ** Request lenses
    drbrRuleId,
    drbrChangeToken,

    -- * Destructuring the response
    DeleteRateBasedRuleResponse (..),
    mkDeleteRateBasedRuleResponse,

    -- ** Response lenses
    drbrrrsChangeToken,
    drbrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkDeleteRateBasedRule' smart constructor.
data DeleteRateBasedRule = DeleteRateBasedRule'
  { -- | The @RuleId@ of the 'RateBasedRule' that you want to delete. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
    ruleId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRateBasedRule' value with any optional fields omitted.
mkDeleteRateBasedRule ::
  -- | 'ruleId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  DeleteRateBasedRule
mkDeleteRateBasedRule ruleId changeToken =
  DeleteRateBasedRule' {ruleId, changeToken}

-- | The @RuleId@ of the 'RateBasedRule' that you want to delete. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrRuleId :: Lens.Lens' DeleteRateBasedRule Types.ResourceId
drbrRuleId = Lens.field @"ruleId"
{-# DEPRECATED drbrRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrChangeToken :: Lens.Lens' DeleteRateBasedRule Types.ChangeToken
drbrChangeToken = Lens.field @"changeToken"
{-# DEPRECATED drbrChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON DeleteRateBasedRule where
  toJSON DeleteRateBasedRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RuleId" Core..= ruleId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest DeleteRateBasedRule where
  type Rs DeleteRateBasedRule = DeleteRateBasedRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.DeleteRateBasedRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRateBasedRuleResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRateBasedRuleResponse' smart constructor.
data DeleteRateBasedRuleResponse = DeleteRateBasedRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRateBasedRuleResponse' value with any optional fields omitted.
mkDeleteRateBasedRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRateBasedRuleResponse
mkDeleteRateBasedRuleResponse responseStatus =
  DeleteRateBasedRuleResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrrrsChangeToken :: Lens.Lens' DeleteRateBasedRuleResponse (Core.Maybe Types.ChangeToken)
drbrrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED drbrrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrrrsResponseStatus :: Lens.Lens' DeleteRateBasedRuleResponse Core.Int
drbrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drbrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
