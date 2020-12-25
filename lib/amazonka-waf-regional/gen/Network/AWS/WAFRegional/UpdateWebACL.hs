{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ActivatedRule' objects in a @WebACL@ . Each @Rule@ identifies web requests that you want to allow, block, or count. When you update a @WebACL@ , you specify the following values:
--
--
--     * A default action for the @WebACL@ , either @ALLOW@ or @BLOCK@ . AWS WAF performs the default action if a request doesn't match the criteria in any of the @Rules@ in a @WebACL@ .
--
--
--     * The @Rules@ that you want to add or delete. If you want to replace one @Rule@ with another, you delete the existing @Rule@ and add the new one.
--
--
--     * For each @Rule@ , whether you want AWS WAF to allow requests, block requests, or count requests that match the conditions in the @Rule@ .
--
--
--     * The order in which you want AWS WAF to evaluate the @Rules@ in a @WebACL@ . If you add more than one @Rule@ to a @WebACL@ , AWS WAF evaluates each request against the @Rules@ in order based on the value of @Priority@ . (The @Rule@ that has the lowest value for @Priority@ is evaluated first.) When a web request matches all the predicates (such as @ByteMatchSets@ and @IPSets@ ) in a @Rule@ , AWS WAF immediately takes the corresponding action, allow or block, and doesn't evaluate the request against the remaining @Rules@ in the @WebACL@ , if any.
--
--
-- To create and configure a @WebACL@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in @Rules@ . For more information, see 'CreateByteMatchSet' , 'UpdateByteMatchSet' , 'CreateIPSet' , 'UpdateIPSet' , 'CreateSqlInjectionMatchSet' , and 'UpdateSqlInjectionMatchSet' .
--
--
--     * Create and update the @Rules@ that you want to include in the @WebACL@ . For more information, see 'CreateRule' and 'UpdateRule' .
--
--
--     * Create a @WebACL@ . See 'CreateWebACL' .
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateWebACL' request.
--
--
--     * Submit an @UpdateWebACL@ request to specify the @Rules@ that you want to include in the @WebACL@ , to specify the default action, and to associate the @WebACL@ with a CloudFront distribution.
-- The @ActivatedRule@ can be a rule group. If you specify a rule group as your @ActivatedRule@ , you can exclude specific rules from that rule group.
-- If you already have a rule group associated with a web ACL and want to submit an @UpdateWebACL@ request to exclude certain rules from that rule group, you must first remove the rule group from the web ACL, the re-insert it again, specifying the excluded rules. For details, see 'ActivatedRule$ExcludedRules' .
--
--
-- Be aware that if you try to add a RATE_BASED rule to a web ACL without setting the rule type when first creating the rule, the 'UpdateWebACL' request will fail because the request tries to add a REGULAR rule (the default rule type) with the specified ID, which does not exist.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateWebACL
  ( -- * Creating a request
    UpdateWebACL (..),
    mkUpdateWebACL,

    -- ** Request lenses
    uwaclWebACLId,
    uwaclChangeToken,
    uwaclDefaultAction,
    uwaclUpdates,

    -- * Destructuring the response
    UpdateWebACLResponse (..),
    mkUpdateWebACLResponse,

    -- ** Response lenses
    uwaclrrsChangeToken,
    uwaclrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkUpdateWebACL' smart constructor.
data UpdateWebACL = UpdateWebACL'
  { -- | The @WebACLId@ of the 'WebACL' that you want to update. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
    webACLId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken,
    -- | A default action for the web ACL, either ALLOW or BLOCK. AWS WAF performs the default action if a request doesn't match the criteria in any of the rules in a web ACL.
    defaultAction :: Core.Maybe Types.WafAction,
    -- | An array of updates to make to the 'WebACL' .
    --
    -- An array of @WebACLUpdate@ objects that you want to insert into or delete from a 'WebACL' . For more information, see the applicable data types:
    --
    --     * 'WebACLUpdate' : Contains @Action@ and @ActivatedRule@
    --
    --
    --     * 'ActivatedRule' : Contains @Action@ , @OverrideAction@ , @Priority@ , @RuleId@ , and @Type@ . @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case, you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
    --
    --
    --     * 'WafAction' : Contains @Type@
    updates :: Core.Maybe [Types.WebACLUpdate]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWebACL' value with any optional fields omitted.
mkUpdateWebACL ::
  -- | 'webACLId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  UpdateWebACL
mkUpdateWebACL webACLId changeToken =
  UpdateWebACL'
    { webACLId,
      changeToken,
      defaultAction = Core.Nothing,
      updates = Core.Nothing
    }

-- | The @WebACLId@ of the 'WebACL' that you want to update. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaclWebACLId :: Lens.Lens' UpdateWebACL Types.ResourceId
uwaclWebACLId = Lens.field @"webACLId"
{-# DEPRECATED uwaclWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaclChangeToken :: Lens.Lens' UpdateWebACL Types.ChangeToken
uwaclChangeToken = Lens.field @"changeToken"
{-# DEPRECATED uwaclChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | A default action for the web ACL, either ALLOW or BLOCK. AWS WAF performs the default action if a request doesn't match the criteria in any of the rules in a web ACL.
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaclDefaultAction :: Lens.Lens' UpdateWebACL (Core.Maybe Types.WafAction)
uwaclDefaultAction = Lens.field @"defaultAction"
{-# DEPRECATED uwaclDefaultAction "Use generic-lens or generic-optics with 'defaultAction' instead." #-}

-- | An array of updates to make to the 'WebACL' .
--
-- An array of @WebACLUpdate@ objects that you want to insert into or delete from a 'WebACL' . For more information, see the applicable data types:
--
--     * 'WebACLUpdate' : Contains @Action@ and @ActivatedRule@
--
--
--     * 'ActivatedRule' : Contains @Action@ , @OverrideAction@ , @Priority@ , @RuleId@ , and @Type@ . @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case, you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
--
--     * 'WafAction' : Contains @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaclUpdates :: Lens.Lens' UpdateWebACL (Core.Maybe [Types.WebACLUpdate])
uwaclUpdates = Lens.field @"updates"
{-# DEPRECATED uwaclUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

instance Core.FromJSON UpdateWebACL where
  toJSON UpdateWebACL {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WebACLId" Core..= webACLId),
            Core.Just ("ChangeToken" Core..= changeToken),
            ("DefaultAction" Core..=) Core.<$> defaultAction,
            ("Updates" Core..=) Core.<$> updates
          ]
      )

instance Core.AWSRequest UpdateWebACL where
  type Rs UpdateWebACL = UpdateWebACLResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_Regional_20161128.UpdateWebACL")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWebACLResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateWebACLResponse' smart constructor.
data UpdateWebACLResponse = UpdateWebACLResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWebACLResponse' value with any optional fields omitted.
mkUpdateWebACLResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateWebACLResponse
mkUpdateWebACLResponse responseStatus =
  UpdateWebACLResponse' {changeToken = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaclrrsChangeToken :: Lens.Lens' UpdateWebACLResponse (Core.Maybe Types.ChangeToken)
uwaclrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED uwaclrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaclrrsResponseStatus :: Lens.Lens' UpdateWebACLResponse Core.Int
uwaclrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uwaclrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
