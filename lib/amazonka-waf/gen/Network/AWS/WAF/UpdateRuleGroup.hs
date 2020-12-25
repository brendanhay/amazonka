{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ActivatedRule' objects in a @RuleGroup@ .
--
-- You can only insert @REGULAR@ rules into a rule group.
-- You can have a maximum of ten rules per rule group.
-- To create and configure a @RuleGroup@ , perform the following steps:
--
--     * Create and update the @Rules@ that you want to include in the @RuleGroup@ . See 'CreateRule' .
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRuleGroup' request.
--
--
--     * Submit an @UpdateRuleGroup@ request to add @Rules@ to the @RuleGroup@ .
--
--
--     * Create and update a @WebACL@ that contains the @RuleGroup@ . See 'CreateWebACL' .
--
--
-- If you want to replace one @Rule@ with another, you delete the existing one and add the new one.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateRuleGroup
  ( -- * Creating a request
    UpdateRuleGroup (..),
    mkUpdateRuleGroup,

    -- ** Request lenses
    urgRuleGroupId,
    urgUpdates,
    urgChangeToken,

    -- * Destructuring the response
    UpdateRuleGroupResponse (..),
    mkUpdateRuleGroupResponse,

    -- ** Response lenses
    urgrrsChangeToken,
    urgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkUpdateRuleGroup' smart constructor.
data UpdateRuleGroup = UpdateRuleGroup'
  { -- | The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
    ruleGroupId :: Types.ResourceId,
    -- | An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' .
    --
    -- You can only insert @REGULAR@ rules into a rule group.
    -- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
    updates :: Core.NonEmpty Types.RuleGroupUpdate,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRuleGroup' value with any optional fields omitted.
mkUpdateRuleGroup ::
  -- | 'ruleGroupId'
  Types.ResourceId ->
  -- | 'updates'
  Core.NonEmpty Types.RuleGroupUpdate ->
  -- | 'changeToken'
  Types.ChangeToken ->
  UpdateRuleGroup
mkUpdateRuleGroup ruleGroupId updates changeToken =
  UpdateRuleGroup' {ruleGroupId, updates, changeToken}

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgRuleGroupId :: Lens.Lens' UpdateRuleGroup Types.ResourceId
urgRuleGroupId = Lens.field @"ruleGroupId"
{-# DEPRECATED urgRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' .
--
-- You can only insert @REGULAR@ rules into a rule group.
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgUpdates :: Lens.Lens' UpdateRuleGroup (Core.NonEmpty Types.RuleGroupUpdate)
urgUpdates = Lens.field @"updates"
{-# DEPRECATED urgUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgChangeToken :: Lens.Lens' UpdateRuleGroup Types.ChangeToken
urgChangeToken = Lens.field @"changeToken"
{-# DEPRECATED urgChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON UpdateRuleGroup where
  toJSON UpdateRuleGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RuleGroupId" Core..= ruleGroupId),
            Core.Just ("Updates" Core..= updates),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest UpdateRuleGroup where
  type Rs UpdateRuleGroup = UpdateRuleGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.UpdateRuleGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleGroupResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRuleGroupResponse' smart constructor.
data UpdateRuleGroupResponse = UpdateRuleGroupResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRuleGroupResponse' value with any optional fields omitted.
mkUpdateRuleGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRuleGroupResponse
mkUpdateRuleGroupResponse responseStatus =
  UpdateRuleGroupResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsChangeToken :: Lens.Lens' UpdateRuleGroupResponse (Core.Maybe Types.ChangeToken)
urgrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED urgrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsResponseStatus :: Lens.Lens' UpdateRuleGroupResponse Core.Int
urgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
