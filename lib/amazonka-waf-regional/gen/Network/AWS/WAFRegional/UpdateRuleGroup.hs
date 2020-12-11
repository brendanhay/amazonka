{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateRuleGroup
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
module Network.AWS.WAFRegional.UpdateRuleGroup
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
    urgrsChangeToken,
    urgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkUpdateRuleGroup' smart constructor.
data UpdateRuleGroup = UpdateRuleGroup'
  { ruleGroupId :: Lude.Text,
    updates :: Lude.NonEmpty RuleGroupUpdate,
    changeToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRuleGroup' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'ruleGroupId' - The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
-- * 'updates' - An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' .
--
-- You can only insert @REGULAR@ rules into a rule group.
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
mkUpdateRuleGroup ::
  -- | 'ruleGroupId'
  Lude.Text ->
  -- | 'updates'
  Lude.NonEmpty RuleGroupUpdate ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateRuleGroup
mkUpdateRuleGroup pRuleGroupId_ pUpdates_ pChangeToken_ =
  UpdateRuleGroup'
    { ruleGroupId = pRuleGroupId_,
      updates = pUpdates_,
      changeToken = pChangeToken_
    }

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgRuleGroupId :: Lens.Lens' UpdateRuleGroup Lude.Text
urgRuleGroupId = Lens.lens (ruleGroupId :: UpdateRuleGroup -> Lude.Text) (\s a -> s {ruleGroupId = a} :: UpdateRuleGroup)
{-# DEPRECATED urgRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' .
--
-- You can only insert @REGULAR@ rules into a rule group.
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgUpdates :: Lens.Lens' UpdateRuleGroup (Lude.NonEmpty RuleGroupUpdate)
urgUpdates = Lens.lens (updates :: UpdateRuleGroup -> Lude.NonEmpty RuleGroupUpdate) (\s a -> s {updates = a} :: UpdateRuleGroup)
{-# DEPRECATED urgUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgChangeToken :: Lens.Lens' UpdateRuleGroup Lude.Text
urgChangeToken = Lens.lens (changeToken :: UpdateRuleGroup -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateRuleGroup)
{-# DEPRECATED urgChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateRuleGroup where
  type Rs UpdateRuleGroup = UpdateRuleGroupResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRuleGroupResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRuleGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.UpdateRuleGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRuleGroup where
  toJSON UpdateRuleGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RuleGroupId" Lude..= ruleGroupId),
            Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateRuleGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRuleGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRuleGroupResponse' smart constructor.
data UpdateRuleGroupResponse = UpdateRuleGroupResponse'
  { changeToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRuleGroupResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateRuleGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRuleGroupResponse
mkUpdateRuleGroupResponse pResponseStatus_ =
  UpdateRuleGroupResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrsChangeToken :: Lens.Lens' UpdateRuleGroupResponse (Lude.Maybe Lude.Text)
urgrsChangeToken = Lens.lens (changeToken :: UpdateRuleGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateRuleGroupResponse)
{-# DEPRECATED urgrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrsResponseStatus :: Lens.Lens' UpdateRuleGroupResponse Lude.Int
urgrsResponseStatus = Lens.lens (responseStatus :: UpdateRuleGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRuleGroupResponse)
{-# DEPRECATED urgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
