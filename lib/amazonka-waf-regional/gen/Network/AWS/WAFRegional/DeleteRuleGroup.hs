{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.WAFRegional.DeleteRuleGroup
  ( -- * Creating a request
    DeleteRuleGroup (..),
    mkDeleteRuleGroup,

    -- ** Request lenses
    drgRuleGroupId,
    drgChangeToken,

    -- * Destructuring the response
    DeleteRuleGroupResponse (..),
    mkDeleteRuleGroupResponse,

    -- ** Response lenses
    drgrsChangeToken,
    drgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkDeleteRuleGroup' smart constructor.
data DeleteRuleGroup = DeleteRuleGroup'
  { -- | The @RuleGroupId@ of the 'RuleGroup' that you want to delete. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
    ruleGroupId :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRuleGroup' with the minimum fields required to make a request.
--
-- * 'ruleGroupId' - The @RuleGroupId@ of the 'RuleGroup' that you want to delete. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkDeleteRuleGroup ::
  -- | 'ruleGroupId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteRuleGroup
mkDeleteRuleGroup pRuleGroupId_ pChangeToken_ =
  DeleteRuleGroup'
    { ruleGroupId = pRuleGroupId_,
      changeToken = pChangeToken_
    }

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to delete. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgRuleGroupId :: Lens.Lens' DeleteRuleGroup Lude.Text
drgRuleGroupId = Lens.lens (ruleGroupId :: DeleteRuleGroup -> Lude.Text) (\s a -> s {ruleGroupId = a} :: DeleteRuleGroup)
{-# DEPRECATED drgRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgChangeToken :: Lens.Lens' DeleteRuleGroup Lude.Text
drgChangeToken = Lens.lens (changeToken :: DeleteRuleGroup -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteRuleGroup)
{-# DEPRECATED drgChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteRuleGroup where
  type Rs DeleteRuleGroup = DeleteRuleGroupResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRuleGroupResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRuleGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.DeleteRuleGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRuleGroup where
  toJSON DeleteRuleGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RuleGroupId" Lude..= ruleGroupId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteRuleGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRuleGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRuleGroupResponse' smart constructor.
data DeleteRuleGroupResponse = DeleteRuleGroupResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRuleGroupResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteRuleGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRuleGroupResponse
mkDeleteRuleGroupResponse pResponseStatus_ =
  DeleteRuleGroupResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsChangeToken :: Lens.Lens' DeleteRuleGroupResponse (Lude.Maybe Lude.Text)
drgrsChangeToken = Lens.lens (changeToken :: DeleteRuleGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteRuleGroupResponse)
{-# DEPRECATED drgrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsResponseStatus :: Lens.Lens' DeleteRuleGroupResponse Lude.Int
drgrsResponseStatus = Lens.lens (responseStatus :: DeleteRuleGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRuleGroupResponse)
{-# DEPRECATED drgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
