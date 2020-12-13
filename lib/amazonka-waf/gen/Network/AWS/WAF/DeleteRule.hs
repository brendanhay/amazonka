{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'Rule' . You can't delete a @Rule@ if it's still used in any @WebACL@ objects or if it still includes any predicates, such as @ByteMatchSet@ objects.
--
-- If you just want to remove a @Rule@ from a @WebACL@ , use 'UpdateWebACL' .
-- To permanently delete a @Rule@ from AWS WAF, perform the following steps:
--
--     * Update the @Rule@ to remove predicates, if any. For more information, see 'UpdateRule' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRule@ request.
--
--
--     * Submit a @DeleteRule@ request.
module Network.AWS.WAF.DeleteRule
  ( -- * Creating a request
    DeleteRule (..),
    mkDeleteRule,

    -- ** Request lenses
    drRuleId,
    drChangeToken,

    -- * Destructuring the response
    DeleteRuleResponse (..),
    mkDeleteRuleResponse,

    -- ** Response lenses
    drrsChangeToken,
    drrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The @RuleId@ of the 'Rule' that you want to delete. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
    ruleId :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRule' with the minimum fields required to make a request.
--
-- * 'ruleId' - The @RuleId@ of the 'Rule' that you want to delete. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkDeleteRule ::
  -- | 'ruleId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteRule
mkDeleteRule pRuleId_ pChangeToken_ =
  DeleteRule' {ruleId = pRuleId_, changeToken = pChangeToken_}

-- | The @RuleId@ of the 'Rule' that you want to delete. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRuleId :: Lens.Lens' DeleteRule Lude.Text
drRuleId = Lens.lens (ruleId :: DeleteRule -> Lude.Text) (\s a -> s {ruleId = a} :: DeleteRule)
{-# DEPRECATED drRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drChangeToken :: Lens.Lens' DeleteRule Lude.Text
drChangeToken = Lens.lens (changeToken :: DeleteRule -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteRule)
{-# DEPRECATED drChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteRule where
  type Rs DeleteRule = DeleteRuleResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRuleResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRule where
  toJSON DeleteRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RuleId" Lude..= ruleId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRuleResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRuleResponse
mkDeleteRuleResponse pResponseStatus_ =
  DeleteRuleResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsChangeToken :: Lens.Lens' DeleteRuleResponse (Lude.Maybe Lude.Text)
drrsChangeToken = Lens.lens (changeToken :: DeleteRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteRuleResponse)
{-# DEPRECATED drrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteRuleResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRuleResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
