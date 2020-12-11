{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    drbrrsChangeToken,
    drbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkDeleteRateBasedRule' smart constructor.
data DeleteRateBasedRule = DeleteRateBasedRule'
  { ruleId ::
      Lude.Text,
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

-- | Creates a value of 'DeleteRateBasedRule' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'ruleId' - The @RuleId@ of the 'RateBasedRule' that you want to delete. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
mkDeleteRateBasedRule ::
  -- | 'ruleId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteRateBasedRule
mkDeleteRateBasedRule pRuleId_ pChangeToken_ =
  DeleteRateBasedRule'
    { ruleId = pRuleId_,
      changeToken = pChangeToken_
    }

-- | The @RuleId@ of the 'RateBasedRule' that you want to delete. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrRuleId :: Lens.Lens' DeleteRateBasedRule Lude.Text
drbrRuleId = Lens.lens (ruleId :: DeleteRateBasedRule -> Lude.Text) (\s a -> s {ruleId = a} :: DeleteRateBasedRule)
{-# DEPRECATED drbrRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrChangeToken :: Lens.Lens' DeleteRateBasedRule Lude.Text
drbrChangeToken = Lens.lens (changeToken :: DeleteRateBasedRule -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteRateBasedRule)
{-# DEPRECATED drbrChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteRateBasedRule where
  type Rs DeleteRateBasedRule = DeleteRateBasedRuleResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRateBasedRuleResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRateBasedRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteRateBasedRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRateBasedRule where
  toJSON DeleteRateBasedRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RuleId" Lude..= ruleId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteRateBasedRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRateBasedRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRateBasedRuleResponse' smart constructor.
data DeleteRateBasedRuleResponse = DeleteRateBasedRuleResponse'
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

-- | Creates a value of 'DeleteRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteRateBasedRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRateBasedRuleResponse
mkDeleteRateBasedRuleResponse pResponseStatus_ =
  DeleteRateBasedRuleResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrrsChangeToken :: Lens.Lens' DeleteRateBasedRuleResponse (Lude.Maybe Lude.Text)
drbrrsChangeToken = Lens.lens (changeToken :: DeleteRateBasedRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteRateBasedRuleResponse)
{-# DEPRECATED drbrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbrrsResponseStatus :: Lens.Lens' DeleteRateBasedRuleResponse Lude.Int
drbrrsResponseStatus = Lens.lens (responseStatus :: DeleteRateBasedRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRateBasedRuleResponse)
{-# DEPRECATED drbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
