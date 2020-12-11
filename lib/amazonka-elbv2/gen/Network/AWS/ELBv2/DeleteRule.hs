{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeleteRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- You can't delete the default rule.
module Network.AWS.ELBv2.DeleteRule
  ( -- * Creating a request
    DeleteRule (..),
    mkDeleteRule,

    -- ** Request lenses
    drRuleARN,

    -- * Destructuring the response
    DeleteRuleResponse (..),
    mkDeleteRuleResponse,

    -- ** Response lenses
    drrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRule' smart constructor.
newtype DeleteRule = DeleteRule' {ruleARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRule' with the minimum fields required to make a request.
--
-- * 'ruleARN' - The Amazon Resource Name (ARN) of the rule.
mkDeleteRule ::
  -- | 'ruleARN'
  Lude.Text ->
  DeleteRule
mkDeleteRule pRuleARN_ = DeleteRule' {ruleARN = pRuleARN_}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRuleARN :: Lens.Lens' DeleteRule Lude.Text
drRuleARN = Lens.lens (ruleARN :: DeleteRule -> Lude.Text) (\s a -> s {ruleARN = a} :: DeleteRule)
{-# DEPRECATED drRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

instance Lude.AWSRequest DeleteRule where
  type Rs DeleteRule = DeleteRuleResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DeleteRuleResult"
      ( \s h x ->
          DeleteRuleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRule where
  toQuery DeleteRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteRule" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "RuleArn" Lude.=: ruleARN
      ]

-- | /See:/ 'mkDeleteRuleResponse' smart constructor.
newtype DeleteRuleResponse = DeleteRuleResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRuleResponse
mkDeleteRuleResponse pResponseStatus_ =
  DeleteRuleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteRuleResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRuleResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
