{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified receipt rule.
--
-- For information about managing receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteReceiptRule
  ( -- * Creating a request
    DeleteReceiptRule (..),
    mkDeleteReceiptRule,

    -- ** Request lenses
    delRuleSetName,
    delRuleName,

    -- * Destructuring the response
    DeleteReceiptRuleResponse (..),
    mkDeleteReceiptRuleResponse,

    -- ** Response lenses
    delrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteReceiptRule' smart constructor.
data DeleteReceiptRule = DeleteReceiptRule'
  { ruleSetName ::
      Lude.Text,
    ruleName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReceiptRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the receipt rule to delete.
-- * 'ruleSetName' - The name of the receipt rule set that contains the receipt rule to delete.
mkDeleteReceiptRule ::
  -- | 'ruleSetName'
  Lude.Text ->
  -- | 'ruleName'
  Lude.Text ->
  DeleteReceiptRule
mkDeleteReceiptRule pRuleSetName_ pRuleName_ =
  DeleteReceiptRule'
    { ruleSetName = pRuleSetName_,
      ruleName = pRuleName_
    }

-- | The name of the receipt rule set that contains the receipt rule to delete.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delRuleSetName :: Lens.Lens' DeleteReceiptRule Lude.Text
delRuleSetName = Lens.lens (ruleSetName :: DeleteReceiptRule -> Lude.Text) (\s a -> s {ruleSetName = a} :: DeleteReceiptRule)
{-# DEPRECATED delRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The name of the receipt rule to delete.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delRuleName :: Lens.Lens' DeleteReceiptRule Lude.Text
delRuleName = Lens.lens (ruleName :: DeleteReceiptRule -> Lude.Text) (\s a -> s {ruleName = a} :: DeleteReceiptRule)
{-# DEPRECATED delRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Lude.AWSRequest DeleteReceiptRule where
  type Rs DeleteReceiptRule = DeleteReceiptRuleResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteReceiptRuleResult"
      ( \s h x ->
          DeleteReceiptRuleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReceiptRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteReceiptRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReceiptRule where
  toQuery DeleteReceiptRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteReceiptRule" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName,
        "RuleName" Lude.=: ruleName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteReceiptRuleResponse' smart constructor.
newtype DeleteReceiptRuleResponse = DeleteReceiptRuleResponse'
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

-- | Creates a value of 'DeleteReceiptRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReceiptRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReceiptRuleResponse
mkDeleteReceiptRuleResponse pResponseStatus_ =
  DeleteReceiptRuleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteReceiptRuleResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteReceiptRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReceiptRuleResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
