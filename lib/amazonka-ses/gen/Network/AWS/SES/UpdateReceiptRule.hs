{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a receipt rule.
--
-- For information about managing receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateReceiptRule
  ( -- * Creating a request
    UpdateReceiptRule (..),
    mkUpdateReceiptRule,

    -- ** Request lenses
    urrRuleSetName,
    urrRule,

    -- * Destructuring the response
    UpdateReceiptRuleResponse (..),
    mkUpdateReceiptRuleResponse,

    -- ** Response lenses
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to update a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkUpdateReceiptRule' smart constructor.
data UpdateReceiptRule = UpdateReceiptRule'
  { -- | The name of the receipt rule set that the receipt rule belongs to.
    ruleSetName :: Lude.Text,
    -- | A data structure that contains the updated receipt rule information.
    rule :: ReceiptRule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateReceiptRule' with the minimum fields required to make a request.
--
-- * 'ruleSetName' - The name of the receipt rule set that the receipt rule belongs to.
-- * 'rule' - A data structure that contains the updated receipt rule information.
mkUpdateReceiptRule ::
  -- | 'ruleSetName'
  Lude.Text ->
  -- | 'rule'
  ReceiptRule ->
  UpdateReceiptRule
mkUpdateReceiptRule pRuleSetName_ pRule_ =
  UpdateReceiptRule' {ruleSetName = pRuleSetName_, rule = pRule_}

-- | The name of the receipt rule set that the receipt rule belongs to.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrRuleSetName :: Lens.Lens' UpdateReceiptRule Lude.Text
urrRuleSetName = Lens.lens (ruleSetName :: UpdateReceiptRule -> Lude.Text) (\s a -> s {ruleSetName = a} :: UpdateReceiptRule)
{-# DEPRECATED urrRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | A data structure that contains the updated receipt rule information.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrRule :: Lens.Lens' UpdateReceiptRule ReceiptRule
urrRule = Lens.lens (rule :: UpdateReceiptRule -> ReceiptRule) (\s a -> s {rule = a} :: UpdateReceiptRule)
{-# DEPRECATED urrRule "Use generic-lens or generic-optics with 'rule' instead." #-}

instance Lude.AWSRequest UpdateReceiptRule where
  type Rs UpdateReceiptRule = UpdateReceiptRuleResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "UpdateReceiptRuleResult"
      ( \s h x ->
          UpdateReceiptRuleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateReceiptRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateReceiptRule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateReceiptRule where
  toQuery UpdateReceiptRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateReceiptRule" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName,
        "Rule" Lude.=: rule
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkUpdateReceiptRuleResponse' smart constructor.
newtype UpdateReceiptRuleResponse = UpdateReceiptRuleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateReceiptRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateReceiptRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateReceiptRuleResponse
mkUpdateReceiptRuleResponse pResponseStatus_ =
  UpdateReceiptRuleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateReceiptRuleResponse Lude.Int
urrrsResponseStatus = Lens.lens (responseStatus :: UpdateReceiptRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateReceiptRuleResponse)
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
