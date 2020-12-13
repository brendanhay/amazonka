{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a receipt rule.
--
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateReceiptRule
  ( -- * Creating a request
    CreateReceiptRule (..),
    mkCreateReceiptRule,

    -- ** Request lenses
    crrAfter,
    crrRuleSetName,
    crrRule,

    -- * Destructuring the response
    CreateReceiptRuleResponse (..),
    mkCreateReceiptRuleResponse,

    -- ** Response lenses
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateReceiptRule' smart constructor.
data CreateReceiptRule = CreateReceiptRule'
  { -- | The name of an existing rule after which the new rule will be placed. If this parameter is null, the new rule will be inserted at the beginning of the rule list.
    after :: Lude.Maybe Lude.Text,
    -- | The name of the rule set that the receipt rule will be added to.
    ruleSetName :: Lude.Text,
    -- | A data structure that contains the specified rule's name, actions, recipients, domains, enabled status, scan status, and TLS policy.
    rule :: ReceiptRule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReceiptRule' with the minimum fields required to make a request.
--
-- * 'after' - The name of an existing rule after which the new rule will be placed. If this parameter is null, the new rule will be inserted at the beginning of the rule list.
-- * 'ruleSetName' - The name of the rule set that the receipt rule will be added to.
-- * 'rule' - A data structure that contains the specified rule's name, actions, recipients, domains, enabled status, scan status, and TLS policy.
mkCreateReceiptRule ::
  -- | 'ruleSetName'
  Lude.Text ->
  -- | 'rule'
  ReceiptRule ->
  CreateReceiptRule
mkCreateReceiptRule pRuleSetName_ pRule_ =
  CreateReceiptRule'
    { after = Lude.Nothing,
      ruleSetName = pRuleSetName_,
      rule = pRule_
    }

-- | The name of an existing rule after which the new rule will be placed. If this parameter is null, the new rule will be inserted at the beginning of the rule list.
--
-- /Note:/ Consider using 'after' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrAfter :: Lens.Lens' CreateReceiptRule (Lude.Maybe Lude.Text)
crrAfter = Lens.lens (after :: CreateReceiptRule -> Lude.Maybe Lude.Text) (\s a -> s {after = a} :: CreateReceiptRule)
{-# DEPRECATED crrAfter "Use generic-lens or generic-optics with 'after' instead." #-}

-- | The name of the rule set that the receipt rule will be added to.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrRuleSetName :: Lens.Lens' CreateReceiptRule Lude.Text
crrRuleSetName = Lens.lens (ruleSetName :: CreateReceiptRule -> Lude.Text) (\s a -> s {ruleSetName = a} :: CreateReceiptRule)
{-# DEPRECATED crrRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | A data structure that contains the specified rule's name, actions, recipients, domains, enabled status, scan status, and TLS policy.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrRule :: Lens.Lens' CreateReceiptRule ReceiptRule
crrRule = Lens.lens (rule :: CreateReceiptRule -> ReceiptRule) (\s a -> s {rule = a} :: CreateReceiptRule)
{-# DEPRECATED crrRule "Use generic-lens or generic-optics with 'rule' instead." #-}

instance Lude.AWSRequest CreateReceiptRule where
  type Rs CreateReceiptRule = CreateReceiptRuleResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CreateReceiptRuleResult"
      ( \s h x ->
          CreateReceiptRuleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReceiptRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateReceiptRule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReceiptRule where
  toQuery CreateReceiptRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateReceiptRule" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "After" Lude.=: after,
        "RuleSetName" Lude.=: ruleSetName,
        "Rule" Lude.=: rule
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateReceiptRuleResponse' smart constructor.
newtype CreateReceiptRuleResponse = CreateReceiptRuleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReceiptRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateReceiptRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReceiptRuleResponse
mkCreateReceiptRuleResponse pResponseStatus_ =
  CreateReceiptRuleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateReceiptRuleResponse Lude.Int
crrrsResponseStatus = Lens.lens (responseStatus :: CreateReceiptRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReceiptRuleResponse)
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
