{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified receipt rule.
--
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeReceiptRule
  ( -- * Creating a request
    DescribeReceiptRule (..),
    mkDescribeReceiptRule,

    -- ** Request lenses
    drrRuleSetName,
    drrRuleName,

    -- * Destructuring the response
    DescribeReceiptRuleResponse (..),
    mkDescribeReceiptRuleResponse,

    -- ** Response lenses
    drrrsRule,
    drrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the details of a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeReceiptRule' smart constructor.
data DescribeReceiptRule = DescribeReceiptRule'
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

-- | Creates a value of 'DescribeReceiptRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the receipt rule.
-- * 'ruleSetName' - The name of the receipt rule set that the receipt rule belongs to.
mkDescribeReceiptRule ::
  -- | 'ruleSetName'
  Lude.Text ->
  -- | 'ruleName'
  Lude.Text ->
  DescribeReceiptRule
mkDescribeReceiptRule pRuleSetName_ pRuleName_ =
  DescribeReceiptRule'
    { ruleSetName = pRuleSetName_,
      ruleName = pRuleName_
    }

-- | The name of the receipt rule set that the receipt rule belongs to.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrRuleSetName :: Lens.Lens' DescribeReceiptRule Lude.Text
drrRuleSetName = Lens.lens (ruleSetName :: DescribeReceiptRule -> Lude.Text) (\s a -> s {ruleSetName = a} :: DescribeReceiptRule)
{-# DEPRECATED drrRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The name of the receipt rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrRuleName :: Lens.Lens' DescribeReceiptRule Lude.Text
drrRuleName = Lens.lens (ruleName :: DescribeReceiptRule -> Lude.Text) (\s a -> s {ruleName = a} :: DescribeReceiptRule)
{-# DEPRECATED drrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Lude.AWSRequest DescribeReceiptRule where
  type Rs DescribeReceiptRule = DescribeReceiptRuleResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DescribeReceiptRuleResult"
      ( \s h x ->
          DescribeReceiptRuleResponse'
            Lude.<$> (x Lude..@? "Rule") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReceiptRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReceiptRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReceiptRule where
  toQuery DescribeReceiptRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeReceiptRule" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName,
        "RuleName" Lude.=: ruleName
      ]

-- | Represents the details of a receipt rule.
--
-- /See:/ 'mkDescribeReceiptRuleResponse' smart constructor.
data DescribeReceiptRuleResponse = DescribeReceiptRuleResponse'
  { rule ::
      Lude.Maybe ReceiptRule,
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

-- | Creates a value of 'DescribeReceiptRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'rule' - A data structure that contains the specified receipt rule's name, actions, recipients, domains, enabled status, scan status, and Transport Layer Security (TLS) policy.
mkDescribeReceiptRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReceiptRuleResponse
mkDescribeReceiptRuleResponse pResponseStatus_ =
  DescribeReceiptRuleResponse'
    { rule = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A data structure that contains the specified receipt rule's name, actions, recipients, domains, enabled status, scan status, and Transport Layer Security (TLS) policy.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRule :: Lens.Lens' DescribeReceiptRuleResponse (Lude.Maybe ReceiptRule)
drrrsRule = Lens.lens (rule :: DescribeReceiptRuleResponse -> Lude.Maybe ReceiptRule) (\s a -> s {rule = a} :: DescribeReceiptRuleResponse)
{-# DEPRECATED drrrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DescribeReceiptRuleResponse Lude.Int
drrrsResponseStatus = Lens.lens (responseStatus :: DescribeReceiptRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReceiptRuleResponse)
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
