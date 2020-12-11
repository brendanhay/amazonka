{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ReorderReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reorders the receipt rules within a receipt rule set.
--
-- For information about managing receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.ReorderReceiptRuleSet
  ( -- * Creating a request
    ReorderReceiptRuleSet (..),
    mkReorderReceiptRuleSet,

    -- ** Request lenses
    rrrsRuleSetName,
    rrrsRuleNames,

    -- * Destructuring the response
    ReorderReceiptRuleSetResponse (..),
    mkReorderReceiptRuleSetResponse,

    -- ** Response lenses
    rrrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to reorder the receipt rules within a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReorderReceiptRuleSet' smart constructor.
data ReorderReceiptRuleSet = ReorderReceiptRuleSet'
  { ruleSetName ::
      Lude.Text,
    ruleNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReorderReceiptRuleSet' with the minimum fields required to make a request.
--
-- * 'ruleNames' - A list of the specified receipt rule set's receipt rules in the order that you want to put them.
-- * 'ruleSetName' - The name of the receipt rule set to reorder.
mkReorderReceiptRuleSet ::
  -- | 'ruleSetName'
  Lude.Text ->
  ReorderReceiptRuleSet
mkReorderReceiptRuleSet pRuleSetName_ =
  ReorderReceiptRuleSet'
    { ruleSetName = pRuleSetName_,
      ruleNames = Lude.mempty
    }

-- | The name of the receipt rule set to reorder.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsRuleSetName :: Lens.Lens' ReorderReceiptRuleSet Lude.Text
rrrsRuleSetName = Lens.lens (ruleSetName :: ReorderReceiptRuleSet -> Lude.Text) (\s a -> s {ruleSetName = a} :: ReorderReceiptRuleSet)
{-# DEPRECATED rrrsRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | A list of the specified receipt rule set's receipt rules in the order that you want to put them.
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsRuleNames :: Lens.Lens' ReorderReceiptRuleSet [Lude.Text]
rrrsRuleNames = Lens.lens (ruleNames :: ReorderReceiptRuleSet -> [Lude.Text]) (\s a -> s {ruleNames = a} :: ReorderReceiptRuleSet)
{-# DEPRECATED rrrsRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Lude.AWSRequest ReorderReceiptRuleSet where
  type Rs ReorderReceiptRuleSet = ReorderReceiptRuleSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ReorderReceiptRuleSetResult"
      ( \s h x ->
          ReorderReceiptRuleSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReorderReceiptRuleSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReorderReceiptRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery ReorderReceiptRuleSet where
  toQuery ReorderReceiptRuleSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ReorderReceiptRuleSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName,
        "RuleNames" Lude.=: Lude.toQueryList "member" ruleNames
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkReorderReceiptRuleSetResponse' smart constructor.
newtype ReorderReceiptRuleSetResponse = ReorderReceiptRuleSetResponse'
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

-- | Creates a value of 'ReorderReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkReorderReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReorderReceiptRuleSetResponse
mkReorderReceiptRuleSetResponse pResponseStatus_ =
  ReorderReceiptRuleSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsrsResponseStatus :: Lens.Lens' ReorderReceiptRuleSetResponse Lude.Int
rrrsrsResponseStatus = Lens.lens (responseStatus :: ReorderReceiptRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReorderReceiptRuleSetResponse)
{-# DEPRECATED rrrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
