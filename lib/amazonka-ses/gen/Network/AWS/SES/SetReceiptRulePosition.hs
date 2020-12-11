{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetReceiptRulePosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the position of the specified receipt rule in the receipt rule set.
--
-- For information about managing receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SetReceiptRulePosition
  ( -- * Creating a request
    SetReceiptRulePosition (..),
    mkSetReceiptRulePosition,

    -- ** Request lenses
    srrpAfter,
    srrpRuleSetName,
    srrpRuleName,

    -- * Destructuring the response
    SetReceiptRulePositionResponse (..),
    mkSetReceiptRulePositionResponse,

    -- ** Response lenses
    srrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to set the position of a receipt rule in a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetReceiptRulePosition' smart constructor.
data SetReceiptRulePosition = SetReceiptRulePosition'
  { after ::
      Lude.Maybe Lude.Text,
    ruleSetName :: Lude.Text,
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

-- | Creates a value of 'SetReceiptRulePosition' with the minimum fields required to make a request.
--
-- * 'after' - The name of the receipt rule after which to place the specified receipt rule.
-- * 'ruleName' - The name of the receipt rule to reposition.
-- * 'ruleSetName' - The name of the receipt rule set that contains the receipt rule to reposition.
mkSetReceiptRulePosition ::
  -- | 'ruleSetName'
  Lude.Text ->
  -- | 'ruleName'
  Lude.Text ->
  SetReceiptRulePosition
mkSetReceiptRulePosition pRuleSetName_ pRuleName_ =
  SetReceiptRulePosition'
    { after = Lude.Nothing,
      ruleSetName = pRuleSetName_,
      ruleName = pRuleName_
    }

-- | The name of the receipt rule after which to place the specified receipt rule.
--
-- /Note:/ Consider using 'after' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpAfter :: Lens.Lens' SetReceiptRulePosition (Lude.Maybe Lude.Text)
srrpAfter = Lens.lens (after :: SetReceiptRulePosition -> Lude.Maybe Lude.Text) (\s a -> s {after = a} :: SetReceiptRulePosition)
{-# DEPRECATED srrpAfter "Use generic-lens or generic-optics with 'after' instead." #-}

-- | The name of the receipt rule set that contains the receipt rule to reposition.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpRuleSetName :: Lens.Lens' SetReceiptRulePosition Lude.Text
srrpRuleSetName = Lens.lens (ruleSetName :: SetReceiptRulePosition -> Lude.Text) (\s a -> s {ruleSetName = a} :: SetReceiptRulePosition)
{-# DEPRECATED srrpRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The name of the receipt rule to reposition.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpRuleName :: Lens.Lens' SetReceiptRulePosition Lude.Text
srrpRuleName = Lens.lens (ruleName :: SetReceiptRulePosition -> Lude.Text) (\s a -> s {ruleName = a} :: SetReceiptRulePosition)
{-# DEPRECATED srrpRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Lude.AWSRequest SetReceiptRulePosition where
  type Rs SetReceiptRulePosition = SetReceiptRulePositionResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SetReceiptRulePositionResult"
      ( \s h x ->
          SetReceiptRulePositionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetReceiptRulePosition where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetReceiptRulePosition where
  toPath = Lude.const "/"

instance Lude.ToQuery SetReceiptRulePosition where
  toQuery SetReceiptRulePosition' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetReceiptRulePosition" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "After" Lude.=: after,
        "RuleSetName" Lude.=: ruleSetName,
        "RuleName" Lude.=: ruleName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetReceiptRulePositionResponse' smart constructor.
newtype SetReceiptRulePositionResponse = SetReceiptRulePositionResponse'
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

-- | Creates a value of 'SetReceiptRulePositionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetReceiptRulePositionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetReceiptRulePositionResponse
mkSetReceiptRulePositionResponse pResponseStatus_ =
  SetReceiptRulePositionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrprsResponseStatus :: Lens.Lens' SetReceiptRulePositionResponse Lude.Int
srrprsResponseStatus = Lens.lens (responseStatus :: SetReceiptRulePositionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetReceiptRulePositionResponse)
{-# DEPRECATED srrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
