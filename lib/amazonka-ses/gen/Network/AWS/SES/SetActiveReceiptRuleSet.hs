{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetActiveReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified receipt rule set as the active receipt rule set.
--
-- For information about managing receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SetActiveReceiptRuleSet
  ( -- * Creating a request
    SetActiveReceiptRuleSet (..),
    mkSetActiveReceiptRuleSet,

    -- ** Request lenses
    sarrsRuleSetName,

    -- * Destructuring the response
    SetActiveReceiptRuleSetResponse (..),
    mkSetActiveReceiptRuleSetResponse,

    -- ** Response lenses
    sarrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to set a receipt rule set as the active receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetActiveReceiptRuleSet' smart constructor.
newtype SetActiveReceiptRuleSet = SetActiveReceiptRuleSet'
  { -- | The name of the receipt rule set to make active. Setting this value to null disables all email receiving.
    ruleSetName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetActiveReceiptRuleSet' with the minimum fields required to make a request.
--
-- * 'ruleSetName' - The name of the receipt rule set to make active. Setting this value to null disables all email receiving.
mkSetActiveReceiptRuleSet ::
  SetActiveReceiptRuleSet
mkSetActiveReceiptRuleSet =
  SetActiveReceiptRuleSet' {ruleSetName = Lude.Nothing}

-- | The name of the receipt rule set to make active. Setting this value to null disables all email receiving.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsRuleSetName :: Lens.Lens' SetActiveReceiptRuleSet (Lude.Maybe Lude.Text)
sarrsRuleSetName = Lens.lens (ruleSetName :: SetActiveReceiptRuleSet -> Lude.Maybe Lude.Text) (\s a -> s {ruleSetName = a} :: SetActiveReceiptRuleSet)
{-# DEPRECATED sarrsRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

instance Lude.AWSRequest SetActiveReceiptRuleSet where
  type Rs SetActiveReceiptRuleSet = SetActiveReceiptRuleSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SetActiveReceiptRuleSetResult"
      ( \s h x ->
          SetActiveReceiptRuleSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetActiveReceiptRuleSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetActiveReceiptRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery SetActiveReceiptRuleSet where
  toQuery SetActiveReceiptRuleSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetActiveReceiptRuleSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetActiveReceiptRuleSetResponse' smart constructor.
newtype SetActiveReceiptRuleSetResponse = SetActiveReceiptRuleSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetActiveReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetActiveReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetActiveReceiptRuleSetResponse
mkSetActiveReceiptRuleSetResponse pResponseStatus_ =
  SetActiveReceiptRuleSetResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsrsResponseStatus :: Lens.Lens' SetActiveReceiptRuleSetResponse Lude.Int
sarrsrsResponseStatus = Lens.lens (responseStatus :: SetActiveReceiptRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetActiveReceiptRuleSetResponse)
{-# DEPRECATED sarrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
