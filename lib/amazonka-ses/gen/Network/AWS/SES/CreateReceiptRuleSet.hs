{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty receipt rule set.
--
-- For information about setting up receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateReceiptRuleSet
  ( -- * Creating a request
    CreateReceiptRuleSet (..),
    mkCreateReceiptRuleSet,

    -- ** Request lenses
    crrsRuleSetName,

    -- * Destructuring the response
    CreateReceiptRuleSetResponse (..),
    mkCreateReceiptRuleSetResponse,

    -- ** Response lenses
    crrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create an empty receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateReceiptRuleSet' smart constructor.
newtype CreateReceiptRuleSet = CreateReceiptRuleSet'
  { ruleSetName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReceiptRuleSet' with the minimum fields required to make a request.
--
-- * 'ruleSetName' - The name of the rule set to create. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
mkCreateReceiptRuleSet ::
  -- | 'ruleSetName'
  Lude.Text ->
  CreateReceiptRuleSet
mkCreateReceiptRuleSet pRuleSetName_ =
  CreateReceiptRuleSet' {ruleSetName = pRuleSetName_}

-- | The name of the rule set to create. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
--
--
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRuleSetName :: Lens.Lens' CreateReceiptRuleSet Lude.Text
crrsRuleSetName = Lens.lens (ruleSetName :: CreateReceiptRuleSet -> Lude.Text) (\s a -> s {ruleSetName = a} :: CreateReceiptRuleSet)
{-# DEPRECATED crrsRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

instance Lude.AWSRequest CreateReceiptRuleSet where
  type Rs CreateReceiptRuleSet = CreateReceiptRuleSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CreateReceiptRuleSetResult"
      ( \s h x ->
          CreateReceiptRuleSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReceiptRuleSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateReceiptRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReceiptRuleSet where
  toQuery CreateReceiptRuleSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateReceiptRuleSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateReceiptRuleSetResponse' smart constructor.
newtype CreateReceiptRuleSetResponse = CreateReceiptRuleSetResponse'
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

-- | Creates a value of 'CreateReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReceiptRuleSetResponse
mkCreateReceiptRuleSetResponse pResponseStatus_ =
  CreateReceiptRuleSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsrsResponseStatus :: Lens.Lens' CreateReceiptRuleSetResponse Lude.Int
crrsrsResponseStatus = Lens.lens (responseStatus :: CreateReceiptRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReceiptRuleSetResponse)
{-# DEPRECATED crrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
