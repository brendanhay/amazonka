{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CloneReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a receipt rule set by cloning an existing one. All receipt rules and configurations are copied to the new receipt rule set and are completely independent of the source rule set.
--
-- For information about setting up rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CloneReceiptRuleSet
  ( -- * Creating a request
    CloneReceiptRuleSet (..),
    mkCloneReceiptRuleSet,

    -- ** Request lenses
    crrsOriginalRuleSetName,
    crrsRuleSetName,

    -- * Destructuring the response
    CloneReceiptRuleSetResponse (..),
    mkCloneReceiptRuleSetResponse,

    -- ** Response lenses
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create a receipt rule set by cloning an existing one. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCloneReceiptRuleSet' smart constructor.
data CloneReceiptRuleSet = CloneReceiptRuleSet'
  { -- | The name of the rule set to clone.
    originalRuleSetName :: Lude.Text,
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
    ruleSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloneReceiptRuleSet' with the minimum fields required to make a request.
--
-- * 'originalRuleSetName' - The name of the rule set to clone.
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
mkCloneReceiptRuleSet ::
  -- | 'originalRuleSetName'
  Lude.Text ->
  -- | 'ruleSetName'
  Lude.Text ->
  CloneReceiptRuleSet
mkCloneReceiptRuleSet pOriginalRuleSetName_ pRuleSetName_ =
  CloneReceiptRuleSet'
    { originalRuleSetName = pOriginalRuleSetName_,
      ruleSetName = pRuleSetName_
    }

-- | The name of the rule set to clone.
--
-- /Note:/ Consider using 'originalRuleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsOriginalRuleSetName :: Lens.Lens' CloneReceiptRuleSet Lude.Text
crrsOriginalRuleSetName = Lens.lens (originalRuleSetName :: CloneReceiptRuleSet -> Lude.Text) (\s a -> s {originalRuleSetName = a} :: CloneReceiptRuleSet)
{-# DEPRECATED crrsOriginalRuleSetName "Use generic-lens or generic-optics with 'originalRuleSetName' instead." #-}

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
crrsRuleSetName :: Lens.Lens' CloneReceiptRuleSet Lude.Text
crrsRuleSetName = Lens.lens (ruleSetName :: CloneReceiptRuleSet -> Lude.Text) (\s a -> s {ruleSetName = a} :: CloneReceiptRuleSet)
{-# DEPRECATED crrsRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

instance Lude.AWSRequest CloneReceiptRuleSet where
  type Rs CloneReceiptRuleSet = CloneReceiptRuleSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CloneReceiptRuleSetResult"
      ( \s h x ->
          CloneReceiptRuleSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CloneReceiptRuleSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CloneReceiptRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CloneReceiptRuleSet where
  toQuery CloneReceiptRuleSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CloneReceiptRuleSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "OriginalRuleSetName" Lude.=: originalRuleSetName,
        "RuleSetName" Lude.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCloneReceiptRuleSetResponse' smart constructor.
newtype CloneReceiptRuleSetResponse = CloneReceiptRuleSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloneReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCloneReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CloneReceiptRuleSetResponse
mkCloneReceiptRuleSetResponse pResponseStatus_ =
  CloneReceiptRuleSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CloneReceiptRuleSetResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CloneReceiptRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CloneReceiptRuleSetResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
