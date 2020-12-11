{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified receipt rule set.
--
-- For information about managing receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeReceiptRuleSet
  ( -- * Creating a request
    DescribeReceiptRuleSet (..),
    mkDescribeReceiptRuleSet,

    -- ** Request lenses
    drrsRuleSetName,

    -- * Destructuring the response
    DescribeReceiptRuleSetResponse (..),
    mkDescribeReceiptRuleSetResponse,

    -- ** Response lenses
    desrsRules,
    desrsMetadata,
    desrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the details of a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeReceiptRuleSet' smart constructor.
newtype DescribeReceiptRuleSet = DescribeReceiptRuleSet'
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

-- | Creates a value of 'DescribeReceiptRuleSet' with the minimum fields required to make a request.
--
-- * 'ruleSetName' - The name of the receipt rule set to describe.
mkDescribeReceiptRuleSet ::
  -- | 'ruleSetName'
  Lude.Text ->
  DescribeReceiptRuleSet
mkDescribeReceiptRuleSet pRuleSetName_ =
  DescribeReceiptRuleSet' {ruleSetName = pRuleSetName_}

-- | The name of the receipt rule set to describe.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRuleSetName :: Lens.Lens' DescribeReceiptRuleSet Lude.Text
drrsRuleSetName = Lens.lens (ruleSetName :: DescribeReceiptRuleSet -> Lude.Text) (\s a -> s {ruleSetName = a} :: DescribeReceiptRuleSet)
{-# DEPRECATED drrsRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

instance Lude.AWSRequest DescribeReceiptRuleSet where
  type Rs DescribeReceiptRuleSet = DescribeReceiptRuleSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DescribeReceiptRuleSetResult"
      ( \s h x ->
          DescribeReceiptRuleSetResponse'
            Lude.<$> ( x Lude..@? "Rules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReceiptRuleSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReceiptRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReceiptRuleSet where
  toQuery DescribeReceiptRuleSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeReceiptRuleSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName
      ]

-- | Represents the details of the specified receipt rule set.
--
-- /See:/ 'mkDescribeReceiptRuleSetResponse' smart constructor.
data DescribeReceiptRuleSetResponse = DescribeReceiptRuleSetResponse'
  { rules ::
      Lude.Maybe [ReceiptRule],
    metadata ::
      Lude.Maybe
        ReceiptRuleSetMetadata,
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

-- | Creates a value of 'DescribeReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'metadata' - The metadata for the receipt rule set, which consists of the rule set name and the timestamp of when the rule set was created.
-- * 'responseStatus' - The response status code.
-- * 'rules' - A list of the receipt rules that belong to the specified receipt rule set.
mkDescribeReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReceiptRuleSetResponse
mkDescribeReceiptRuleSetResponse pResponseStatus_ =
  DescribeReceiptRuleSetResponse'
    { rules = Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the receipt rules that belong to the specified receipt rule set.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsRules :: Lens.Lens' DescribeReceiptRuleSetResponse (Lude.Maybe [ReceiptRule])
desrsRules = Lens.lens (rules :: DescribeReceiptRuleSetResponse -> Lude.Maybe [ReceiptRule]) (\s a -> s {rules = a} :: DescribeReceiptRuleSetResponse)
{-# DEPRECATED desrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The metadata for the receipt rule set, which consists of the rule set name and the timestamp of when the rule set was created.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsMetadata :: Lens.Lens' DescribeReceiptRuleSetResponse (Lude.Maybe ReceiptRuleSetMetadata)
desrsMetadata = Lens.lens (metadata :: DescribeReceiptRuleSetResponse -> Lude.Maybe ReceiptRuleSetMetadata) (\s a -> s {metadata = a} :: DescribeReceiptRuleSetResponse)
{-# DEPRECATED desrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeReceiptRuleSetResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeReceiptRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReceiptRuleSetResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
