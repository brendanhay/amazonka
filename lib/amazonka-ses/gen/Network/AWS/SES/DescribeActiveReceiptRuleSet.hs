{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeActiveReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata and receipt rules for the receipt rule set that is currently active.
--
-- For information about setting up receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeActiveReceiptRuleSet
  ( -- * Creating a request
    DescribeActiveReceiptRuleSet (..),
    mkDescribeActiveReceiptRuleSet,

    -- * Destructuring the response
    DescribeActiveReceiptRuleSetResponse (..),
    mkDescribeActiveReceiptRuleSetResponse,

    -- ** Response lenses
    darrsrsRules,
    darrsrsMetadata,
    darrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the metadata and receipt rules for the receipt rule set that is currently active. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeActiveReceiptRuleSet' smart constructor.
data DescribeActiveReceiptRuleSet = DescribeActiveReceiptRuleSet'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActiveReceiptRuleSet' with the minimum fields required to make a request.
mkDescribeActiveReceiptRuleSet ::
  DescribeActiveReceiptRuleSet
mkDescribeActiveReceiptRuleSet = DescribeActiveReceiptRuleSet'

instance Lude.AWSRequest DescribeActiveReceiptRuleSet where
  type
    Rs DescribeActiveReceiptRuleSet =
      DescribeActiveReceiptRuleSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DescribeActiveReceiptRuleSetResult"
      ( \s h x ->
          DescribeActiveReceiptRuleSetResponse'
            Lude.<$> ( x Lude..@? "Rules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeActiveReceiptRuleSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeActiveReceiptRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeActiveReceiptRuleSet where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("DescribeActiveReceiptRuleSet" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | Represents the metadata and receipt rules for the receipt rule set that is currently active.
--
-- /See:/ 'mkDescribeActiveReceiptRuleSetResponse' smart constructor.
data DescribeActiveReceiptRuleSetResponse = DescribeActiveReceiptRuleSetResponse'
  { -- | The receipt rules that belong to the active rule set.
    rules :: Lude.Maybe [ReceiptRule],
    -- | The metadata for the currently active receipt rule set. The metadata consists of the rule set name and a timestamp of when the rule set was created.
    metadata :: Lude.Maybe ReceiptRuleSetMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActiveReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'rules' - The receipt rules that belong to the active rule set.
-- * 'metadata' - The metadata for the currently active receipt rule set. The metadata consists of the rule set name and a timestamp of when the rule set was created.
-- * 'responseStatus' - The response status code.
mkDescribeActiveReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeActiveReceiptRuleSetResponse
mkDescribeActiveReceiptRuleSetResponse pResponseStatus_ =
  DescribeActiveReceiptRuleSetResponse'
    { rules = Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The receipt rules that belong to the active rule set.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsrsRules :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Lude.Maybe [ReceiptRule])
darrsrsRules = Lens.lens (rules :: DescribeActiveReceiptRuleSetResponse -> Lude.Maybe [ReceiptRule]) (\s a -> s {rules = a} :: DescribeActiveReceiptRuleSetResponse)
{-# DEPRECATED darrsrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The metadata for the currently active receipt rule set. The metadata consists of the rule set name and a timestamp of when the rule set was created.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsrsMetadata :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Lude.Maybe ReceiptRuleSetMetadata)
darrsrsMetadata = Lens.lens (metadata :: DescribeActiveReceiptRuleSetResponse -> Lude.Maybe ReceiptRuleSetMetadata) (\s a -> s {metadata = a} :: DescribeActiveReceiptRuleSetResponse)
{-# DEPRECATED darrsrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsrsResponseStatus :: Lens.Lens' DescribeActiveReceiptRuleSetResponse Lude.Int
darrsrsResponseStatus = Lens.lens (responseStatus :: DescribeActiveReceiptRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeActiveReceiptRuleSetResponse)
{-# DEPRECATED darrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
