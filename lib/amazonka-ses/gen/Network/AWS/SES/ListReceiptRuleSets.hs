{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListReceiptRuleSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the receipt rule sets that exist under your AWS account in the current AWS Region. If there are additional receipt rule sets to be retrieved, you will receive a @NextToken@ that you can provide to the next call to @ListReceiptRuleSets@ to retrieve the additional entries.
--
-- For information about managing receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListReceiptRuleSets
  ( -- * Creating a request
    ListReceiptRuleSets (..),
    mkListReceiptRuleSets,

    -- ** Request lenses
    lrrsNextToken,

    -- * Destructuring the response
    ListReceiptRuleSetsResponse (..),
    mkListReceiptRuleSetsResponse,

    -- ** Response lenses
    lrrsrsRuleSets,
    lrrsrsNextToken,
    lrrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to list the receipt rule sets that exist under your AWS account. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListReceiptRuleSets' smart constructor.
newtype ListReceiptRuleSets = ListReceiptRuleSets'
  { nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReceiptRuleSets' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token returned from a previous call to @ListReceiptRuleSets@ to indicate the position in the receipt rule set list.
mkListReceiptRuleSets ::
  ListReceiptRuleSets
mkListReceiptRuleSets =
  ListReceiptRuleSets' {nextToken = Lude.Nothing}

-- | A token returned from a previous call to @ListReceiptRuleSets@ to indicate the position in the receipt rule set list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListReceiptRuleSets (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListReceiptRuleSets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReceiptRuleSets)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListReceiptRuleSets where
  page rq rs
    | Page.stop (rs Lens.^. lrrsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsrsRuleSets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrrsNextToken Lens..~ rs Lens.^. lrrsrsNextToken

instance Lude.AWSRequest ListReceiptRuleSets where
  type Rs ListReceiptRuleSets = ListReceiptRuleSetsResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListReceiptRuleSetsResult"
      ( \s h x ->
          ListReceiptRuleSetsResponse'
            Lude.<$> ( x Lude..@? "RuleSets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReceiptRuleSets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListReceiptRuleSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReceiptRuleSets where
  toQuery ListReceiptRuleSets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListReceiptRuleSets" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken
      ]

-- | A list of receipt rule sets that exist under your AWS account.
--
-- /See:/ 'mkListReceiptRuleSetsResponse' smart constructor.
data ListReceiptRuleSetsResponse = ListReceiptRuleSetsResponse'
  { ruleSets ::
      Lude.Maybe [ReceiptRuleSetMetadata],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListReceiptRuleSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token indicating that there are additional receipt rule sets available to be listed. Pass this token to successive calls of @ListReceiptRuleSets@ to retrieve up to 100 receipt rule sets at a time.
-- * 'responseStatus' - The response status code.
-- * 'ruleSets' - The metadata for the currently active receipt rule set. The metadata consists of the rule set name and the timestamp of when the rule set was created.
mkListReceiptRuleSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReceiptRuleSetsResponse
mkListReceiptRuleSetsResponse pResponseStatus_ =
  ListReceiptRuleSetsResponse'
    { ruleSets = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The metadata for the currently active receipt rule set. The metadata consists of the rule set name and the timestamp of when the rule set was created.
--
-- /Note:/ Consider using 'ruleSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsRuleSets :: Lens.Lens' ListReceiptRuleSetsResponse (Lude.Maybe [ReceiptRuleSetMetadata])
lrrsrsRuleSets = Lens.lens (ruleSets :: ListReceiptRuleSetsResponse -> Lude.Maybe [ReceiptRuleSetMetadata]) (\s a -> s {ruleSets = a} :: ListReceiptRuleSetsResponse)
{-# DEPRECATED lrrsrsRuleSets "Use generic-lens or generic-optics with 'ruleSets' instead." #-}

-- | A token indicating that there are additional receipt rule sets available to be listed. Pass this token to successive calls of @ListReceiptRuleSets@ to retrieve up to 100 receipt rule sets at a time.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsNextToken :: Lens.Lens' ListReceiptRuleSetsResponse (Lude.Maybe Lude.Text)
lrrsrsNextToken = Lens.lens (nextToken :: ListReceiptRuleSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReceiptRuleSetsResponse)
{-# DEPRECATED lrrsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsResponseStatus :: Lens.Lens' ListReceiptRuleSetsResponse Lude.Int
lrrsrsResponseStatus = Lens.lens (responseStatus :: ListReceiptRuleSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReceiptRuleSetsResponse)
{-# DEPRECATED lrrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
