{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListOfferingTransactions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all historical purchases, renewals, and system renewal transactions for an AWS account. The list is paginated and ordered by a descending timestamp (most recent transactions are first). The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferingTransactions
  ( -- * Creating a request
    ListOfferingTransactions (..),
    mkListOfferingTransactions,

    -- ** Request lenses
    lotNextToken,

    -- * Destructuring the response
    ListOfferingTransactionsResponse (..),
    mkListOfferingTransactionsResponse,

    -- ** Response lenses
    lotrsOfferingTransactions,
    lotrsNextToken,
    lotrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list the offering transaction history.
--
-- /See:/ 'mkListOfferingTransactions' smart constructor.
newtype ListOfferingTransactions = ListOfferingTransactions'
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

-- | Creates a value of 'ListOfferingTransactions' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListOfferingTransactions ::
  ListOfferingTransactions
mkListOfferingTransactions =
  ListOfferingTransactions' {nextToken = Lude.Nothing}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotNextToken :: Lens.Lens' ListOfferingTransactions (Lude.Maybe Lude.Text)
lotNextToken = Lens.lens (nextToken :: ListOfferingTransactions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferingTransactions)
{-# DEPRECATED lotNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListOfferingTransactions where
  page rq rs
    | Page.stop (rs Lens.^. lotrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lotrsOfferingTransactions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lotNextToken Lens..~ rs Lens.^. lotrsNextToken

instance Lude.AWSRequest ListOfferingTransactions where
  type Rs ListOfferingTransactions = ListOfferingTransactionsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOfferingTransactionsResponse'
            Lude.<$> (x Lude..?> "offeringTransactions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOfferingTransactions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DeviceFarm_20150623.ListOfferingTransactions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOfferingTransactions where
  toJSON ListOfferingTransactions' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListOfferingTransactions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOfferingTransactions where
  toQuery = Lude.const Lude.mempty

-- | Returns the transaction log of the specified offerings.
--
-- /See:/ 'mkListOfferingTransactionsResponse' smart constructor.
data ListOfferingTransactionsResponse = ListOfferingTransactionsResponse'
  { offeringTransactions ::
      Lude.Maybe
        [OfferingTransaction],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOfferingTransactionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'offeringTransactions' - The audit log of subscriptions you have purchased and modified through AWS Device Farm.
-- * 'responseStatus' - The response status code.
mkListOfferingTransactionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOfferingTransactionsResponse
mkListOfferingTransactionsResponse pResponseStatus_ =
  ListOfferingTransactionsResponse'
    { offeringTransactions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The audit log of subscriptions you have purchased and modified through AWS Device Farm.
--
-- /Note:/ Consider using 'offeringTransactions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotrsOfferingTransactions :: Lens.Lens' ListOfferingTransactionsResponse (Lude.Maybe [OfferingTransaction])
lotrsOfferingTransactions = Lens.lens (offeringTransactions :: ListOfferingTransactionsResponse -> Lude.Maybe [OfferingTransaction]) (\s a -> s {offeringTransactions = a} :: ListOfferingTransactionsResponse)
{-# DEPRECATED lotrsOfferingTransactions "Use generic-lens or generic-optics with 'offeringTransactions' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotrsNextToken :: Lens.Lens' ListOfferingTransactionsResponse (Lude.Maybe Lude.Text)
lotrsNextToken = Lens.lens (nextToken :: ListOfferingTransactionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferingTransactionsResponse)
{-# DEPRECATED lotrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotrsResponseStatus :: Lens.Lens' ListOfferingTransactionsResponse Lude.Int
lotrsResponseStatus = Lens.lens (responseStatus :: ListOfferingTransactionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOfferingTransactionsResponse)
{-# DEPRECATED lotrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
