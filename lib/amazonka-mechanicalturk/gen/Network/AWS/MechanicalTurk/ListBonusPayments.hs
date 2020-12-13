{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListBonusPayments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListBonusPayments@ operation retrieves the amounts of bonuses you have paid to Workers for a given HIT or assignment.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListBonusPayments
  ( -- * Creating a request
    ListBonusPayments (..),
    mkListBonusPayments,

    -- ** Request lenses
    lbpNextToken,
    lbpHITId,
    lbpAssignmentId,
    lbpMaxResults,

    -- * Destructuring the response
    ListBonusPaymentsResponse (..),
    mkListBonusPaymentsResponse,

    -- ** Response lenses
    lbprsBonusPayments,
    lbprsNextToken,
    lbprsNumResults,
    lbprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBonusPayments' smart constructor.
data ListBonusPayments = ListBonusPayments'
  { -- | Pagination token
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the HIT associated with the bonus payments to retrieve. If not specified, all bonus payments for all assignments for the given HIT are returned. Either the HITId parameter or the AssignmentId parameter must be specified
    hITId :: Lude.Maybe Lude.Text,
    -- | The ID of the assignment associated with the bonus payments to retrieve. If specified, only bonus payments for the given assignment are returned. Either the HITId parameter or the AssignmentId parameter must be specified
    assignmentId :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBonusPayments' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token
-- * 'hITId' - The ID of the HIT associated with the bonus payments to retrieve. If not specified, all bonus payments for all assignments for the given HIT are returned. Either the HITId parameter or the AssignmentId parameter must be specified
-- * 'assignmentId' - The ID of the assignment associated with the bonus payments to retrieve. If specified, only bonus payments for the given assignment are returned. Either the HITId parameter or the AssignmentId parameter must be specified
-- * 'maxResults' -
mkListBonusPayments ::
  ListBonusPayments
mkListBonusPayments =
  ListBonusPayments'
    { nextToken = Lude.Nothing,
      hITId = Lude.Nothing,
      assignmentId = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpNextToken :: Lens.Lens' ListBonusPayments (Lude.Maybe Lude.Text)
lbpNextToken = Lens.lens (nextToken :: ListBonusPayments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBonusPayments)
{-# DEPRECATED lbpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the HIT associated with the bonus payments to retrieve. If not specified, all bonus payments for all assignments for the given HIT are returned. Either the HITId parameter or the AssignmentId parameter must be specified
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpHITId :: Lens.Lens' ListBonusPayments (Lude.Maybe Lude.Text)
lbpHITId = Lens.lens (hITId :: ListBonusPayments -> Lude.Maybe Lude.Text) (\s a -> s {hITId = a} :: ListBonusPayments)
{-# DEPRECATED lbpHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The ID of the assignment associated with the bonus payments to retrieve. If specified, only bonus payments for the given assignment are returned. Either the HITId parameter or the AssignmentId parameter must be specified
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpAssignmentId :: Lens.Lens' ListBonusPayments (Lude.Maybe Lude.Text)
lbpAssignmentId = Lens.lens (assignmentId :: ListBonusPayments -> Lude.Maybe Lude.Text) (\s a -> s {assignmentId = a} :: ListBonusPayments)
{-# DEPRECATED lbpAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpMaxResults :: Lens.Lens' ListBonusPayments (Lude.Maybe Lude.Natural)
lbpMaxResults = Lens.lens (maxResults :: ListBonusPayments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListBonusPayments)
{-# DEPRECATED lbpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBonusPayments where
  page rq rs
    | Page.stop (rs Lens.^. lbprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbprsBonusPayments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbpNextToken Lens..~ rs Lens.^. lbprsNextToken

instance Lude.AWSRequest ListBonusPayments where
  type Rs ListBonusPayments = ListBonusPaymentsResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBonusPaymentsResponse'
            Lude.<$> (x Lude..?> "BonusPayments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBonusPayments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListBonusPayments" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBonusPayments where
  toJSON ListBonusPayments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("HITId" Lude..=) Lude.<$> hITId,
            ("AssignmentId" Lude..=) Lude.<$> assignmentId,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListBonusPayments where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBonusPayments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBonusPaymentsResponse' smart constructor.
data ListBonusPaymentsResponse = ListBonusPaymentsResponse'
  { -- | A successful request to the ListBonusPayments operation returns a list of BonusPayment objects.
    bonusPayments :: Lude.Maybe [BonusPayment],
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of bonus payments on this page in the filtered results list, equivalent to the number of bonus payments being returned by this call.
    numResults :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBonusPaymentsResponse' with the minimum fields required to make a request.
--
-- * 'bonusPayments' - A successful request to the ListBonusPayments operation returns a list of BonusPayment objects.
-- * 'nextToken' -
-- * 'numResults' - The number of bonus payments on this page in the filtered results list, equivalent to the number of bonus payments being returned by this call.
-- * 'responseStatus' - The response status code.
mkListBonusPaymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBonusPaymentsResponse
mkListBonusPaymentsResponse pResponseStatus_ =
  ListBonusPaymentsResponse'
    { bonusPayments = Lude.Nothing,
      nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A successful request to the ListBonusPayments operation returns a list of BonusPayment objects.
--
-- /Note:/ Consider using 'bonusPayments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprsBonusPayments :: Lens.Lens' ListBonusPaymentsResponse (Lude.Maybe [BonusPayment])
lbprsBonusPayments = Lens.lens (bonusPayments :: ListBonusPaymentsResponse -> Lude.Maybe [BonusPayment]) (\s a -> s {bonusPayments = a} :: ListBonusPaymentsResponse)
{-# DEPRECATED lbprsBonusPayments "Use generic-lens or generic-optics with 'bonusPayments' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprsNextToken :: Lens.Lens' ListBonusPaymentsResponse (Lude.Maybe Lude.Text)
lbprsNextToken = Lens.lens (nextToken :: ListBonusPaymentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBonusPaymentsResponse)
{-# DEPRECATED lbprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of bonus payments on this page in the filtered results list, equivalent to the number of bonus payments being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprsNumResults :: Lens.Lens' ListBonusPaymentsResponse (Lude.Maybe Lude.Int)
lbprsNumResults = Lens.lens (numResults :: ListBonusPaymentsResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListBonusPaymentsResponse)
{-# DEPRECATED lbprsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprsResponseStatus :: Lens.Lens' ListBonusPaymentsResponse Lude.Int
lbprsResponseStatus = Lens.lens (responseStatus :: ListBonusPaymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBonusPaymentsResponse)
{-# DEPRECATED lbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
