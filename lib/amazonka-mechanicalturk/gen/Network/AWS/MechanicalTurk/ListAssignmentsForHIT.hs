{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListAssignmentsForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListAssignmentsForHIT@ operation retrieves completed assignments for a HIT. You can use this operation to retrieve the results for a HIT.
--
-- You can get assignments for a HIT at any time, even if the HIT is not yet Reviewable. If a HIT requested multiple assignments, and has received some results but has not yet become Reviewable, you can still retrieve the partial results with this operation.
-- Use the AssignmentStatus parameter to control which set of assignments for a HIT are returned. The ListAssignmentsForHIT operation can return submitted assignments awaiting approval, or it can return assignments that have already been approved or rejected. You can set AssignmentStatus=Approved,Rejected to get assignments that have already been approved and rejected together in one result set.
-- Only the Requester who created the HIT can retrieve the assignments for that HIT.
-- Results are sorted and divided into numbered pages and the operation returns a single page of results. You can use the parameters of the operation to control sorting and pagination.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListAssignmentsForHIT
  ( -- * Creating a request
    ListAssignmentsForHIT (..),
    mkListAssignmentsForHIT,

    -- ** Request lenses
    lafhitAssignmentStatuses,
    lafhitNextToken,
    lafhitHITId,
    lafhitMaxResults,

    -- * Destructuring the response
    ListAssignmentsForHITResponse (..),
    mkListAssignmentsForHITResponse,

    -- ** Response lenses
    lafhitrsNextToken,
    lafhitrsNumResults,
    lafhitrsAssignments,
    lafhitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssignmentsForHIT' smart constructor.
data ListAssignmentsForHIT = ListAssignmentsForHIT'
  { -- | The status of the assignments to return: Submitted | Approved | Rejected
    assignmentStatuses :: Lude.Maybe [AssignmentStatus],
    -- | Pagination token
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the HIT.
    hITId :: Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssignmentsForHIT' with the minimum fields required to make a request.
--
-- * 'assignmentStatuses' - The status of the assignments to return: Submitted | Approved | Rejected
-- * 'nextToken' - Pagination token
-- * 'hITId' - The ID of the HIT.
-- * 'maxResults' -
mkListAssignmentsForHIT ::
  -- | 'hITId'
  Lude.Text ->
  ListAssignmentsForHIT
mkListAssignmentsForHIT pHITId_ =
  ListAssignmentsForHIT'
    { assignmentStatuses = Lude.Nothing,
      nextToken = Lude.Nothing,
      hITId = pHITId_,
      maxResults = Lude.Nothing
    }

-- | The status of the assignments to return: Submitted | Approved | Rejected
--
-- /Note:/ Consider using 'assignmentStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitAssignmentStatuses :: Lens.Lens' ListAssignmentsForHIT (Lude.Maybe [AssignmentStatus])
lafhitAssignmentStatuses = Lens.lens (assignmentStatuses :: ListAssignmentsForHIT -> Lude.Maybe [AssignmentStatus]) (\s a -> s {assignmentStatuses = a} :: ListAssignmentsForHIT)
{-# DEPRECATED lafhitAssignmentStatuses "Use generic-lens or generic-optics with 'assignmentStatuses' instead." #-}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitNextToken :: Lens.Lens' ListAssignmentsForHIT (Lude.Maybe Lude.Text)
lafhitNextToken = Lens.lens (nextToken :: ListAssignmentsForHIT -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssignmentsForHIT)
{-# DEPRECATED lafhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the HIT.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitHITId :: Lens.Lens' ListAssignmentsForHIT Lude.Text
lafhitHITId = Lens.lens (hITId :: ListAssignmentsForHIT -> Lude.Text) (\s a -> s {hITId = a} :: ListAssignmentsForHIT)
{-# DEPRECATED lafhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitMaxResults :: Lens.Lens' ListAssignmentsForHIT (Lude.Maybe Lude.Natural)
lafhitMaxResults = Lens.lens (maxResults :: ListAssignmentsForHIT -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAssignmentsForHIT)
{-# DEPRECATED lafhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAssignmentsForHIT where
  page rq rs
    | Page.stop (rs Lens.^. lafhitrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lafhitrsAssignments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lafhitNextToken Lens..~ rs Lens.^. lafhitrsNextToken

instance Lude.AWSRequest ListAssignmentsForHIT where
  type Rs ListAssignmentsForHIT = ListAssignmentsForHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssignmentsForHITResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (x Lude..?> "Assignments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssignmentsForHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListAssignmentsForHIT" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssignmentsForHIT where
  toJSON ListAssignmentsForHIT' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AssignmentStatuses" Lude..=) Lude.<$> assignmentStatuses,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("HITId" Lude..= hITId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAssignmentsForHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssignmentsForHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssignmentsForHITResponse' smart constructor.
data ListAssignmentsForHITResponse = ListAssignmentsForHITResponse'
  { nextToken :: Lude.Maybe Lude.Text,
    -- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
    numResults :: Lude.Maybe Lude.Int,
    -- | The collection of Assignment data structures returned by this call.
    assignments :: Lude.Maybe [Assignment],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssignmentsForHITResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'numResults' - The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
-- * 'assignments' - The collection of Assignment data structures returned by this call.
-- * 'responseStatus' - The response status code.
mkListAssignmentsForHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssignmentsForHITResponse
mkListAssignmentsForHITResponse pResponseStatus_ =
  ListAssignmentsForHITResponse'
    { nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      assignments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrsNextToken :: Lens.Lens' ListAssignmentsForHITResponse (Lude.Maybe Lude.Text)
lafhitrsNextToken = Lens.lens (nextToken :: ListAssignmentsForHITResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssignmentsForHITResponse)
{-# DEPRECATED lafhitrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrsNumResults :: Lens.Lens' ListAssignmentsForHITResponse (Lude.Maybe Lude.Int)
lafhitrsNumResults = Lens.lens (numResults :: ListAssignmentsForHITResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListAssignmentsForHITResponse)
{-# DEPRECATED lafhitrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The collection of Assignment data structures returned by this call.
--
-- /Note:/ Consider using 'assignments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrsAssignments :: Lens.Lens' ListAssignmentsForHITResponse (Lude.Maybe [Assignment])
lafhitrsAssignments = Lens.lens (assignments :: ListAssignmentsForHITResponse -> Lude.Maybe [Assignment]) (\s a -> s {assignments = a} :: ListAssignmentsForHITResponse)
{-# DEPRECATED lafhitrsAssignments "Use generic-lens or generic-optics with 'assignments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrsResponseStatus :: Lens.Lens' ListAssignmentsForHITResponse Lude.Int
lafhitrsResponseStatus = Lens.lens (responseStatus :: ListAssignmentsForHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssignmentsForHITResponse)
{-# DEPRECATED lafhitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
