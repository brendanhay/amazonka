{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListQualificationRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationRequests@ operation retrieves requests for Qualifications of a particular Qualification type. The owner of the Qualification type calls this operation to poll for pending requests, and accepts them using the AcceptQualification operation.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationRequests
  ( -- * Creating a request
    ListQualificationRequests (..),
    mkListQualificationRequests,

    -- ** Request lenses
    lqrNextToken,
    lqrQualificationTypeId,
    lqrMaxResults,

    -- * Destructuring the response
    ListQualificationRequestsResponse (..),
    mkListQualificationRequestsResponse,

    -- ** Response lenses
    lqrrsQualificationRequests,
    lqrrsNextToken,
    lqrrsNumResults,
    lqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListQualificationRequests' smart constructor.
data ListQualificationRequests = ListQualificationRequests'
  { nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the QualificationType.
    qualificationTypeId :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQualificationRequests' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'qualificationTypeId' - The ID of the QualificationType.
-- * 'maxResults' - The maximum number of results to return in a single call.
mkListQualificationRequests ::
  ListQualificationRequests
mkListQualificationRequests =
  ListQualificationRequests'
    { nextToken = Lude.Nothing,
      qualificationTypeId = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrNextToken :: Lens.Lens' ListQualificationRequests (Lude.Maybe Lude.Text)
lqrNextToken = Lens.lens (nextToken :: ListQualificationRequests -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQualificationRequests)
{-# DEPRECATED lqrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the QualificationType.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrQualificationTypeId :: Lens.Lens' ListQualificationRequests (Lude.Maybe Lude.Text)
lqrQualificationTypeId = Lens.lens (qualificationTypeId :: ListQualificationRequests -> Lude.Maybe Lude.Text) (\s a -> s {qualificationTypeId = a} :: ListQualificationRequests)
{-# DEPRECATED lqrQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrMaxResults :: Lens.Lens' ListQualificationRequests (Lude.Maybe Lude.Natural)
lqrMaxResults = Lens.lens (maxResults :: ListQualificationRequests -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListQualificationRequests)
{-# DEPRECATED lqrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListQualificationRequests where
  page rq rs
    | Page.stop (rs Lens.^. lqrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lqrrsQualificationRequests) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lqrNextToken Lens..~ rs Lens.^. lqrrsNextToken

instance Lude.AWSRequest ListQualificationRequests where
  type
    Rs ListQualificationRequests =
      ListQualificationRequestsResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListQualificationRequestsResponse'
            Lude.<$> (x Lude..?> "QualificationRequests" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListQualificationRequests where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListQualificationRequests" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListQualificationRequests where
  toJSON ListQualificationRequests' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("QualificationTypeId" Lude..=) Lude.<$> qualificationTypeId,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListQualificationRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery ListQualificationRequests where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListQualificationRequestsResponse' smart constructor.
data ListQualificationRequestsResponse = ListQualificationRequestsResponse'
  { -- | The Qualification request. The response includes one QualificationRequest element for each Qualification request returned by the query.
    qualificationRequests :: Lude.Maybe [QualificationRequest],
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of Qualification requests on this page in the filtered results list, equivalent to the number of Qualification requests being returned by this call.
    numResults :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQualificationRequestsResponse' with the minimum fields required to make a request.
--
-- * 'qualificationRequests' - The Qualification request. The response includes one QualificationRequest element for each Qualification request returned by the query.
-- * 'nextToken' -
-- * 'numResults' - The number of Qualification requests on this page in the filtered results list, equivalent to the number of Qualification requests being returned by this call.
-- * 'responseStatus' - The response status code.
mkListQualificationRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListQualificationRequestsResponse
mkListQualificationRequestsResponse pResponseStatus_ =
  ListQualificationRequestsResponse'
    { qualificationRequests =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Qualification request. The response includes one QualificationRequest element for each Qualification request returned by the query.
--
-- /Note:/ Consider using 'qualificationRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsQualificationRequests :: Lens.Lens' ListQualificationRequestsResponse (Lude.Maybe [QualificationRequest])
lqrrsQualificationRequests = Lens.lens (qualificationRequests :: ListQualificationRequestsResponse -> Lude.Maybe [QualificationRequest]) (\s a -> s {qualificationRequests = a} :: ListQualificationRequestsResponse)
{-# DEPRECATED lqrrsQualificationRequests "Use generic-lens or generic-optics with 'qualificationRequests' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsNextToken :: Lens.Lens' ListQualificationRequestsResponse (Lude.Maybe Lude.Text)
lqrrsNextToken = Lens.lens (nextToken :: ListQualificationRequestsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQualificationRequestsResponse)
{-# DEPRECATED lqrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of Qualification requests on this page in the filtered results list, equivalent to the number of Qualification requests being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsNumResults :: Lens.Lens' ListQualificationRequestsResponse (Lude.Maybe Lude.Int)
lqrrsNumResults = Lens.lens (numResults :: ListQualificationRequestsResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListQualificationRequestsResponse)
{-# DEPRECATED lqrrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsResponseStatus :: Lens.Lens' ListQualificationRequestsResponse Lude.Int
lqrrsResponseStatus = Lens.lens (responseStatus :: ListQualificationRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListQualificationRequestsResponse)
{-# DEPRECATED lqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
