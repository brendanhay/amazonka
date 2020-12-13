{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListReviewableHITs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewableHITs@ operation retrieves the HITs with Status equal to Reviewable or Status equal to Reviewing that belong to the Requester calling the operation.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListReviewableHITs
  ( -- * Creating a request
    ListReviewableHITs (..),
    mkListReviewableHITs,

    -- ** Request lenses
    lrhitStatus,
    lrhitHITTypeId,
    lrhitNextToken,
    lrhitMaxResults,

    -- * Destructuring the response
    ListReviewableHITsResponse (..),
    mkListReviewableHITsResponse,

    -- ** Response lenses
    lrhitrsNextToken,
    lrhitrsNumResults,
    lrhitrsHITs,
    lrhitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListReviewableHITs' smart constructor.
data ListReviewableHITs = ListReviewableHITs'
  { -- | Can be either @Reviewable@ or @Reviewing@ . Reviewable is the default value.
    status :: Lude.Maybe ReviewableHITStatus,
    -- | The ID of the HIT type of the HITs to consider for the query. If not specified, all HITs for the Reviewer are considered
    hITTypeId :: Lude.Maybe Lude.Text,
    -- | Pagination Token
    nextToken :: Lude.Maybe Lude.Text,
    -- | Limit the number of results returned.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReviewableHITs' with the minimum fields required to make a request.
--
-- * 'status' - Can be either @Reviewable@ or @Reviewing@ . Reviewable is the default value.
-- * 'hITTypeId' - The ID of the HIT type of the HITs to consider for the query. If not specified, all HITs for the Reviewer are considered
-- * 'nextToken' - Pagination Token
-- * 'maxResults' - Limit the number of results returned.
mkListReviewableHITs ::
  ListReviewableHITs
mkListReviewableHITs =
  ListReviewableHITs'
    { status = Lude.Nothing,
      hITTypeId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Can be either @Reviewable@ or @Reviewing@ . Reviewable is the default value.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitStatus :: Lens.Lens' ListReviewableHITs (Lude.Maybe ReviewableHITStatus)
lrhitStatus = Lens.lens (status :: ListReviewableHITs -> Lude.Maybe ReviewableHITStatus) (\s a -> s {status = a} :: ListReviewableHITs)
{-# DEPRECATED lrhitStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the HIT type of the HITs to consider for the query. If not specified, all HITs for the Reviewer are considered
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitHITTypeId :: Lens.Lens' ListReviewableHITs (Lude.Maybe Lude.Text)
lrhitHITTypeId = Lens.lens (hITTypeId :: ListReviewableHITs -> Lude.Maybe Lude.Text) (\s a -> s {hITTypeId = a} :: ListReviewableHITs)
{-# DEPRECATED lrhitHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | Pagination Token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitNextToken :: Lens.Lens' ListReviewableHITs (Lude.Maybe Lude.Text)
lrhitNextToken = Lens.lens (nextToken :: ListReviewableHITs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReviewableHITs)
{-# DEPRECATED lrhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limit the number of results returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitMaxResults :: Lens.Lens' ListReviewableHITs (Lude.Maybe Lude.Natural)
lrhitMaxResults = Lens.lens (maxResults :: ListReviewableHITs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListReviewableHITs)
{-# DEPRECATED lrhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListReviewableHITs where
  page rq rs
    | Page.stop (rs Lens.^. lrhitrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrhitrsHITs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrhitNextToken Lens..~ rs Lens.^. lrhitrsNextToken

instance Lude.AWSRequest ListReviewableHITs where
  type Rs ListReviewableHITs = ListReviewableHITsResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListReviewableHITsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (x Lude..?> "HITs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReviewableHITs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListReviewableHITs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListReviewableHITs where
  toJSON ListReviewableHITs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("HITTypeId" Lude..=) Lude.<$> hITTypeId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListReviewableHITs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReviewableHITs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListReviewableHITsResponse' smart constructor.
data ListReviewableHITsResponse = ListReviewableHITsResponse'
  { nextToken :: Lude.Maybe Lude.Text,
    -- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
    numResults :: Lude.Maybe Lude.Int,
    -- | The list of HIT elements returned by the query.
    hITs :: Lude.Maybe [HIT],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReviewableHITsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'numResults' - The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
-- * 'hITs' - The list of HIT elements returned by the query.
-- * 'responseStatus' - The response status code.
mkListReviewableHITsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReviewableHITsResponse
mkListReviewableHITsResponse pResponseStatus_ =
  ListReviewableHITsResponse'
    { nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      hITs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrsNextToken :: Lens.Lens' ListReviewableHITsResponse (Lude.Maybe Lude.Text)
lrhitrsNextToken = Lens.lens (nextToken :: ListReviewableHITsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReviewableHITsResponse)
{-# DEPRECATED lrhitrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrsNumResults :: Lens.Lens' ListReviewableHITsResponse (Lude.Maybe Lude.Int)
lrhitrsNumResults = Lens.lens (numResults :: ListReviewableHITsResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListReviewableHITsResponse)
{-# DEPRECATED lrhitrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The list of HIT elements returned by the query.
--
-- /Note:/ Consider using 'hITs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrsHITs :: Lens.Lens' ListReviewableHITsResponse (Lude.Maybe [HIT])
lrhitrsHITs = Lens.lens (hITs :: ListReviewableHITsResponse -> Lude.Maybe [HIT]) (\s a -> s {hITs = a} :: ListReviewableHITsResponse)
{-# DEPRECATED lrhitrsHITs "Use generic-lens or generic-optics with 'hITs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhitrsResponseStatus :: Lens.Lens' ListReviewableHITsResponse Lude.Int
lrhitrsResponseStatus = Lens.lens (responseStatus :: ListReviewableHITsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReviewableHITsResponse)
{-# DEPRECATED lrhitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
