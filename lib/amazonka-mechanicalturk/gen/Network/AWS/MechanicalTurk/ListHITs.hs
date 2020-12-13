{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListHITs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListHITs@ operation returns all of a Requester's HITs. The operation returns HITs of any status, except for HITs that have been deleted of with the DeleteHIT operation or that have been auto-deleted.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListHITs
  ( -- * Creating a request
    ListHITs (..),
    mkListHITs,

    -- ** Request lenses
    lhitNextToken,
    lhitMaxResults,

    -- * Destructuring the response
    ListHITsResponse (..),
    mkListHITsResponse,

    -- ** Response lenses
    lhitrsNextToken,
    lhitrsNumResults,
    lhitrsHITs,
    lhitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListHITs' smart constructor.
data ListHITs = ListHITs'
  { -- | Pagination token
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHITs' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token
-- * 'maxResults' -
mkListHITs ::
  ListHITs
mkListHITs =
  ListHITs' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitNextToken :: Lens.Lens' ListHITs (Lude.Maybe Lude.Text)
lhitNextToken = Lens.lens (nextToken :: ListHITs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHITs)
{-# DEPRECATED lhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitMaxResults :: Lens.Lens' ListHITs (Lude.Maybe Lude.Natural)
lhitMaxResults = Lens.lens (maxResults :: ListHITs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListHITs)
{-# DEPRECATED lhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListHITs where
  page rq rs
    | Page.stop (rs Lens.^. lhitrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lhitrsHITs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhitNextToken Lens..~ rs Lens.^. lhitrsNextToken

instance Lude.AWSRequest ListHITs where
  type Rs ListHITs = ListHITsResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListHITsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (x Lude..?> "HITs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHITs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MTurkRequesterServiceV20170117.ListHITs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListHITs where
  toJSON ListHITs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListHITs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListHITs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListHITsResponse' smart constructor.
data ListHITsResponse = ListHITsResponse'
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

-- | Creates a value of 'ListHITsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'numResults' - The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
-- * 'hITs' - The list of HIT elements returned by the query.
-- * 'responseStatus' - The response status code.
mkListHITsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListHITsResponse
mkListHITsResponse pResponseStatus_ =
  ListHITsResponse'
    { nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      hITs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrsNextToken :: Lens.Lens' ListHITsResponse (Lude.Maybe Lude.Text)
lhitrsNextToken = Lens.lens (nextToken :: ListHITsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHITsResponse)
{-# DEPRECATED lhitrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrsNumResults :: Lens.Lens' ListHITsResponse (Lude.Maybe Lude.Int)
lhitrsNumResults = Lens.lens (numResults :: ListHITsResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListHITsResponse)
{-# DEPRECATED lhitrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The list of HIT elements returned by the query.
--
-- /Note:/ Consider using 'hITs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrsHITs :: Lens.Lens' ListHITsResponse (Lude.Maybe [HIT])
lhitrsHITs = Lens.lens (hITs :: ListHITsResponse -> Lude.Maybe [HIT]) (\s a -> s {hITs = a} :: ListHITsResponse)
{-# DEPRECATED lhitrsHITs "Use generic-lens or generic-optics with 'hITs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhitrsResponseStatus :: Lens.Lens' ListHITsResponse Lude.Int
lhitrsResponseStatus = Lens.lens (responseStatus :: ListHITsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHITsResponse)
{-# DEPRECATED lhitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
