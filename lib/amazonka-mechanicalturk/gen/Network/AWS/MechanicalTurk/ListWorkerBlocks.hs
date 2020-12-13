{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListWorkerBlocks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersBlocks@ operation retrieves a list of Workers who are blocked from working on your HITs.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListWorkerBlocks
  ( -- * Creating a request
    ListWorkerBlocks (..),
    mkListWorkerBlocks,

    -- ** Request lenses
    lwbNextToken,
    lwbMaxResults,

    -- * Destructuring the response
    ListWorkerBlocksResponse (..),
    mkListWorkerBlocksResponse,

    -- ** Response lenses
    lwbrsWorkerBlocks,
    lwbrsNextToken,
    lwbrsNumResults,
    lwbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListWorkerBlocks' smart constructor.
data ListWorkerBlocks = ListWorkerBlocks'
  { -- | Pagination token
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkerBlocks' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token
-- * 'maxResults' -
mkListWorkerBlocks ::
  ListWorkerBlocks
mkListWorkerBlocks =
  ListWorkerBlocks'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbNextToken :: Lens.Lens' ListWorkerBlocks (Lude.Maybe Lude.Text)
lwbNextToken = Lens.lens (nextToken :: ListWorkerBlocks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkerBlocks)
{-# DEPRECATED lwbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbMaxResults :: Lens.Lens' ListWorkerBlocks (Lude.Maybe Lude.Natural)
lwbMaxResults = Lens.lens (maxResults :: ListWorkerBlocks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWorkerBlocks)
{-# DEPRECATED lwbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListWorkerBlocks where
  page rq rs
    | Page.stop (rs Lens.^. lwbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lwbrsWorkerBlocks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwbNextToken Lens..~ rs Lens.^. lwbrsNextToken

instance Lude.AWSRequest ListWorkerBlocks where
  type Rs ListWorkerBlocks = ListWorkerBlocksResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkerBlocksResponse'
            Lude.<$> (x Lude..?> "WorkerBlocks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "NumResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListWorkerBlocks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListWorkerBlocks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWorkerBlocks where
  toJSON ListWorkerBlocks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListWorkerBlocks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWorkerBlocks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListWorkerBlocksResponse' smart constructor.
data ListWorkerBlocksResponse = ListWorkerBlocksResponse'
  { -- | The list of WorkerBlocks, containing the collection of Worker IDs and reasons for blocking.
    workerBlocks :: Lude.Maybe [WorkerBlock],
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
    numResults :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkerBlocksResponse' with the minimum fields required to make a request.
--
-- * 'workerBlocks' - The list of WorkerBlocks, containing the collection of Worker IDs and reasons for blocking.
-- * 'nextToken' -
-- * 'numResults' - The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
-- * 'responseStatus' - The response status code.
mkListWorkerBlocksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkerBlocksResponse
mkListWorkerBlocksResponse pResponseStatus_ =
  ListWorkerBlocksResponse'
    { workerBlocks = Lude.Nothing,
      nextToken = Lude.Nothing,
      numResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of WorkerBlocks, containing the collection of Worker IDs and reasons for blocking.
--
-- /Note:/ Consider using 'workerBlocks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrsWorkerBlocks :: Lens.Lens' ListWorkerBlocksResponse (Lude.Maybe [WorkerBlock])
lwbrsWorkerBlocks = Lens.lens (workerBlocks :: ListWorkerBlocksResponse -> Lude.Maybe [WorkerBlock]) (\s a -> s {workerBlocks = a} :: ListWorkerBlocksResponse)
{-# DEPRECATED lwbrsWorkerBlocks "Use generic-lens or generic-optics with 'workerBlocks' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrsNextToken :: Lens.Lens' ListWorkerBlocksResponse (Lude.Maybe Lude.Text)
lwbrsNextToken = Lens.lens (nextToken :: ListWorkerBlocksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkerBlocksResponse)
{-# DEPRECATED lwbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrsNumResults :: Lens.Lens' ListWorkerBlocksResponse (Lude.Maybe Lude.Int)
lwbrsNumResults = Lens.lens (numResults :: ListWorkerBlocksResponse -> Lude.Maybe Lude.Int) (\s a -> s {numResults = a} :: ListWorkerBlocksResponse)
{-# DEPRECATED lwbrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrsResponseStatus :: Lens.Lens' ListWorkerBlocksResponse Lude.Int
lwbrsResponseStatus = Lens.lens (responseStatus :: ListWorkerBlocksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkerBlocksResponse)
{-# DEPRECATED lwbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
