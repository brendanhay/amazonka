{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ListHarvestJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of HarvestJob records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListHarvestJobs
  ( -- * Creating a request
    ListHarvestJobs (..),
    mkListHarvestJobs,

    -- ** Request lenses
    lhjIncludeStatus,
    lhjNextToken,
    lhjIncludeChannelId,
    lhjMaxResults,

    -- * Destructuring the response
    ListHarvestJobsResponse (..),
    mkListHarvestJobsResponse,

    -- ** Response lenses
    lhjrsHarvestJobs,
    lhjrsNextToken,
    lhjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListHarvestJobs' smart constructor.
data ListHarvestJobs = ListHarvestJobs'
  { includeStatus ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    includeChannelId :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHarvestJobs' with the minimum fields required to make a request.
--
-- * 'includeChannelId' - When specified, the request will return only HarvestJobs associated with the given Channel ID.
-- * 'includeStatus' - When specified, the request will return only HarvestJobs in the given status.
-- * 'maxResults' - The upper bound on the number of records to return.
-- * 'nextToken' - A token used to resume pagination from the end of a previous request.
mkListHarvestJobs ::
  ListHarvestJobs
mkListHarvestJobs =
  ListHarvestJobs'
    { includeStatus = Lude.Nothing,
      nextToken = Lude.Nothing,
      includeChannelId = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | When specified, the request will return only HarvestJobs in the given status.
--
-- /Note:/ Consider using 'includeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjIncludeStatus :: Lens.Lens' ListHarvestJobs (Lude.Maybe Lude.Text)
lhjIncludeStatus = Lens.lens (includeStatus :: ListHarvestJobs -> Lude.Maybe Lude.Text) (\s a -> s {includeStatus = a} :: ListHarvestJobs)
{-# DEPRECATED lhjIncludeStatus "Use generic-lens or generic-optics with 'includeStatus' instead." #-}

-- | A token used to resume pagination from the end of a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjNextToken :: Lens.Lens' ListHarvestJobs (Lude.Maybe Lude.Text)
lhjNextToken = Lens.lens (nextToken :: ListHarvestJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHarvestJobs)
{-# DEPRECATED lhjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, the request will return only HarvestJobs associated with the given Channel ID.
--
-- /Note:/ Consider using 'includeChannelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjIncludeChannelId :: Lens.Lens' ListHarvestJobs (Lude.Maybe Lude.Text)
lhjIncludeChannelId = Lens.lens (includeChannelId :: ListHarvestJobs -> Lude.Maybe Lude.Text) (\s a -> s {includeChannelId = a} :: ListHarvestJobs)
{-# DEPRECATED lhjIncludeChannelId "Use generic-lens or generic-optics with 'includeChannelId' instead." #-}

-- | The upper bound on the number of records to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjMaxResults :: Lens.Lens' ListHarvestJobs (Lude.Maybe Lude.Natural)
lhjMaxResults = Lens.lens (maxResults :: ListHarvestJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListHarvestJobs)
{-# DEPRECATED lhjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListHarvestJobs where
  page rq rs
    | Page.stop (rs Lens.^. lhjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lhjrsHarvestJobs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhjNextToken Lens..~ rs Lens.^. lhjrsNextToken

instance Lude.AWSRequest ListHarvestJobs where
  type Rs ListHarvestJobs = ListHarvestJobsResponse
  request = Req.get mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListHarvestJobsResponse'
            Lude.<$> (x Lude..?> "harvestJobs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHarvestJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListHarvestJobs where
  toPath = Lude.const "/harvest_jobs"

instance Lude.ToQuery ListHarvestJobs where
  toQuery ListHarvestJobs' {..} =
    Lude.mconcat
      [ "includeStatus" Lude.=: includeStatus,
        "nextToken" Lude.=: nextToken,
        "includeChannelId" Lude.=: includeChannelId,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListHarvestJobsResponse' smart constructor.
data ListHarvestJobsResponse = ListHarvestJobsResponse'
  { harvestJobs ::
      Lude.Maybe [HarvestJob],
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

-- | Creates a value of 'ListHarvestJobsResponse' with the minimum fields required to make a request.
--
-- * 'harvestJobs' - A list of HarvestJob records.
-- * 'nextToken' - A token that can be used to resume pagination from the end of the collection.
-- * 'responseStatus' - The response status code.
mkListHarvestJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListHarvestJobsResponse
mkListHarvestJobsResponse pResponseStatus_ =
  ListHarvestJobsResponse'
    { harvestJobs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of HarvestJob records.
--
-- /Note:/ Consider using 'harvestJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjrsHarvestJobs :: Lens.Lens' ListHarvestJobsResponse (Lude.Maybe [HarvestJob])
lhjrsHarvestJobs = Lens.lens (harvestJobs :: ListHarvestJobsResponse -> Lude.Maybe [HarvestJob]) (\s a -> s {harvestJobs = a} :: ListHarvestJobsResponse)
{-# DEPRECATED lhjrsHarvestJobs "Use generic-lens or generic-optics with 'harvestJobs' instead." #-}

-- | A token that can be used to resume pagination from the end of the collection.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjrsNextToken :: Lens.Lens' ListHarvestJobsResponse (Lude.Maybe Lude.Text)
lhjrsNextToken = Lens.lens (nextToken :: ListHarvestJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHarvestJobsResponse)
{-# DEPRECATED lhjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhjrsResponseStatus :: Lens.Lens' ListHarvestJobsResponse Lude.Int
lhjrsResponseStatus = Lens.lens (responseStatus :: ListHarvestJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHarvestJobsResponse)
{-# DEPRECATED lhjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
