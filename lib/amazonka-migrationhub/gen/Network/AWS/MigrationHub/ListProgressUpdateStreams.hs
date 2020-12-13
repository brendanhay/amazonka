{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListProgressUpdateStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists progress update streams associated with the user account making this call.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListProgressUpdateStreams
  ( -- * Creating a request
    ListProgressUpdateStreams (..),
    mkListProgressUpdateStreams,

    -- ** Request lenses
    lpusNextToken,
    lpusMaxResults,

    -- * Destructuring the response
    ListProgressUpdateStreamsResponse (..),
    mkListProgressUpdateStreamsResponse,

    -- ** Response lenses
    lpusrsProgressUpdateStreamSummaryList,
    lpusrsNextToken,
    lpusrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProgressUpdateStreams' smart constructor.
data ListProgressUpdateStreams = ListProgressUpdateStreams'
  { -- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filter to limit the maximum number of results to list per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProgressUpdateStreams' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
-- * 'maxResults' - Filter to limit the maximum number of results to list per page.
mkListProgressUpdateStreams ::
  ListProgressUpdateStreams
mkListProgressUpdateStreams =
  ListProgressUpdateStreams'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusNextToken :: Lens.Lens' ListProgressUpdateStreams (Lude.Maybe Lude.Text)
lpusNextToken = Lens.lens (nextToken :: ListProgressUpdateStreams -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProgressUpdateStreams)
{-# DEPRECATED lpusNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter to limit the maximum number of results to list per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusMaxResults :: Lens.Lens' ListProgressUpdateStreams (Lude.Maybe Lude.Natural)
lpusMaxResults = Lens.lens (maxResults :: ListProgressUpdateStreams -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListProgressUpdateStreams)
{-# DEPRECATED lpusMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListProgressUpdateStreams where
  page rq rs
    | Page.stop (rs Lens.^. lpusrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpusrsProgressUpdateStreamSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpusNextToken Lens..~ rs Lens.^. lpusrsNextToken

instance Lude.AWSRequest ListProgressUpdateStreams where
  type
    Rs ListProgressUpdateStreams =
      ListProgressUpdateStreamsResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProgressUpdateStreamsResponse'
            Lude.<$> (x Lude..?> "ProgressUpdateStreamSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProgressUpdateStreams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.ListProgressUpdateStreams" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProgressUpdateStreams where
  toJSON ListProgressUpdateStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListProgressUpdateStreams where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProgressUpdateStreams where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProgressUpdateStreamsResponse' smart constructor.
data ListProgressUpdateStreamsResponse = ListProgressUpdateStreamsResponse'
  { -- | List of progress update streams up to the max number of results passed in the input.
    progressUpdateStreamSummaryList :: Lude.Maybe [ProgressUpdateStreamSummary],
    -- | If there are more streams created than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProgressUpdateStreamsResponse' with the minimum fields required to make a request.
--
-- * 'progressUpdateStreamSummaryList' - List of progress update streams up to the max number of results passed in the input.
-- * 'nextToken' - If there are more streams created than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
-- * 'responseStatus' - The response status code.
mkListProgressUpdateStreamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProgressUpdateStreamsResponse
mkListProgressUpdateStreamsResponse pResponseStatus_ =
  ListProgressUpdateStreamsResponse'
    { progressUpdateStreamSummaryList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of progress update streams up to the max number of results passed in the input.
--
-- /Note:/ Consider using 'progressUpdateStreamSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusrsProgressUpdateStreamSummaryList :: Lens.Lens' ListProgressUpdateStreamsResponse (Lude.Maybe [ProgressUpdateStreamSummary])
lpusrsProgressUpdateStreamSummaryList = Lens.lens (progressUpdateStreamSummaryList :: ListProgressUpdateStreamsResponse -> Lude.Maybe [ProgressUpdateStreamSummary]) (\s a -> s {progressUpdateStreamSummaryList = a} :: ListProgressUpdateStreamsResponse)
{-# DEPRECATED lpusrsProgressUpdateStreamSummaryList "Use generic-lens or generic-optics with 'progressUpdateStreamSummaryList' instead." #-}

-- | If there are more streams created than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusrsNextToken :: Lens.Lens' ListProgressUpdateStreamsResponse (Lude.Maybe Lude.Text)
lpusrsNextToken = Lens.lens (nextToken :: ListProgressUpdateStreamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProgressUpdateStreamsResponse)
{-# DEPRECATED lpusrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpusrsResponseStatus :: Lens.Lens' ListProgressUpdateStreamsResponse Lude.Int
lpusrsResponseStatus = Lens.lens (responseStatus :: ListProgressUpdateStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProgressUpdateStreamsResponse)
{-# DEPRECATED lpusrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
