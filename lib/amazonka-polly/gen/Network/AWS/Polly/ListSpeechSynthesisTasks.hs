{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.ListSpeechSynthesisTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of SpeechSynthesisTask objects ordered by their creation date. This operation can filter the tasks by their status, for example, allowing users to list only tasks that are completed.
--
-- This operation returns paginated results.
module Network.AWS.Polly.ListSpeechSynthesisTasks
  ( -- * Creating a request
    ListSpeechSynthesisTasks (..),
    mkListSpeechSynthesisTasks,

    -- ** Request lenses
    lsstStatus,
    lsstNextToken,
    lsstMaxResults,

    -- * Destructuring the response
    ListSpeechSynthesisTasksResponse (..),
    mkListSpeechSynthesisTasksResponse,

    -- ** Response lenses
    lsstrsNextToken,
    lsstrsSynthesisTasks,
    lsstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSpeechSynthesisTasks' smart constructor.
data ListSpeechSynthesisTasks = ListSpeechSynthesisTasks'
  { -- | Status of the speech synthesis tasks returned in a List operation
    status :: Lude.Maybe TaskStatus,
    -- | The pagination token to use in the next request to continue the listing of speech synthesis tasks.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of speech synthesis tasks returned in a List operation.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSpeechSynthesisTasks' with the minimum fields required to make a request.
--
-- * 'status' - Status of the speech synthesis tasks returned in a List operation
-- * 'nextToken' - The pagination token to use in the next request to continue the listing of speech synthesis tasks.
-- * 'maxResults' - Maximum number of speech synthesis tasks returned in a List operation.
mkListSpeechSynthesisTasks ::
  ListSpeechSynthesisTasks
mkListSpeechSynthesisTasks =
  ListSpeechSynthesisTasks'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Status of the speech synthesis tasks returned in a List operation
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstStatus :: Lens.Lens' ListSpeechSynthesisTasks (Lude.Maybe TaskStatus)
lsstStatus = Lens.lens (status :: ListSpeechSynthesisTasks -> Lude.Maybe TaskStatus) (\s a -> s {status = a} :: ListSpeechSynthesisTasks)
{-# DEPRECATED lsstStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The pagination token to use in the next request to continue the listing of speech synthesis tasks.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstNextToken :: Lens.Lens' ListSpeechSynthesisTasks (Lude.Maybe Lude.Text)
lsstNextToken = Lens.lens (nextToken :: ListSpeechSynthesisTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSpeechSynthesisTasks)
{-# DEPRECATED lsstNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of speech synthesis tasks returned in a List operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstMaxResults :: Lens.Lens' ListSpeechSynthesisTasks (Lude.Maybe Lude.Natural)
lsstMaxResults = Lens.lens (maxResults :: ListSpeechSynthesisTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSpeechSynthesisTasks)
{-# DEPRECATED lsstMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSpeechSynthesisTasks where
  page rq rs
    | Page.stop (rs Lens.^. lsstrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsstrsSynthesisTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsstNextToken Lens..~ rs Lens.^. lsstrsNextToken

instance Lude.AWSRequest ListSpeechSynthesisTasks where
  type Rs ListSpeechSynthesisTasks = ListSpeechSynthesisTasksResponse
  request = Req.get pollyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSpeechSynthesisTasksResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SynthesisTasks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSpeechSynthesisTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSpeechSynthesisTasks where
  toPath = Lude.const "/v1/synthesisTasks"

instance Lude.ToQuery ListSpeechSynthesisTasks where
  toQuery ListSpeechSynthesisTasks' {..} =
    Lude.mconcat
      [ "Status" Lude.=: status,
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListSpeechSynthesisTasksResponse' smart constructor.
data ListSpeechSynthesisTasksResponse = ListSpeechSynthesisTasksResponse'
  { -- | An opaque pagination token returned from the previous List operation in this request. If present, this indicates where to continue the listing.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of SynthesisTask objects that provides information from the specified task in the list request, including output format, creation time, task status, and so on.
    synthesisTasks :: Lude.Maybe [SynthesisTask],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSpeechSynthesisTasksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An opaque pagination token returned from the previous List operation in this request. If present, this indicates where to continue the listing.
-- * 'synthesisTasks' - List of SynthesisTask objects that provides information from the specified task in the list request, including output format, creation time, task status, and so on.
-- * 'responseStatus' - The response status code.
mkListSpeechSynthesisTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSpeechSynthesisTasksResponse
mkListSpeechSynthesisTasksResponse pResponseStatus_ =
  ListSpeechSynthesisTasksResponse'
    { nextToken = Lude.Nothing,
      synthesisTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An opaque pagination token returned from the previous List operation in this request. If present, this indicates where to continue the listing.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstrsNextToken :: Lens.Lens' ListSpeechSynthesisTasksResponse (Lude.Maybe Lude.Text)
lsstrsNextToken = Lens.lens (nextToken :: ListSpeechSynthesisTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSpeechSynthesisTasksResponse)
{-# DEPRECATED lsstrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of SynthesisTask objects that provides information from the specified task in the list request, including output format, creation time, task status, and so on.
--
-- /Note:/ Consider using 'synthesisTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstrsSynthesisTasks :: Lens.Lens' ListSpeechSynthesisTasksResponse (Lude.Maybe [SynthesisTask])
lsstrsSynthesisTasks = Lens.lens (synthesisTasks :: ListSpeechSynthesisTasksResponse -> Lude.Maybe [SynthesisTask]) (\s a -> s {synthesisTasks = a} :: ListSpeechSynthesisTasksResponse)
{-# DEPRECATED lsstrsSynthesisTasks "Use generic-lens or generic-optics with 'synthesisTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstrsResponseStatus :: Lens.Lens' ListSpeechSynthesisTasksResponse Lude.Int
lsstrsResponseStatus = Lens.lens (responseStatus :: ListSpeechSynthesisTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSpeechSynthesisTasksResponse)
{-# DEPRECATED lsstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
