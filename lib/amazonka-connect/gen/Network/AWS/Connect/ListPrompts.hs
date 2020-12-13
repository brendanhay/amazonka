{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListPrompts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the prompts for the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListPrompts
  ( -- * Creating a request
    ListPrompts (..),
    mkListPrompts,

    -- ** Request lenses
    lpInstanceId,
    lpNextToken,
    lpMaxResults,

    -- * Destructuring the response
    ListPromptsResponse (..),
    mkListPromptsResponse,

    -- ** Response lenses
    lprsPromptSummaryList,
    lprsNextToken,
    lprsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPrompts' smart constructor.
data ListPrompts = ListPrompts'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPrompts' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'maxResults' - The maximum number of results to return per page.
mkListPrompts ::
  -- | 'instanceId'
  Lude.Text ->
  ListPrompts
mkListPrompts pInstanceId_ =
  ListPrompts'
    { instanceId = pInstanceId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpInstanceId :: Lens.Lens' ListPrompts Lude.Text
lpInstanceId = Lens.lens (instanceId :: ListPrompts -> Lude.Text) (\s a -> s {instanceId = a} :: ListPrompts)
{-# DEPRECATED lpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPrompts (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListPrompts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPrompts)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPrompts (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: ListPrompts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPrompts)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListPrompts where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPromptSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListPrompts where
  type Rs ListPrompts = ListPromptsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPromptsResponse'
            Lude.<$> (x Lude..?> "PromptSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPrompts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListPrompts where
  toPath ListPrompts' {..} =
    Lude.mconcat ["/prompts-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListPrompts where
  toQuery ListPrompts' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListPromptsResponse' smart constructor.
data ListPromptsResponse = ListPromptsResponse'
  { -- | Information about the prompts.
    promptSummaryList :: Lude.Maybe [PromptSummary],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPromptsResponse' with the minimum fields required to make a request.
--
-- * 'promptSummaryList' - Information about the prompts.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListPromptsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPromptsResponse
mkListPromptsResponse pResponseStatus_ =
  ListPromptsResponse'
    { promptSummaryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prompts.
--
-- /Note:/ Consider using 'promptSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPromptSummaryList :: Lens.Lens' ListPromptsResponse (Lude.Maybe [PromptSummary])
lprsPromptSummaryList = Lens.lens (promptSummaryList :: ListPromptsResponse -> Lude.Maybe [PromptSummary]) (\s a -> s {promptSummaryList = a} :: ListPromptsResponse)
{-# DEPRECATED lprsPromptSummaryList "Use generic-lens or generic-optics with 'promptSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListPromptsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListPromptsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPromptsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPromptsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPromptsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPromptsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
