{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListActionExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the action executions that have occurred in a pipeline.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListActionExecutions
  ( -- * Creating a request
    ListActionExecutions (..),
    mkListActionExecutions,

    -- ** Request lenses
    laePipelineName,
    laeNextToken,
    laeFilter,
    laeMaxResults,

    -- * Destructuring the response
    ListActionExecutionsResponse (..),
    mkListActionExecutionsResponse,

    -- ** Response lenses
    laersActionExecutionDetails,
    laersNextToken,
    laersResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListActionExecutions' smart constructor.
data ListActionExecutions = ListActionExecutions'
  { -- | The name of the pipeline for which you want to list action execution history.
    pipelineName :: Lude.Text,
    -- | The token that was returned from the previous @ListActionExecutions@ call, which can be used to return the next set of action executions in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Input information used to filter action execution history.
    filter :: Lude.Maybe ActionExecutionFilter,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Action execution history is retained for up to 12 months, based on action execution start times. Default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActionExecutions' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline for which you want to list action execution history.
-- * 'nextToken' - The token that was returned from the previous @ListActionExecutions@ call, which can be used to return the next set of action executions in the list.
-- * 'filter' - Input information used to filter action execution history.
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Action execution history is retained for up to 12 months, based on action execution start times. Default value is 100.
mkListActionExecutions ::
  -- | 'pipelineName'
  Lude.Text ->
  ListActionExecutions
mkListActionExecutions pPipelineName_ =
  ListActionExecutions'
    { pipelineName = pPipelineName_,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the pipeline for which you want to list action execution history.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laePipelineName :: Lens.Lens' ListActionExecutions Lude.Text
laePipelineName = Lens.lens (pipelineName :: ListActionExecutions -> Lude.Text) (\s a -> s {pipelineName = a} :: ListActionExecutions)
{-# DEPRECATED laePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The token that was returned from the previous @ListActionExecutions@ call, which can be used to return the next set of action executions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laeNextToken :: Lens.Lens' ListActionExecutions (Lude.Maybe Lude.Text)
laeNextToken = Lens.lens (nextToken :: ListActionExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActionExecutions)
{-# DEPRECATED laeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Input information used to filter action execution history.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laeFilter :: Lens.Lens' ListActionExecutions (Lude.Maybe ActionExecutionFilter)
laeFilter = Lens.lens (filter :: ListActionExecutions -> Lude.Maybe ActionExecutionFilter) (\s a -> s {filter = a} :: ListActionExecutions)
{-# DEPRECATED laeFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Action execution history is retained for up to 12 months, based on action execution start times. Default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laeMaxResults :: Lens.Lens' ListActionExecutions (Lude.Maybe Lude.Natural)
laeMaxResults = Lens.lens (maxResults :: ListActionExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListActionExecutions)
{-# DEPRECATED laeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListActionExecutions where
  page rq rs
    | Page.stop (rs Lens.^. laersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. laersActionExecutionDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laeNextToken Lens..~ rs Lens.^. laersNextToken

instance Lude.AWSRequest ListActionExecutions where
  type Rs ListActionExecutions = ListActionExecutionsResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListActionExecutionsResponse'
            Lude.<$> (x Lude..?> "actionExecutionDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListActionExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.ListActionExecutions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListActionExecutions where
  toJSON ListActionExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListActionExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListActionExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListActionExecutionsResponse' smart constructor.
data ListActionExecutionsResponse = ListActionExecutionsResponse'
  { -- | The details for a list of recent executions, such as action execution ID.
    actionExecutionDetails :: Lude.Maybe [ActionExecutionDetail],
    -- | If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent @ListActionExecutions@ call to return the next set of action executions in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActionExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'actionExecutionDetails' - The details for a list of recent executions, such as action execution ID.
-- * 'nextToken' - If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent @ListActionExecutions@ call to return the next set of action executions in the list.
-- * 'responseStatus' - The response status code.
mkListActionExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListActionExecutionsResponse
mkListActionExecutionsResponse pResponseStatus_ =
  ListActionExecutionsResponse'
    { actionExecutionDetails =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details for a list of recent executions, such as action execution ID.
--
-- /Note:/ Consider using 'actionExecutionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laersActionExecutionDetails :: Lens.Lens' ListActionExecutionsResponse (Lude.Maybe [ActionExecutionDetail])
laersActionExecutionDetails = Lens.lens (actionExecutionDetails :: ListActionExecutionsResponse -> Lude.Maybe [ActionExecutionDetail]) (\s a -> s {actionExecutionDetails = a} :: ListActionExecutionsResponse)
{-# DEPRECATED laersActionExecutionDetails "Use generic-lens or generic-optics with 'actionExecutionDetails' instead." #-}

-- | If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent @ListActionExecutions@ call to return the next set of action executions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laersNextToken :: Lens.Lens' ListActionExecutionsResponse (Lude.Maybe Lude.Text)
laersNextToken = Lens.lens (nextToken :: ListActionExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActionExecutionsResponse)
{-# DEPRECATED laersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laersResponseStatus :: Lens.Lens' ListActionExecutionsResponse Lude.Int
laersResponseStatus = Lens.lens (responseStatus :: ListActionExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListActionExecutionsResponse)
{-# DEPRECATED laersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
