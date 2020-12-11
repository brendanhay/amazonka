{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListPipelineExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of the most recent executions for a pipeline.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListPipelineExecutions
  ( -- * Creating a request
    ListPipelineExecutions (..),
    mkListPipelineExecutions,

    -- ** Request lenses
    lpeNextToken,
    lpeMaxResults,
    lpePipelineName,

    -- * Destructuring the response
    ListPipelineExecutionsResponse (..),
    mkListPipelineExecutionsResponse,

    -- ** Response lenses
    lpersNextToken,
    lpersPipelineExecutionSummaries,
    lpersResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListPipelineExecutions@ action.
--
-- /See:/ 'mkListPipelineExecutions' smart constructor.
data ListPipelineExecutions = ListPipelineExecutions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    pipelineName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelineExecutions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Pipeline history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
-- * 'nextToken' - The token that was returned from the previous @ListPipelineExecutions@ call, which can be used to return the next set of pipeline executions in the list.
-- * 'pipelineName' - The name of the pipeline for which you want to get execution summary information.
mkListPipelineExecutions ::
  -- | 'pipelineName'
  Lude.Text ->
  ListPipelineExecutions
mkListPipelineExecutions pPipelineName_ =
  ListPipelineExecutions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      pipelineName = pPipelineName_
    }

-- | The token that was returned from the previous @ListPipelineExecutions@ call, which can be used to return the next set of pipeline executions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpeNextToken :: Lens.Lens' ListPipelineExecutions (Lude.Maybe Lude.Text)
lpeNextToken = Lens.lens (nextToken :: ListPipelineExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPipelineExecutions)
{-# DEPRECATED lpeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Pipeline history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpeMaxResults :: Lens.Lens' ListPipelineExecutions (Lude.Maybe Lude.Natural)
lpeMaxResults = Lens.lens (maxResults :: ListPipelineExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPipelineExecutions)
{-# DEPRECATED lpeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the pipeline for which you want to get execution summary information.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpePipelineName :: Lens.Lens' ListPipelineExecutions Lude.Text
lpePipelineName = Lens.lens (pipelineName :: ListPipelineExecutions -> Lude.Text) (\s a -> s {pipelineName = a} :: ListPipelineExecutions)
{-# DEPRECATED lpePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

instance Page.AWSPager ListPipelineExecutions where
  page rq rs
    | Page.stop (rs Lens.^. lpersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpersPipelineExecutionSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpeNextToken Lens..~ rs Lens.^. lpersNextToken

instance Lude.AWSRequest ListPipelineExecutions where
  type Rs ListPipelineExecutions = ListPipelineExecutionsResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPipelineExecutionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "pipelineExecutionSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPipelineExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.ListPipelineExecutions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPipelineExecutions where
  toJSON ListPipelineExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("pipelineName" Lude..= pipelineName)
          ]
      )

instance Lude.ToPath ListPipelineExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPipelineExecutions where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListPipelineExecutions@ action.
--
-- /See:/ 'mkListPipelineExecutionsResponse' smart constructor.
data ListPipelineExecutionsResponse = ListPipelineExecutionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    pipelineExecutionSummaries ::
      Lude.Maybe
        [PipelineExecutionSummary],
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

-- | Creates a value of 'ListPipelineExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that can be used in the next @ListPipelineExecutions@ call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
-- * 'pipelineExecutionSummaries' - A list of executions in the history of a pipeline.
-- * 'responseStatus' - The response status code.
mkListPipelineExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPipelineExecutionsResponse
mkListPipelineExecutionsResponse pResponseStatus_ =
  ListPipelineExecutionsResponse'
    { nextToken = Lude.Nothing,
      pipelineExecutionSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token that can be used in the next @ListPipelineExecutions@ call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpersNextToken :: Lens.Lens' ListPipelineExecutionsResponse (Lude.Maybe Lude.Text)
lpersNextToken = Lens.lens (nextToken :: ListPipelineExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPipelineExecutionsResponse)
{-# DEPRECATED lpersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of executions in the history of a pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpersPipelineExecutionSummaries :: Lens.Lens' ListPipelineExecutionsResponse (Lude.Maybe [PipelineExecutionSummary])
lpersPipelineExecutionSummaries = Lens.lens (pipelineExecutionSummaries :: ListPipelineExecutionsResponse -> Lude.Maybe [PipelineExecutionSummary]) (\s a -> s {pipelineExecutionSummaries = a} :: ListPipelineExecutionsResponse)
{-# DEPRECATED lpersPipelineExecutionSummaries "Use generic-lens or generic-optics with 'pipelineExecutionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpersResponseStatus :: Lens.Lens' ListPipelineExecutionsResponse Lude.Int
lpersResponseStatus = Lens.lens (responseStatus :: ListPipelineExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPipelineExecutionsResponse)
{-# DEPRECATED lpersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
