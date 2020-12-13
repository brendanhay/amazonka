{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all of the pipelines associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListPipelines
  ( -- * Creating a request
    ListPipelines (..),
    mkListPipelines,

    -- ** Request lenses
    lpNextToken,

    -- * Destructuring the response
    ListPipelinesResponse (..),
    mkListPipelinesResponse,

    -- ** Response lenses
    lprsPipelines,
    lprsNextToken,
    lprsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListPipelines@ action.
--
-- /See:/ 'mkListPipelines' smart constructor.
newtype ListPipelines = ListPipelines'
  { -- | An identifier that was returned from the previous list pipelines call. It can be used to return the next set of pipelines in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous list pipelines call. It can be used to return the next set of pipelines in the list.
mkListPipelines ::
  ListPipelines
mkListPipelines = ListPipelines' {nextToken = Lude.Nothing}

-- | An identifier that was returned from the previous list pipelines call. It can be used to return the next set of pipelines in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPipelines (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListPipelines -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPipelines)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListPipelines where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPipelines) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Lude.<$> (x Lude..?> "pipelines" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPipelines where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.ListPipelines" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPipelines where
  toJSON ListPipelines' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListPipelines where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPipelines where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListPipelines@ action.
--
-- /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | The list of pipelines.
    pipelines :: Lude.Maybe [PipelineSummary],
    -- | If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list pipelines call to return the next set of pipelines in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- * 'pipelines' - The list of pipelines.
-- * 'nextToken' - If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list pipelines call to return the next set of pipelines in the list.
-- * 'responseStatus' - The response status code.
mkListPipelinesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPipelinesResponse
mkListPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { pipelines = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of pipelines.
--
-- /Note:/ Consider using 'pipelines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPipelines :: Lens.Lens' ListPipelinesResponse (Lude.Maybe [PipelineSummary])
lprsPipelines = Lens.lens (pipelines :: ListPipelinesResponse -> Lude.Maybe [PipelineSummary]) (\s a -> s {pipelines = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsPipelines "Use generic-lens or generic-optics with 'pipelines' instead." #-}

-- | If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list pipelines call to return the next set of pipelines in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListPipelinesResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListPipelinesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPipelinesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPipelinesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
