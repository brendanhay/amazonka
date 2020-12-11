{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a pipeline.
module Network.AWS.IoTAnalytics.DescribePipeline
  ( -- * Creating a request
    DescribePipeline (..),
    mkDescribePipeline,

    -- ** Request lenses
    dpPipelineName,

    -- * Destructuring the response
    DescribePipelineResponse (..),
    mkDescribePipelineResponse,

    -- ** Response lenses
    dprsPipeline,
    dprsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePipeline' smart constructor.
newtype DescribePipeline = DescribePipeline'
  { pipelineName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePipeline' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline whose information is retrieved.
mkDescribePipeline ::
  -- | 'pipelineName'
  Lude.Text ->
  DescribePipeline
mkDescribePipeline pPipelineName_ =
  DescribePipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline whose information is retrieved.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPipelineName :: Lens.Lens' DescribePipeline Lude.Text
dpPipelineName = Lens.lens (pipelineName :: DescribePipeline -> Lude.Text) (\s a -> s {pipelineName = a} :: DescribePipeline)
{-# DEPRECATED dpPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

instance Lude.AWSRequest DescribePipeline where
  type Rs DescribePipeline = DescribePipelineResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePipelineResponse'
            Lude.<$> (x Lude..?> "pipeline") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePipeline where
  toPath DescribePipeline' {..} =
    Lude.mconcat ["/pipelines/", Lude.toBS pipelineName]

instance Lude.ToQuery DescribePipeline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePipelineResponse' smart constructor.
data DescribePipelineResponse = DescribePipelineResponse'
  { pipeline ::
      Lude.Maybe Pipeline,
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

-- | Creates a value of 'DescribePipelineResponse' with the minimum fields required to make a request.
--
-- * 'pipeline' - A @Pipeline@ object that contains information about the pipeline.
-- * 'responseStatus' - The response status code.
mkDescribePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePipelineResponse
mkDescribePipelineResponse pResponseStatus_ =
  DescribePipelineResponse'
    { pipeline = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @Pipeline@ object that contains information about the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsPipeline :: Lens.Lens' DescribePipelineResponse (Lude.Maybe Pipeline)
dprsPipeline = Lens.lens (pipeline :: DescribePipelineResponse -> Lude.Maybe Pipeline) (\s a -> s {pipeline = a} :: DescribePipelineResponse)
{-# DEPRECATED dprsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribePipelineResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePipelineResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
