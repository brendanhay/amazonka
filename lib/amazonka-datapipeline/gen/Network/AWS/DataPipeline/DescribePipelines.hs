{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DescribePipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about one or more pipelines. The information retrieved includes the name of the pipeline, the pipeline identifier, its current state, and the user account that owns the pipeline. Using account credentials, you can retrieve metadata about pipelines that you or your IAM users have created. If you are using an IAM user account, you can retrieve metadata about only those pipelines for which you have read permissions.
--
-- To retrieve the full pipeline definition instead of metadata about the pipeline, call 'GetPipelineDefinition' .
module Network.AWS.DataPipeline.DescribePipelines
  ( -- * Creating a request
    DescribePipelines (..),
    mkDescribePipelines,

    -- ** Request lenses
    dpPipelineIds,

    -- * Destructuring the response
    DescribePipelinesResponse (..),
    mkDescribePipelinesResponse,

    -- ** Response lenses
    dprsResponseStatus,
    dprsPipelineDescriptionList,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribePipelines.
--
-- /See:/ 'mkDescribePipelines' smart constructor.
newtype DescribePipelines = DescribePipelines'
  { pipelineIds ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePipelines' with the minimum fields required to make a request.
--
-- * 'pipelineIds' - The IDs of the pipelines to describe. You can pass as many as 25 identifiers in a single call. To obtain pipeline IDs, call 'ListPipelines' .
mkDescribePipelines ::
  DescribePipelines
mkDescribePipelines = DescribePipelines' {pipelineIds = Lude.mempty}

-- | The IDs of the pipelines to describe. You can pass as many as 25 identifiers in a single call. To obtain pipeline IDs, call 'ListPipelines' .
--
-- /Note:/ Consider using 'pipelineIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPipelineIds :: Lens.Lens' DescribePipelines [Lude.Text]
dpPipelineIds = Lens.lens (pipelineIds :: DescribePipelines -> [Lude.Text]) (\s a -> s {pipelineIds = a} :: DescribePipelines)
{-# DEPRECATED dpPipelineIds "Use generic-lens or generic-optics with 'pipelineIds' instead." #-}

instance Lude.AWSRequest DescribePipelines where
  type Rs DescribePipelines = DescribePipelinesResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePipelinesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "pipelineDescriptionList" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribePipelines where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.DescribePipelines" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePipelines where
  toJSON DescribePipelines' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("pipelineIds" Lude..= pipelineIds)])

instance Lude.ToPath DescribePipelines where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePipelines where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of DescribePipelines.
--
-- /See:/ 'mkDescribePipelinesResponse' smart constructor.
data DescribePipelinesResponse = DescribePipelinesResponse'
  { responseStatus ::
      Lude.Int,
    pipelineDescriptionList ::
      [PipelineDescription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePipelinesResponse' with the minimum fields required to make a request.
--
-- * 'pipelineDescriptionList' - An array of descriptions for the specified pipelines.
-- * 'responseStatus' - The response status code.
mkDescribePipelinesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePipelinesResponse
mkDescribePipelinesResponse pResponseStatus_ =
  DescribePipelinesResponse'
    { responseStatus = pResponseStatus_,
      pipelineDescriptionList = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribePipelinesResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribePipelinesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePipelinesResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array of descriptions for the specified pipelines.
--
-- /Note:/ Consider using 'pipelineDescriptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsPipelineDescriptionList :: Lens.Lens' DescribePipelinesResponse [PipelineDescription]
dprsPipelineDescriptionList = Lens.lens (pipelineDescriptionList :: DescribePipelinesResponse -> [PipelineDescription]) (\s a -> s {pipelineDescriptionList = a} :: DescribePipelinesResponse)
{-# DEPRECATED dprsPipelineDescriptionList "Use generic-lens or generic-optics with 'pipelineDescriptionList' instead." #-}
