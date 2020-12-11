{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The UpdatePipelineStatus operation pauses or reactivates a pipeline, so that the pipeline stops or restarts the processing of jobs.
--
-- Changing the pipeline status is useful if you want to cancel one or more jobs. You can't cancel jobs after Elastic Transcoder has started processing them; if you pause the pipeline to which you submitted the jobs, you have more time to get the job IDs for the jobs that you want to cancel, and to send a 'CancelJob' request.
module Network.AWS.ElasticTranscoder.UpdatePipelineStatus
  ( -- * Creating a request
    UpdatePipelineStatus (..),
    mkUpdatePipelineStatus,

    -- ** Request lenses
    upsId,
    upsStatus,

    -- * Destructuring the response
    UpdatePipelineStatusResponse (..),
    mkUpdatePipelineStatusResponse,

    -- ** Response lenses
    upsrsPipeline,
    upsrsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @UpdatePipelineStatusRequest@ structure.
--
-- /See:/ 'mkUpdatePipelineStatus' smart constructor.
data UpdatePipelineStatus = UpdatePipelineStatus'
  { id :: Lude.Text,
    status :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipelineStatus' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the pipeline to update.
-- * 'status' - The desired status of the pipeline:
--
--
--     * @Active@ : The pipeline is processing jobs.
--
--
--     * @Paused@ : The pipeline is not currently processing jobs.
mkUpdatePipelineStatus ::
  -- | 'id'
  Lude.Text ->
  -- | 'status'
  Lude.Text ->
  UpdatePipelineStatus
mkUpdatePipelineStatus pId_ pStatus_ =
  UpdatePipelineStatus' {id = pId_, status = pStatus_}

-- | The identifier of the pipeline to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsId :: Lens.Lens' UpdatePipelineStatus Lude.Text
upsId = Lens.lens (id :: UpdatePipelineStatus -> Lude.Text) (\s a -> s {id = a} :: UpdatePipelineStatus)
{-# DEPRECATED upsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The desired status of the pipeline:
--
--
--     * @Active@ : The pipeline is processing jobs.
--
--
--     * @Paused@ : The pipeline is not currently processing jobs.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsStatus :: Lens.Lens' UpdatePipelineStatus Lude.Text
upsStatus = Lens.lens (status :: UpdatePipelineStatus -> Lude.Text) (\s a -> s {status = a} :: UpdatePipelineStatus)
{-# DEPRECATED upsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.AWSRequest UpdatePipelineStatus where
  type Rs UpdatePipelineStatus = UpdatePipelineStatusResponse
  request = Req.postJSON elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePipelineStatusResponse'
            Lude.<$> (x Lude..?> "Pipeline") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePipelineStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdatePipelineStatus where
  toJSON UpdatePipelineStatus' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Status" Lude..= status)])

instance Lude.ToPath UpdatePipelineStatus where
  toPath UpdatePipelineStatus' {..} =
    Lude.mconcat ["/2012-09-25/pipelines/", Lude.toBS id, "/status"]

instance Lude.ToQuery UpdatePipelineStatus where
  toQuery = Lude.const Lude.mempty

-- | When you update status for a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
-- /See:/ 'mkUpdatePipelineStatusResponse' smart constructor.
data UpdatePipelineStatusResponse = UpdatePipelineStatusResponse'
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

-- | Creates a value of 'UpdatePipelineStatusResponse' with the minimum fields required to make a request.
--
-- * 'pipeline' - A section of the response body that provides information about the pipeline.
-- * 'responseStatus' - The response status code.
mkUpdatePipelineStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePipelineStatusResponse
mkUpdatePipelineStatusResponse pResponseStatus_ =
  UpdatePipelineStatusResponse'
    { pipeline = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A section of the response body that provides information about the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrsPipeline :: Lens.Lens' UpdatePipelineStatusResponse (Lude.Maybe Pipeline)
upsrsPipeline = Lens.lens (pipeline :: UpdatePipelineStatusResponse -> Lude.Maybe Pipeline) (\s a -> s {pipeline = a} :: UpdatePipelineStatusResponse)
{-# DEPRECATED upsrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrsResponseStatus :: Lens.Lens' UpdatePipelineStatusResponse Lude.Int
upsrsResponseStatus = Lens.lens (responseStatus :: UpdatePipelineStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePipelineStatusResponse)
{-# DEPRECATED upsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
