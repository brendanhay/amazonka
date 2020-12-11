{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.StartPipelineReprocessing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the reprocessing of raw message data through the pipeline.
module Network.AWS.IoTAnalytics.StartPipelineReprocessing
  ( -- * Creating a request
    StartPipelineReprocessing (..),
    mkStartPipelineReprocessing,

    -- ** Request lenses
    sprStartTime,
    sprEndTime,
    sprPipelineName,

    -- * Destructuring the response
    StartPipelineReprocessingResponse (..),
    mkStartPipelineReprocessingResponse,

    -- ** Response lenses
    sprrsReprocessingId,
    sprrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartPipelineReprocessing' smart constructor.
data StartPipelineReprocessing = StartPipelineReprocessing'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    endTime :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'StartPipelineReprocessing' with the minimum fields required to make a request.
--
-- * 'endTime' - The end time (exclusive) of raw message data that is reprocessed.
-- * 'pipelineName' - The name of the pipeline on which to start reprocessing.
-- * 'startTime' - The start time (inclusive) of raw message data that is reprocessed.
mkStartPipelineReprocessing ::
  -- | 'pipelineName'
  Lude.Text ->
  StartPipelineReprocessing
mkStartPipelineReprocessing pPipelineName_ =
  StartPipelineReprocessing'
    { startTime = Lude.Nothing,
      endTime = Lude.Nothing,
      pipelineName = pPipelineName_
    }

-- | The start time (inclusive) of raw message data that is reprocessed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprStartTime :: Lens.Lens' StartPipelineReprocessing (Lude.Maybe Lude.Timestamp)
sprStartTime = Lens.lens (startTime :: StartPipelineReprocessing -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: StartPipelineReprocessing)
{-# DEPRECATED sprStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end time (exclusive) of raw message data that is reprocessed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprEndTime :: Lens.Lens' StartPipelineReprocessing (Lude.Maybe Lude.Timestamp)
sprEndTime = Lens.lens (endTime :: StartPipelineReprocessing -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: StartPipelineReprocessing)
{-# DEPRECATED sprEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the pipeline on which to start reprocessing.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprPipelineName :: Lens.Lens' StartPipelineReprocessing Lude.Text
sprPipelineName = Lens.lens (pipelineName :: StartPipelineReprocessing -> Lude.Text) (\s a -> s {pipelineName = a} :: StartPipelineReprocessing)
{-# DEPRECATED sprPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

instance Lude.AWSRequest StartPipelineReprocessing where
  type
    Rs StartPipelineReprocessing =
      StartPipelineReprocessingResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartPipelineReprocessingResponse'
            Lude.<$> (x Lude..?> "reprocessingId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartPipelineReprocessing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StartPipelineReprocessing where
  toJSON StartPipelineReprocessing' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("startTime" Lude..=) Lude.<$> startTime,
            ("endTime" Lude..=) Lude.<$> endTime
          ]
      )

instance Lude.ToPath StartPipelineReprocessing where
  toPath StartPipelineReprocessing' {..} =
    Lude.mconcat
      ["/pipelines/", Lude.toBS pipelineName, "/reprocessing"]

instance Lude.ToQuery StartPipelineReprocessing where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartPipelineReprocessingResponse' smart constructor.
data StartPipelineReprocessingResponse = StartPipelineReprocessingResponse'
  { reprocessingId ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartPipelineReprocessingResponse' with the minimum fields required to make a request.
--
-- * 'reprocessingId' - The ID of the pipeline reprocessing activity that was started.
-- * 'responseStatus' - The response status code.
mkStartPipelineReprocessingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartPipelineReprocessingResponse
mkStartPipelineReprocessingResponse pResponseStatus_ =
  StartPipelineReprocessingResponse'
    { reprocessingId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the pipeline reprocessing activity that was started.
--
-- /Note:/ Consider using 'reprocessingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsReprocessingId :: Lens.Lens' StartPipelineReprocessingResponse (Lude.Maybe Lude.Text)
sprrsReprocessingId = Lens.lens (reprocessingId :: StartPipelineReprocessingResponse -> Lude.Maybe Lude.Text) (\s a -> s {reprocessingId = a} :: StartPipelineReprocessingResponse)
{-# DEPRECATED sprrsReprocessingId "Use generic-lens or generic-optics with 'reprocessingId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsResponseStatus :: Lens.Lens' StartPipelineReprocessingResponse Lude.Int
sprrsResponseStatus = Lens.lens (responseStatus :: StartPipelineReprocessingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartPipelineReprocessingResponse)
{-# DEPRECATED sprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
