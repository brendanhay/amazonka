{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetPipelineState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the state of a pipeline, including the stages and actions.
module Network.AWS.CodePipeline.GetPipelineState
  ( -- * Creating a request
    GetPipelineState (..),
    mkGetPipelineState,

    -- ** Request lenses
    gpsName,

    -- * Destructuring the response
    GetPipelineStateResponse (..),
    mkGetPipelineStateResponse,

    -- ** Response lenses
    gpsrsPipelineName,
    gpsrsCreated,
    gpsrsStageStates,
    gpsrsPipelineVersion,
    gpsrsUpdated,
    gpsrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetPipelineState@ action.
--
-- /See:/ 'mkGetPipelineState' smart constructor.
newtype GetPipelineState = GetPipelineState' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPipelineState' with the minimum fields required to make a request.
--
-- * 'name' - The name of the pipeline about which you want to get information.
mkGetPipelineState ::
  -- | 'name'
  Lude.Text ->
  GetPipelineState
mkGetPipelineState pName_ = GetPipelineState' {name = pName_}

-- | The name of the pipeline about which you want to get information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsName :: Lens.Lens' GetPipelineState Lude.Text
gpsName = Lens.lens (name :: GetPipelineState -> Lude.Text) (\s a -> s {name = a} :: GetPipelineState)
{-# DEPRECATED gpsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetPipelineState where
  type Rs GetPipelineState = GetPipelineStateResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPipelineStateResponse'
            Lude.<$> (x Lude..?> "pipelineName")
            Lude.<*> (x Lude..?> "created")
            Lude.<*> (x Lude..?> "stageStates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "pipelineVersion")
            Lude.<*> (x Lude..?> "updated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPipelineState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.GetPipelineState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPipelineState where
  toJSON GetPipelineState' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

instance Lude.ToPath GetPipelineState where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPipelineState where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetPipelineState@ action.
--
-- /See:/ 'mkGetPipelineStateResponse' smart constructor.
data GetPipelineStateResponse = GetPipelineStateResponse'
  { pipelineName ::
      Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    stageStates :: Lude.Maybe [StageState],
    pipelineVersion ::
      Lude.Maybe Lude.Natural,
    updated :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'GetPipelineStateResponse' with the minimum fields required to make a request.
--
-- * 'created' - The date and time the pipeline was created, in timestamp format.
-- * 'pipelineName' - The name of the pipeline for which you want to get the state.
-- * 'pipelineVersion' - The version number of the pipeline.
-- * 'responseStatus' - The response status code.
-- * 'stageStates' - A list of the pipeline stage output information, including stage name, state, most recent run details, whether the stage is disabled, and other data.
-- * 'updated' - The date and time the pipeline was last updated, in timestamp format.
mkGetPipelineStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPipelineStateResponse
mkGetPipelineStateResponse pResponseStatus_ =
  GetPipelineStateResponse'
    { pipelineName = Lude.Nothing,
      created = Lude.Nothing,
      stageStates = Lude.Nothing,
      pipelineVersion = Lude.Nothing,
      updated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the pipeline for which you want to get the state.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsPipelineName :: Lens.Lens' GetPipelineStateResponse (Lude.Maybe Lude.Text)
gpsrsPipelineName = Lens.lens (pipelineName :: GetPipelineStateResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineName = a} :: GetPipelineStateResponse)
{-# DEPRECATED gpsrsPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The date and time the pipeline was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsCreated :: Lens.Lens' GetPipelineStateResponse (Lude.Maybe Lude.Timestamp)
gpsrsCreated = Lens.lens (created :: GetPipelineStateResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: GetPipelineStateResponse)
{-# DEPRECATED gpsrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | A list of the pipeline stage output information, including stage name, state, most recent run details, whether the stage is disabled, and other data.
--
-- /Note:/ Consider using 'stageStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsStageStates :: Lens.Lens' GetPipelineStateResponse (Lude.Maybe [StageState])
gpsrsStageStates = Lens.lens (stageStates :: GetPipelineStateResponse -> Lude.Maybe [StageState]) (\s a -> s {stageStates = a} :: GetPipelineStateResponse)
{-# DEPRECATED gpsrsStageStates "Use generic-lens or generic-optics with 'stageStates' instead." #-}

-- | The version number of the pipeline.
--
-- /Note:/ Consider using 'pipelineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsPipelineVersion :: Lens.Lens' GetPipelineStateResponse (Lude.Maybe Lude.Natural)
gpsrsPipelineVersion = Lens.lens (pipelineVersion :: GetPipelineStateResponse -> Lude.Maybe Lude.Natural) (\s a -> s {pipelineVersion = a} :: GetPipelineStateResponse)
{-# DEPRECATED gpsrsPipelineVersion "Use generic-lens or generic-optics with 'pipelineVersion' instead." #-}

-- | The date and time the pipeline was last updated, in timestamp format.
--
-- /Note:/ Consider using 'updated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsUpdated :: Lens.Lens' GetPipelineStateResponse (Lude.Maybe Lude.Timestamp)
gpsrsUpdated = Lens.lens (updated :: GetPipelineStateResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {updated = a} :: GetPipelineStateResponse)
{-# DEPRECATED gpsrsUpdated "Use generic-lens or generic-optics with 'updated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrsResponseStatus :: Lens.Lens' GetPipelineStateResponse Lude.Int
gpsrsResponseStatus = Lens.lens (responseStatus :: GetPipelineStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPipelineStateResponse)
{-# DEPRECATED gpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
