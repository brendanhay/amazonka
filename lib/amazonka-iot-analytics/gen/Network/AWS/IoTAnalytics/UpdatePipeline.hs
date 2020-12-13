{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.UpdatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a pipeline. You must specify both a @channel@ and a @datastore@ activity and, optionally, as many as 23 additional activities in the @pipelineActivities@ array.
module Network.AWS.IoTAnalytics.UpdatePipeline
  ( -- * Creating a request
    UpdatePipeline (..),
    mkUpdatePipeline,

    -- ** Request lenses
    upPipelineName,
    upPipelineActivities,

    -- * Destructuring the response
    UpdatePipelineResponse (..),
    mkUpdatePipelineResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | The name of the pipeline to update.
    pipelineName :: Lude.Text,
    -- | A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
    --
    -- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
    -- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
    pipelineActivities :: Lude.NonEmpty PipelineActivity
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipeline' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline to update.
-- * 'pipelineActivities' - A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
--
-- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
-- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
mkUpdatePipeline ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'pipelineActivities'
  Lude.NonEmpty PipelineActivity ->
  UpdatePipeline
mkUpdatePipeline pPipelineName_ pPipelineActivities_ =
  UpdatePipeline'
    { pipelineName = pPipelineName_,
      pipelineActivities = pPipelineActivities_
    }

-- | The name of the pipeline to update.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPipelineName :: Lens.Lens' UpdatePipeline Lude.Text
upPipelineName = Lens.lens (pipelineName :: UpdatePipeline -> Lude.Text) (\s a -> s {pipelineName = a} :: UpdatePipeline)
{-# DEPRECATED upPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
--
-- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
-- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
--
-- /Note:/ Consider using 'pipelineActivities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPipelineActivities :: Lens.Lens' UpdatePipeline (Lude.NonEmpty PipelineActivity)
upPipelineActivities = Lens.lens (pipelineActivities :: UpdatePipeline -> Lude.NonEmpty PipelineActivity) (\s a -> s {pipelineActivities = a} :: UpdatePipeline)
{-# DEPRECATED upPipelineActivities "Use generic-lens or generic-optics with 'pipelineActivities' instead." #-}

instance Lude.AWSRequest UpdatePipeline where
  type Rs UpdatePipeline = UpdatePipelineResponse
  request = Req.putJSON ioTAnalyticsService
  response = Res.receiveNull UpdatePipelineResponse'

instance Lude.ToHeaders UpdatePipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("pipelineActivities" Lude..= pipelineActivities)]
      )

instance Lude.ToPath UpdatePipeline where
  toPath UpdatePipeline' {..} =
    Lude.mconcat ["/pipelines/", Lude.toBS pipelineName]

instance Lude.ToQuery UpdatePipeline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipelineResponse' with the minimum fields required to make a request.
mkUpdatePipelineResponse ::
  UpdatePipelineResponse
mkUpdatePipelineResponse = UpdatePipelineResponse'
