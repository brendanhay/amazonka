{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline. A pipeline consumes messages from a channel and allows you to process the messages before storing them in a data store. You must specify both a @channel@ and a @datastore@ activity and, optionally, as many as 23 additional activities in the @pipelineActivities@ array.
module Network.AWS.IoTAnalytics.CreatePipeline
  ( -- * Creating a request
    CreatePipeline (..),
    mkCreatePipeline,

    -- ** Request lenses
    cpPipelineName,
    cpPipelineActivities,
    cpTags,

    -- * Destructuring the response
    CreatePipelineResponse (..),
    mkCreatePipelineResponse,

    -- ** Response lenses
    cprsPipelineName,
    cprsPipelineARN,
    cprsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | The name of the pipeline.
    pipelineName :: Lude.Text,
    -- | A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
    --
    -- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
    -- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
    pipelineActivities :: Lude.NonEmpty PipelineActivity,
    -- | Metadata which can be used to manage the pipeline.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePipeline' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline.
-- * 'pipelineActivities' - A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
--
-- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
-- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
-- * 'tags' - Metadata which can be used to manage the pipeline.
mkCreatePipeline ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'pipelineActivities'
  Lude.NonEmpty PipelineActivity ->
  CreatePipeline
mkCreatePipeline pPipelineName_ pPipelineActivities_ =
  CreatePipeline'
    { pipelineName = pPipelineName_,
      pipelineActivities = pPipelineActivities_,
      tags = Lude.Nothing
    }

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPipelineName :: Lens.Lens' CreatePipeline Lude.Text
cpPipelineName = Lens.lens (pipelineName :: CreatePipeline -> Lude.Text) (\s a -> s {pipelineName = a} :: CreatePipeline)
{-# DEPRECATED cpPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
--
-- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
-- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
--
-- /Note:/ Consider using 'pipelineActivities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPipelineActivities :: Lens.Lens' CreatePipeline (Lude.NonEmpty PipelineActivity)
cpPipelineActivities = Lens.lens (pipelineActivities :: CreatePipeline -> Lude.NonEmpty PipelineActivity) (\s a -> s {pipelineActivities = a} :: CreatePipeline)
{-# DEPRECATED cpPipelineActivities "Use generic-lens or generic-optics with 'pipelineActivities' instead." #-}

-- | Metadata which can be used to manage the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePipeline (Lude.Maybe (Lude.NonEmpty Tag))
cpTags = Lens.lens (tags :: CreatePipeline -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreatePipeline)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Lude.<$> (x Lude..?> "pipelineName")
            Lude.<*> (x Lude..?> "pipelineArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            Lude.Just ("pipelineActivities" Lude..= pipelineActivities),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreatePipeline where
  toPath = Lude.const "/pipelines"

instance Lude.ToQuery CreatePipeline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | The name of the pipeline.
    pipelineName :: Lude.Maybe Lude.Text,
    -- | The ARN of the pipeline.
    pipelineARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline.
-- * 'pipelineARN' - The ARN of the pipeline.
-- * 'responseStatus' - The response status code.
mkCreatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePipelineResponse
mkCreatePipelineResponse pResponseStatus_ =
  CreatePipelineResponse'
    { pipelineName = Lude.Nothing,
      pipelineARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPipelineName :: Lens.Lens' CreatePipelineResponse (Lude.Maybe Lude.Text)
cprsPipelineName = Lens.lens (pipelineName :: CreatePipelineResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineName = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The ARN of the pipeline.
--
-- /Note:/ Consider using 'pipelineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPipelineARN :: Lens.Lens' CreatePipelineResponse (Lude.Maybe Lude.Text)
cprsPipelineARN = Lens.lens (pipelineARN :: CreatePipelineResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineARN = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsPipelineARN "Use generic-lens or generic-optics with 'pipelineARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePipelineResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePipelineResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
