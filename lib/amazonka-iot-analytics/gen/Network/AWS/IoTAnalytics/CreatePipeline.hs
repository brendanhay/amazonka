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
    cprrsPipelineArn,
    cprrsPipelineName,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | The name of the pipeline.
    pipelineName :: Types.PipelineName,
    -- | A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
    --
    -- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
    -- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
    pipelineActivities :: Core.NonEmpty Types.PipelineActivity,
    -- | Metadata which can be used to manage the pipeline.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipeline' value with any optional fields omitted.
mkCreatePipeline ::
  -- | 'pipelineName'
  Types.PipelineName ->
  -- | 'pipelineActivities'
  Core.NonEmpty Types.PipelineActivity ->
  CreatePipeline
mkCreatePipeline pipelineName pipelineActivities =
  CreatePipeline'
    { pipelineName,
      pipelineActivities,
      tags = Core.Nothing
    }

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPipelineName :: Lens.Lens' CreatePipeline Types.PipelineName
cpPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED cpPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | A list of @PipelineActivity@ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
--
-- The list can be 2-25 @PipelineActivity@ objects and must contain both a @channel@ and a @datastore@ activity. Each entry in the list must contain only one activity. For example:
-- @pipelineActivities = [ { "channel": { ... } }, { "lambda": { ... } }, ... ]@
--
-- /Note:/ Consider using 'pipelineActivities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPipelineActivities :: Lens.Lens' CreatePipeline (Core.NonEmpty Types.PipelineActivity)
cpPipelineActivities = Lens.field @"pipelineActivities"
{-# DEPRECATED cpPipelineActivities "Use generic-lens or generic-optics with 'pipelineActivities' instead." #-}

-- | Metadata which can be used to manage the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePipeline (Core.Maybe (Core.NonEmpty Types.Tag))
cpTags = Lens.field @"tags"
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreatePipeline where
  toJSON CreatePipeline {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("pipelineActivities" Core..= pipelineActivities),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/pipelines",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Core.<$> (x Core..:? "pipelineArn")
            Core.<*> (x Core..:? "pipelineName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | The ARN of the pipeline.
    pipelineArn :: Core.Maybe Types.PipelineArn,
    -- | The name of the pipeline.
    pipelineName :: Core.Maybe Types.PipelineName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipelineResponse' value with any optional fields omitted.
mkCreatePipelineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePipelineResponse
mkCreatePipelineResponse responseStatus =
  CreatePipelineResponse'
    { pipelineArn = Core.Nothing,
      pipelineName = Core.Nothing,
      responseStatus
    }

-- | The ARN of the pipeline.
--
-- /Note:/ Consider using 'pipelineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPipelineArn :: Lens.Lens' CreatePipelineResponse (Core.Maybe Types.PipelineArn)
cprrsPipelineArn = Lens.field @"pipelineArn"
{-# DEPRECATED cprrsPipelineArn "Use generic-lens or generic-optics with 'pipelineArn' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPipelineName :: Lens.Lens' CreatePipelineResponse (Core.Maybe Types.PipelineName)
cprrsPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED cprrsPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePipelineResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
