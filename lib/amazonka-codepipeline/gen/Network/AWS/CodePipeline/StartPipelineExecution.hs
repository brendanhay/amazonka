{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.StartPipelineExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified pipeline. Specifically, it begins processing the latest commit to the source location specified as part of the pipeline.
module Network.AWS.CodePipeline.StartPipelineExecution
  ( -- * Creating a request
    StartPipelineExecution (..),
    mkStartPipelineExecution,

    -- ** Request lenses
    speName,
    speClientRequestToken,

    -- * Destructuring the response
    StartPipelineExecutionResponse (..),
    mkStartPipelineExecutionResponse,

    -- ** Response lenses
    sperrsPipelineExecutionId,
    sperrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @StartPipelineExecution@ action.
--
-- /See:/ 'mkStartPipelineExecution' smart constructor.
data StartPipelineExecution = StartPipelineExecution'
  { -- | The name of the pipeline to start.
    name :: Types.PipelineName,
    -- | The system-generated unique ID used to identify a unique execution request.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPipelineExecution' value with any optional fields omitted.
mkStartPipelineExecution ::
  -- | 'name'
  Types.PipelineName ->
  StartPipelineExecution
mkStartPipelineExecution name =
  StartPipelineExecution' {name, clientRequestToken = Core.Nothing}

-- | The name of the pipeline to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speName :: Lens.Lens' StartPipelineExecution Types.PipelineName
speName = Lens.field @"name"
{-# DEPRECATED speName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The system-generated unique ID used to identify a unique execution request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speClientRequestToken :: Lens.Lens' StartPipelineExecution (Core.Maybe Types.ClientRequestToken)
speClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED speClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Core.FromJSON StartPipelineExecution where
  toJSON StartPipelineExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("clientRequestToken" Core..=) Core.<$> clientRequestToken
          ]
      )

instance Core.AWSRequest StartPipelineExecution where
  type Rs StartPipelineExecution = StartPipelineExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.StartPipelineExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipelineExecutionResponse'
            Core.<$> (x Core..:? "pipelineExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @StartPipelineExecution@ action.
--
-- /See:/ 'mkStartPipelineExecutionResponse' smart constructor.
data StartPipelineExecutionResponse = StartPipelineExecutionResponse'
  { -- | The unique system-generated ID of the pipeline execution that was started.
    pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPipelineExecutionResponse' value with any optional fields omitted.
mkStartPipelineExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartPipelineExecutionResponse
mkStartPipelineExecutionResponse responseStatus =
  StartPipelineExecutionResponse'
    { pipelineExecutionId =
        Core.Nothing,
      responseStatus
    }

-- | The unique system-generated ID of the pipeline execution that was started.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sperrsPipelineExecutionId :: Lens.Lens' StartPipelineExecutionResponse (Core.Maybe Types.PipelineExecutionId)
sperrsPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED sperrsPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sperrsResponseStatus :: Lens.Lens' StartPipelineExecutionResponse Core.Int
sperrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sperrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
