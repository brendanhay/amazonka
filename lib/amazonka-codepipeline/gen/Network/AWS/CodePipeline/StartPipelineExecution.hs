{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartPipelineExecution (..)
    , mkStartPipelineExecution
    -- ** Request lenses
    , speName
    , speClientRequestToken

    -- * Destructuring the response
    , StartPipelineExecutionResponse (..)
    , mkStartPipelineExecutionResponse
    -- ** Response lenses
    , sperrsPipelineExecutionId
    , sperrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @StartPipelineExecution@ action.
--
-- /See:/ 'mkStartPipelineExecution' smart constructor.
data StartPipelineExecution = StartPipelineExecution'
  { name :: Types.PipelineName
    -- ^ The name of the pipeline to start.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ The system-generated unique ID used to identify a unique execution request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPipelineExecution' value with any optional fields omitted.
mkStartPipelineExecution
    :: Types.PipelineName -- ^ 'name'
    -> StartPipelineExecution
mkStartPipelineExecution name
  = StartPipelineExecution'{name, clientRequestToken = Core.Nothing}

-- | The name of the pipeline to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speName :: Lens.Lens' StartPipelineExecution Types.PipelineName
speName = Lens.field @"name"
{-# INLINEABLE speName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The system-generated unique ID used to identify a unique execution request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speClientRequestToken :: Lens.Lens' StartPipelineExecution (Core.Maybe Types.ClientRequestToken)
speClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE speClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

instance Core.ToQuery StartPipelineExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartPipelineExecution where
        toHeaders StartPipelineExecution{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.StartPipelineExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartPipelineExecution where
        toJSON StartPipelineExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken])

instance Core.AWSRequest StartPipelineExecution where
        type Rs StartPipelineExecution = StartPipelineExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartPipelineExecutionResponse' Core.<$>
                   (x Core..:? "pipelineExecutionId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @StartPipelineExecution@ action.
--
-- /See:/ 'mkStartPipelineExecutionResponse' smart constructor.
data StartPipelineExecutionResponse = StartPipelineExecutionResponse'
  { pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId
    -- ^ The unique system-generated ID of the pipeline execution that was started.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPipelineExecutionResponse' value with any optional fields omitted.
mkStartPipelineExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartPipelineExecutionResponse
mkStartPipelineExecutionResponse responseStatus
  = StartPipelineExecutionResponse'{pipelineExecutionId =
                                      Core.Nothing,
                                    responseStatus}

-- | The unique system-generated ID of the pipeline execution that was started.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sperrsPipelineExecutionId :: Lens.Lens' StartPipelineExecutionResponse (Core.Maybe Types.PipelineExecutionId)
sperrsPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# INLINEABLE sperrsPipelineExecutionId #-}
{-# DEPRECATED pipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sperrsResponseStatus :: Lens.Lens' StartPipelineExecutionResponse Core.Int
sperrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sperrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
