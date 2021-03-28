{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.EnableStageTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables artifacts in a pipeline to transition to a stage in a pipeline.
module Network.AWS.CodePipeline.EnableStageTransition
    (
    -- * Creating a request
      EnableStageTransition (..)
    , mkEnableStageTransition
    -- ** Request lenses
    , estPipelineName
    , estStageName
    , estTransitionType

    -- * Destructuring the response
    , EnableStageTransitionResponse (..)
    , mkEnableStageTransitionResponse
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @EnableStageTransition@ action.
--
-- /See:/ 'mkEnableStageTransition' smart constructor.
data EnableStageTransition = EnableStageTransition'
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline in which you want to enable the flow of artifacts from one stage to another.
  , stageName :: Types.StageName
    -- ^ The name of the stage where you want to enable the transition of artifacts, either into the stage (inbound) or from that stage to the next stage (outbound).
  , transitionType :: Types.StageTransitionType
    -- ^ Specifies whether artifacts are allowed to enter the stage and be processed by the actions in that stage (inbound) or whether already processed artifacts are allowed to transition to the next stage (outbound).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableStageTransition' value with any optional fields omitted.
mkEnableStageTransition
    :: Types.PipelineName -- ^ 'pipelineName'
    -> Types.StageName -- ^ 'stageName'
    -> Types.StageTransitionType -- ^ 'transitionType'
    -> EnableStageTransition
mkEnableStageTransition pipelineName stageName transitionType
  = EnableStageTransition'{pipelineName, stageName, transitionType}

-- | The name of the pipeline in which you want to enable the flow of artifacts from one stage to another.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
estPipelineName :: Lens.Lens' EnableStageTransition Types.PipelineName
estPipelineName = Lens.field @"pipelineName"
{-# INLINEABLE estPipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The name of the stage where you want to enable the transition of artifacts, either into the stage (inbound) or from that stage to the next stage (outbound).
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
estStageName :: Lens.Lens' EnableStageTransition Types.StageName
estStageName = Lens.field @"stageName"
{-# INLINEABLE estStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | Specifies whether artifacts are allowed to enter the stage and be processed by the actions in that stage (inbound) or whether already processed artifacts are allowed to transition to the next stage (outbound).
--
-- /Note:/ Consider using 'transitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
estTransitionType :: Lens.Lens' EnableStageTransition Types.StageTransitionType
estTransitionType = Lens.field @"transitionType"
{-# INLINEABLE estTransitionType #-}
{-# DEPRECATED transitionType "Use generic-lens or generic-optics with 'transitionType' instead"  #-}

instance Core.ToQuery EnableStageTransition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableStageTransition where
        toHeaders EnableStageTransition{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.EnableStageTransition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableStageTransition where
        toJSON EnableStageTransition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineName" Core..= pipelineName),
                  Core.Just ("stageName" Core..= stageName),
                  Core.Just ("transitionType" Core..= transitionType)])

instance Core.AWSRequest EnableStageTransition where
        type Rs EnableStageTransition = EnableStageTransitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull EnableStageTransitionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableStageTransitionResponse' smart constructor.
data EnableStageTransitionResponse = EnableStageTransitionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableStageTransitionResponse' value with any optional fields omitted.
mkEnableStageTransitionResponse
    :: EnableStageTransitionResponse
mkEnableStageTransitionResponse = EnableStageTransitionResponse'
