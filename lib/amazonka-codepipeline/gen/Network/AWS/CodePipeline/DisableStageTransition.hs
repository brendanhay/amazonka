{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DisableStageTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Prevents artifacts in a pipeline from transitioning to the next stage in the pipeline.
module Network.AWS.CodePipeline.DisableStageTransition
    (
    -- * Creating a request
      DisableStageTransition (..)
    , mkDisableStageTransition
    -- ** Request lenses
    , dstPipelineName
    , dstStageName
    , dstTransitionType
    , dstReason

    -- * Destructuring the response
    , DisableStageTransitionResponse (..)
    , mkDisableStageTransitionResponse
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DisableStageTransition@ action.
--
-- /See:/ 'mkDisableStageTransition' smart constructor.
data DisableStageTransition = DisableStageTransition'
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline in which you want to disable the flow of artifacts from one stage to another.
  , stageName :: Types.StageName
    -- ^ The name of the stage where you want to disable the inbound or outbound transition of artifacts.
  , transitionType :: Types.StageTransitionType
    -- ^ Specifies whether artifacts are prevented from transitioning into the stage and being processed by the actions in that stage (inbound), or prevented from transitioning from the stage after they have been processed by the actions in that stage (outbound).
  , reason :: Types.DisabledReason
    -- ^ The reason given to the user that a stage is disabled, such as waiting for manual approval or manual tests. This message is displayed in the pipeline console UI.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableStageTransition' value with any optional fields omitted.
mkDisableStageTransition
    :: Types.PipelineName -- ^ 'pipelineName'
    -> Types.StageName -- ^ 'stageName'
    -> Types.StageTransitionType -- ^ 'transitionType'
    -> Types.DisabledReason -- ^ 'reason'
    -> DisableStageTransition
mkDisableStageTransition pipelineName stageName transitionType
  reason
  = DisableStageTransition'{pipelineName, stageName, transitionType,
                            reason}

-- | The name of the pipeline in which you want to disable the flow of artifacts from one stage to another.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstPipelineName :: Lens.Lens' DisableStageTransition Types.PipelineName
dstPipelineName = Lens.field @"pipelineName"
{-# INLINEABLE dstPipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The name of the stage where you want to disable the inbound or outbound transition of artifacts.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstStageName :: Lens.Lens' DisableStageTransition Types.StageName
dstStageName = Lens.field @"stageName"
{-# INLINEABLE dstStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | Specifies whether artifacts are prevented from transitioning into the stage and being processed by the actions in that stage (inbound), or prevented from transitioning from the stage after they have been processed by the actions in that stage (outbound).
--
-- /Note:/ Consider using 'transitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstTransitionType :: Lens.Lens' DisableStageTransition Types.StageTransitionType
dstTransitionType = Lens.field @"transitionType"
{-# INLINEABLE dstTransitionType #-}
{-# DEPRECATED transitionType "Use generic-lens or generic-optics with 'transitionType' instead"  #-}

-- | The reason given to the user that a stage is disabled, such as waiting for manual approval or manual tests. This message is displayed in the pipeline console UI.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstReason :: Lens.Lens' DisableStageTransition Types.DisabledReason
dstReason = Lens.field @"reason"
{-# INLINEABLE dstReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.ToQuery DisableStageTransition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableStageTransition where
        toHeaders DisableStageTransition{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.DisableStageTransition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableStageTransition where
        toJSON DisableStageTransition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineName" Core..= pipelineName),
                  Core.Just ("stageName" Core..= stageName),
                  Core.Just ("transitionType" Core..= transitionType),
                  Core.Just ("reason" Core..= reason)])

instance Core.AWSRequest DisableStageTransition where
        type Rs DisableStageTransition = DisableStageTransitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DisableStageTransitionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableStageTransitionResponse' smart constructor.
data DisableStageTransitionResponse = DisableStageTransitionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableStageTransitionResponse' value with any optional fields omitted.
mkDisableStageTransitionResponse
    :: DisableStageTransitionResponse
mkDisableStageTransitionResponse = DisableStageTransitionResponse'
