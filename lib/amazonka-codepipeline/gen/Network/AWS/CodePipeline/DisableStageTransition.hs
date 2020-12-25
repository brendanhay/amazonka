{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisableStageTransition (..),
    mkDisableStageTransition,

    -- ** Request lenses
    dstPipelineName,
    dstStageName,
    dstTransitionType,
    dstReason,

    -- * Destructuring the response
    DisableStageTransitionResponse (..),
    mkDisableStageTransitionResponse,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DisableStageTransition@ action.
--
-- /See:/ 'mkDisableStageTransition' smart constructor.
data DisableStageTransition = DisableStageTransition'
  { -- | The name of the pipeline in which you want to disable the flow of artifacts from one stage to another.
    pipelineName :: Types.PipelineName,
    -- | The name of the stage where you want to disable the inbound or outbound transition of artifacts.
    stageName :: Types.StageName,
    -- | Specifies whether artifacts are prevented from transitioning into the stage and being processed by the actions in that stage (inbound), or prevented from transitioning from the stage after they have been processed by the actions in that stage (outbound).
    transitionType :: Types.StageTransitionType,
    -- | The reason given to the user that a stage is disabled, such as waiting for manual approval or manual tests. This message is displayed in the pipeline console UI.
    reason :: Types.DisabledReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableStageTransition' value with any optional fields omitted.
mkDisableStageTransition ::
  -- | 'pipelineName'
  Types.PipelineName ->
  -- | 'stageName'
  Types.StageName ->
  -- | 'transitionType'
  Types.StageTransitionType ->
  -- | 'reason'
  Types.DisabledReason ->
  DisableStageTransition
mkDisableStageTransition
  pipelineName
  stageName
  transitionType
  reason =
    DisableStageTransition'
      { pipelineName,
        stageName,
        transitionType,
        reason
      }

-- | The name of the pipeline in which you want to disable the flow of artifacts from one stage to another.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstPipelineName :: Lens.Lens' DisableStageTransition Types.PipelineName
dstPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED dstPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the stage where you want to disable the inbound or outbound transition of artifacts.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstStageName :: Lens.Lens' DisableStageTransition Types.StageName
dstStageName = Lens.field @"stageName"
{-# DEPRECATED dstStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | Specifies whether artifacts are prevented from transitioning into the stage and being processed by the actions in that stage (inbound), or prevented from transitioning from the stage after they have been processed by the actions in that stage (outbound).
--
-- /Note:/ Consider using 'transitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstTransitionType :: Lens.Lens' DisableStageTransition Types.StageTransitionType
dstTransitionType = Lens.field @"transitionType"
{-# DEPRECATED dstTransitionType "Use generic-lens or generic-optics with 'transitionType' instead." #-}

-- | The reason given to the user that a stage is disabled, such as waiting for manual approval or manual tests. This message is displayed in the pipeline console UI.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstReason :: Lens.Lens' DisableStageTransition Types.DisabledReason
dstReason = Lens.field @"reason"
{-# DEPRECATED dstReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON DisableStageTransition where
  toJSON DisableStageTransition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("stageName" Core..= stageName),
            Core.Just ("transitionType" Core..= transitionType),
            Core.Just ("reason" Core..= reason)
          ]
      )

instance Core.AWSRequest DisableStageTransition where
  type Rs DisableStageTransition = DisableStageTransitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.DisableStageTransition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DisableStageTransitionResponse'

-- | /See:/ 'mkDisableStageTransitionResponse' smart constructor.
data DisableStageTransitionResponse = DisableStageTransitionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableStageTransitionResponse' value with any optional fields omitted.
mkDisableStageTransitionResponse ::
  DisableStageTransitionResponse
mkDisableStageTransitionResponse = DisableStageTransitionResponse'
