{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DisableStageTransition@ action.
--
-- /See:/ 'mkDisableStageTransition' smart constructor.
data DisableStageTransition = DisableStageTransition'
  { pipelineName ::
      Lude.Text,
    stageName :: Lude.Text,
    transitionType :: StageTransitionType,
    reason :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableStageTransition' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline in which you want to disable the flow of artifacts from one stage to another.
-- * 'reason' - The reason given to the user that a stage is disabled, such as waiting for manual approval or manual tests. This message is displayed in the pipeline console UI.
-- * 'stageName' - The name of the stage where you want to disable the inbound or outbound transition of artifacts.
-- * 'transitionType' - Specifies whether artifacts are prevented from transitioning into the stage and being processed by the actions in that stage (inbound), or prevented from transitioning from the stage after they have been processed by the actions in that stage (outbound).
mkDisableStageTransition ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  -- | 'transitionType'
  StageTransitionType ->
  -- | 'reason'
  Lude.Text ->
  DisableStageTransition
mkDisableStageTransition
  pPipelineName_
  pStageName_
  pTransitionType_
  pReason_ =
    DisableStageTransition'
      { pipelineName = pPipelineName_,
        stageName = pStageName_,
        transitionType = pTransitionType_,
        reason = pReason_
      }

-- | The name of the pipeline in which you want to disable the flow of artifacts from one stage to another.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstPipelineName :: Lens.Lens' DisableStageTransition Lude.Text
dstPipelineName = Lens.lens (pipelineName :: DisableStageTransition -> Lude.Text) (\s a -> s {pipelineName = a} :: DisableStageTransition)
{-# DEPRECATED dstPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the stage where you want to disable the inbound or outbound transition of artifacts.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstStageName :: Lens.Lens' DisableStageTransition Lude.Text
dstStageName = Lens.lens (stageName :: DisableStageTransition -> Lude.Text) (\s a -> s {stageName = a} :: DisableStageTransition)
{-# DEPRECATED dstStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | Specifies whether artifacts are prevented from transitioning into the stage and being processed by the actions in that stage (inbound), or prevented from transitioning from the stage after they have been processed by the actions in that stage (outbound).
--
-- /Note:/ Consider using 'transitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstTransitionType :: Lens.Lens' DisableStageTransition StageTransitionType
dstTransitionType = Lens.lens (transitionType :: DisableStageTransition -> StageTransitionType) (\s a -> s {transitionType = a} :: DisableStageTransition)
{-# DEPRECATED dstTransitionType "Use generic-lens or generic-optics with 'transitionType' instead." #-}

-- | The reason given to the user that a stage is disabled, such as waiting for manual approval or manual tests. This message is displayed in the pipeline console UI.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstReason :: Lens.Lens' DisableStageTransition Lude.Text
dstReason = Lens.lens (reason :: DisableStageTransition -> Lude.Text) (\s a -> s {reason = a} :: DisableStageTransition)
{-# DEPRECATED dstReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.AWSRequest DisableStageTransition where
  type Rs DisableStageTransition = DisableStageTransitionResponse
  request = Req.postJSON codePipelineService
  response = Res.receiveNull DisableStageTransitionResponse'

instance Lude.ToHeaders DisableStageTransition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.DisableStageTransition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableStageTransition where
  toJSON DisableStageTransition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            Lude.Just ("stageName" Lude..= stageName),
            Lude.Just ("transitionType" Lude..= transitionType),
            Lude.Just ("reason" Lude..= reason)
          ]
      )

instance Lude.ToPath DisableStageTransition where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableStageTransition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableStageTransitionResponse' smart constructor.
data DisableStageTransitionResponse = DisableStageTransitionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableStageTransitionResponse' with the minimum fields required to make a request.
mkDisableStageTransitionResponse ::
  DisableStageTransitionResponse
mkDisableStageTransitionResponse = DisableStageTransitionResponse'
