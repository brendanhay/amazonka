{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    EnableStageTransition (..),
    mkEnableStageTransition,

    -- ** Request lenses
    estPipelineName,
    estStageName,
    estTransitionType,

    -- * Destructuring the response
    EnableStageTransitionResponse (..),
    mkEnableStageTransitionResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an @EnableStageTransition@ action.
--
-- /See:/ 'mkEnableStageTransition' smart constructor.
data EnableStageTransition = EnableStageTransition'
  { pipelineName ::
      Lude.Text,
    stageName :: Lude.Text,
    transitionType :: StageTransitionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableStageTransition' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline in which you want to enable the flow of artifacts from one stage to another.
-- * 'stageName' - The name of the stage where you want to enable the transition of artifacts, either into the stage (inbound) or from that stage to the next stage (outbound).
-- * 'transitionType' - Specifies whether artifacts are allowed to enter the stage and be processed by the actions in that stage (inbound) or whether already processed artifacts are allowed to transition to the next stage (outbound).
mkEnableStageTransition ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  -- | 'transitionType'
  StageTransitionType ->
  EnableStageTransition
mkEnableStageTransition pPipelineName_ pStageName_ pTransitionType_ =
  EnableStageTransition'
    { pipelineName = pPipelineName_,
      stageName = pStageName_,
      transitionType = pTransitionType_
    }

-- | The name of the pipeline in which you want to enable the flow of artifacts from one stage to another.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
estPipelineName :: Lens.Lens' EnableStageTransition Lude.Text
estPipelineName = Lens.lens (pipelineName :: EnableStageTransition -> Lude.Text) (\s a -> s {pipelineName = a} :: EnableStageTransition)
{-# DEPRECATED estPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the stage where you want to enable the transition of artifacts, either into the stage (inbound) or from that stage to the next stage (outbound).
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
estStageName :: Lens.Lens' EnableStageTransition Lude.Text
estStageName = Lens.lens (stageName :: EnableStageTransition -> Lude.Text) (\s a -> s {stageName = a} :: EnableStageTransition)
{-# DEPRECATED estStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | Specifies whether artifacts are allowed to enter the stage and be processed by the actions in that stage (inbound) or whether already processed artifacts are allowed to transition to the next stage (outbound).
--
-- /Note:/ Consider using 'transitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
estTransitionType :: Lens.Lens' EnableStageTransition StageTransitionType
estTransitionType = Lens.lens (transitionType :: EnableStageTransition -> StageTransitionType) (\s a -> s {transitionType = a} :: EnableStageTransition)
{-# DEPRECATED estTransitionType "Use generic-lens or generic-optics with 'transitionType' instead." #-}

instance Lude.AWSRequest EnableStageTransition where
  type Rs EnableStageTransition = EnableStageTransitionResponse
  request = Req.postJSON codePipelineService
  response = Res.receiveNull EnableStageTransitionResponse'

instance Lude.ToHeaders EnableStageTransition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.EnableStageTransition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableStageTransition where
  toJSON EnableStageTransition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            Lude.Just ("stageName" Lude..= stageName),
            Lude.Just ("transitionType" Lude..= transitionType)
          ]
      )

instance Lude.ToPath EnableStageTransition where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableStageTransition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableStageTransitionResponse' smart constructor.
data EnableStageTransitionResponse = EnableStageTransitionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableStageTransitionResponse' with the minimum fields required to make a request.
mkEnableStageTransitionResponse ::
  EnableStageTransitionResponse
mkEnableStageTransitionResponse = EnableStageTransitionResponse'
