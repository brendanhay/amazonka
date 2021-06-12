{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.EnableStageTransition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables artifacts in a pipeline to transition to a stage in a pipeline.
module Network.AWS.CodePipeline.EnableStageTransition
  ( -- * Creating a Request
    EnableStageTransition (..),
    newEnableStageTransition,

    -- * Request Lenses
    enableStageTransition_pipelineName,
    enableStageTransition_stageName,
    enableStageTransition_transitionType,

    -- * Destructuring the Response
    EnableStageTransitionResponse (..),
    newEnableStageTransitionResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @EnableStageTransition@ action.
--
-- /See:/ 'newEnableStageTransition' smart constructor.
data EnableStageTransition = EnableStageTransition'
  { -- | The name of the pipeline in which you want to enable the flow of
    -- artifacts from one stage to another.
    pipelineName :: Core.Text,
    -- | The name of the stage where you want to enable the transition of
    -- artifacts, either into the stage (inbound) or from that stage to the
    -- next stage (outbound).
    stageName :: Core.Text,
    -- | Specifies whether artifacts are allowed to enter the stage and be
    -- processed by the actions in that stage (inbound) or whether already
    -- processed artifacts are allowed to transition to the next stage
    -- (outbound).
    transitionType :: StageTransitionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableStageTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'enableStageTransition_pipelineName' - The name of the pipeline in which you want to enable the flow of
-- artifacts from one stage to another.
--
-- 'stageName', 'enableStageTransition_stageName' - The name of the stage where you want to enable the transition of
-- artifacts, either into the stage (inbound) or from that stage to the
-- next stage (outbound).
--
-- 'transitionType', 'enableStageTransition_transitionType' - Specifies whether artifacts are allowed to enter the stage and be
-- processed by the actions in that stage (inbound) or whether already
-- processed artifacts are allowed to transition to the next stage
-- (outbound).
newEnableStageTransition ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'stageName'
  Core.Text ->
  -- | 'transitionType'
  StageTransitionType ->
  EnableStageTransition
newEnableStageTransition
  pPipelineName_
  pStageName_
  pTransitionType_ =
    EnableStageTransition'
      { pipelineName =
          pPipelineName_,
        stageName = pStageName_,
        transitionType = pTransitionType_
      }

-- | The name of the pipeline in which you want to enable the flow of
-- artifacts from one stage to another.
enableStageTransition_pipelineName :: Lens.Lens' EnableStageTransition Core.Text
enableStageTransition_pipelineName = Lens.lens (\EnableStageTransition' {pipelineName} -> pipelineName) (\s@EnableStageTransition' {} a -> s {pipelineName = a} :: EnableStageTransition)

-- | The name of the stage where you want to enable the transition of
-- artifacts, either into the stage (inbound) or from that stage to the
-- next stage (outbound).
enableStageTransition_stageName :: Lens.Lens' EnableStageTransition Core.Text
enableStageTransition_stageName = Lens.lens (\EnableStageTransition' {stageName} -> stageName) (\s@EnableStageTransition' {} a -> s {stageName = a} :: EnableStageTransition)

-- | Specifies whether artifacts are allowed to enter the stage and be
-- processed by the actions in that stage (inbound) or whether already
-- processed artifacts are allowed to transition to the next stage
-- (outbound).
enableStageTransition_transitionType :: Lens.Lens' EnableStageTransition StageTransitionType
enableStageTransition_transitionType = Lens.lens (\EnableStageTransition' {transitionType} -> transitionType) (\s@EnableStageTransition' {} a -> s {transitionType = a} :: EnableStageTransition)

instance Core.AWSRequest EnableStageTransition where
  type
    AWSResponse EnableStageTransition =
      EnableStageTransitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull EnableStageTransitionResponse'

instance Core.Hashable EnableStageTransition

instance Core.NFData EnableStageTransition

instance Core.ToHeaders EnableStageTransition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.EnableStageTransition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableStageTransition where
  toJSON EnableStageTransition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("stageName" Core..= stageName),
            Core.Just ("transitionType" Core..= transitionType)
          ]
      )

instance Core.ToPath EnableStageTransition where
  toPath = Core.const "/"

instance Core.ToQuery EnableStageTransition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableStageTransitionResponse' smart constructor.
data EnableStageTransitionResponse = EnableStageTransitionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableStageTransitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableStageTransitionResponse ::
  EnableStageTransitionResponse
newEnableStageTransitionResponse =
  EnableStageTransitionResponse'

instance Core.NFData EnableStageTransitionResponse
