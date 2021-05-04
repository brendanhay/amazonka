{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.DisableStageTransition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Prevents artifacts in a pipeline from transitioning to the next stage in
-- the pipeline.
module Network.AWS.CodePipeline.DisableStageTransition
  ( -- * Creating a Request
    DisableStageTransition (..),
    newDisableStageTransition,

    -- * Request Lenses
    disableStageTransition_pipelineName,
    disableStageTransition_stageName,
    disableStageTransition_transitionType,
    disableStageTransition_reason,

    -- * Destructuring the Response
    DisableStageTransitionResponse (..),
    newDisableStageTransitionResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DisableStageTransition@ action.
--
-- /See:/ 'newDisableStageTransition' smart constructor.
data DisableStageTransition = DisableStageTransition'
  { -- | The name of the pipeline in which you want to disable the flow of
    -- artifacts from one stage to another.
    pipelineName :: Prelude.Text,
    -- | The name of the stage where you want to disable the inbound or outbound
    -- transition of artifacts.
    stageName :: Prelude.Text,
    -- | Specifies whether artifacts are prevented from transitioning into the
    -- stage and being processed by the actions in that stage (inbound), or
    -- prevented from transitioning from the stage after they have been
    -- processed by the actions in that stage (outbound).
    transitionType :: StageTransitionType,
    -- | The reason given to the user that a stage is disabled, such as waiting
    -- for manual approval or manual tests. This message is displayed in the
    -- pipeline console UI.
    reason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableStageTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'disableStageTransition_pipelineName' - The name of the pipeline in which you want to disable the flow of
-- artifacts from one stage to another.
--
-- 'stageName', 'disableStageTransition_stageName' - The name of the stage where you want to disable the inbound or outbound
-- transition of artifacts.
--
-- 'transitionType', 'disableStageTransition_transitionType' - Specifies whether artifacts are prevented from transitioning into the
-- stage and being processed by the actions in that stage (inbound), or
-- prevented from transitioning from the stage after they have been
-- processed by the actions in that stage (outbound).
--
-- 'reason', 'disableStageTransition_reason' - The reason given to the user that a stage is disabled, such as waiting
-- for manual approval or manual tests. This message is displayed in the
-- pipeline console UI.
newDisableStageTransition ::
  -- | 'pipelineName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'transitionType'
  StageTransitionType ->
  -- | 'reason'
  Prelude.Text ->
  DisableStageTransition
newDisableStageTransition
  pPipelineName_
  pStageName_
  pTransitionType_
  pReason_ =
    DisableStageTransition'
      { pipelineName =
          pPipelineName_,
        stageName = pStageName_,
        transitionType = pTransitionType_,
        reason = pReason_
      }

-- | The name of the pipeline in which you want to disable the flow of
-- artifacts from one stage to another.
disableStageTransition_pipelineName :: Lens.Lens' DisableStageTransition Prelude.Text
disableStageTransition_pipelineName = Lens.lens (\DisableStageTransition' {pipelineName} -> pipelineName) (\s@DisableStageTransition' {} a -> s {pipelineName = a} :: DisableStageTransition)

-- | The name of the stage where you want to disable the inbound or outbound
-- transition of artifacts.
disableStageTransition_stageName :: Lens.Lens' DisableStageTransition Prelude.Text
disableStageTransition_stageName = Lens.lens (\DisableStageTransition' {stageName} -> stageName) (\s@DisableStageTransition' {} a -> s {stageName = a} :: DisableStageTransition)

-- | Specifies whether artifacts are prevented from transitioning into the
-- stage and being processed by the actions in that stage (inbound), or
-- prevented from transitioning from the stage after they have been
-- processed by the actions in that stage (outbound).
disableStageTransition_transitionType :: Lens.Lens' DisableStageTransition StageTransitionType
disableStageTransition_transitionType = Lens.lens (\DisableStageTransition' {transitionType} -> transitionType) (\s@DisableStageTransition' {} a -> s {transitionType = a} :: DisableStageTransition)

-- | The reason given to the user that a stage is disabled, such as waiting
-- for manual approval or manual tests. This message is displayed in the
-- pipeline console UI.
disableStageTransition_reason :: Lens.Lens' DisableStageTransition Prelude.Text
disableStageTransition_reason = Lens.lens (\DisableStageTransition' {reason} -> reason) (\s@DisableStageTransition' {} a -> s {reason = a} :: DisableStageTransition)

instance Prelude.AWSRequest DisableStageTransition where
  type
    Rs DisableStageTransition =
      DisableStageTransitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisableStageTransitionResponse'

instance Prelude.Hashable DisableStageTransition

instance Prelude.NFData DisableStageTransition

instance Prelude.ToHeaders DisableStageTransition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.DisableStageTransition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableStageTransition where
  toJSON DisableStageTransition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pipelineName" Prelude..= pipelineName),
            Prelude.Just ("stageName" Prelude..= stageName),
            Prelude.Just
              ("transitionType" Prelude..= transitionType),
            Prelude.Just ("reason" Prelude..= reason)
          ]
      )

instance Prelude.ToPath DisableStageTransition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableStageTransition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableStageTransitionResponse' smart constructor.
data DisableStageTransitionResponse = DisableStageTransitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableStageTransitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableStageTransitionResponse ::
  DisableStageTransitionResponse
newDisableStageTransitionResponse =
  DisableStageTransitionResponse'

instance
  Prelude.NFData
    DisableStageTransitionResponse
