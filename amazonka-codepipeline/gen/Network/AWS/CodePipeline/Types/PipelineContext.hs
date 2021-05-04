{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineContext where

import Network.AWS.CodePipeline.Types.ActionContext
import Network.AWS.CodePipeline.Types.StageContext
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about a pipeline to a job worker.
--
-- PipelineContext contains @pipelineArn@ and @pipelineExecutionId@ for
-- custom action jobs. The @pipelineArn@ and @pipelineExecutionId@ fields
-- are not populated for ThirdParty action jobs.
--
-- /See:/ 'newPipelineContext' smart constructor.
data PipelineContext = PipelineContext'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The stage of the pipeline.
    stage :: Prelude.Maybe StageContext,
    -- | The context of an action to a job worker in the stage of a pipeline.
    action :: Prelude.Maybe ActionContext,
    -- | The name of the pipeline. This is a user-specified value. Pipeline names
    -- must be unique across all pipeline names under an Amazon Web Services
    -- account.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The execution ID of the pipeline.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PipelineContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'pipelineContext_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'stage', 'pipelineContext_stage' - The stage of the pipeline.
--
-- 'action', 'pipelineContext_action' - The context of an action to a job worker in the stage of a pipeline.
--
-- 'pipelineName', 'pipelineContext_pipelineName' - The name of the pipeline. This is a user-specified value. Pipeline names
-- must be unique across all pipeline names under an Amazon Web Services
-- account.
--
-- 'pipelineExecutionId', 'pipelineContext_pipelineExecutionId' - The execution ID of the pipeline.
newPipelineContext ::
  PipelineContext
newPipelineContext =
  PipelineContext'
    { pipelineArn = Prelude.Nothing,
      stage = Prelude.Nothing,
      action = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      pipelineExecutionId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineContext_pipelineArn :: Lens.Lens' PipelineContext (Prelude.Maybe Prelude.Text)
pipelineContext_pipelineArn = Lens.lens (\PipelineContext' {pipelineArn} -> pipelineArn) (\s@PipelineContext' {} a -> s {pipelineArn = a} :: PipelineContext)

-- | The stage of the pipeline.
pipelineContext_stage :: Lens.Lens' PipelineContext (Prelude.Maybe StageContext)
pipelineContext_stage = Lens.lens (\PipelineContext' {stage} -> stage) (\s@PipelineContext' {} a -> s {stage = a} :: PipelineContext)

-- | The context of an action to a job worker in the stage of a pipeline.
pipelineContext_action :: Lens.Lens' PipelineContext (Prelude.Maybe ActionContext)
pipelineContext_action = Lens.lens (\PipelineContext' {action} -> action) (\s@PipelineContext' {} a -> s {action = a} :: PipelineContext)

-- | The name of the pipeline. This is a user-specified value. Pipeline names
-- must be unique across all pipeline names under an Amazon Web Services
-- account.
pipelineContext_pipelineName :: Lens.Lens' PipelineContext (Prelude.Maybe Prelude.Text)
pipelineContext_pipelineName = Lens.lens (\PipelineContext' {pipelineName} -> pipelineName) (\s@PipelineContext' {} a -> s {pipelineName = a} :: PipelineContext)

-- | The execution ID of the pipeline.
pipelineContext_pipelineExecutionId :: Lens.Lens' PipelineContext (Prelude.Maybe Prelude.Text)
pipelineContext_pipelineExecutionId = Lens.lens (\PipelineContext' {pipelineExecutionId} -> pipelineExecutionId) (\s@PipelineContext' {} a -> s {pipelineExecutionId = a} :: PipelineContext)

instance Prelude.FromJSON PipelineContext where
  parseJSON =
    Prelude.withObject
      "PipelineContext"
      ( \x ->
          PipelineContext'
            Prelude.<$> (x Prelude..:? "pipelineArn")
            Prelude.<*> (x Prelude..:? "stage")
            Prelude.<*> (x Prelude..:? "action")
            Prelude.<*> (x Prelude..:? "pipelineName")
            Prelude.<*> (x Prelude..:? "pipelineExecutionId")
      )

instance Prelude.Hashable PipelineContext

instance Prelude.NFData PipelineContext
