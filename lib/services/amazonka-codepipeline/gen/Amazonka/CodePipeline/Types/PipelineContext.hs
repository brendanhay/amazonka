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
-- Module      : Amazonka.CodePipeline.Types.PipelineContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.PipelineContext where

import Amazonka.CodePipeline.Types.ActionContext
import Amazonka.CodePipeline.Types.StageContext
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about a pipeline to a job worker.
--
-- PipelineContext contains @pipelineArn@ and @pipelineExecutionId@ for
-- custom action jobs. The @pipelineArn@ and @pipelineExecutionId@ fields
-- are not populated for ThirdParty action jobs.
--
-- /See:/ 'newPipelineContext' smart constructor.
data PipelineContext = PipelineContext'
  { -- | The context of an action to a job worker in the stage of a pipeline.
    action :: Prelude.Maybe ActionContext,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The execution ID of the pipeline.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline. This is a user-specified value. Pipeline names
    -- must be unique across all pipeline names under an Amazon Web Services
    -- account.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The stage of the pipeline.
    stage :: Prelude.Maybe StageContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'pipelineContext_action' - The context of an action to a job worker in the stage of a pipeline.
--
-- 'pipelineArn', 'pipelineContext_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineExecutionId', 'pipelineContext_pipelineExecutionId' - The execution ID of the pipeline.
--
-- 'pipelineName', 'pipelineContext_pipelineName' - The name of the pipeline. This is a user-specified value. Pipeline names
-- must be unique across all pipeline names under an Amazon Web Services
-- account.
--
-- 'stage', 'pipelineContext_stage' - The stage of the pipeline.
newPipelineContext ::
  PipelineContext
newPipelineContext =
  PipelineContext'
    { action = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineExecutionId = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      stage = Prelude.Nothing
    }

-- | The context of an action to a job worker in the stage of a pipeline.
pipelineContext_action :: Lens.Lens' PipelineContext (Prelude.Maybe ActionContext)
pipelineContext_action = Lens.lens (\PipelineContext' {action} -> action) (\s@PipelineContext' {} a -> s {action = a} :: PipelineContext)

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineContext_pipelineArn :: Lens.Lens' PipelineContext (Prelude.Maybe Prelude.Text)
pipelineContext_pipelineArn = Lens.lens (\PipelineContext' {pipelineArn} -> pipelineArn) (\s@PipelineContext' {} a -> s {pipelineArn = a} :: PipelineContext)

-- | The execution ID of the pipeline.
pipelineContext_pipelineExecutionId :: Lens.Lens' PipelineContext (Prelude.Maybe Prelude.Text)
pipelineContext_pipelineExecutionId = Lens.lens (\PipelineContext' {pipelineExecutionId} -> pipelineExecutionId) (\s@PipelineContext' {} a -> s {pipelineExecutionId = a} :: PipelineContext)

-- | The name of the pipeline. This is a user-specified value. Pipeline names
-- must be unique across all pipeline names under an Amazon Web Services
-- account.
pipelineContext_pipelineName :: Lens.Lens' PipelineContext (Prelude.Maybe Prelude.Text)
pipelineContext_pipelineName = Lens.lens (\PipelineContext' {pipelineName} -> pipelineName) (\s@PipelineContext' {} a -> s {pipelineName = a} :: PipelineContext)

-- | The stage of the pipeline.
pipelineContext_stage :: Lens.Lens' PipelineContext (Prelude.Maybe StageContext)
pipelineContext_stage = Lens.lens (\PipelineContext' {stage} -> stage) (\s@PipelineContext' {} a -> s {stage = a} :: PipelineContext)

instance Data.FromJSON PipelineContext where
  parseJSON =
    Data.withObject
      "PipelineContext"
      ( \x ->
          PipelineContext'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "pipelineArn")
            Prelude.<*> (x Data..:? "pipelineExecutionId")
            Prelude.<*> (x Data..:? "pipelineName")
            Prelude.<*> (x Data..:? "stage")
      )

instance Prelude.Hashable PipelineContext where
  hashWithSalt _salt PipelineContext' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineExecutionId
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` stage

instance Prelude.NFData PipelineContext where
  rnf PipelineContext' {..} =
    Prelude.rnf action `Prelude.seq`
      Prelude.rnf pipelineArn `Prelude.seq`
        Prelude.rnf pipelineExecutionId `Prelude.seq`
          Prelude.rnf pipelineName `Prelude.seq`
            Prelude.rnf stage
