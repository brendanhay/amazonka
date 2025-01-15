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
-- Module      : Amazonka.CodePipeline.Types.StageExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.StageExecution where

import Amazonka.CodePipeline.Types.StageExecutionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the run of a stage.
--
-- /See:/ 'newStageExecution' smart constructor.
data StageExecution = StageExecution'
  { -- | The ID of the pipeline execution associated with the stage.
    pipelineExecutionId :: Prelude.Text,
    -- | The status of the stage, or for a completed stage, the last status of
    -- the stage.
    --
    -- A status of cancelled means that the pipeline’s definition was updated
    -- before the stage execution could be completed.
    status :: StageExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionId', 'stageExecution_pipelineExecutionId' - The ID of the pipeline execution associated with the stage.
--
-- 'status', 'stageExecution_status' - The status of the stage, or for a completed stage, the last status of
-- the stage.
--
-- A status of cancelled means that the pipeline’s definition was updated
-- before the stage execution could be completed.
newStageExecution ::
  -- | 'pipelineExecutionId'
  Prelude.Text ->
  -- | 'status'
  StageExecutionStatus ->
  StageExecution
newStageExecution pPipelineExecutionId_ pStatus_ =
  StageExecution'
    { pipelineExecutionId =
        pPipelineExecutionId_,
      status = pStatus_
    }

-- | The ID of the pipeline execution associated with the stage.
stageExecution_pipelineExecutionId :: Lens.Lens' StageExecution Prelude.Text
stageExecution_pipelineExecutionId = Lens.lens (\StageExecution' {pipelineExecutionId} -> pipelineExecutionId) (\s@StageExecution' {} a -> s {pipelineExecutionId = a} :: StageExecution)

-- | The status of the stage, or for a completed stage, the last status of
-- the stage.
--
-- A status of cancelled means that the pipeline’s definition was updated
-- before the stage execution could be completed.
stageExecution_status :: Lens.Lens' StageExecution StageExecutionStatus
stageExecution_status = Lens.lens (\StageExecution' {status} -> status) (\s@StageExecution' {} a -> s {status = a} :: StageExecution)

instance Data.FromJSON StageExecution where
  parseJSON =
    Data.withObject
      "StageExecution"
      ( \x ->
          StageExecution'
            Prelude.<$> (x Data..: "pipelineExecutionId")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable StageExecution where
  hashWithSalt _salt StageExecution' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineExecutionId
      `Prelude.hashWithSalt` status

instance Prelude.NFData StageExecution where
  rnf StageExecution' {..} =
    Prelude.rnf pipelineExecutionId `Prelude.seq`
      Prelude.rnf status
