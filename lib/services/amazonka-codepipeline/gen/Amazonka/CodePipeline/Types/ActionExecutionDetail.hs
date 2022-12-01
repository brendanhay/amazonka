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
-- Module      : Amazonka.CodePipeline.Types.ActionExecutionDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecutionDetail where

import Amazonka.CodePipeline.Types.ActionExecutionInput
import Amazonka.CodePipeline.Types.ActionExecutionOutput
import Amazonka.CodePipeline.Types.ActionExecutionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about an execution of an action, including the
-- action execution ID, and the name, version, and timing of the action.
--
-- /See:/ 'newActionExecutionDetail' smart constructor.
data ActionExecutionDetail = ActionExecutionDetail'
  { -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the stage that contains the action.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The pipeline execution ID for the action execution.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The action execution ID.
    actionExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the action execution. Status categories are @InProgress@,
    -- @Succeeded@, and @Failed@.
    status :: Prelude.Maybe ActionExecutionStatus,
    -- | Input details for the action execution, such as role ARN, Region, and
    -- input artifacts.
    input :: Prelude.Maybe ActionExecutionInput,
    -- | The version of the pipeline where the action was run.
    pipelineVersion :: Prelude.Maybe Prelude.Natural,
    -- | Output details for the action execution, such as the action execution
    -- result.
    output :: Prelude.Maybe ActionExecutionOutput,
    -- | The last update time of the action execution.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The start time of the action execution.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionExecutionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'actionExecutionDetail_actionName' - The name of the action.
--
-- 'stageName', 'actionExecutionDetail_stageName' - The name of the stage that contains the action.
--
-- 'pipelineExecutionId', 'actionExecutionDetail_pipelineExecutionId' - The pipeline execution ID for the action execution.
--
-- 'actionExecutionId', 'actionExecutionDetail_actionExecutionId' - The action execution ID.
--
-- 'status', 'actionExecutionDetail_status' - The status of the action execution. Status categories are @InProgress@,
-- @Succeeded@, and @Failed@.
--
-- 'input', 'actionExecutionDetail_input' - Input details for the action execution, such as role ARN, Region, and
-- input artifacts.
--
-- 'pipelineVersion', 'actionExecutionDetail_pipelineVersion' - The version of the pipeline where the action was run.
--
-- 'output', 'actionExecutionDetail_output' - Output details for the action execution, such as the action execution
-- result.
--
-- 'lastUpdateTime', 'actionExecutionDetail_lastUpdateTime' - The last update time of the action execution.
--
-- 'startTime', 'actionExecutionDetail_startTime' - The start time of the action execution.
newActionExecutionDetail ::
  ActionExecutionDetail
newActionExecutionDetail =
  ActionExecutionDetail'
    { actionName =
        Prelude.Nothing,
      stageName = Prelude.Nothing,
      pipelineExecutionId = Prelude.Nothing,
      actionExecutionId = Prelude.Nothing,
      status = Prelude.Nothing,
      input = Prelude.Nothing,
      pipelineVersion = Prelude.Nothing,
      output = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The name of the action.
actionExecutionDetail_actionName :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_actionName = Lens.lens (\ActionExecutionDetail' {actionName} -> actionName) (\s@ActionExecutionDetail' {} a -> s {actionName = a} :: ActionExecutionDetail)

-- | The name of the stage that contains the action.
actionExecutionDetail_stageName :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_stageName = Lens.lens (\ActionExecutionDetail' {stageName} -> stageName) (\s@ActionExecutionDetail' {} a -> s {stageName = a} :: ActionExecutionDetail)

-- | The pipeline execution ID for the action execution.
actionExecutionDetail_pipelineExecutionId :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_pipelineExecutionId = Lens.lens (\ActionExecutionDetail' {pipelineExecutionId} -> pipelineExecutionId) (\s@ActionExecutionDetail' {} a -> s {pipelineExecutionId = a} :: ActionExecutionDetail)

-- | The action execution ID.
actionExecutionDetail_actionExecutionId :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_actionExecutionId = Lens.lens (\ActionExecutionDetail' {actionExecutionId} -> actionExecutionId) (\s@ActionExecutionDetail' {} a -> s {actionExecutionId = a} :: ActionExecutionDetail)

-- | The status of the action execution. Status categories are @InProgress@,
-- @Succeeded@, and @Failed@.
actionExecutionDetail_status :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe ActionExecutionStatus)
actionExecutionDetail_status = Lens.lens (\ActionExecutionDetail' {status} -> status) (\s@ActionExecutionDetail' {} a -> s {status = a} :: ActionExecutionDetail)

-- | Input details for the action execution, such as role ARN, Region, and
-- input artifacts.
actionExecutionDetail_input :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe ActionExecutionInput)
actionExecutionDetail_input = Lens.lens (\ActionExecutionDetail' {input} -> input) (\s@ActionExecutionDetail' {} a -> s {input = a} :: ActionExecutionDetail)

-- | The version of the pipeline where the action was run.
actionExecutionDetail_pipelineVersion :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Natural)
actionExecutionDetail_pipelineVersion = Lens.lens (\ActionExecutionDetail' {pipelineVersion} -> pipelineVersion) (\s@ActionExecutionDetail' {} a -> s {pipelineVersion = a} :: ActionExecutionDetail)

-- | Output details for the action execution, such as the action execution
-- result.
actionExecutionDetail_output :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe ActionExecutionOutput)
actionExecutionDetail_output = Lens.lens (\ActionExecutionDetail' {output} -> output) (\s@ActionExecutionDetail' {} a -> s {output = a} :: ActionExecutionDetail)

-- | The last update time of the action execution.
actionExecutionDetail_lastUpdateTime :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.UTCTime)
actionExecutionDetail_lastUpdateTime = Lens.lens (\ActionExecutionDetail' {lastUpdateTime} -> lastUpdateTime) (\s@ActionExecutionDetail' {} a -> s {lastUpdateTime = a} :: ActionExecutionDetail) Prelude.. Lens.mapping Core._Time

-- | The start time of the action execution.
actionExecutionDetail_startTime :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.UTCTime)
actionExecutionDetail_startTime = Lens.lens (\ActionExecutionDetail' {startTime} -> startTime) (\s@ActionExecutionDetail' {} a -> s {startTime = a} :: ActionExecutionDetail) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ActionExecutionDetail where
  parseJSON =
    Core.withObject
      "ActionExecutionDetail"
      ( \x ->
          ActionExecutionDetail'
            Prelude.<$> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "stageName")
            Prelude.<*> (x Core..:? "pipelineExecutionId")
            Prelude.<*> (x Core..:? "actionExecutionId")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "input")
            Prelude.<*> (x Core..:? "pipelineVersion")
            Prelude.<*> (x Core..:? "output")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "startTime")
      )

instance Prelude.Hashable ActionExecutionDetail where
  hashWithSalt _salt ActionExecutionDetail' {..} =
    _salt `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` pipelineExecutionId
      `Prelude.hashWithSalt` actionExecutionId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` pipelineVersion
      `Prelude.hashWithSalt` output
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ActionExecutionDetail where
  rnf ActionExecutionDetail' {..} =
    Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf pipelineExecutionId
      `Prelude.seq` Prelude.rnf actionExecutionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf pipelineVersion
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf startTime
