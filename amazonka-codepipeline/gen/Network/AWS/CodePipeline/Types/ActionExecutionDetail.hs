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
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionDetail where

import Network.AWS.CodePipeline.Types.ActionExecutionInput
import Network.AWS.CodePipeline.Types.ActionExecutionOutput
import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about an execution of an action, including the
-- action execution ID, and the name, version, and timing of the action.
--
-- /See:/ 'newActionExecutionDetail' smart constructor.
data ActionExecutionDetail = ActionExecutionDetail'
  { -- | The status of the action execution. Status categories are @InProgress@,
    -- @Succeeded@, and @Failed@.
    status :: Prelude.Maybe ActionExecutionStatus,
    -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The action execution ID.
    actionExecutionId :: Prelude.Maybe Prelude.Text,
    -- | Input details for the action execution, such as role ARN, Region, and
    -- input artifacts.
    input :: Prelude.Maybe ActionExecutionInput,
    -- | The last update time of the action execution.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the stage that contains the action.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The start time of the action execution.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Output details for the action execution, such as the action execution
    -- result.
    output :: Prelude.Maybe ActionExecutionOutput,
    -- | The version of the pipeline where the action was run.
    pipelineVersion :: Prelude.Maybe Prelude.Natural,
    -- | The pipeline execution ID for the action execution.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text
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
-- 'status', 'actionExecutionDetail_status' - The status of the action execution. Status categories are @InProgress@,
-- @Succeeded@, and @Failed@.
--
-- 'actionName', 'actionExecutionDetail_actionName' - The name of the action.
--
-- 'actionExecutionId', 'actionExecutionDetail_actionExecutionId' - The action execution ID.
--
-- 'input', 'actionExecutionDetail_input' - Input details for the action execution, such as role ARN, Region, and
-- input artifacts.
--
-- 'lastUpdateTime', 'actionExecutionDetail_lastUpdateTime' - The last update time of the action execution.
--
-- 'stageName', 'actionExecutionDetail_stageName' - The name of the stage that contains the action.
--
-- 'startTime', 'actionExecutionDetail_startTime' - The start time of the action execution.
--
-- 'output', 'actionExecutionDetail_output' - Output details for the action execution, such as the action execution
-- result.
--
-- 'pipelineVersion', 'actionExecutionDetail_pipelineVersion' - The version of the pipeline where the action was run.
--
-- 'pipelineExecutionId', 'actionExecutionDetail_pipelineExecutionId' - The pipeline execution ID for the action execution.
newActionExecutionDetail ::
  ActionExecutionDetail
newActionExecutionDetail =
  ActionExecutionDetail'
    { status = Prelude.Nothing,
      actionName = Prelude.Nothing,
      actionExecutionId = Prelude.Nothing,
      input = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      stageName = Prelude.Nothing,
      startTime = Prelude.Nothing,
      output = Prelude.Nothing,
      pipelineVersion = Prelude.Nothing,
      pipelineExecutionId = Prelude.Nothing
    }

-- | The status of the action execution. Status categories are @InProgress@,
-- @Succeeded@, and @Failed@.
actionExecutionDetail_status :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe ActionExecutionStatus)
actionExecutionDetail_status = Lens.lens (\ActionExecutionDetail' {status} -> status) (\s@ActionExecutionDetail' {} a -> s {status = a} :: ActionExecutionDetail)

-- | The name of the action.
actionExecutionDetail_actionName :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_actionName = Lens.lens (\ActionExecutionDetail' {actionName} -> actionName) (\s@ActionExecutionDetail' {} a -> s {actionName = a} :: ActionExecutionDetail)

-- | The action execution ID.
actionExecutionDetail_actionExecutionId :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_actionExecutionId = Lens.lens (\ActionExecutionDetail' {actionExecutionId} -> actionExecutionId) (\s@ActionExecutionDetail' {} a -> s {actionExecutionId = a} :: ActionExecutionDetail)

-- | Input details for the action execution, such as role ARN, Region, and
-- input artifacts.
actionExecutionDetail_input :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe ActionExecutionInput)
actionExecutionDetail_input = Lens.lens (\ActionExecutionDetail' {input} -> input) (\s@ActionExecutionDetail' {} a -> s {input = a} :: ActionExecutionDetail)

-- | The last update time of the action execution.
actionExecutionDetail_lastUpdateTime :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.UTCTime)
actionExecutionDetail_lastUpdateTime = Lens.lens (\ActionExecutionDetail' {lastUpdateTime} -> lastUpdateTime) (\s@ActionExecutionDetail' {} a -> s {lastUpdateTime = a} :: ActionExecutionDetail) Prelude.. Lens.mapping Core._Time

-- | The name of the stage that contains the action.
actionExecutionDetail_stageName :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_stageName = Lens.lens (\ActionExecutionDetail' {stageName} -> stageName) (\s@ActionExecutionDetail' {} a -> s {stageName = a} :: ActionExecutionDetail)

-- | The start time of the action execution.
actionExecutionDetail_startTime :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.UTCTime)
actionExecutionDetail_startTime = Lens.lens (\ActionExecutionDetail' {startTime} -> startTime) (\s@ActionExecutionDetail' {} a -> s {startTime = a} :: ActionExecutionDetail) Prelude.. Lens.mapping Core._Time

-- | Output details for the action execution, such as the action execution
-- result.
actionExecutionDetail_output :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe ActionExecutionOutput)
actionExecutionDetail_output = Lens.lens (\ActionExecutionDetail' {output} -> output) (\s@ActionExecutionDetail' {} a -> s {output = a} :: ActionExecutionDetail)

-- | The version of the pipeline where the action was run.
actionExecutionDetail_pipelineVersion :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Natural)
actionExecutionDetail_pipelineVersion = Lens.lens (\ActionExecutionDetail' {pipelineVersion} -> pipelineVersion) (\s@ActionExecutionDetail' {} a -> s {pipelineVersion = a} :: ActionExecutionDetail)

-- | The pipeline execution ID for the action execution.
actionExecutionDetail_pipelineExecutionId :: Lens.Lens' ActionExecutionDetail (Prelude.Maybe Prelude.Text)
actionExecutionDetail_pipelineExecutionId = Lens.lens (\ActionExecutionDetail' {pipelineExecutionId} -> pipelineExecutionId) (\s@ActionExecutionDetail' {} a -> s {pipelineExecutionId = a} :: ActionExecutionDetail)

instance Core.FromJSON ActionExecutionDetail where
  parseJSON =
    Core.withObject
      "ActionExecutionDetail"
      ( \x ->
          ActionExecutionDetail'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "actionExecutionId")
            Prelude.<*> (x Core..:? "input")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "stageName")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "output")
            Prelude.<*> (x Core..:? "pipelineVersion")
            Prelude.<*> (x Core..:? "pipelineExecutionId")
      )

instance Prelude.Hashable ActionExecutionDetail

instance Prelude.NFData ActionExecutionDetail
