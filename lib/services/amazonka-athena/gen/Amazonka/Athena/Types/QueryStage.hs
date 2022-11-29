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
-- Module      : Amazonka.Athena.Types.QueryStage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryStage where

import Amazonka.Athena.Types.QueryStagePlanNode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Stage statistics such as input and output rows and bytes, execution time
-- and stage state. This information also includes substages and the query
-- stage plan.
--
-- /See:/ 'newQueryStage' smart constructor.
data QueryStage = QueryStage'
  { -- | The number of bytes input into the stage for execution.
    inputBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of bytes output from the stage after execution.
    outputBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of rows input into the stage for execution.
    inputRows :: Prelude.Maybe Prelude.Integer,
    -- | Stage plan information such as name, identifier, sub plans, and source
    -- stages.
    queryStagePlan :: Prelude.Maybe QueryStagePlanNode,
    -- | State of the stage after query execution.
    state :: Prelude.Maybe Prelude.Text,
    -- | Time taken to execute this stage.
    executionTime :: Prelude.Maybe Prelude.Integer,
    -- | List of sub query stages that form this stage execution plan.
    subStages :: Prelude.Maybe [QueryStage],
    -- | The identifier for a stage.
    stageId :: Prelude.Maybe Prelude.Integer,
    -- | The number of rows output from the stage after execution.
    outputRows :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputBytes', 'queryStage_inputBytes' - The number of bytes input into the stage for execution.
--
-- 'outputBytes', 'queryStage_outputBytes' - The number of bytes output from the stage after execution.
--
-- 'inputRows', 'queryStage_inputRows' - The number of rows input into the stage for execution.
--
-- 'queryStagePlan', 'queryStage_queryStagePlan' - Stage plan information such as name, identifier, sub plans, and source
-- stages.
--
-- 'state', 'queryStage_state' - State of the stage after query execution.
--
-- 'executionTime', 'queryStage_executionTime' - Time taken to execute this stage.
--
-- 'subStages', 'queryStage_subStages' - List of sub query stages that form this stage execution plan.
--
-- 'stageId', 'queryStage_stageId' - The identifier for a stage.
--
-- 'outputRows', 'queryStage_outputRows' - The number of rows output from the stage after execution.
newQueryStage ::
  QueryStage
newQueryStage =
  QueryStage'
    { inputBytes = Prelude.Nothing,
      outputBytes = Prelude.Nothing,
      inputRows = Prelude.Nothing,
      queryStagePlan = Prelude.Nothing,
      state = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      subStages = Prelude.Nothing,
      stageId = Prelude.Nothing,
      outputRows = Prelude.Nothing
    }

-- | The number of bytes input into the stage for execution.
queryStage_inputBytes :: Lens.Lens' QueryStage (Prelude.Maybe Prelude.Integer)
queryStage_inputBytes = Lens.lens (\QueryStage' {inputBytes} -> inputBytes) (\s@QueryStage' {} a -> s {inputBytes = a} :: QueryStage)

-- | The number of bytes output from the stage after execution.
queryStage_outputBytes :: Lens.Lens' QueryStage (Prelude.Maybe Prelude.Integer)
queryStage_outputBytes = Lens.lens (\QueryStage' {outputBytes} -> outputBytes) (\s@QueryStage' {} a -> s {outputBytes = a} :: QueryStage)

-- | The number of rows input into the stage for execution.
queryStage_inputRows :: Lens.Lens' QueryStage (Prelude.Maybe Prelude.Integer)
queryStage_inputRows = Lens.lens (\QueryStage' {inputRows} -> inputRows) (\s@QueryStage' {} a -> s {inputRows = a} :: QueryStage)

-- | Stage plan information such as name, identifier, sub plans, and source
-- stages.
queryStage_queryStagePlan :: Lens.Lens' QueryStage (Prelude.Maybe QueryStagePlanNode)
queryStage_queryStagePlan = Lens.lens (\QueryStage' {queryStagePlan} -> queryStagePlan) (\s@QueryStage' {} a -> s {queryStagePlan = a} :: QueryStage)

-- | State of the stage after query execution.
queryStage_state :: Lens.Lens' QueryStage (Prelude.Maybe Prelude.Text)
queryStage_state = Lens.lens (\QueryStage' {state} -> state) (\s@QueryStage' {} a -> s {state = a} :: QueryStage)

-- | Time taken to execute this stage.
queryStage_executionTime :: Lens.Lens' QueryStage (Prelude.Maybe Prelude.Integer)
queryStage_executionTime = Lens.lens (\QueryStage' {executionTime} -> executionTime) (\s@QueryStage' {} a -> s {executionTime = a} :: QueryStage)

-- | List of sub query stages that form this stage execution plan.
queryStage_subStages :: Lens.Lens' QueryStage (Prelude.Maybe [QueryStage])
queryStage_subStages = Lens.lens (\QueryStage' {subStages} -> subStages) (\s@QueryStage' {} a -> s {subStages = a} :: QueryStage) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for a stage.
queryStage_stageId :: Lens.Lens' QueryStage (Prelude.Maybe Prelude.Integer)
queryStage_stageId = Lens.lens (\QueryStage' {stageId} -> stageId) (\s@QueryStage' {} a -> s {stageId = a} :: QueryStage)

-- | The number of rows output from the stage after execution.
queryStage_outputRows :: Lens.Lens' QueryStage (Prelude.Maybe Prelude.Integer)
queryStage_outputRows = Lens.lens (\QueryStage' {outputRows} -> outputRows) (\s@QueryStage' {} a -> s {outputRows = a} :: QueryStage)

instance Core.FromJSON QueryStage where
  parseJSON =
    Core.withObject
      "QueryStage"
      ( \x ->
          QueryStage'
            Prelude.<$> (x Core..:? "InputBytes")
            Prelude.<*> (x Core..:? "OutputBytes")
            Prelude.<*> (x Core..:? "InputRows")
            Prelude.<*> (x Core..:? "QueryStagePlan")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "ExecutionTime")
            Prelude.<*> (x Core..:? "SubStages" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StageId")
            Prelude.<*> (x Core..:? "OutputRows")
      )

instance Prelude.Hashable QueryStage where
  hashWithSalt _salt QueryStage' {..} =
    _salt `Prelude.hashWithSalt` inputBytes
      `Prelude.hashWithSalt` outputBytes
      `Prelude.hashWithSalt` inputRows
      `Prelude.hashWithSalt` queryStagePlan
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` executionTime
      `Prelude.hashWithSalt` subStages
      `Prelude.hashWithSalt` stageId
      `Prelude.hashWithSalt` outputRows

instance Prelude.NFData QueryStage where
  rnf QueryStage' {..} =
    Prelude.rnf inputBytes
      `Prelude.seq` Prelude.rnf outputBytes
      `Prelude.seq` Prelude.rnf inputRows
      `Prelude.seq` Prelude.rnf queryStagePlan
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf subStages
      `Prelude.seq` Prelude.rnf stageId
      `Prelude.seq` Prelude.rnf outputRows
