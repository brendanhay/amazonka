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
-- Module      : Amazonka.Glue.Types.DataQualityRulesetEvaluationRunDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityRulesetEvaluationRunDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataSource
import Amazonka.Glue.Types.TaskStatusType
import qualified Amazonka.Prelude as Prelude

-- | Describes the result of a data quality ruleset evaluation run.
--
-- /See:/ 'newDataQualityRulesetEvaluationRunDescription' smart constructor.
data DataQualityRulesetEvaluationRunDescription = DataQualityRulesetEvaluationRunDescription'
  { -- | The data source (an Glue table) associated with the run.
    dataSource :: Prelude.Maybe DataSource,
    -- | The unique run identifier associated with this run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the run started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The status for this run.
    status :: Prelude.Maybe TaskStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityRulesetEvaluationRunDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'dataQualityRulesetEvaluationRunDescription_dataSource' - The data source (an Glue table) associated with the run.
--
-- 'runId', 'dataQualityRulesetEvaluationRunDescription_runId' - The unique run identifier associated with this run.
--
-- 'startedOn', 'dataQualityRulesetEvaluationRunDescription_startedOn' - The date and time when the run started.
--
-- 'status', 'dataQualityRulesetEvaluationRunDescription_status' - The status for this run.
newDataQualityRulesetEvaluationRunDescription ::
  DataQualityRulesetEvaluationRunDescription
newDataQualityRulesetEvaluationRunDescription =
  DataQualityRulesetEvaluationRunDescription'
    { dataSource =
        Prelude.Nothing,
      runId = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The data source (an Glue table) associated with the run.
dataQualityRulesetEvaluationRunDescription_dataSource :: Lens.Lens' DataQualityRulesetEvaluationRunDescription (Prelude.Maybe DataSource)
dataQualityRulesetEvaluationRunDescription_dataSource = Lens.lens (\DataQualityRulesetEvaluationRunDescription' {dataSource} -> dataSource) (\s@DataQualityRulesetEvaluationRunDescription' {} a -> s {dataSource = a} :: DataQualityRulesetEvaluationRunDescription)

-- | The unique run identifier associated with this run.
dataQualityRulesetEvaluationRunDescription_runId :: Lens.Lens' DataQualityRulesetEvaluationRunDescription (Prelude.Maybe Prelude.Text)
dataQualityRulesetEvaluationRunDescription_runId = Lens.lens (\DataQualityRulesetEvaluationRunDescription' {runId} -> runId) (\s@DataQualityRulesetEvaluationRunDescription' {} a -> s {runId = a} :: DataQualityRulesetEvaluationRunDescription)

-- | The date and time when the run started.
dataQualityRulesetEvaluationRunDescription_startedOn :: Lens.Lens' DataQualityRulesetEvaluationRunDescription (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetEvaluationRunDescription_startedOn = Lens.lens (\DataQualityRulesetEvaluationRunDescription' {startedOn} -> startedOn) (\s@DataQualityRulesetEvaluationRunDescription' {} a -> s {startedOn = a} :: DataQualityRulesetEvaluationRunDescription) Prelude.. Lens.mapping Data._Time

-- | The status for this run.
dataQualityRulesetEvaluationRunDescription_status :: Lens.Lens' DataQualityRulesetEvaluationRunDescription (Prelude.Maybe TaskStatusType)
dataQualityRulesetEvaluationRunDescription_status = Lens.lens (\DataQualityRulesetEvaluationRunDescription' {status} -> status) (\s@DataQualityRulesetEvaluationRunDescription' {} a -> s {status = a} :: DataQualityRulesetEvaluationRunDescription)

instance
  Data.FromJSON
    DataQualityRulesetEvaluationRunDescription
  where
  parseJSON =
    Data.withObject
      "DataQualityRulesetEvaluationRunDescription"
      ( \x ->
          DataQualityRulesetEvaluationRunDescription'
            Prelude.<$> (x Data..:? "DataSource")
              Prelude.<*> (x Data..:? "RunId")
              Prelude.<*> (x Data..:? "StartedOn")
              Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    DataQualityRulesetEvaluationRunDescription
  where
  hashWithSalt
    _salt
    DataQualityRulesetEvaluationRunDescription' {..} =
      _salt `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` startedOn
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    DataQualityRulesetEvaluationRunDescription
  where
  rnf DataQualityRulesetEvaluationRunDescription' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf status
