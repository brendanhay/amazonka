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
-- Module      : Amazonka.Glue.Types.DataQualityRuleRecommendationRunDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityRuleRecommendationRunDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataSource
import Amazonka.Glue.Types.TaskStatusType
import qualified Amazonka.Prelude as Prelude

-- | Describes the result of a data quality rule recommendation run.
--
-- /See:/ 'newDataQualityRuleRecommendationRunDescription' smart constructor.
data DataQualityRuleRecommendationRunDescription = DataQualityRuleRecommendationRunDescription'
  { -- | The data source (Glue table) associated with the recommendation run.
    dataSource :: Prelude.Maybe DataSource,
    -- | The unique run identifier associated with this run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when this run started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The status for this run.
    status :: Prelude.Maybe TaskStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityRuleRecommendationRunDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'dataQualityRuleRecommendationRunDescription_dataSource' - The data source (Glue table) associated with the recommendation run.
--
-- 'runId', 'dataQualityRuleRecommendationRunDescription_runId' - The unique run identifier associated with this run.
--
-- 'startedOn', 'dataQualityRuleRecommendationRunDescription_startedOn' - The date and time when this run started.
--
-- 'status', 'dataQualityRuleRecommendationRunDescription_status' - The status for this run.
newDataQualityRuleRecommendationRunDescription ::
  DataQualityRuleRecommendationRunDescription
newDataQualityRuleRecommendationRunDescription =
  DataQualityRuleRecommendationRunDescription'
    { dataSource =
        Prelude.Nothing,
      runId = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The data source (Glue table) associated with the recommendation run.
dataQualityRuleRecommendationRunDescription_dataSource :: Lens.Lens' DataQualityRuleRecommendationRunDescription (Prelude.Maybe DataSource)
dataQualityRuleRecommendationRunDescription_dataSource = Lens.lens (\DataQualityRuleRecommendationRunDescription' {dataSource} -> dataSource) (\s@DataQualityRuleRecommendationRunDescription' {} a -> s {dataSource = a} :: DataQualityRuleRecommendationRunDescription)

-- | The unique run identifier associated with this run.
dataQualityRuleRecommendationRunDescription_runId :: Lens.Lens' DataQualityRuleRecommendationRunDescription (Prelude.Maybe Prelude.Text)
dataQualityRuleRecommendationRunDescription_runId = Lens.lens (\DataQualityRuleRecommendationRunDescription' {runId} -> runId) (\s@DataQualityRuleRecommendationRunDescription' {} a -> s {runId = a} :: DataQualityRuleRecommendationRunDescription)

-- | The date and time when this run started.
dataQualityRuleRecommendationRunDescription_startedOn :: Lens.Lens' DataQualityRuleRecommendationRunDescription (Prelude.Maybe Prelude.UTCTime)
dataQualityRuleRecommendationRunDescription_startedOn = Lens.lens (\DataQualityRuleRecommendationRunDescription' {startedOn} -> startedOn) (\s@DataQualityRuleRecommendationRunDescription' {} a -> s {startedOn = a} :: DataQualityRuleRecommendationRunDescription) Prelude.. Lens.mapping Data._Time

-- | The status for this run.
dataQualityRuleRecommendationRunDescription_status :: Lens.Lens' DataQualityRuleRecommendationRunDescription (Prelude.Maybe TaskStatusType)
dataQualityRuleRecommendationRunDescription_status = Lens.lens (\DataQualityRuleRecommendationRunDescription' {status} -> status) (\s@DataQualityRuleRecommendationRunDescription' {} a -> s {status = a} :: DataQualityRuleRecommendationRunDescription)

instance
  Data.FromJSON
    DataQualityRuleRecommendationRunDescription
  where
  parseJSON =
    Data.withObject
      "DataQualityRuleRecommendationRunDescription"
      ( \x ->
          DataQualityRuleRecommendationRunDescription'
            Prelude.<$> (x Data..:? "DataSource")
            Prelude.<*> (x Data..:? "RunId")
            Prelude.<*> (x Data..:? "StartedOn")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    DataQualityRuleRecommendationRunDescription
  where
  hashWithSalt
    _salt
    DataQualityRuleRecommendationRunDescription' {..} =
      _salt
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` startedOn
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    DataQualityRuleRecommendationRunDescription
  where
  rnf DataQualityRuleRecommendationRunDescription' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf status
