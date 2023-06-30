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
-- Module      : Amazonka.Glue.Types.DataQualityRulesetEvaluationRunFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityRulesetEvaluationRunFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataSource
import qualified Amazonka.Prelude as Prelude

-- | The filter criteria.
--
-- /See:/ 'newDataQualityRulesetEvaluationRunFilter' smart constructor.
data DataQualityRulesetEvaluationRunFilter = DataQualityRulesetEvaluationRunFilter'
  { -- | Filter results by runs that started after this time.
    startedAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter results by runs that started before this time.
    startedBefore :: Prelude.Maybe Data.POSIX,
    -- | Filter based on a data source (an Glue table) associated with the run.
    dataSource :: DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityRulesetEvaluationRunFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startedAfter', 'dataQualityRulesetEvaluationRunFilter_startedAfter' - Filter results by runs that started after this time.
--
-- 'startedBefore', 'dataQualityRulesetEvaluationRunFilter_startedBefore' - Filter results by runs that started before this time.
--
-- 'dataSource', 'dataQualityRulesetEvaluationRunFilter_dataSource' - Filter based on a data source (an Glue table) associated with the run.
newDataQualityRulesetEvaluationRunFilter ::
  -- | 'dataSource'
  DataSource ->
  DataQualityRulesetEvaluationRunFilter
newDataQualityRulesetEvaluationRunFilter pDataSource_ =
  DataQualityRulesetEvaluationRunFilter'
    { startedAfter =
        Prelude.Nothing,
      startedBefore = Prelude.Nothing,
      dataSource = pDataSource_
    }

-- | Filter results by runs that started after this time.
dataQualityRulesetEvaluationRunFilter_startedAfter :: Lens.Lens' DataQualityRulesetEvaluationRunFilter (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetEvaluationRunFilter_startedAfter = Lens.lens (\DataQualityRulesetEvaluationRunFilter' {startedAfter} -> startedAfter) (\s@DataQualityRulesetEvaluationRunFilter' {} a -> s {startedAfter = a} :: DataQualityRulesetEvaluationRunFilter) Prelude.. Lens.mapping Data._Time

-- | Filter results by runs that started before this time.
dataQualityRulesetEvaluationRunFilter_startedBefore :: Lens.Lens' DataQualityRulesetEvaluationRunFilter (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetEvaluationRunFilter_startedBefore = Lens.lens (\DataQualityRulesetEvaluationRunFilter' {startedBefore} -> startedBefore) (\s@DataQualityRulesetEvaluationRunFilter' {} a -> s {startedBefore = a} :: DataQualityRulesetEvaluationRunFilter) Prelude.. Lens.mapping Data._Time

-- | Filter based on a data source (an Glue table) associated with the run.
dataQualityRulesetEvaluationRunFilter_dataSource :: Lens.Lens' DataQualityRulesetEvaluationRunFilter DataSource
dataQualityRulesetEvaluationRunFilter_dataSource = Lens.lens (\DataQualityRulesetEvaluationRunFilter' {dataSource} -> dataSource) (\s@DataQualityRulesetEvaluationRunFilter' {} a -> s {dataSource = a} :: DataQualityRulesetEvaluationRunFilter)

instance
  Prelude.Hashable
    DataQualityRulesetEvaluationRunFilter
  where
  hashWithSalt
    _salt
    DataQualityRulesetEvaluationRunFilter' {..} =
      _salt
        `Prelude.hashWithSalt` startedAfter
        `Prelude.hashWithSalt` startedBefore
        `Prelude.hashWithSalt` dataSource

instance
  Prelude.NFData
    DataQualityRulesetEvaluationRunFilter
  where
  rnf DataQualityRulesetEvaluationRunFilter' {..} =
    Prelude.rnf startedAfter
      `Prelude.seq` Prelude.rnf startedBefore
      `Prelude.seq` Prelude.rnf dataSource

instance
  Data.ToJSON
    DataQualityRulesetEvaluationRunFilter
  where
  toJSON DataQualityRulesetEvaluationRunFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StartedAfter" Data..=) Prelude.<$> startedAfter,
            ("StartedBefore" Data..=) Prelude.<$> startedBefore,
            Prelude.Just ("DataSource" Data..= dataSource)
          ]
      )
