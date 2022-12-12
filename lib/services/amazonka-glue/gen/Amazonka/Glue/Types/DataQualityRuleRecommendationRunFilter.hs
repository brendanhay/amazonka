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
-- Module      : Amazonka.Glue.Types.DataQualityRuleRecommendationRunFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityRuleRecommendationRunFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataSource
import qualified Amazonka.Prelude as Prelude

-- | A filter for listing data quality recommendation runs.
--
-- /See:/ 'newDataQualityRuleRecommendationRunFilter' smart constructor.
data DataQualityRuleRecommendationRunFilter = DataQualityRuleRecommendationRunFilter'
  { -- | Filter based on time for results started after provided time.
    startedAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter based on time for results started before provided time.
    startedBefore :: Prelude.Maybe Data.POSIX,
    -- | Filter based on a specified data source (Glue table).
    dataSource :: DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityRuleRecommendationRunFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startedAfter', 'dataQualityRuleRecommendationRunFilter_startedAfter' - Filter based on time for results started after provided time.
--
-- 'startedBefore', 'dataQualityRuleRecommendationRunFilter_startedBefore' - Filter based on time for results started before provided time.
--
-- 'dataSource', 'dataQualityRuleRecommendationRunFilter_dataSource' - Filter based on a specified data source (Glue table).
newDataQualityRuleRecommendationRunFilter ::
  -- | 'dataSource'
  DataSource ->
  DataQualityRuleRecommendationRunFilter
newDataQualityRuleRecommendationRunFilter
  pDataSource_ =
    DataQualityRuleRecommendationRunFilter'
      { startedAfter =
          Prelude.Nothing,
        startedBefore = Prelude.Nothing,
        dataSource = pDataSource_
      }

-- | Filter based on time for results started after provided time.
dataQualityRuleRecommendationRunFilter_startedAfter :: Lens.Lens' DataQualityRuleRecommendationRunFilter (Prelude.Maybe Prelude.UTCTime)
dataQualityRuleRecommendationRunFilter_startedAfter = Lens.lens (\DataQualityRuleRecommendationRunFilter' {startedAfter} -> startedAfter) (\s@DataQualityRuleRecommendationRunFilter' {} a -> s {startedAfter = a} :: DataQualityRuleRecommendationRunFilter) Prelude.. Lens.mapping Data._Time

-- | Filter based on time for results started before provided time.
dataQualityRuleRecommendationRunFilter_startedBefore :: Lens.Lens' DataQualityRuleRecommendationRunFilter (Prelude.Maybe Prelude.UTCTime)
dataQualityRuleRecommendationRunFilter_startedBefore = Lens.lens (\DataQualityRuleRecommendationRunFilter' {startedBefore} -> startedBefore) (\s@DataQualityRuleRecommendationRunFilter' {} a -> s {startedBefore = a} :: DataQualityRuleRecommendationRunFilter) Prelude.. Lens.mapping Data._Time

-- | Filter based on a specified data source (Glue table).
dataQualityRuleRecommendationRunFilter_dataSource :: Lens.Lens' DataQualityRuleRecommendationRunFilter DataSource
dataQualityRuleRecommendationRunFilter_dataSource = Lens.lens (\DataQualityRuleRecommendationRunFilter' {dataSource} -> dataSource) (\s@DataQualityRuleRecommendationRunFilter' {} a -> s {dataSource = a} :: DataQualityRuleRecommendationRunFilter)

instance
  Prelude.Hashable
    DataQualityRuleRecommendationRunFilter
  where
  hashWithSalt
    _salt
    DataQualityRuleRecommendationRunFilter' {..} =
      _salt `Prelude.hashWithSalt` startedAfter
        `Prelude.hashWithSalt` startedBefore
        `Prelude.hashWithSalt` dataSource

instance
  Prelude.NFData
    DataQualityRuleRecommendationRunFilter
  where
  rnf DataQualityRuleRecommendationRunFilter' {..} =
    Prelude.rnf startedAfter
      `Prelude.seq` Prelude.rnf startedBefore
      `Prelude.seq` Prelude.rnf dataSource

instance
  Data.ToJSON
    DataQualityRuleRecommendationRunFilter
  where
  toJSON DataQualityRuleRecommendationRunFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StartedAfter" Data..=) Prelude.<$> startedAfter,
            ("StartedBefore" Data..=) Prelude.<$> startedBefore,
            Prelude.Just ("DataSource" Data..= dataSource)
          ]
      )
