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
-- Module      : Amazonka.Glue.Types.DataQualityResultFilterCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityResultFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataSource
import qualified Amazonka.Prelude as Prelude

-- | Criteria used to return data quality results.
--
-- /See:/ 'newDataQualityResultFilterCriteria' smart constructor.
data DataQualityResultFilterCriteria = DataQualityResultFilterCriteria'
  { -- | Filter results by the specified data source. For example, retrieving all
    -- results for an Glue table.
    dataSource :: Prelude.Maybe DataSource,
    -- | Filter results by the specified job name.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Filter results by the specified job run ID.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | Filter results by runs that started after this time.
    startedAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter results by runs that started before this time.
    startedBefore :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityResultFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'dataQualityResultFilterCriteria_dataSource' - Filter results by the specified data source. For example, retrieving all
-- results for an Glue table.
--
-- 'jobName', 'dataQualityResultFilterCriteria_jobName' - Filter results by the specified job name.
--
-- 'jobRunId', 'dataQualityResultFilterCriteria_jobRunId' - Filter results by the specified job run ID.
--
-- 'startedAfter', 'dataQualityResultFilterCriteria_startedAfter' - Filter results by runs that started after this time.
--
-- 'startedBefore', 'dataQualityResultFilterCriteria_startedBefore' - Filter results by runs that started before this time.
newDataQualityResultFilterCriteria ::
  DataQualityResultFilterCriteria
newDataQualityResultFilterCriteria =
  DataQualityResultFilterCriteria'
    { dataSource =
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      startedAfter = Prelude.Nothing,
      startedBefore = Prelude.Nothing
    }

-- | Filter results by the specified data source. For example, retrieving all
-- results for an Glue table.
dataQualityResultFilterCriteria_dataSource :: Lens.Lens' DataQualityResultFilterCriteria (Prelude.Maybe DataSource)
dataQualityResultFilterCriteria_dataSource = Lens.lens (\DataQualityResultFilterCriteria' {dataSource} -> dataSource) (\s@DataQualityResultFilterCriteria' {} a -> s {dataSource = a} :: DataQualityResultFilterCriteria)

-- | Filter results by the specified job name.
dataQualityResultFilterCriteria_jobName :: Lens.Lens' DataQualityResultFilterCriteria (Prelude.Maybe Prelude.Text)
dataQualityResultFilterCriteria_jobName = Lens.lens (\DataQualityResultFilterCriteria' {jobName} -> jobName) (\s@DataQualityResultFilterCriteria' {} a -> s {jobName = a} :: DataQualityResultFilterCriteria)

-- | Filter results by the specified job run ID.
dataQualityResultFilterCriteria_jobRunId :: Lens.Lens' DataQualityResultFilterCriteria (Prelude.Maybe Prelude.Text)
dataQualityResultFilterCriteria_jobRunId = Lens.lens (\DataQualityResultFilterCriteria' {jobRunId} -> jobRunId) (\s@DataQualityResultFilterCriteria' {} a -> s {jobRunId = a} :: DataQualityResultFilterCriteria)

-- | Filter results by runs that started after this time.
dataQualityResultFilterCriteria_startedAfter :: Lens.Lens' DataQualityResultFilterCriteria (Prelude.Maybe Prelude.UTCTime)
dataQualityResultFilterCriteria_startedAfter = Lens.lens (\DataQualityResultFilterCriteria' {startedAfter} -> startedAfter) (\s@DataQualityResultFilterCriteria' {} a -> s {startedAfter = a} :: DataQualityResultFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | Filter results by runs that started before this time.
dataQualityResultFilterCriteria_startedBefore :: Lens.Lens' DataQualityResultFilterCriteria (Prelude.Maybe Prelude.UTCTime)
dataQualityResultFilterCriteria_startedBefore = Lens.lens (\DataQualityResultFilterCriteria' {startedBefore} -> startedBefore) (\s@DataQualityResultFilterCriteria' {} a -> s {startedBefore = a} :: DataQualityResultFilterCriteria) Prelude.. Lens.mapping Data._Time

instance
  Prelude.Hashable
    DataQualityResultFilterCriteria
  where
  hashWithSalt
    _salt
    DataQualityResultFilterCriteria' {..} =
      _salt
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobRunId
        `Prelude.hashWithSalt` startedAfter
        `Prelude.hashWithSalt` startedBefore

instance
  Prelude.NFData
    DataQualityResultFilterCriteria
  where
  rnf DataQualityResultFilterCriteria' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRunId
      `Prelude.seq` Prelude.rnf startedAfter
      `Prelude.seq` Prelude.rnf startedBefore

instance Data.ToJSON DataQualityResultFilterCriteria where
  toJSON DataQualityResultFilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSource" Data..=) Prelude.<$> dataSource,
            ("JobName" Data..=) Prelude.<$> jobName,
            ("JobRunId" Data..=) Prelude.<$> jobRunId,
            ("StartedAfter" Data..=) Prelude.<$> startedAfter,
            ("StartedBefore" Data..=) Prelude.<$> startedBefore
          ]
      )
