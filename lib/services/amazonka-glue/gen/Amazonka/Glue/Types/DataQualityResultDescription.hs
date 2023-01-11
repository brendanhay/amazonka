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
-- Module      : Amazonka.Glue.Types.DataQualityResultDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityResultDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataSource
import qualified Amazonka.Prelude as Prelude

-- | Describes a data quality result.
--
-- /See:/ 'newDataQualityResultDescription' smart constructor.
data DataQualityResultDescription = DataQualityResultDescription'
  { -- | The table name associated with the data quality result.
    dataSource :: Prelude.Maybe DataSource,
    -- | The job name associated with the data quality result.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The job run ID associated with the data quality result.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | The unique result ID for this data quality result.
    resultId :: Prelude.Maybe Prelude.Text,
    -- | The time that the run started for this data quality result.
    startedOn :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityResultDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'dataQualityResultDescription_dataSource' - The table name associated with the data quality result.
--
-- 'jobName', 'dataQualityResultDescription_jobName' - The job name associated with the data quality result.
--
-- 'jobRunId', 'dataQualityResultDescription_jobRunId' - The job run ID associated with the data quality result.
--
-- 'resultId', 'dataQualityResultDescription_resultId' - The unique result ID for this data quality result.
--
-- 'startedOn', 'dataQualityResultDescription_startedOn' - The time that the run started for this data quality result.
newDataQualityResultDescription ::
  DataQualityResultDescription
newDataQualityResultDescription =
  DataQualityResultDescription'
    { dataSource =
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      resultId = Prelude.Nothing,
      startedOn = Prelude.Nothing
    }

-- | The table name associated with the data quality result.
dataQualityResultDescription_dataSource :: Lens.Lens' DataQualityResultDescription (Prelude.Maybe DataSource)
dataQualityResultDescription_dataSource = Lens.lens (\DataQualityResultDescription' {dataSource} -> dataSource) (\s@DataQualityResultDescription' {} a -> s {dataSource = a} :: DataQualityResultDescription)

-- | The job name associated with the data quality result.
dataQualityResultDescription_jobName :: Lens.Lens' DataQualityResultDescription (Prelude.Maybe Prelude.Text)
dataQualityResultDescription_jobName = Lens.lens (\DataQualityResultDescription' {jobName} -> jobName) (\s@DataQualityResultDescription' {} a -> s {jobName = a} :: DataQualityResultDescription)

-- | The job run ID associated with the data quality result.
dataQualityResultDescription_jobRunId :: Lens.Lens' DataQualityResultDescription (Prelude.Maybe Prelude.Text)
dataQualityResultDescription_jobRunId = Lens.lens (\DataQualityResultDescription' {jobRunId} -> jobRunId) (\s@DataQualityResultDescription' {} a -> s {jobRunId = a} :: DataQualityResultDescription)

-- | The unique result ID for this data quality result.
dataQualityResultDescription_resultId :: Lens.Lens' DataQualityResultDescription (Prelude.Maybe Prelude.Text)
dataQualityResultDescription_resultId = Lens.lens (\DataQualityResultDescription' {resultId} -> resultId) (\s@DataQualityResultDescription' {} a -> s {resultId = a} :: DataQualityResultDescription)

-- | The time that the run started for this data quality result.
dataQualityResultDescription_startedOn :: Lens.Lens' DataQualityResultDescription (Prelude.Maybe Prelude.UTCTime)
dataQualityResultDescription_startedOn = Lens.lens (\DataQualityResultDescription' {startedOn} -> startedOn) (\s@DataQualityResultDescription' {} a -> s {startedOn = a} :: DataQualityResultDescription) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DataQualityResultDescription where
  parseJSON =
    Data.withObject
      "DataQualityResultDescription"
      ( \x ->
          DataQualityResultDescription'
            Prelude.<$> (x Data..:? "DataSource")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobRunId")
            Prelude.<*> (x Data..:? "ResultId")
            Prelude.<*> (x Data..:? "StartedOn")
      )

instance
  Prelude.Hashable
    DataQualityResultDescription
  where
  hashWithSalt _salt DataQualityResultDescription' {..} =
    _salt `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobRunId
      `Prelude.hashWithSalt` resultId
      `Prelude.hashWithSalt` startedOn

instance Prelude.NFData DataQualityResultDescription where
  rnf DataQualityResultDescription' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRunId
      `Prelude.seq` Prelude.rnf resultId
      `Prelude.seq` Prelude.rnf startedOn
