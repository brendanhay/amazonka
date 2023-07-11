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
-- Module      : Amazonka.Kendra.Types.DataSourceSyncJobMetricTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceSyncJobMetricTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Maps a particular data source sync job to a particular data source.
--
-- /See:/ 'newDataSourceSyncJobMetricTarget' smart constructor.
data DataSourceSyncJobMetricTarget = DataSourceSyncJobMetricTarget'
  { -- | The ID of the sync job that is running on the data source.
    --
    -- If the ID of a sync job is not provided and there is a sync job running,
    -- then the ID of this sync job is used and metrics are generated for this
    -- sync job.
    --
    -- If the ID of a sync job is not provided and there is no sync job
    -- running, then no metrics are generated and documents are
    -- indexed\/deleted at the index level without sync job metrics included.
    dataSourceSyncJobId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the data source that is running the sync job.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceSyncJobMetricTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceSyncJobId', 'dataSourceSyncJobMetricTarget_dataSourceSyncJobId' - The ID of the sync job that is running on the data source.
--
-- If the ID of a sync job is not provided and there is a sync job running,
-- then the ID of this sync job is used and metrics are generated for this
-- sync job.
--
-- If the ID of a sync job is not provided and there is no sync job
-- running, then no metrics are generated and documents are
-- indexed\/deleted at the index level without sync job metrics included.
--
-- 'dataSourceId', 'dataSourceSyncJobMetricTarget_dataSourceId' - The ID of the data source that is running the sync job.
newDataSourceSyncJobMetricTarget ::
  -- | 'dataSourceId'
  Prelude.Text ->
  DataSourceSyncJobMetricTarget
newDataSourceSyncJobMetricTarget pDataSourceId_ =
  DataSourceSyncJobMetricTarget'
    { dataSourceSyncJobId =
        Prelude.Nothing,
      dataSourceId = pDataSourceId_
    }

-- | The ID of the sync job that is running on the data source.
--
-- If the ID of a sync job is not provided and there is a sync job running,
-- then the ID of this sync job is used and metrics are generated for this
-- sync job.
--
-- If the ID of a sync job is not provided and there is no sync job
-- running, then no metrics are generated and documents are
-- indexed\/deleted at the index level without sync job metrics included.
dataSourceSyncJobMetricTarget_dataSourceSyncJobId :: Lens.Lens' DataSourceSyncJobMetricTarget (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetricTarget_dataSourceSyncJobId = Lens.lens (\DataSourceSyncJobMetricTarget' {dataSourceSyncJobId} -> dataSourceSyncJobId) (\s@DataSourceSyncJobMetricTarget' {} a -> s {dataSourceSyncJobId = a} :: DataSourceSyncJobMetricTarget)

-- | The ID of the data source that is running the sync job.
dataSourceSyncJobMetricTarget_dataSourceId :: Lens.Lens' DataSourceSyncJobMetricTarget Prelude.Text
dataSourceSyncJobMetricTarget_dataSourceId = Lens.lens (\DataSourceSyncJobMetricTarget' {dataSourceId} -> dataSourceId) (\s@DataSourceSyncJobMetricTarget' {} a -> s {dataSourceId = a} :: DataSourceSyncJobMetricTarget)

instance
  Prelude.Hashable
    DataSourceSyncJobMetricTarget
  where
  hashWithSalt _salt DataSourceSyncJobMetricTarget' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceSyncJobId
      `Prelude.hashWithSalt` dataSourceId

instance Prelude.NFData DataSourceSyncJobMetricTarget where
  rnf DataSourceSyncJobMetricTarget' {..} =
    Prelude.rnf dataSourceSyncJobId
      `Prelude.seq` Prelude.rnf dataSourceId

instance Data.ToJSON DataSourceSyncJobMetricTarget where
  toJSON DataSourceSyncJobMetricTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceSyncJobId" Data..=)
              Prelude.<$> dataSourceSyncJobId,
            Prelude.Just ("DataSourceId" Data..= dataSourceId)
          ]
      )
