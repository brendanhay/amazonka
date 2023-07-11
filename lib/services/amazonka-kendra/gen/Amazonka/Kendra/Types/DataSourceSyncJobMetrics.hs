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
-- Module      : Amazonka.Kendra.Types.DataSourceSyncJobMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceSyncJobMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Maps a batch delete document request to a specific data source sync job.
-- This is optional and should only be supplied when documents are deleted
-- by a data source connector.
--
-- /See:/ 'newDataSourceSyncJobMetrics' smart constructor.
data DataSourceSyncJobMetrics = DataSourceSyncJobMetrics'
  { -- | The number of documents added from the data source up to now in the data
    -- source sync.
    documentsAdded :: Prelude.Maybe Prelude.Text,
    -- | The number of documents deleted from the data source up to now in the
    -- data source sync run.
    documentsDeleted :: Prelude.Maybe Prelude.Text,
    -- | The number of documents that failed to sync from the data source up to
    -- now in the data source sync run.
    documentsFailed :: Prelude.Maybe Prelude.Text,
    -- | The number of documents modified in the data source up to now in the
    -- data source sync run.
    documentsModified :: Prelude.Maybe Prelude.Text,
    -- | The current number of documents crawled by the current sync job in the
    -- data source.
    documentsScanned :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceSyncJobMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentsAdded', 'dataSourceSyncJobMetrics_documentsAdded' - The number of documents added from the data source up to now in the data
-- source sync.
--
-- 'documentsDeleted', 'dataSourceSyncJobMetrics_documentsDeleted' - The number of documents deleted from the data source up to now in the
-- data source sync run.
--
-- 'documentsFailed', 'dataSourceSyncJobMetrics_documentsFailed' - The number of documents that failed to sync from the data source up to
-- now in the data source sync run.
--
-- 'documentsModified', 'dataSourceSyncJobMetrics_documentsModified' - The number of documents modified in the data source up to now in the
-- data source sync run.
--
-- 'documentsScanned', 'dataSourceSyncJobMetrics_documentsScanned' - The current number of documents crawled by the current sync job in the
-- data source.
newDataSourceSyncJobMetrics ::
  DataSourceSyncJobMetrics
newDataSourceSyncJobMetrics =
  DataSourceSyncJobMetrics'
    { documentsAdded =
        Prelude.Nothing,
      documentsDeleted = Prelude.Nothing,
      documentsFailed = Prelude.Nothing,
      documentsModified = Prelude.Nothing,
      documentsScanned = Prelude.Nothing
    }

-- | The number of documents added from the data source up to now in the data
-- source sync.
dataSourceSyncJobMetrics_documentsAdded :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsAdded = Lens.lens (\DataSourceSyncJobMetrics' {documentsAdded} -> documentsAdded) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsAdded = a} :: DataSourceSyncJobMetrics)

-- | The number of documents deleted from the data source up to now in the
-- data source sync run.
dataSourceSyncJobMetrics_documentsDeleted :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsDeleted = Lens.lens (\DataSourceSyncJobMetrics' {documentsDeleted} -> documentsDeleted) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsDeleted = a} :: DataSourceSyncJobMetrics)

-- | The number of documents that failed to sync from the data source up to
-- now in the data source sync run.
dataSourceSyncJobMetrics_documentsFailed :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsFailed = Lens.lens (\DataSourceSyncJobMetrics' {documentsFailed} -> documentsFailed) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsFailed = a} :: DataSourceSyncJobMetrics)

-- | The number of documents modified in the data source up to now in the
-- data source sync run.
dataSourceSyncJobMetrics_documentsModified :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsModified = Lens.lens (\DataSourceSyncJobMetrics' {documentsModified} -> documentsModified) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsModified = a} :: DataSourceSyncJobMetrics)

-- | The current number of documents crawled by the current sync job in the
-- data source.
dataSourceSyncJobMetrics_documentsScanned :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsScanned = Lens.lens (\DataSourceSyncJobMetrics' {documentsScanned} -> documentsScanned) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsScanned = a} :: DataSourceSyncJobMetrics)

instance Data.FromJSON DataSourceSyncJobMetrics where
  parseJSON =
    Data.withObject
      "DataSourceSyncJobMetrics"
      ( \x ->
          DataSourceSyncJobMetrics'
            Prelude.<$> (x Data..:? "DocumentsAdded")
            Prelude.<*> (x Data..:? "DocumentsDeleted")
            Prelude.<*> (x Data..:? "DocumentsFailed")
            Prelude.<*> (x Data..:? "DocumentsModified")
            Prelude.<*> (x Data..:? "DocumentsScanned")
      )

instance Prelude.Hashable DataSourceSyncJobMetrics where
  hashWithSalt _salt DataSourceSyncJobMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` documentsAdded
      `Prelude.hashWithSalt` documentsDeleted
      `Prelude.hashWithSalt` documentsFailed
      `Prelude.hashWithSalt` documentsModified
      `Prelude.hashWithSalt` documentsScanned

instance Prelude.NFData DataSourceSyncJobMetrics where
  rnf DataSourceSyncJobMetrics' {..} =
    Prelude.rnf documentsAdded
      `Prelude.seq` Prelude.rnf documentsDeleted
      `Prelude.seq` Prelude.rnf documentsFailed
      `Prelude.seq` Prelude.rnf documentsModified
      `Prelude.seq` Prelude.rnf documentsScanned
