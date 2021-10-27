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
-- Module      : Network.AWS.Kendra.Types.DataSourceSyncJobMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DataSourceSyncJobMetrics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Maps a batch delete document request to a specific data source sync job.
-- This is optional and should only be supplied when documents are deleted
-- by a data source connector.
--
-- /See:/ 'newDataSourceSyncJobMetrics' smart constructor.
data DataSourceSyncJobMetrics = DataSourceSyncJobMetrics'
  { -- | The number of documents added from the data source up to now in the data
    -- source sync.
    documentsAdded :: Prelude.Maybe Prelude.Text,
    -- | The number of documents that failed to sync from the data source up to
    -- now in the data source sync run.
    documentsFailed :: Prelude.Maybe Prelude.Text,
    -- | The number of documents deleted from the data source up to now in the
    -- data source sync run.
    documentsDeleted :: Prelude.Maybe Prelude.Text,
    -- | The current number of documents crawled by the current sync job in the
    -- data source.
    documentsScanned :: Prelude.Maybe Prelude.Text,
    -- | The number of documents modified in the data source up to now in the
    -- data source sync run.
    documentsModified :: Prelude.Maybe Prelude.Text
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
-- 'documentsFailed', 'dataSourceSyncJobMetrics_documentsFailed' - The number of documents that failed to sync from the data source up to
-- now in the data source sync run.
--
-- 'documentsDeleted', 'dataSourceSyncJobMetrics_documentsDeleted' - The number of documents deleted from the data source up to now in the
-- data source sync run.
--
-- 'documentsScanned', 'dataSourceSyncJobMetrics_documentsScanned' - The current number of documents crawled by the current sync job in the
-- data source.
--
-- 'documentsModified', 'dataSourceSyncJobMetrics_documentsModified' - The number of documents modified in the data source up to now in the
-- data source sync run.
newDataSourceSyncJobMetrics ::
  DataSourceSyncJobMetrics
newDataSourceSyncJobMetrics =
  DataSourceSyncJobMetrics'
    { documentsAdded =
        Prelude.Nothing,
      documentsFailed = Prelude.Nothing,
      documentsDeleted = Prelude.Nothing,
      documentsScanned = Prelude.Nothing,
      documentsModified = Prelude.Nothing
    }

-- | The number of documents added from the data source up to now in the data
-- source sync.
dataSourceSyncJobMetrics_documentsAdded :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsAdded = Lens.lens (\DataSourceSyncJobMetrics' {documentsAdded} -> documentsAdded) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsAdded = a} :: DataSourceSyncJobMetrics)

-- | The number of documents that failed to sync from the data source up to
-- now in the data source sync run.
dataSourceSyncJobMetrics_documentsFailed :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsFailed = Lens.lens (\DataSourceSyncJobMetrics' {documentsFailed} -> documentsFailed) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsFailed = a} :: DataSourceSyncJobMetrics)

-- | The number of documents deleted from the data source up to now in the
-- data source sync run.
dataSourceSyncJobMetrics_documentsDeleted :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsDeleted = Lens.lens (\DataSourceSyncJobMetrics' {documentsDeleted} -> documentsDeleted) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsDeleted = a} :: DataSourceSyncJobMetrics)

-- | The current number of documents crawled by the current sync job in the
-- data source.
dataSourceSyncJobMetrics_documentsScanned :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsScanned = Lens.lens (\DataSourceSyncJobMetrics' {documentsScanned} -> documentsScanned) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsScanned = a} :: DataSourceSyncJobMetrics)

-- | The number of documents modified in the data source up to now in the
-- data source sync run.
dataSourceSyncJobMetrics_documentsModified :: Lens.Lens' DataSourceSyncJobMetrics (Prelude.Maybe Prelude.Text)
dataSourceSyncJobMetrics_documentsModified = Lens.lens (\DataSourceSyncJobMetrics' {documentsModified} -> documentsModified) (\s@DataSourceSyncJobMetrics' {} a -> s {documentsModified = a} :: DataSourceSyncJobMetrics)

instance Core.FromJSON DataSourceSyncJobMetrics where
  parseJSON =
    Core.withObject
      "DataSourceSyncJobMetrics"
      ( \x ->
          DataSourceSyncJobMetrics'
            Prelude.<$> (x Core..:? "DocumentsAdded")
            Prelude.<*> (x Core..:? "DocumentsFailed")
            Prelude.<*> (x Core..:? "DocumentsDeleted")
            Prelude.<*> (x Core..:? "DocumentsScanned")
            Prelude.<*> (x Core..:? "DocumentsModified")
      )

instance Prelude.Hashable DataSourceSyncJobMetrics

instance Prelude.NFData DataSourceSyncJobMetrics
