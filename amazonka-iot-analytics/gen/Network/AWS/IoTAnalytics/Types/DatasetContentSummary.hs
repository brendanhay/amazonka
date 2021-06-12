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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.DatasetContentStatus
import qualified Network.AWS.Lens as Lens

-- | Summary information about dataset contents.
--
-- /See:/ 'newDatasetContentSummary' smart constructor.
data DatasetContentSummary = DatasetContentSummary'
  { -- | The status of the data set contents.
    status :: Core.Maybe DatasetContentStatus,
    -- | The actual time the creation of the dataset contents was started.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The time the dataset content status was updated to SUCCEEDED or FAILED.
    completionTime :: Core.Maybe Core.POSIX,
    -- | The time the creation of the dataset contents was scheduled to start.
    scheduleTime :: Core.Maybe Core.POSIX,
    -- | The version of the dataset contents.
    version :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DatasetContentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'datasetContentSummary_status' - The status of the data set contents.
--
-- 'creationTime', 'datasetContentSummary_creationTime' - The actual time the creation of the dataset contents was started.
--
-- 'completionTime', 'datasetContentSummary_completionTime' - The time the dataset content status was updated to SUCCEEDED or FAILED.
--
-- 'scheduleTime', 'datasetContentSummary_scheduleTime' - The time the creation of the dataset contents was scheduled to start.
--
-- 'version', 'datasetContentSummary_version' - The version of the dataset contents.
newDatasetContentSummary ::
  DatasetContentSummary
newDatasetContentSummary =
  DatasetContentSummary'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      completionTime = Core.Nothing,
      scheduleTime = Core.Nothing,
      version = Core.Nothing
    }

-- | The status of the data set contents.
datasetContentSummary_status :: Lens.Lens' DatasetContentSummary (Core.Maybe DatasetContentStatus)
datasetContentSummary_status = Lens.lens (\DatasetContentSummary' {status} -> status) (\s@DatasetContentSummary' {} a -> s {status = a} :: DatasetContentSummary)

-- | The actual time the creation of the dataset contents was started.
datasetContentSummary_creationTime :: Lens.Lens' DatasetContentSummary (Core.Maybe Core.UTCTime)
datasetContentSummary_creationTime = Lens.lens (\DatasetContentSummary' {creationTime} -> creationTime) (\s@DatasetContentSummary' {} a -> s {creationTime = a} :: DatasetContentSummary) Core.. Lens.mapping Core._Time

-- | The time the dataset content status was updated to SUCCEEDED or FAILED.
datasetContentSummary_completionTime :: Lens.Lens' DatasetContentSummary (Core.Maybe Core.UTCTime)
datasetContentSummary_completionTime = Lens.lens (\DatasetContentSummary' {completionTime} -> completionTime) (\s@DatasetContentSummary' {} a -> s {completionTime = a} :: DatasetContentSummary) Core.. Lens.mapping Core._Time

-- | The time the creation of the dataset contents was scheduled to start.
datasetContentSummary_scheduleTime :: Lens.Lens' DatasetContentSummary (Core.Maybe Core.UTCTime)
datasetContentSummary_scheduleTime = Lens.lens (\DatasetContentSummary' {scheduleTime} -> scheduleTime) (\s@DatasetContentSummary' {} a -> s {scheduleTime = a} :: DatasetContentSummary) Core.. Lens.mapping Core._Time

-- | The version of the dataset contents.
datasetContentSummary_version :: Lens.Lens' DatasetContentSummary (Core.Maybe Core.Text)
datasetContentSummary_version = Lens.lens (\DatasetContentSummary' {version} -> version) (\s@DatasetContentSummary' {} a -> s {version = a} :: DatasetContentSummary)

instance Core.FromJSON DatasetContentSummary where
  parseJSON =
    Core.withObject
      "DatasetContentSummary"
      ( \x ->
          DatasetContentSummary'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "completionTime")
            Core.<*> (x Core..:? "scheduleTime")
            Core.<*> (x Core..:? "version")
      )

instance Core.Hashable DatasetContentSummary

instance Core.NFData DatasetContentSummary
