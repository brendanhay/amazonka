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
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the status of an export task.
--
-- /See:/ 'newExportTaskExecutionInfo' smart constructor.
data ExportTaskExecutionInfo = ExportTaskExecutionInfo'
  { -- | The creation time of the export task, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Core.Maybe Core.Natural,
    -- | The completion time of the export task, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    completionTime :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportTaskExecutionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'exportTaskExecutionInfo_creationTime' - The creation time of the export task, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'completionTime', 'exportTaskExecutionInfo_completionTime' - The completion time of the export task, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
newExportTaskExecutionInfo ::
  ExportTaskExecutionInfo
newExportTaskExecutionInfo =
  ExportTaskExecutionInfo'
    { creationTime =
        Core.Nothing,
      completionTime = Core.Nothing
    }

-- | The creation time of the export task, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
exportTaskExecutionInfo_creationTime :: Lens.Lens' ExportTaskExecutionInfo (Core.Maybe Core.Natural)
exportTaskExecutionInfo_creationTime = Lens.lens (\ExportTaskExecutionInfo' {creationTime} -> creationTime) (\s@ExportTaskExecutionInfo' {} a -> s {creationTime = a} :: ExportTaskExecutionInfo)

-- | The completion time of the export task, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
exportTaskExecutionInfo_completionTime :: Lens.Lens' ExportTaskExecutionInfo (Core.Maybe Core.Natural)
exportTaskExecutionInfo_completionTime = Lens.lens (\ExportTaskExecutionInfo' {completionTime} -> completionTime) (\s@ExportTaskExecutionInfo' {} a -> s {completionTime = a} :: ExportTaskExecutionInfo)

instance Core.FromJSON ExportTaskExecutionInfo where
  parseJSON =
    Core.withObject
      "ExportTaskExecutionInfo"
      ( \x ->
          ExportTaskExecutionInfo'
            Core.<$> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "completionTime")
      )

instance Core.Hashable ExportTaskExecutionInfo

instance Core.NFData ExportTaskExecutionInfo
