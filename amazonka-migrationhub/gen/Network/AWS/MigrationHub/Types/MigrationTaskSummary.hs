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
-- Module      : Network.AWS.MigrationHub.Types.MigrationTaskSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationTaskSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.MigrationStatus

-- | MigrationTaskSummary includes @MigrationTaskName@, @ProgressPercent@,
-- @ProgressUpdateStream@, @Status@, and @UpdateDateTime@ for each task.
--
-- /See:/ 'newMigrationTaskSummary' smart constructor.
data MigrationTaskSummary = MigrationTaskSummary'
  { -- | Status of the task.
    status :: Core.Maybe MigrationStatus,
    -- | Indication of the percentage completion of the task.
    progressPercent :: Core.Maybe Core.Natural,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Core.Maybe Core.POSIX,
    -- | Detail information of what is being done within the overall status
    -- state.
    statusDetail :: Core.Maybe Core.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Core.Maybe Core.Text,
    -- | An AWS resource used for access control. It should uniquely identify the
    -- migration tool as it is used for all updates made by the tool.
    progressUpdateStream :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MigrationTaskSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'migrationTaskSummary_status' - Status of the task.
--
-- 'progressPercent', 'migrationTaskSummary_progressPercent' - Indication of the percentage completion of the task.
--
-- 'updateDateTime', 'migrationTaskSummary_updateDateTime' - The timestamp when the task was gathered.
--
-- 'statusDetail', 'migrationTaskSummary_statusDetail' - Detail information of what is being done within the overall status
-- state.
--
-- 'migrationTaskName', 'migrationTaskSummary_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'progressUpdateStream', 'migrationTaskSummary_progressUpdateStream' - An AWS resource used for access control. It should uniquely identify the
-- migration tool as it is used for all updates made by the tool.
newMigrationTaskSummary ::
  MigrationTaskSummary
newMigrationTaskSummary =
  MigrationTaskSummary'
    { status = Core.Nothing,
      progressPercent = Core.Nothing,
      updateDateTime = Core.Nothing,
      statusDetail = Core.Nothing,
      migrationTaskName = Core.Nothing,
      progressUpdateStream = Core.Nothing
    }

-- | Status of the task.
migrationTaskSummary_status :: Lens.Lens' MigrationTaskSummary (Core.Maybe MigrationStatus)
migrationTaskSummary_status = Lens.lens (\MigrationTaskSummary' {status} -> status) (\s@MigrationTaskSummary' {} a -> s {status = a} :: MigrationTaskSummary)

-- | Indication of the percentage completion of the task.
migrationTaskSummary_progressPercent :: Lens.Lens' MigrationTaskSummary (Core.Maybe Core.Natural)
migrationTaskSummary_progressPercent = Lens.lens (\MigrationTaskSummary' {progressPercent} -> progressPercent) (\s@MigrationTaskSummary' {} a -> s {progressPercent = a} :: MigrationTaskSummary)

-- | The timestamp when the task was gathered.
migrationTaskSummary_updateDateTime :: Lens.Lens' MigrationTaskSummary (Core.Maybe Core.UTCTime)
migrationTaskSummary_updateDateTime = Lens.lens (\MigrationTaskSummary' {updateDateTime} -> updateDateTime) (\s@MigrationTaskSummary' {} a -> s {updateDateTime = a} :: MigrationTaskSummary) Core.. Lens.mapping Core._Time

-- | Detail information of what is being done within the overall status
-- state.
migrationTaskSummary_statusDetail :: Lens.Lens' MigrationTaskSummary (Core.Maybe Core.Text)
migrationTaskSummary_statusDetail = Lens.lens (\MigrationTaskSummary' {statusDetail} -> statusDetail) (\s@MigrationTaskSummary' {} a -> s {statusDetail = a} :: MigrationTaskSummary)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
migrationTaskSummary_migrationTaskName :: Lens.Lens' MigrationTaskSummary (Core.Maybe Core.Text)
migrationTaskSummary_migrationTaskName = Lens.lens (\MigrationTaskSummary' {migrationTaskName} -> migrationTaskName) (\s@MigrationTaskSummary' {} a -> s {migrationTaskName = a} :: MigrationTaskSummary)

-- | An AWS resource used for access control. It should uniquely identify the
-- migration tool as it is used for all updates made by the tool.
migrationTaskSummary_progressUpdateStream :: Lens.Lens' MigrationTaskSummary (Core.Maybe Core.Text)
migrationTaskSummary_progressUpdateStream = Lens.lens (\MigrationTaskSummary' {progressUpdateStream} -> progressUpdateStream) (\s@MigrationTaskSummary' {} a -> s {progressUpdateStream = a} :: MigrationTaskSummary)

instance Core.FromJSON MigrationTaskSummary where
  parseJSON =
    Core.withObject
      "MigrationTaskSummary"
      ( \x ->
          MigrationTaskSummary'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "ProgressPercent")
            Core.<*> (x Core..:? "UpdateDateTime")
            Core.<*> (x Core..:? "StatusDetail")
            Core.<*> (x Core..:? "MigrationTaskName")
            Core.<*> (x Core..:? "ProgressUpdateStream")
      )

instance Core.Hashable MigrationTaskSummary

instance Core.NFData MigrationTaskSummary
