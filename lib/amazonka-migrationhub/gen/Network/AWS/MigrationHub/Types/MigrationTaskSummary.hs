{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationTaskSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationTaskSummary
  ( MigrationTaskSummary (..),

    -- * Smart constructor
    mkMigrationTaskSummary,

    -- * Lenses
    mtsStatus,
    mtsUpdateDateTime,
    mtsProgressPercent,
    mtsStatusDetail,
    mtsProgressUpdateStream,
    mtsMigrationTaskName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.MigrationStatus
import qualified Network.AWS.Prelude as Lude

-- | MigrationTaskSummary includes @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and @UpdateDateTime@ for each task.
--
-- /See:/ 'mkMigrationTaskSummary' smart constructor.
data MigrationTaskSummary = MigrationTaskSummary'
  { -- | Status of the task.
    status :: Lude.Maybe MigrationStatus,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Lude.Maybe Lude.Timestamp,
    -- | Indication of the percentage completion of the task.
    progressPercent :: Lude.Maybe Lude.Natural,
    -- | Detail information of what is being done within the overall status state.
    statusDetail :: Lude.Maybe Lude.Text,
    -- | An AWS resource used for access control. It should uniquely identify the migration tool as it is used for all updates made by the tool.
    progressUpdateStream :: Lude.Maybe Lude.Text,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MigrationTaskSummary' with the minimum fields required to make a request.
--
-- * 'status' - Status of the task.
-- * 'updateDateTime' - The timestamp when the task was gathered.
-- * 'progressPercent' - Indication of the percentage completion of the task.
-- * 'statusDetail' - Detail information of what is being done within the overall status state.
-- * 'progressUpdateStream' - An AWS resource used for access control. It should uniquely identify the migration tool as it is used for all updates made by the tool.
-- * 'migrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
mkMigrationTaskSummary ::
  MigrationTaskSummary
mkMigrationTaskSummary =
  MigrationTaskSummary'
    { status = Lude.Nothing,
      updateDateTime = Lude.Nothing,
      progressPercent = Lude.Nothing,
      statusDetail = Lude.Nothing,
      progressUpdateStream = Lude.Nothing,
      migrationTaskName = Lude.Nothing
    }

-- | Status of the task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsStatus :: Lens.Lens' MigrationTaskSummary (Lude.Maybe MigrationStatus)
mtsStatus = Lens.lens (status :: MigrationTaskSummary -> Lude.Maybe MigrationStatus) (\s a -> s {status = a} :: MigrationTaskSummary)
{-# DEPRECATED mtsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The timestamp when the task was gathered.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsUpdateDateTime :: Lens.Lens' MigrationTaskSummary (Lude.Maybe Lude.Timestamp)
mtsUpdateDateTime = Lens.lens (updateDateTime :: MigrationTaskSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {updateDateTime = a} :: MigrationTaskSummary)
{-# DEPRECATED mtsUpdateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead." #-}

-- | Indication of the percentage completion of the task.
--
-- /Note:/ Consider using 'progressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsProgressPercent :: Lens.Lens' MigrationTaskSummary (Lude.Maybe Lude.Natural)
mtsProgressPercent = Lens.lens (progressPercent :: MigrationTaskSummary -> Lude.Maybe Lude.Natural) (\s a -> s {progressPercent = a} :: MigrationTaskSummary)
{-# DEPRECATED mtsProgressPercent "Use generic-lens or generic-optics with 'progressPercent' instead." #-}

-- | Detail information of what is being done within the overall status state.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsStatusDetail :: Lens.Lens' MigrationTaskSummary (Lude.Maybe Lude.Text)
mtsStatusDetail = Lens.lens (statusDetail :: MigrationTaskSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusDetail = a} :: MigrationTaskSummary)
{-# DEPRECATED mtsStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | An AWS resource used for access control. It should uniquely identify the migration tool as it is used for all updates made by the tool.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsProgressUpdateStream :: Lens.Lens' MigrationTaskSummary (Lude.Maybe Lude.Text)
mtsProgressUpdateStream = Lens.lens (progressUpdateStream :: MigrationTaskSummary -> Lude.Maybe Lude.Text) (\s a -> s {progressUpdateStream = a} :: MigrationTaskSummary)
{-# DEPRECATED mtsProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsMigrationTaskName :: Lens.Lens' MigrationTaskSummary (Lude.Maybe Lude.Text)
mtsMigrationTaskName = Lens.lens (migrationTaskName :: MigrationTaskSummary -> Lude.Maybe Lude.Text) (\s a -> s {migrationTaskName = a} :: MigrationTaskSummary)
{-# DEPRECATED mtsMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

instance Lude.FromJSON MigrationTaskSummary where
  parseJSON =
    Lude.withObject
      "MigrationTaskSummary"
      ( \x ->
          MigrationTaskSummary'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "UpdateDateTime")
            Lude.<*> (x Lude..:? "ProgressPercent")
            Lude.<*> (x Lude..:? "StatusDetail")
            Lude.<*> (x Lude..:? "ProgressUpdateStream")
            Lude.<*> (x Lude..:? "MigrationTaskName")
      )
