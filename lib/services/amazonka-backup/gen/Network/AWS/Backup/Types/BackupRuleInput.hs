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
-- Module      : Network.AWS.Backup.Types.BackupRuleInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Backup.Types.BackupRuleInput where

import Network.AWS.Backup.Types.CopyAction
import Network.AWS.Backup.Types.Lifecycle
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a scheduled task used to back up a selection of resources.
--
-- /See:/ 'newBackupRuleInput' smart constructor.
data BackupRuleInput = BackupRuleInput'
  { -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup will transition and expire backups
    -- automatically according to the lifecycle that you define.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days. Therefore, the “expire after days” setting must be
    -- 90 days greater than the “transition to cold after days” setting. The
    -- “transition to cold after days” setting cannot be changed after a backup
    -- has been transitioned to cold.
    --
    -- Only Amazon EFS file system backups can be transitioned to cold storage.
    lifecycle :: Prelude.Maybe Lifecycle,
    -- | To help organize your resources, you can assign your own metadata to the
    -- resources that you create. Each tag is a key-value pair.
    recoveryPointTags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A CRON expression in UTC specifying when Backup initiates a backup job.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether Backup creates continuous backups. True causes Backup
    -- to create continuous backups capable of point-in-time restore (PITR).
    -- False (or not specified) causes Backup to create snapshot backups.
    enableContinuousBackup :: Prelude.Maybe Prelude.Bool,
    -- | A value in minutes after a backup job is successfully started before it
    -- must be completed or it will be canceled by Backup. This value is
    -- optional.
    completionWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | An array of @CopyAction@ objects, which contains the details of the copy
    -- operation.
    copyActions :: Prelude.Maybe [CopyAction],
    -- | A value in minutes after a backup is scheduled before a job will be
    -- canceled if it doesn\'t start successfully. This value is optional.
    startWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | An optional display name for a backup rule.
    ruleName :: Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    targetBackupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupRuleInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'backupRuleInput_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup will transition and expire backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “expire after days” setting must be
-- 90 days greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Only Amazon EFS file system backups can be transitioned to cold storage.
--
-- 'recoveryPointTags', 'backupRuleInput_recoveryPointTags' - To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair.
--
-- 'scheduleExpression', 'backupRuleInput_scheduleExpression' - A CRON expression in UTC specifying when Backup initiates a backup job.
--
-- 'enableContinuousBackup', 'backupRuleInput_enableContinuousBackup' - Specifies whether Backup creates continuous backups. True causes Backup
-- to create continuous backups capable of point-in-time restore (PITR).
-- False (or not specified) causes Backup to create snapshot backups.
--
-- 'completionWindowMinutes', 'backupRuleInput_completionWindowMinutes' - A value in minutes after a backup job is successfully started before it
-- must be completed or it will be canceled by Backup. This value is
-- optional.
--
-- 'copyActions', 'backupRuleInput_copyActions' - An array of @CopyAction@ objects, which contains the details of the copy
-- operation.
--
-- 'startWindowMinutes', 'backupRuleInput_startWindowMinutes' - A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional.
--
-- 'ruleName', 'backupRuleInput_ruleName' - An optional display name for a backup rule.
--
-- 'targetBackupVaultName', 'backupRuleInput_targetBackupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newBackupRuleInput ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'targetBackupVaultName'
  Prelude.Text ->
  BackupRuleInput
newBackupRuleInput pRuleName_ pTargetBackupVaultName_ =
  BackupRuleInput'
    { lifecycle = Prelude.Nothing,
      recoveryPointTags = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      enableContinuousBackup = Prelude.Nothing,
      completionWindowMinutes = Prelude.Nothing,
      copyActions = Prelude.Nothing,
      startWindowMinutes = Prelude.Nothing,
      ruleName = pRuleName_,
      targetBackupVaultName = pTargetBackupVaultName_
    }

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup will transition and expire backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “expire after days” setting must be
-- 90 days greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Only Amazon EFS file system backups can be transitioned to cold storage.
backupRuleInput_lifecycle :: Lens.Lens' BackupRuleInput (Prelude.Maybe Lifecycle)
backupRuleInput_lifecycle = Lens.lens (\BackupRuleInput' {lifecycle} -> lifecycle) (\s@BackupRuleInput' {} a -> s {lifecycle = a} :: BackupRuleInput)

-- | To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair.
backupRuleInput_recoveryPointTags :: Lens.Lens' BackupRuleInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
backupRuleInput_recoveryPointTags = Lens.lens (\BackupRuleInput' {recoveryPointTags} -> recoveryPointTags) (\s@BackupRuleInput' {} a -> s {recoveryPointTags = a} :: BackupRuleInput) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | A CRON expression in UTC specifying when Backup initiates a backup job.
backupRuleInput_scheduleExpression :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Text)
backupRuleInput_scheduleExpression = Lens.lens (\BackupRuleInput' {scheduleExpression} -> scheduleExpression) (\s@BackupRuleInput' {} a -> s {scheduleExpression = a} :: BackupRuleInput)

-- | Specifies whether Backup creates continuous backups. True causes Backup
-- to create continuous backups capable of point-in-time restore (PITR).
-- False (or not specified) causes Backup to create snapshot backups.
backupRuleInput_enableContinuousBackup :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Bool)
backupRuleInput_enableContinuousBackup = Lens.lens (\BackupRuleInput' {enableContinuousBackup} -> enableContinuousBackup) (\s@BackupRuleInput' {} a -> s {enableContinuousBackup = a} :: BackupRuleInput)

-- | A value in minutes after a backup job is successfully started before it
-- must be completed or it will be canceled by Backup. This value is
-- optional.
backupRuleInput_completionWindowMinutes :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Integer)
backupRuleInput_completionWindowMinutes = Lens.lens (\BackupRuleInput' {completionWindowMinutes} -> completionWindowMinutes) (\s@BackupRuleInput' {} a -> s {completionWindowMinutes = a} :: BackupRuleInput)

-- | An array of @CopyAction@ objects, which contains the details of the copy
-- operation.
backupRuleInput_copyActions :: Lens.Lens' BackupRuleInput (Prelude.Maybe [CopyAction])
backupRuleInput_copyActions = Lens.lens (\BackupRuleInput' {copyActions} -> copyActions) (\s@BackupRuleInput' {} a -> s {copyActions = a} :: BackupRuleInput) Prelude.. Lens.mapping Lens.coerced

-- | A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional.
backupRuleInput_startWindowMinutes :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Integer)
backupRuleInput_startWindowMinutes = Lens.lens (\BackupRuleInput' {startWindowMinutes} -> startWindowMinutes) (\s@BackupRuleInput' {} a -> s {startWindowMinutes = a} :: BackupRuleInput)

-- | An optional display name for a backup rule.
backupRuleInput_ruleName :: Lens.Lens' BackupRuleInput Prelude.Text
backupRuleInput_ruleName = Lens.lens (\BackupRuleInput' {ruleName} -> ruleName) (\s@BackupRuleInput' {} a -> s {ruleName = a} :: BackupRuleInput)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
backupRuleInput_targetBackupVaultName :: Lens.Lens' BackupRuleInput Prelude.Text
backupRuleInput_targetBackupVaultName = Lens.lens (\BackupRuleInput' {targetBackupVaultName} -> targetBackupVaultName) (\s@BackupRuleInput' {} a -> s {targetBackupVaultName = a} :: BackupRuleInput)

instance Prelude.Hashable BackupRuleInput

instance Prelude.NFData BackupRuleInput

instance Core.ToJSON BackupRuleInput where
  toJSON BackupRuleInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Lifecycle" Core..=) Prelude.<$> lifecycle,
            ("RecoveryPointTags" Core..=)
              Prelude.<$> recoveryPointTags,
            ("ScheduleExpression" Core..=)
              Prelude.<$> scheduleExpression,
            ("EnableContinuousBackup" Core..=)
              Prelude.<$> enableContinuousBackup,
            ("CompletionWindowMinutes" Core..=)
              Prelude.<$> completionWindowMinutes,
            ("CopyActions" Core..=) Prelude.<$> copyActions,
            ("StartWindowMinutes" Core..=)
              Prelude.<$> startWindowMinutes,
            Prelude.Just ("RuleName" Core..= ruleName),
            Prelude.Just
              ( "TargetBackupVaultName"
                  Core..= targetBackupVaultName
              )
          ]
      )
