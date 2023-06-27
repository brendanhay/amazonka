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
-- Module      : Amazonka.Backup.Types.BackupRuleInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupRuleInput where

import Amazonka.Backup.Types.CopyAction
import Amazonka.Backup.Types.Lifecycle
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a scheduled task used to back up a selection of resources.
--
-- /See:/ 'newBackupRuleInput' smart constructor.
data BackupRuleInput = BackupRuleInput'
  { -- | A value in minutes after a backup job is successfully started before it
    -- must be completed or it will be canceled by Backup. This value is
    -- optional.
    completionWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | An array of @CopyAction@ objects, which contains the details of the copy
    -- operation.
    copyActions :: Prelude.Maybe [CopyAction],
    -- | Specifies whether Backup creates continuous backups. True causes Backup
    -- to create continuous backups capable of point-in-time restore (PITR).
    -- False (or not specified) causes Backup to create snapshot backups.
    enableContinuousBackup :: Prelude.Maybe Prelude.Bool,
    -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup will transition and expire backups
    -- automatically according to the lifecycle that you define.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
    -- greater than the “transition to cold after days” setting. The
    -- “transition to cold after days” setting cannot be changed after a backup
    -- has been transitioned to cold.
    --
    -- Resource types that are able to be transitioned to cold storage are
    -- listed in the \"Lifecycle to cold storage\" section of the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
    -- table. Backup ignores this expression for other resource types.
    lifecycle :: Prelude.Maybe Lifecycle,
    -- | To help organize your resources, you can assign your own metadata to the
    -- resources that you create. Each tag is a key-value pair.
    recoveryPointTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A CRON expression in UTC specifying when Backup initiates a backup job.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | A value in minutes after a backup is scheduled before a job will be
    -- canceled if it doesn\'t start successfully. This value is optional. If
    -- this value is included, it must be at least 60 minutes to avoid errors.
    --
    -- During the start window, the backup job status remains in @CREATED@
    -- status until it has successfully begun or until the start window time
    -- has run out. If within the start window time Backup receives an error
    -- that allows the job to be retried, Backup will automatically retry to
    -- begin the job at least every 10 minutes until the backup successfully
    -- begins (the job status changes to @RUNNING@) or until the job status
    -- changes to @EXPIRED@ (which is expected to occur when the start window
    -- time is over).
    startWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | A display name for a backup rule. Must contain 1 to 50 alphanumeric or
    -- \'-_.\' characters.
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
-- 'completionWindowMinutes', 'backupRuleInput_completionWindowMinutes' - A value in minutes after a backup job is successfully started before it
-- must be completed or it will be canceled by Backup. This value is
-- optional.
--
-- 'copyActions', 'backupRuleInput_copyActions' - An array of @CopyAction@ objects, which contains the details of the copy
-- operation.
--
-- 'enableContinuousBackup', 'backupRuleInput_enableContinuousBackup' - Specifies whether Backup creates continuous backups. True causes Backup
-- to create continuous backups capable of point-in-time restore (PITR).
-- False (or not specified) causes Backup to create snapshot backups.
--
-- 'lifecycle', 'backupRuleInput_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup will transition and expire backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
--
-- 'recoveryPointTags', 'backupRuleInput_recoveryPointTags' - To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair.
--
-- 'scheduleExpression', 'backupRuleInput_scheduleExpression' - A CRON expression in UTC specifying when Backup initiates a backup job.
--
-- 'startWindowMinutes', 'backupRuleInput_startWindowMinutes' - A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional. If
-- this value is included, it must be at least 60 minutes to avoid errors.
--
-- During the start window, the backup job status remains in @CREATED@
-- status until it has successfully begun or until the start window time
-- has run out. If within the start window time Backup receives an error
-- that allows the job to be retried, Backup will automatically retry to
-- begin the job at least every 10 minutes until the backup successfully
-- begins (the job status changes to @RUNNING@) or until the job status
-- changes to @EXPIRED@ (which is expected to occur when the start window
-- time is over).
--
-- 'ruleName', 'backupRuleInput_ruleName' - A display name for a backup rule. Must contain 1 to 50 alphanumeric or
-- \'-_.\' characters.
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
    { completionWindowMinutes =
        Prelude.Nothing,
      copyActions = Prelude.Nothing,
      enableContinuousBackup = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      recoveryPointTags = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      startWindowMinutes = Prelude.Nothing,
      ruleName = pRuleName_,
      targetBackupVaultName = pTargetBackupVaultName_
    }

-- | A value in minutes after a backup job is successfully started before it
-- must be completed or it will be canceled by Backup. This value is
-- optional.
backupRuleInput_completionWindowMinutes :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Integer)
backupRuleInput_completionWindowMinutes = Lens.lens (\BackupRuleInput' {completionWindowMinutes} -> completionWindowMinutes) (\s@BackupRuleInput' {} a -> s {completionWindowMinutes = a} :: BackupRuleInput)

-- | An array of @CopyAction@ objects, which contains the details of the copy
-- operation.
backupRuleInput_copyActions :: Lens.Lens' BackupRuleInput (Prelude.Maybe [CopyAction])
backupRuleInput_copyActions = Lens.lens (\BackupRuleInput' {copyActions} -> copyActions) (\s@BackupRuleInput' {} a -> s {copyActions = a} :: BackupRuleInput) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether Backup creates continuous backups. True causes Backup
-- to create continuous backups capable of point-in-time restore (PITR).
-- False (or not specified) causes Backup to create snapshot backups.
backupRuleInput_enableContinuousBackup :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Bool)
backupRuleInput_enableContinuousBackup = Lens.lens (\BackupRuleInput' {enableContinuousBackup} -> enableContinuousBackup) (\s@BackupRuleInput' {} a -> s {enableContinuousBackup = a} :: BackupRuleInput)

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup will transition and expire backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
backupRuleInput_lifecycle :: Lens.Lens' BackupRuleInput (Prelude.Maybe Lifecycle)
backupRuleInput_lifecycle = Lens.lens (\BackupRuleInput' {lifecycle} -> lifecycle) (\s@BackupRuleInput' {} a -> s {lifecycle = a} :: BackupRuleInput)

-- | To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair.
backupRuleInput_recoveryPointTags :: Lens.Lens' BackupRuleInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
backupRuleInput_recoveryPointTags = Lens.lens (\BackupRuleInput' {recoveryPointTags} -> recoveryPointTags) (\s@BackupRuleInput' {} a -> s {recoveryPointTags = a} :: BackupRuleInput) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A CRON expression in UTC specifying when Backup initiates a backup job.
backupRuleInput_scheduleExpression :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Text)
backupRuleInput_scheduleExpression = Lens.lens (\BackupRuleInput' {scheduleExpression} -> scheduleExpression) (\s@BackupRuleInput' {} a -> s {scheduleExpression = a} :: BackupRuleInput)

-- | A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional. If
-- this value is included, it must be at least 60 minutes to avoid errors.
--
-- During the start window, the backup job status remains in @CREATED@
-- status until it has successfully begun or until the start window time
-- has run out. If within the start window time Backup receives an error
-- that allows the job to be retried, Backup will automatically retry to
-- begin the job at least every 10 minutes until the backup successfully
-- begins (the job status changes to @RUNNING@) or until the job status
-- changes to @EXPIRED@ (which is expected to occur when the start window
-- time is over).
backupRuleInput_startWindowMinutes :: Lens.Lens' BackupRuleInput (Prelude.Maybe Prelude.Integer)
backupRuleInput_startWindowMinutes = Lens.lens (\BackupRuleInput' {startWindowMinutes} -> startWindowMinutes) (\s@BackupRuleInput' {} a -> s {startWindowMinutes = a} :: BackupRuleInput)

-- | A display name for a backup rule. Must contain 1 to 50 alphanumeric or
-- \'-_.\' characters.
backupRuleInput_ruleName :: Lens.Lens' BackupRuleInput Prelude.Text
backupRuleInput_ruleName = Lens.lens (\BackupRuleInput' {ruleName} -> ruleName) (\s@BackupRuleInput' {} a -> s {ruleName = a} :: BackupRuleInput)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
backupRuleInput_targetBackupVaultName :: Lens.Lens' BackupRuleInput Prelude.Text
backupRuleInput_targetBackupVaultName = Lens.lens (\BackupRuleInput' {targetBackupVaultName} -> targetBackupVaultName) (\s@BackupRuleInput' {} a -> s {targetBackupVaultName = a} :: BackupRuleInput)

instance Prelude.Hashable BackupRuleInput where
  hashWithSalt _salt BackupRuleInput' {..} =
    _salt
      `Prelude.hashWithSalt` completionWindowMinutes
      `Prelude.hashWithSalt` copyActions
      `Prelude.hashWithSalt` enableContinuousBackup
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` recoveryPointTags
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` startWindowMinutes
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` targetBackupVaultName

instance Prelude.NFData BackupRuleInput where
  rnf BackupRuleInput' {..} =
    Prelude.rnf completionWindowMinutes
      `Prelude.seq` Prelude.rnf copyActions
      `Prelude.seq` Prelude.rnf enableContinuousBackup
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf recoveryPointTags
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf startWindowMinutes
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf targetBackupVaultName

instance Data.ToJSON BackupRuleInput where
  toJSON BackupRuleInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompletionWindowMinutes" Data..=)
              Prelude.<$> completionWindowMinutes,
            ("CopyActions" Data..=) Prelude.<$> copyActions,
            ("EnableContinuousBackup" Data..=)
              Prelude.<$> enableContinuousBackup,
            ("Lifecycle" Data..=) Prelude.<$> lifecycle,
            ("RecoveryPointTags" Data..=)
              Prelude.<$> recoveryPointTags,
            ("ScheduleExpression" Data..=)
              Prelude.<$> scheduleExpression,
            ("StartWindowMinutes" Data..=)
              Prelude.<$> startWindowMinutes,
            Prelude.Just ("RuleName" Data..= ruleName),
            Prelude.Just
              ( "TargetBackupVaultName"
                  Data..= targetBackupVaultName
              )
          ]
      )
