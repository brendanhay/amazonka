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
-- Module      : Amazonka.Backup.Types.BackupRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupRule where

import Amazonka.Backup.Types.CopyAction
import Amazonka.Backup.Types.Lifecycle
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a scheduled task used to back up a selection of resources.
--
-- /See:/ 'newBackupRule' smart constructor.
data BackupRule = BackupRule'
  { -- | Uniquely identifies a rule that is used to schedule the backup of a
    -- selection of resources.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup transitions and expires backups
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
    -- | An array of key-value pair strings that are assigned to resources that
    -- are associated with this rule when restored from backup.
    recoveryPointTags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A cron expression in UTC specifying when Backup initiates a backup job.
    -- For more information about cron expressions, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
    -- in the /Amazon CloudWatch Events User Guide./. Prior to specifying a
    -- value for this parameter, we recommend testing your cron expression
    -- using one of the many available cron generator and testing tools.
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
-- Create a value of 'BackupRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'backupRule_ruleId' - Uniquely identifies a rule that is used to schedule the backup of a
-- selection of resources.
--
-- 'lifecycle', 'backupRule_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
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
-- 'recoveryPointTags', 'backupRule_recoveryPointTags' - An array of key-value pair strings that are assigned to resources that
-- are associated with this rule when restored from backup.
--
-- 'scheduleExpression', 'backupRule_scheduleExpression' - A cron expression in UTC specifying when Backup initiates a backup job.
-- For more information about cron expressions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
-- in the /Amazon CloudWatch Events User Guide./. Prior to specifying a
-- value for this parameter, we recommend testing your cron expression
-- using one of the many available cron generator and testing tools.
--
-- 'enableContinuousBackup', 'backupRule_enableContinuousBackup' - Specifies whether Backup creates continuous backups. True causes Backup
-- to create continuous backups capable of point-in-time restore (PITR).
-- False (or not specified) causes Backup to create snapshot backups.
--
-- 'completionWindowMinutes', 'backupRule_completionWindowMinutes' - A value in minutes after a backup job is successfully started before it
-- must be completed or it will be canceled by Backup. This value is
-- optional.
--
-- 'copyActions', 'backupRule_copyActions' - An array of @CopyAction@ objects, which contains the details of the copy
-- operation.
--
-- 'startWindowMinutes', 'backupRule_startWindowMinutes' - A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional.
--
-- 'ruleName', 'backupRule_ruleName' - An optional display name for a backup rule.
--
-- 'targetBackupVaultName', 'backupRule_targetBackupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newBackupRule ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'targetBackupVaultName'
  Prelude.Text ->
  BackupRule
newBackupRule pRuleName_ pTargetBackupVaultName_ =
  BackupRule'
    { ruleId = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      recoveryPointTags = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      enableContinuousBackup = Prelude.Nothing,
      completionWindowMinutes = Prelude.Nothing,
      copyActions = Prelude.Nothing,
      startWindowMinutes = Prelude.Nothing,
      ruleName = pRuleName_,
      targetBackupVaultName = pTargetBackupVaultName_
    }

-- | Uniquely identifies a rule that is used to schedule the backup of a
-- selection of resources.
backupRule_ruleId :: Lens.Lens' BackupRule (Prelude.Maybe Prelude.Text)
backupRule_ruleId = Lens.lens (\BackupRule' {ruleId} -> ruleId) (\s@BackupRule' {} a -> s {ruleId = a} :: BackupRule)

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “expire after days” setting must be
-- 90 days greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Only Amazon EFS file system backups can be transitioned to cold storage.
backupRule_lifecycle :: Lens.Lens' BackupRule (Prelude.Maybe Lifecycle)
backupRule_lifecycle = Lens.lens (\BackupRule' {lifecycle} -> lifecycle) (\s@BackupRule' {} a -> s {lifecycle = a} :: BackupRule)

-- | An array of key-value pair strings that are assigned to resources that
-- are associated with this rule when restored from backup.
backupRule_recoveryPointTags :: Lens.Lens' BackupRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
backupRule_recoveryPointTags = Lens.lens (\BackupRule' {recoveryPointTags} -> recoveryPointTags) (\s@BackupRule' {} a -> s {recoveryPointTags = a} :: BackupRule) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | A cron expression in UTC specifying when Backup initiates a backup job.
-- For more information about cron expressions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
-- in the /Amazon CloudWatch Events User Guide./. Prior to specifying a
-- value for this parameter, we recommend testing your cron expression
-- using one of the many available cron generator and testing tools.
backupRule_scheduleExpression :: Lens.Lens' BackupRule (Prelude.Maybe Prelude.Text)
backupRule_scheduleExpression = Lens.lens (\BackupRule' {scheduleExpression} -> scheduleExpression) (\s@BackupRule' {} a -> s {scheduleExpression = a} :: BackupRule)

-- | Specifies whether Backup creates continuous backups. True causes Backup
-- to create continuous backups capable of point-in-time restore (PITR).
-- False (or not specified) causes Backup to create snapshot backups.
backupRule_enableContinuousBackup :: Lens.Lens' BackupRule (Prelude.Maybe Prelude.Bool)
backupRule_enableContinuousBackup = Lens.lens (\BackupRule' {enableContinuousBackup} -> enableContinuousBackup) (\s@BackupRule' {} a -> s {enableContinuousBackup = a} :: BackupRule)

-- | A value in minutes after a backup job is successfully started before it
-- must be completed or it will be canceled by Backup. This value is
-- optional.
backupRule_completionWindowMinutes :: Lens.Lens' BackupRule (Prelude.Maybe Prelude.Integer)
backupRule_completionWindowMinutes = Lens.lens (\BackupRule' {completionWindowMinutes} -> completionWindowMinutes) (\s@BackupRule' {} a -> s {completionWindowMinutes = a} :: BackupRule)

-- | An array of @CopyAction@ objects, which contains the details of the copy
-- operation.
backupRule_copyActions :: Lens.Lens' BackupRule (Prelude.Maybe [CopyAction])
backupRule_copyActions = Lens.lens (\BackupRule' {copyActions} -> copyActions) (\s@BackupRule' {} a -> s {copyActions = a} :: BackupRule) Prelude.. Lens.mapping Lens.coerced

-- | A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional.
backupRule_startWindowMinutes :: Lens.Lens' BackupRule (Prelude.Maybe Prelude.Integer)
backupRule_startWindowMinutes = Lens.lens (\BackupRule' {startWindowMinutes} -> startWindowMinutes) (\s@BackupRule' {} a -> s {startWindowMinutes = a} :: BackupRule)

-- | An optional display name for a backup rule.
backupRule_ruleName :: Lens.Lens' BackupRule Prelude.Text
backupRule_ruleName = Lens.lens (\BackupRule' {ruleName} -> ruleName) (\s@BackupRule' {} a -> s {ruleName = a} :: BackupRule)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
backupRule_targetBackupVaultName :: Lens.Lens' BackupRule Prelude.Text
backupRule_targetBackupVaultName = Lens.lens (\BackupRule' {targetBackupVaultName} -> targetBackupVaultName) (\s@BackupRule' {} a -> s {targetBackupVaultName = a} :: BackupRule)

instance Core.FromJSON BackupRule where
  parseJSON =
    Core.withObject
      "BackupRule"
      ( \x ->
          BackupRule'
            Prelude.<$> (x Core..:? "RuleId")
            Prelude.<*> (x Core..:? "Lifecycle")
            Prelude.<*> ( x Core..:? "RecoveryPointTags"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "EnableContinuousBackup")
            Prelude.<*> (x Core..:? "CompletionWindowMinutes")
            Prelude.<*> (x Core..:? "CopyActions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StartWindowMinutes")
            Prelude.<*> (x Core..: "RuleName")
            Prelude.<*> (x Core..: "TargetBackupVaultName")
      )

instance Prelude.Hashable BackupRule where
  hashWithSalt _salt BackupRule' {..} =
    _salt `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` recoveryPointTags
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` enableContinuousBackup
      `Prelude.hashWithSalt` completionWindowMinutes
      `Prelude.hashWithSalt` copyActions
      `Prelude.hashWithSalt` startWindowMinutes
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` targetBackupVaultName

instance Prelude.NFData BackupRule where
  rnf BackupRule' {..} =
    Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf recoveryPointTags
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf enableContinuousBackup
      `Prelude.seq` Prelude.rnf completionWindowMinutes
      `Prelude.seq` Prelude.rnf copyActions
      `Prelude.seq` Prelude.rnf startWindowMinutes
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf targetBackupVaultName
