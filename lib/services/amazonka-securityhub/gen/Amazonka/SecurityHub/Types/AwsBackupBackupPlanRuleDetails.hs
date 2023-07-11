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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanLifecycleDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleCopyActionsDetails

-- | Provides details about an array of @BackupRule@ objects, each of which
-- specifies a scheduled task that is used to back up a selection of
-- resources.
--
-- /See:/ 'newAwsBackupBackupPlanRuleDetails' smart constructor.
data AwsBackupBackupPlanRuleDetails = AwsBackupBackupPlanRuleDetails'
  { -- | A value in minutes after a backup job is successfully started before it
    -- must be completed, or it is canceled by Backup.
    completionWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | An array of @CopyAction@ objects, each of which contains details of the
    -- copy operation.
    copyActions :: Prelude.Maybe [AwsBackupBackupPlanRuleCopyActionsDetails],
    -- | Specifies whether Backup creates continuous backups capable of
    -- point-in-time restore (PITR).
    enableContinuousBackup :: Prelude.Maybe Prelude.Bool,
    -- | Defines when a protected resource is transitioned to cold storage and
    -- when it expires. Backup transitions and expires backups automatically
    -- according to the lifecycle that you define. If you do not specify a
    -- lifecycle, Backup applies the lifecycle policy of the source backup to
    -- the destination backup.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days.
    lifecycle :: Prelude.Maybe AwsBackupBackupPlanLifecycleDetails,
    -- | Uniquely identifies a rule that is used to schedule the backup of a
    -- selection of resources.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | A display name for a backup rule. Must contain 1 to 50 alphanumeric or
    -- \'-_.\' characters.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | A cron expression in UTC specifying when Backup initiates a backup job.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | A value in minutes after a backup is scheduled before a job will be
    -- canceled if it doesn\'t start successfully.
    startWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the Amazon Web Services
    -- account used to create them and the Amazon Web Services Region where
    -- they are created. They consist of letters, numbers, and hyphens.
    targetBackupVault :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupPlanRuleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionWindowMinutes', 'awsBackupBackupPlanRuleDetails_completionWindowMinutes' - A value in minutes after a backup job is successfully started before it
-- must be completed, or it is canceled by Backup.
--
-- 'copyActions', 'awsBackupBackupPlanRuleDetails_copyActions' - An array of @CopyAction@ objects, each of which contains details of the
-- copy operation.
--
-- 'enableContinuousBackup', 'awsBackupBackupPlanRuleDetails_enableContinuousBackup' - Specifies whether Backup creates continuous backups capable of
-- point-in-time restore (PITR).
--
-- 'lifecycle', 'awsBackupBackupPlanRuleDetails_lifecycle' - Defines when a protected resource is transitioned to cold storage and
-- when it expires. Backup transitions and expires backups automatically
-- according to the lifecycle that you define. If you do not specify a
-- lifecycle, Backup applies the lifecycle policy of the source backup to
-- the destination backup.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days.
--
-- 'ruleId', 'awsBackupBackupPlanRuleDetails_ruleId' - Uniquely identifies a rule that is used to schedule the backup of a
-- selection of resources.
--
-- 'ruleName', 'awsBackupBackupPlanRuleDetails_ruleName' - A display name for a backup rule. Must contain 1 to 50 alphanumeric or
-- \'-_.\' characters.
--
-- 'scheduleExpression', 'awsBackupBackupPlanRuleDetails_scheduleExpression' - A cron expression in UTC specifying when Backup initiates a backup job.
--
-- 'startWindowMinutes', 'awsBackupBackupPlanRuleDetails_startWindowMinutes' - A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully.
--
-- 'targetBackupVault', 'awsBackupBackupPlanRuleDetails_targetBackupVault' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the Amazon Web Services
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of letters, numbers, and hyphens.
newAwsBackupBackupPlanRuleDetails ::
  AwsBackupBackupPlanRuleDetails
newAwsBackupBackupPlanRuleDetails =
  AwsBackupBackupPlanRuleDetails'
    { completionWindowMinutes =
        Prelude.Nothing,
      copyActions = Prelude.Nothing,
      enableContinuousBackup = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      startWindowMinutes = Prelude.Nothing,
      targetBackupVault = Prelude.Nothing
    }

-- | A value in minutes after a backup job is successfully started before it
-- must be completed, or it is canceled by Backup.
awsBackupBackupPlanRuleDetails_completionWindowMinutes :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe Prelude.Integer)
awsBackupBackupPlanRuleDetails_completionWindowMinutes = Lens.lens (\AwsBackupBackupPlanRuleDetails' {completionWindowMinutes} -> completionWindowMinutes) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {completionWindowMinutes = a} :: AwsBackupBackupPlanRuleDetails)

-- | An array of @CopyAction@ objects, each of which contains details of the
-- copy operation.
awsBackupBackupPlanRuleDetails_copyActions :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe [AwsBackupBackupPlanRuleCopyActionsDetails])
awsBackupBackupPlanRuleDetails_copyActions = Lens.lens (\AwsBackupBackupPlanRuleDetails' {copyActions} -> copyActions) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {copyActions = a} :: AwsBackupBackupPlanRuleDetails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether Backup creates continuous backups capable of
-- point-in-time restore (PITR).
awsBackupBackupPlanRuleDetails_enableContinuousBackup :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe Prelude.Bool)
awsBackupBackupPlanRuleDetails_enableContinuousBackup = Lens.lens (\AwsBackupBackupPlanRuleDetails' {enableContinuousBackup} -> enableContinuousBackup) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {enableContinuousBackup = a} :: AwsBackupBackupPlanRuleDetails)

-- | Defines when a protected resource is transitioned to cold storage and
-- when it expires. Backup transitions and expires backups automatically
-- according to the lifecycle that you define. If you do not specify a
-- lifecycle, Backup applies the lifecycle policy of the source backup to
-- the destination backup.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days.
awsBackupBackupPlanRuleDetails_lifecycle :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe AwsBackupBackupPlanLifecycleDetails)
awsBackupBackupPlanRuleDetails_lifecycle = Lens.lens (\AwsBackupBackupPlanRuleDetails' {lifecycle} -> lifecycle) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {lifecycle = a} :: AwsBackupBackupPlanRuleDetails)

-- | Uniquely identifies a rule that is used to schedule the backup of a
-- selection of resources.
awsBackupBackupPlanRuleDetails_ruleId :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanRuleDetails_ruleId = Lens.lens (\AwsBackupBackupPlanRuleDetails' {ruleId} -> ruleId) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {ruleId = a} :: AwsBackupBackupPlanRuleDetails)

-- | A display name for a backup rule. Must contain 1 to 50 alphanumeric or
-- \'-_.\' characters.
awsBackupBackupPlanRuleDetails_ruleName :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanRuleDetails_ruleName = Lens.lens (\AwsBackupBackupPlanRuleDetails' {ruleName} -> ruleName) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {ruleName = a} :: AwsBackupBackupPlanRuleDetails)

-- | A cron expression in UTC specifying when Backup initiates a backup job.
awsBackupBackupPlanRuleDetails_scheduleExpression :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanRuleDetails_scheduleExpression = Lens.lens (\AwsBackupBackupPlanRuleDetails' {scheduleExpression} -> scheduleExpression) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {scheduleExpression = a} :: AwsBackupBackupPlanRuleDetails)

-- | A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully.
awsBackupBackupPlanRuleDetails_startWindowMinutes :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe Prelude.Integer)
awsBackupBackupPlanRuleDetails_startWindowMinutes = Lens.lens (\AwsBackupBackupPlanRuleDetails' {startWindowMinutes} -> startWindowMinutes) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {startWindowMinutes = a} :: AwsBackupBackupPlanRuleDetails)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the Amazon Web Services
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of letters, numbers, and hyphens.
awsBackupBackupPlanRuleDetails_targetBackupVault :: Lens.Lens' AwsBackupBackupPlanRuleDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanRuleDetails_targetBackupVault = Lens.lens (\AwsBackupBackupPlanRuleDetails' {targetBackupVault} -> targetBackupVault) (\s@AwsBackupBackupPlanRuleDetails' {} a -> s {targetBackupVault = a} :: AwsBackupBackupPlanRuleDetails)

instance Data.FromJSON AwsBackupBackupPlanRuleDetails where
  parseJSON =
    Data.withObject
      "AwsBackupBackupPlanRuleDetails"
      ( \x ->
          AwsBackupBackupPlanRuleDetails'
            Prelude.<$> (x Data..:? "CompletionWindowMinutes")
            Prelude.<*> (x Data..:? "CopyActions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EnableContinuousBackup")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "RuleId")
            Prelude.<*> (x Data..:? "RuleName")
            Prelude.<*> (x Data..:? "ScheduleExpression")
            Prelude.<*> (x Data..:? "StartWindowMinutes")
            Prelude.<*> (x Data..:? "TargetBackupVault")
      )

instance
  Prelude.Hashable
    AwsBackupBackupPlanRuleDetails
  where
  hashWithSalt
    _salt
    AwsBackupBackupPlanRuleDetails' {..} =
      _salt
        `Prelude.hashWithSalt` completionWindowMinutes
        `Prelude.hashWithSalt` copyActions
        `Prelude.hashWithSalt` enableContinuousBackup
        `Prelude.hashWithSalt` lifecycle
        `Prelude.hashWithSalt` ruleId
        `Prelude.hashWithSalt` ruleName
        `Prelude.hashWithSalt` scheduleExpression
        `Prelude.hashWithSalt` startWindowMinutes
        `Prelude.hashWithSalt` targetBackupVault

instance
  Prelude.NFData
    AwsBackupBackupPlanRuleDetails
  where
  rnf AwsBackupBackupPlanRuleDetails' {..} =
    Prelude.rnf completionWindowMinutes
      `Prelude.seq` Prelude.rnf copyActions
      `Prelude.seq` Prelude.rnf enableContinuousBackup
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf startWindowMinutes
      `Prelude.seq` Prelude.rnf targetBackupVault

instance Data.ToJSON AwsBackupBackupPlanRuleDetails where
  toJSON AwsBackupBackupPlanRuleDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompletionWindowMinutes" Data..=)
              Prelude.<$> completionWindowMinutes,
            ("CopyActions" Data..=) Prelude.<$> copyActions,
            ("EnableContinuousBackup" Data..=)
              Prelude.<$> enableContinuousBackup,
            ("Lifecycle" Data..=) Prelude.<$> lifecycle,
            ("RuleId" Data..=) Prelude.<$> ruleId,
            ("RuleName" Data..=) Prelude.<$> ruleName,
            ("ScheduleExpression" Data..=)
              Prelude.<$> scheduleExpression,
            ("StartWindowMinutes" Data..=)
              Prelude.<$> startWindowMinutes,
            ("TargetBackupVault" Data..=)
              Prelude.<$> targetBackupVault
          ]
      )
