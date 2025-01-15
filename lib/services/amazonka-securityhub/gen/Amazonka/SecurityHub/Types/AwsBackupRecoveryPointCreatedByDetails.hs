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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCreatedByDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCreatedByDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the backup plan and rule that Backup used to
-- initiate the recovery point backup.
--
-- /See:/ 'newAwsBackupRecoveryPointCreatedByDetails' smart constructor.
data AwsBackupRecoveryPointCreatedByDetails = AwsBackupRecoveryPointCreatedByDetails'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version IDs cannot be edited.
    backupPlanVersion :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a rule used to schedule the backup of a selection of
    -- resources.
    backupRuleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupRecoveryPointCreatedByDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanArn', 'awsBackupRecoveryPointCreatedByDetails_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan.
--
-- 'backupPlanId', 'awsBackupRecoveryPointCreatedByDetails_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'backupPlanVersion', 'awsBackupRecoveryPointCreatedByDetails_backupPlanVersion' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
--
-- 'backupRuleId', 'awsBackupRecoveryPointCreatedByDetails_backupRuleId' - Uniquely identifies a rule used to schedule the backup of a selection of
-- resources.
newAwsBackupRecoveryPointCreatedByDetails ::
  AwsBackupRecoveryPointCreatedByDetails
newAwsBackupRecoveryPointCreatedByDetails =
  AwsBackupRecoveryPointCreatedByDetails'
    { backupPlanArn =
        Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      backupPlanVersion = Prelude.Nothing,
      backupRuleId = Prelude.Nothing
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan.
awsBackupRecoveryPointCreatedByDetails_backupPlanArn :: Lens.Lens' AwsBackupRecoveryPointCreatedByDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCreatedByDetails_backupPlanArn = Lens.lens (\AwsBackupRecoveryPointCreatedByDetails' {backupPlanArn} -> backupPlanArn) (\s@AwsBackupRecoveryPointCreatedByDetails' {} a -> s {backupPlanArn = a} :: AwsBackupRecoveryPointCreatedByDetails)

-- | Uniquely identifies a backup plan.
awsBackupRecoveryPointCreatedByDetails_backupPlanId :: Lens.Lens' AwsBackupRecoveryPointCreatedByDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCreatedByDetails_backupPlanId = Lens.lens (\AwsBackupRecoveryPointCreatedByDetails' {backupPlanId} -> backupPlanId) (\s@AwsBackupRecoveryPointCreatedByDetails' {} a -> s {backupPlanId = a} :: AwsBackupRecoveryPointCreatedByDetails)

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
awsBackupRecoveryPointCreatedByDetails_backupPlanVersion :: Lens.Lens' AwsBackupRecoveryPointCreatedByDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCreatedByDetails_backupPlanVersion = Lens.lens (\AwsBackupRecoveryPointCreatedByDetails' {backupPlanVersion} -> backupPlanVersion) (\s@AwsBackupRecoveryPointCreatedByDetails' {} a -> s {backupPlanVersion = a} :: AwsBackupRecoveryPointCreatedByDetails)

-- | Uniquely identifies a rule used to schedule the backup of a selection of
-- resources.
awsBackupRecoveryPointCreatedByDetails_backupRuleId :: Lens.Lens' AwsBackupRecoveryPointCreatedByDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCreatedByDetails_backupRuleId = Lens.lens (\AwsBackupRecoveryPointCreatedByDetails' {backupRuleId} -> backupRuleId) (\s@AwsBackupRecoveryPointCreatedByDetails' {} a -> s {backupRuleId = a} :: AwsBackupRecoveryPointCreatedByDetails)

instance
  Data.FromJSON
    AwsBackupRecoveryPointCreatedByDetails
  where
  parseJSON =
    Data.withObject
      "AwsBackupRecoveryPointCreatedByDetails"
      ( \x ->
          AwsBackupRecoveryPointCreatedByDetails'
            Prelude.<$> (x Data..:? "BackupPlanArn")
            Prelude.<*> (x Data..:? "BackupPlanId")
            Prelude.<*> (x Data..:? "BackupPlanVersion")
            Prelude.<*> (x Data..:? "BackupRuleId")
      )

instance
  Prelude.Hashable
    AwsBackupRecoveryPointCreatedByDetails
  where
  hashWithSalt
    _salt
    AwsBackupRecoveryPointCreatedByDetails' {..} =
      _salt
        `Prelude.hashWithSalt` backupPlanArn
        `Prelude.hashWithSalt` backupPlanId
        `Prelude.hashWithSalt` backupPlanVersion
        `Prelude.hashWithSalt` backupRuleId

instance
  Prelude.NFData
    AwsBackupRecoveryPointCreatedByDetails
  where
  rnf AwsBackupRecoveryPointCreatedByDetails' {..} =
    Prelude.rnf backupPlanArn `Prelude.seq`
      Prelude.rnf backupPlanId `Prelude.seq`
        Prelude.rnf backupPlanVersion `Prelude.seq`
          Prelude.rnf backupRuleId

instance
  Data.ToJSON
    AwsBackupRecoveryPointCreatedByDetails
  where
  toJSON AwsBackupRecoveryPointCreatedByDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupPlanArn" Data..=) Prelude.<$> backupPlanArn,
            ("BackupPlanId" Data..=) Prelude.<$> backupPlanId,
            ("BackupPlanVersion" Data..=)
              Prelude.<$> backupPlanVersion,
            ("BackupRuleId" Data..=) Prelude.<$> backupRuleId
          ]
      )
