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
-- Module      : Amazonka.Backup.Types.BackupPlansListMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupPlansListMember where

import Amazonka.Backup.Types.AdvancedBackupSetting
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about a backup plan.
--
-- /See:/ 'newBackupPlansListMember' smart constructor.
data BackupPlansListMember = BackupPlansListMember'
  { -- | Contains a list of @BackupOptions@ for a resource type.
    advancedBackupSettings :: Prelude.Maybe [AdvancedBackupSetting],
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | The display name of a saved backup plan.
    backupPlanName :: Prelude.Maybe Prelude.Text,
    -- | The date and time a resource backup plan is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice. This
    -- parameter is optional.
    --
    -- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
    -- characters.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The date and time a backup plan is deleted, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    deletionDate :: Prelude.Maybe Data.POSIX,
    -- | The last time a job to back up resources was run with this rule. A date
    -- and time, in Unix format and Coordinated Universal Time (UTC). The value
    -- of @LastExecutionDate@ is accurate to milliseconds. For example, the
    -- value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087
    -- AM.
    lastExecutionDate :: Prelude.Maybe Data.POSIX,
    -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version IDs cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupPlansListMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedBackupSettings', 'backupPlansListMember_advancedBackupSettings' - Contains a list of @BackupOptions@ for a resource type.
--
-- 'backupPlanArn', 'backupPlansListMember_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'backupPlanId', 'backupPlansListMember_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'backupPlanName', 'backupPlansListMember_backupPlanName' - The display name of a saved backup plan.
--
-- 'creationDate', 'backupPlansListMember_creationDate' - The date and time a resource backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'creatorRequestId', 'backupPlansListMember_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
--
-- 'deletionDate', 'backupPlansListMember_deletionDate' - The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'lastExecutionDate', 'backupPlansListMember_lastExecutionDate' - The last time a job to back up resources was run with this rule. A date
-- and time, in Unix format and Coordinated Universal Time (UTC). The value
-- of @LastExecutionDate@ is accurate to milliseconds. For example, the
-- value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087
-- AM.
--
-- 'versionId', 'backupPlansListMember_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
newBackupPlansListMember ::
  BackupPlansListMember
newBackupPlansListMember =
  BackupPlansListMember'
    { advancedBackupSettings =
        Prelude.Nothing,
      backupPlanArn = Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      backupPlanName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | Contains a list of @BackupOptions@ for a resource type.
backupPlansListMember_advancedBackupSettings :: Lens.Lens' BackupPlansListMember (Prelude.Maybe [AdvancedBackupSetting])
backupPlansListMember_advancedBackupSettings = Lens.lens (\BackupPlansListMember' {advancedBackupSettings} -> advancedBackupSettings) (\s@BackupPlansListMember' {} a -> s {advancedBackupSettings = a} :: BackupPlansListMember) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
backupPlansListMember_backupPlanArn :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_backupPlanArn = Lens.lens (\BackupPlansListMember' {backupPlanArn} -> backupPlanArn) (\s@BackupPlansListMember' {} a -> s {backupPlanArn = a} :: BackupPlansListMember)

-- | Uniquely identifies a backup plan.
backupPlansListMember_backupPlanId :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_backupPlanId = Lens.lens (\BackupPlansListMember' {backupPlanId} -> backupPlanId) (\s@BackupPlansListMember' {} a -> s {backupPlanId = a} :: BackupPlansListMember)

-- | The display name of a saved backup plan.
backupPlansListMember_backupPlanName :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_backupPlanName = Lens.lens (\BackupPlansListMember' {backupPlanName} -> backupPlanName) (\s@BackupPlansListMember' {} a -> s {backupPlanName = a} :: BackupPlansListMember)

-- | The date and time a resource backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupPlansListMember_creationDate :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.UTCTime)
backupPlansListMember_creationDate = Lens.lens (\BackupPlansListMember' {creationDate} -> creationDate) (\s@BackupPlansListMember' {} a -> s {creationDate = a} :: BackupPlansListMember) Prelude.. Lens.mapping Data._Time

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
backupPlansListMember_creatorRequestId :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_creatorRequestId = Lens.lens (\BackupPlansListMember' {creatorRequestId} -> creatorRequestId) (\s@BackupPlansListMember' {} a -> s {creatorRequestId = a} :: BackupPlansListMember)

-- | The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupPlansListMember_deletionDate :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.UTCTime)
backupPlansListMember_deletionDate = Lens.lens (\BackupPlansListMember' {deletionDate} -> deletionDate) (\s@BackupPlansListMember' {} a -> s {deletionDate = a} :: BackupPlansListMember) Prelude.. Lens.mapping Data._Time

-- | The last time a job to back up resources was run with this rule. A date
-- and time, in Unix format and Coordinated Universal Time (UTC). The value
-- of @LastExecutionDate@ is accurate to milliseconds. For example, the
-- value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087
-- AM.
backupPlansListMember_lastExecutionDate :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.UTCTime)
backupPlansListMember_lastExecutionDate = Lens.lens (\BackupPlansListMember' {lastExecutionDate} -> lastExecutionDate) (\s@BackupPlansListMember' {} a -> s {lastExecutionDate = a} :: BackupPlansListMember) Prelude.. Lens.mapping Data._Time

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
backupPlansListMember_versionId :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_versionId = Lens.lens (\BackupPlansListMember' {versionId} -> versionId) (\s@BackupPlansListMember' {} a -> s {versionId = a} :: BackupPlansListMember)

instance Data.FromJSON BackupPlansListMember where
  parseJSON =
    Data.withObject
      "BackupPlansListMember"
      ( \x ->
          BackupPlansListMember'
            Prelude.<$> ( x Data..:? "AdvancedBackupSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BackupPlanArn")
            Prelude.<*> (x Data..:? "BackupPlanId")
            Prelude.<*> (x Data..:? "BackupPlanName")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "CreatorRequestId")
            Prelude.<*> (x Data..:? "DeletionDate")
            Prelude.<*> (x Data..:? "LastExecutionDate")
            Prelude.<*> (x Data..:? "VersionId")
      )

instance Prelude.Hashable BackupPlansListMember where
  hashWithSalt _salt BackupPlansListMember' {..} =
    _salt `Prelude.hashWithSalt` advancedBackupSettings
      `Prelude.hashWithSalt` backupPlanArn
      `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` backupPlanName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` deletionDate
      `Prelude.hashWithSalt` lastExecutionDate
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData BackupPlansListMember where
  rnf BackupPlansListMember' {..} =
    Prelude.rnf advancedBackupSettings
      `Prelude.seq` Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf backupPlanName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf lastExecutionDate
      `Prelude.seq` Prelude.rnf versionId
