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
-- Module      : Amazonka.Backup.Types.BackupSelectionsListMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupSelectionsListMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about a @BackupSelection@ object.
--
-- /See:/ 'newBackupSelectionsListMember' smart constructor.
data BackupSelectionsListMember = BackupSelectionsListMember'
  { -- | The display name of a resource selection document.
    selectionName :: Prelude.Maybe Prelude.Text,
    -- | The date and time a backup plan is created, in Unix format and
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
    -- | Specifies the IAM role Amazon Resource Name (ARN) to create the target
    -- recovery point; for example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a request to assign a set of resources to a backup
    -- plan.
    selectionId :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupSelectionsListMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectionName', 'backupSelectionsListMember_selectionName' - The display name of a resource selection document.
--
-- 'creationDate', 'backupSelectionsListMember_creationDate' - The date and time a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'creatorRequestId', 'backupSelectionsListMember_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
--
-- 'iamRoleArn', 'backupSelectionsListMember_iamRoleArn' - Specifies the IAM role Amazon Resource Name (ARN) to create the target
-- recovery point; for example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'selectionId', 'backupSelectionsListMember_selectionId' - Uniquely identifies a request to assign a set of resources to a backup
-- plan.
--
-- 'backupPlanId', 'backupSelectionsListMember_backupPlanId' - Uniquely identifies a backup plan.
newBackupSelectionsListMember ::
  BackupSelectionsListMember
newBackupSelectionsListMember =
  BackupSelectionsListMember'
    { selectionName =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      selectionId = Prelude.Nothing,
      backupPlanId = Prelude.Nothing
    }

-- | The display name of a resource selection document.
backupSelectionsListMember_selectionName :: Lens.Lens' BackupSelectionsListMember (Prelude.Maybe Prelude.Text)
backupSelectionsListMember_selectionName = Lens.lens (\BackupSelectionsListMember' {selectionName} -> selectionName) (\s@BackupSelectionsListMember' {} a -> s {selectionName = a} :: BackupSelectionsListMember)

-- | The date and time a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupSelectionsListMember_creationDate :: Lens.Lens' BackupSelectionsListMember (Prelude.Maybe Prelude.UTCTime)
backupSelectionsListMember_creationDate = Lens.lens (\BackupSelectionsListMember' {creationDate} -> creationDate) (\s@BackupSelectionsListMember' {} a -> s {creationDate = a} :: BackupSelectionsListMember) Prelude.. Lens.mapping Data._Time

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
backupSelectionsListMember_creatorRequestId :: Lens.Lens' BackupSelectionsListMember (Prelude.Maybe Prelude.Text)
backupSelectionsListMember_creatorRequestId = Lens.lens (\BackupSelectionsListMember' {creatorRequestId} -> creatorRequestId) (\s@BackupSelectionsListMember' {} a -> s {creatorRequestId = a} :: BackupSelectionsListMember)

-- | Specifies the IAM role Amazon Resource Name (ARN) to create the target
-- recovery point; for example, @arn:aws:iam::123456789012:role\/S3Access@.
backupSelectionsListMember_iamRoleArn :: Lens.Lens' BackupSelectionsListMember (Prelude.Maybe Prelude.Text)
backupSelectionsListMember_iamRoleArn = Lens.lens (\BackupSelectionsListMember' {iamRoleArn} -> iamRoleArn) (\s@BackupSelectionsListMember' {} a -> s {iamRoleArn = a} :: BackupSelectionsListMember)

-- | Uniquely identifies a request to assign a set of resources to a backup
-- plan.
backupSelectionsListMember_selectionId :: Lens.Lens' BackupSelectionsListMember (Prelude.Maybe Prelude.Text)
backupSelectionsListMember_selectionId = Lens.lens (\BackupSelectionsListMember' {selectionId} -> selectionId) (\s@BackupSelectionsListMember' {} a -> s {selectionId = a} :: BackupSelectionsListMember)

-- | Uniquely identifies a backup plan.
backupSelectionsListMember_backupPlanId :: Lens.Lens' BackupSelectionsListMember (Prelude.Maybe Prelude.Text)
backupSelectionsListMember_backupPlanId = Lens.lens (\BackupSelectionsListMember' {backupPlanId} -> backupPlanId) (\s@BackupSelectionsListMember' {} a -> s {backupPlanId = a} :: BackupSelectionsListMember)

instance Data.FromJSON BackupSelectionsListMember where
  parseJSON =
    Data.withObject
      "BackupSelectionsListMember"
      ( \x ->
          BackupSelectionsListMember'
            Prelude.<$> (x Data..:? "SelectionName")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "CreatorRequestId")
            Prelude.<*> (x Data..:? "IamRoleArn")
            Prelude.<*> (x Data..:? "SelectionId")
            Prelude.<*> (x Data..:? "BackupPlanId")
      )

instance Prelude.Hashable BackupSelectionsListMember where
  hashWithSalt _salt BackupSelectionsListMember' {..} =
    _salt `Prelude.hashWithSalt` selectionName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` selectionId
      `Prelude.hashWithSalt` backupPlanId

instance Prelude.NFData BackupSelectionsListMember where
  rnf BackupSelectionsListMember' {..} =
    Prelude.rnf selectionName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf selectionId
      `Prelude.seq` Prelude.rnf backupPlanId
