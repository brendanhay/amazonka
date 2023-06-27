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
-- Module      : Amazonka.Backup.Types.RecoveryPointByResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RecoveryPointByResource where

import Amazonka.Backup.Types.RecoveryPointStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a saved recovery point.
--
-- /See:/ 'newRecoveryPointByResource' smart constructor.
data RecoveryPointByResource = RecoveryPointByResource'
  { -- | The size, in bytes, of a backup.
    backupSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The date and time a recovery point is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The server-side encryption key that is used to protect your backups; for
    -- example,
    -- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | This is a boolean value indicating this is a parent (composite) recovery
    -- point.
    isParent :: Prelude.Maybe Prelude.Bool,
    -- | This is the Amazon Resource Name (ARN) of the parent (composite)
    -- recovery point.
    parentRecoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | This is the non-unique name of the resource that belongs to the
    -- specified backup.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | A status code specifying the state of the recovery point.
    status :: Prelude.Maybe RecoveryPointStatus,
    -- | A message explaining the reason of the recovery point deletion failure.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryPointByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupSizeBytes', 'recoveryPointByResource_backupSizeBytes' - The size, in bytes, of a backup.
--
-- 'backupVaultName', 'recoveryPointByResource_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'creationDate', 'recoveryPointByResource_creationDate' - The date and time a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'encryptionKeyArn', 'recoveryPointByResource_encryptionKeyArn' - The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 'isParent', 'recoveryPointByResource_isParent' - This is a boolean value indicating this is a parent (composite) recovery
-- point.
--
-- 'parentRecoveryPointArn', 'recoveryPointByResource_parentRecoveryPointArn' - This is the Amazon Resource Name (ARN) of the parent (composite)
-- recovery point.
--
-- 'recoveryPointArn', 'recoveryPointByResource_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'resourceName', 'recoveryPointByResource_resourceName' - This is the non-unique name of the resource that belongs to the
-- specified backup.
--
-- 'status', 'recoveryPointByResource_status' - A status code specifying the state of the recovery point.
--
-- 'statusMessage', 'recoveryPointByResource_statusMessage' - A message explaining the reason of the recovery point deletion failure.
newRecoveryPointByResource ::
  RecoveryPointByResource
newRecoveryPointByResource =
  RecoveryPointByResource'
    { backupSizeBytes =
        Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      isParent = Prelude.Nothing,
      parentRecoveryPointArn = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The size, in bytes, of a backup.
recoveryPointByResource_backupSizeBytes :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Integer)
recoveryPointByResource_backupSizeBytes = Lens.lens (\RecoveryPointByResource' {backupSizeBytes} -> backupSizeBytes) (\s@RecoveryPointByResource' {} a -> s {backupSizeBytes = a} :: RecoveryPointByResource)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
recoveryPointByResource_backupVaultName :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_backupVaultName = Lens.lens (\RecoveryPointByResource' {backupVaultName} -> backupVaultName) (\s@RecoveryPointByResource' {} a -> s {backupVaultName = a} :: RecoveryPointByResource)

-- | The date and time a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
recoveryPointByResource_creationDate :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.UTCTime)
recoveryPointByResource_creationDate = Lens.lens (\RecoveryPointByResource' {creationDate} -> creationDate) (\s@RecoveryPointByResource' {} a -> s {creationDate = a} :: RecoveryPointByResource) Prelude.. Lens.mapping Data._Time

-- | The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
recoveryPointByResource_encryptionKeyArn :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_encryptionKeyArn = Lens.lens (\RecoveryPointByResource' {encryptionKeyArn} -> encryptionKeyArn) (\s@RecoveryPointByResource' {} a -> s {encryptionKeyArn = a} :: RecoveryPointByResource)

-- | This is a boolean value indicating this is a parent (composite) recovery
-- point.
recoveryPointByResource_isParent :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Bool)
recoveryPointByResource_isParent = Lens.lens (\RecoveryPointByResource' {isParent} -> isParent) (\s@RecoveryPointByResource' {} a -> s {isParent = a} :: RecoveryPointByResource)

-- | This is the Amazon Resource Name (ARN) of the parent (composite)
-- recovery point.
recoveryPointByResource_parentRecoveryPointArn :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_parentRecoveryPointArn = Lens.lens (\RecoveryPointByResource' {parentRecoveryPointArn} -> parentRecoveryPointArn) (\s@RecoveryPointByResource' {} a -> s {parentRecoveryPointArn = a} :: RecoveryPointByResource)

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
recoveryPointByResource_recoveryPointArn :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_recoveryPointArn = Lens.lens (\RecoveryPointByResource' {recoveryPointArn} -> recoveryPointArn) (\s@RecoveryPointByResource' {} a -> s {recoveryPointArn = a} :: RecoveryPointByResource)

-- | This is the non-unique name of the resource that belongs to the
-- specified backup.
recoveryPointByResource_resourceName :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_resourceName = Lens.lens (\RecoveryPointByResource' {resourceName} -> resourceName) (\s@RecoveryPointByResource' {} a -> s {resourceName = a} :: RecoveryPointByResource)

-- | A status code specifying the state of the recovery point.
recoveryPointByResource_status :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe RecoveryPointStatus)
recoveryPointByResource_status = Lens.lens (\RecoveryPointByResource' {status} -> status) (\s@RecoveryPointByResource' {} a -> s {status = a} :: RecoveryPointByResource)

-- | A message explaining the reason of the recovery point deletion failure.
recoveryPointByResource_statusMessage :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_statusMessage = Lens.lens (\RecoveryPointByResource' {statusMessage} -> statusMessage) (\s@RecoveryPointByResource' {} a -> s {statusMessage = a} :: RecoveryPointByResource)

instance Data.FromJSON RecoveryPointByResource where
  parseJSON =
    Data.withObject
      "RecoveryPointByResource"
      ( \x ->
          RecoveryPointByResource'
            Prelude.<$> (x Data..:? "BackupSizeBytes")
            Prelude.<*> (x Data..:? "BackupVaultName")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "EncryptionKeyArn")
            Prelude.<*> (x Data..:? "IsParent")
            Prelude.<*> (x Data..:? "ParentRecoveryPointArn")
            Prelude.<*> (x Data..:? "RecoveryPointArn")
            Prelude.<*> (x Data..:? "ResourceName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable RecoveryPointByResource where
  hashWithSalt _salt RecoveryPointByResource' {..} =
    _salt
      `Prelude.hashWithSalt` backupSizeBytes
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` isParent
      `Prelude.hashWithSalt` parentRecoveryPointArn
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData RecoveryPointByResource where
  rnf RecoveryPointByResource' {..} =
    Prelude.rnf backupSizeBytes
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf isParent
      `Prelude.seq` Prelude.rnf parentRecoveryPointArn
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
