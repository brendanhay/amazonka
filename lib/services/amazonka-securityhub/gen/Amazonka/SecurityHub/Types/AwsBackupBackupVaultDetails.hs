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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupVaultDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupVaultDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsBackupBackupVaultNotificationsDetails

-- | Provides details about an Backup backup vault. In Backup, a backup vault
-- is a container that stores and organizes your backups.
--
-- /See:/ 'newAwsBackupBackupVaultDetails' smart constructor.
data AwsBackupBackupVaultDetails = AwsBackupBackupVaultDetails'
  { -- | A resource-based policy that is used to manage access permissions on the
    -- target backup vault.
    accessPolicy :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the Amazon Web Services
    -- account used to create them and the Amazon Web Services Region where
    -- they are created. They consist of lowercase letters, numbers, and
    -- hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The unique ARN associated with the server-side encryption key. You can
    -- specify a key to encrypt your backups from services that support full
    -- Backup management. If you do not specify a key, Backup creates an KMS
    -- key for you by default.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS event notifications for the specified backup vault.
    notifications :: Prelude.Maybe AwsBackupBackupVaultNotificationsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupVaultDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicy', 'awsBackupBackupVaultDetails_accessPolicy' - A resource-based policy that is used to manage access permissions on the
-- target backup vault.
--
-- 'backupVaultArn', 'awsBackupBackupVaultDetails_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault.
--
-- 'backupVaultName', 'awsBackupBackupVaultDetails_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the Amazon Web Services
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
--
-- 'encryptionKeyArn', 'awsBackupBackupVaultDetails_encryptionKeyArn' - The unique ARN associated with the server-side encryption key. You can
-- specify a key to encrypt your backups from services that support full
-- Backup management. If you do not specify a key, Backup creates an KMS
-- key for you by default.
--
-- 'notifications', 'awsBackupBackupVaultDetails_notifications' - The Amazon SNS event notifications for the specified backup vault.
newAwsBackupBackupVaultDetails ::
  AwsBackupBackupVaultDetails
newAwsBackupBackupVaultDetails =
  AwsBackupBackupVaultDetails'
    { accessPolicy =
        Prelude.Nothing,
      backupVaultArn = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      notifications = Prelude.Nothing
    }

-- | A resource-based policy that is used to manage access permissions on the
-- target backup vault.
awsBackupBackupVaultDetails_accessPolicy :: Lens.Lens' AwsBackupBackupVaultDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupVaultDetails_accessPolicy = Lens.lens (\AwsBackupBackupVaultDetails' {accessPolicy} -> accessPolicy) (\s@AwsBackupBackupVaultDetails' {} a -> s {accessPolicy = a} :: AwsBackupBackupVaultDetails)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault.
awsBackupBackupVaultDetails_backupVaultArn :: Lens.Lens' AwsBackupBackupVaultDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupVaultDetails_backupVaultArn = Lens.lens (\AwsBackupBackupVaultDetails' {backupVaultArn} -> backupVaultArn) (\s@AwsBackupBackupVaultDetails' {} a -> s {backupVaultArn = a} :: AwsBackupBackupVaultDetails)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the Amazon Web Services
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
awsBackupBackupVaultDetails_backupVaultName :: Lens.Lens' AwsBackupBackupVaultDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupVaultDetails_backupVaultName = Lens.lens (\AwsBackupBackupVaultDetails' {backupVaultName} -> backupVaultName) (\s@AwsBackupBackupVaultDetails' {} a -> s {backupVaultName = a} :: AwsBackupBackupVaultDetails)

-- | The unique ARN associated with the server-side encryption key. You can
-- specify a key to encrypt your backups from services that support full
-- Backup management. If you do not specify a key, Backup creates an KMS
-- key for you by default.
awsBackupBackupVaultDetails_encryptionKeyArn :: Lens.Lens' AwsBackupBackupVaultDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupVaultDetails_encryptionKeyArn = Lens.lens (\AwsBackupBackupVaultDetails' {encryptionKeyArn} -> encryptionKeyArn) (\s@AwsBackupBackupVaultDetails' {} a -> s {encryptionKeyArn = a} :: AwsBackupBackupVaultDetails)

-- | The Amazon SNS event notifications for the specified backup vault.
awsBackupBackupVaultDetails_notifications :: Lens.Lens' AwsBackupBackupVaultDetails (Prelude.Maybe AwsBackupBackupVaultNotificationsDetails)
awsBackupBackupVaultDetails_notifications = Lens.lens (\AwsBackupBackupVaultDetails' {notifications} -> notifications) (\s@AwsBackupBackupVaultDetails' {} a -> s {notifications = a} :: AwsBackupBackupVaultDetails)

instance Data.FromJSON AwsBackupBackupVaultDetails where
  parseJSON =
    Data.withObject
      "AwsBackupBackupVaultDetails"
      ( \x ->
          AwsBackupBackupVaultDetails'
            Prelude.<$> (x Data..:? "AccessPolicy")
            Prelude.<*> (x Data..:? "BackupVaultArn")
            Prelude.<*> (x Data..:? "BackupVaultName")
            Prelude.<*> (x Data..:? "EncryptionKeyArn")
            Prelude.<*> (x Data..:? "Notifications")
      )

instance Prelude.Hashable AwsBackupBackupVaultDetails where
  hashWithSalt _salt AwsBackupBackupVaultDetails' {..} =
    _salt
      `Prelude.hashWithSalt` accessPolicy
      `Prelude.hashWithSalt` backupVaultArn
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` notifications

instance Prelude.NFData AwsBackupBackupVaultDetails where
  rnf AwsBackupBackupVaultDetails' {..} =
    Prelude.rnf accessPolicy
      `Prelude.seq` Prelude.rnf backupVaultArn
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf notifications

instance Data.ToJSON AwsBackupBackupVaultDetails where
  toJSON AwsBackupBackupVaultDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessPolicy" Data..=) Prelude.<$> accessPolicy,
            ("BackupVaultArn" Data..=)
              Prelude.<$> backupVaultArn,
            ("BackupVaultName" Data..=)
              Prelude.<$> backupVaultName,
            ("EncryptionKeyArn" Data..=)
              Prelude.<$> encryptionKeyArn,
            ("Notifications" Data..=) Prelude.<$> notifications
          ]
      )
