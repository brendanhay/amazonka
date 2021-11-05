{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Backup.DescribeBackupVault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a backup vault specified by its name.
module Network.AWS.Backup.DescribeBackupVault
  ( -- * Creating a Request
    DescribeBackupVault (..),
    newDescribeBackupVault,

    -- * Request Lenses
    describeBackupVault_backupVaultName,

    -- * Destructuring the Response
    DescribeBackupVaultResponse (..),
    newDescribeBackupVaultResponse,

    -- * Response Lenses
    describeBackupVaultResponse_lockDate,
    describeBackupVaultResponse_maxRetentionDays,
    describeBackupVaultResponse_locked,
    describeBackupVaultResponse_creatorRequestId,
    describeBackupVaultResponse_numberOfRecoveryPoints,
    describeBackupVaultResponse_backupVaultArn,
    describeBackupVaultResponse_encryptionKeyArn,
    describeBackupVaultResponse_creationDate,
    describeBackupVaultResponse_backupVaultName,
    describeBackupVaultResponse_minRetentionDays,
    describeBackupVaultResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBackupVault' smart constructor.
data DescribeBackupVault = DescribeBackupVault'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'describeBackupVault_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newDescribeBackupVault ::
  -- | 'backupVaultName'
  Prelude.Text ->
  DescribeBackupVault
newDescribeBackupVault pBackupVaultName_ =
  DescribeBackupVault'
    { backupVaultName =
        pBackupVaultName_
    }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
describeBackupVault_backupVaultName :: Lens.Lens' DescribeBackupVault Prelude.Text
describeBackupVault_backupVaultName = Lens.lens (\DescribeBackupVault' {backupVaultName} -> backupVaultName) (\s@DescribeBackupVault' {} a -> s {backupVaultName = a} :: DescribeBackupVault)

instance Core.AWSRequest DescribeBackupVault where
  type
    AWSResponse DescribeBackupVault =
      DescribeBackupVaultResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBackupVaultResponse'
            Prelude.<$> (x Core..?> "LockDate")
            Prelude.<*> (x Core..?> "MaxRetentionDays")
            Prelude.<*> (x Core..?> "Locked")
            Prelude.<*> (x Core..?> "CreatorRequestId")
            Prelude.<*> (x Core..?> "NumberOfRecoveryPoints")
            Prelude.<*> (x Core..?> "BackupVaultArn")
            Prelude.<*> (x Core..?> "EncryptionKeyArn")
            Prelude.<*> (x Core..?> "CreationDate")
            Prelude.<*> (x Core..?> "BackupVaultName")
            Prelude.<*> (x Core..?> "MinRetentionDays")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBackupVault

instance Prelude.NFData DescribeBackupVault

instance Core.ToHeaders DescribeBackupVault where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeBackupVault where
  toPath DescribeBackupVault' {..} =
    Prelude.mconcat
      ["/backup-vaults/", Core.toBS backupVaultName]

instance Core.ToQuery DescribeBackupVault where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBackupVaultResponse' smart constructor.
data DescribeBackupVaultResponse = DescribeBackupVaultResponse'
  { -- | The date and time when Backup Vault Lock configuration cannot be changed
    -- or deleted.
    --
    -- If you applied Vault Lock to your vault without specifying a lock date,
    -- you can change any of your Vault Lock settings, or delete Vault Lock
    -- from the vault entirely, at any time.
    --
    -- This value is in Unix format, Coordinated Universal Time (UTC), and
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    lockDate :: Prelude.Maybe Core.POSIX,
    -- | The Backup Vault Lock setting that specifies the maximum retention
    -- period that the vault retains its recovery points. If this parameter is
    -- not specified, Vault Lock does not enforce a maximum retention period on
    -- the recovery points in the vault (allowing indefinite storage).
    --
    -- If specified, any backup or copy job to the vault must have a lifecycle
    -- policy with a retention period equal to or shorter than the maximum
    -- retention period. If the job\'s retention period is longer than that
    -- maximum retention period, then the vault fails the backup or copy job,
    -- and you should either modify your lifecycle settings or use a different
    -- vault. Recovery points already stored in the vault prior to Vault Lock
    -- are not affected.
    maxRetentionDays :: Prelude.Maybe Prelude.Integer,
    -- | A Boolean that indicates whether Backup Vault Lock is currently
    -- protecting the backup vault. @True@ means that Vault Lock causes delete
    -- or update operations on the recovery points stored in the vault to fail.
    locked :: Prelude.Maybe Prelude.Bool,
    -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The number of recovery points that are stored in a backup vault.
    numberOfRecoveryPoints :: Prelude.Maybe Prelude.Integer,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
    -- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption key that is used to protect your backups; for
    -- example,
    -- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a backup vault is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Region where they are created. They consist of lowercase
    -- letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The Backup Vault Lock setting that specifies the minimum retention
    -- period that the vault retains its recovery points. If this parameter is
    -- not specified, Vault Lock does not enforce a minimum retention period.
    --
    -- If specified, any backup or copy job to the vault must have a lifecycle
    -- policy with a retention period equal to or longer than the minimum
    -- retention period. If the job\'s retention period is shorter than that
    -- minimum retention period, then the vault fails the backup or copy job,
    -- and you should either modify your lifecycle settings or use a different
    -- vault. Recovery points already stored in the vault prior to Vault Lock
    -- are not affected.
    minRetentionDays :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockDate', 'describeBackupVaultResponse_lockDate' - The date and time when Backup Vault Lock configuration cannot be changed
-- or deleted.
--
-- If you applied Vault Lock to your vault without specifying a lock date,
-- you can change any of your Vault Lock settings, or delete Vault Lock
-- from the vault entirely, at any time.
--
-- This value is in Unix format, Coordinated Universal Time (UTC), and
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'maxRetentionDays', 'describeBackupVaultResponse_maxRetentionDays' - The Backup Vault Lock setting that specifies the maximum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a maximum retention period on
-- the recovery points in the vault (allowing indefinite storage).
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or shorter than the maximum
-- retention period. If the job\'s retention period is longer than that
-- maximum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
--
-- 'locked', 'describeBackupVaultResponse_locked' - A Boolean that indicates whether Backup Vault Lock is currently
-- protecting the backup vault. @True@ means that Vault Lock causes delete
-- or update operations on the recovery points stored in the vault to fail.
--
-- 'creatorRequestId', 'describeBackupVaultResponse_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
--
-- 'numberOfRecoveryPoints', 'describeBackupVaultResponse_numberOfRecoveryPoints' - The number of recovery points that are stored in a backup vault.
--
-- 'backupVaultArn', 'describeBackupVaultResponse_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'encryptionKeyArn', 'describeBackupVaultResponse_encryptionKeyArn' - The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 'creationDate', 'describeBackupVaultResponse_creationDate' - The date and time that a backup vault is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupVaultName', 'describeBackupVaultResponse_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
--
-- 'minRetentionDays', 'describeBackupVaultResponse_minRetentionDays' - The Backup Vault Lock setting that specifies the minimum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a minimum retention period.
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or longer than the minimum
-- retention period. If the job\'s retention period is shorter than that
-- minimum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
--
-- 'httpStatus', 'describeBackupVaultResponse_httpStatus' - The response's http status code.
newDescribeBackupVaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBackupVaultResponse
newDescribeBackupVaultResponse pHttpStatus_ =
  DescribeBackupVaultResponse'
    { lockDate =
        Prelude.Nothing,
      maxRetentionDays = Prelude.Nothing,
      locked = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      numberOfRecoveryPoints = Prelude.Nothing,
      backupVaultArn = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      minRetentionDays = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when Backup Vault Lock configuration cannot be changed
-- or deleted.
--
-- If you applied Vault Lock to your vault without specifying a lock date,
-- you can change any of your Vault Lock settings, or delete Vault Lock
-- from the vault entirely, at any time.
--
-- This value is in Unix format, Coordinated Universal Time (UTC), and
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeBackupVaultResponse_lockDate :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.UTCTime)
describeBackupVaultResponse_lockDate = Lens.lens (\DescribeBackupVaultResponse' {lockDate} -> lockDate) (\s@DescribeBackupVaultResponse' {} a -> s {lockDate = a} :: DescribeBackupVaultResponse) Prelude.. Lens.mapping Core._Time

-- | The Backup Vault Lock setting that specifies the maximum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a maximum retention period on
-- the recovery points in the vault (allowing indefinite storage).
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or shorter than the maximum
-- retention period. If the job\'s retention period is longer than that
-- maximum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
describeBackupVaultResponse_maxRetentionDays :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Integer)
describeBackupVaultResponse_maxRetentionDays = Lens.lens (\DescribeBackupVaultResponse' {maxRetentionDays} -> maxRetentionDays) (\s@DescribeBackupVaultResponse' {} a -> s {maxRetentionDays = a} :: DescribeBackupVaultResponse)

-- | A Boolean that indicates whether Backup Vault Lock is currently
-- protecting the backup vault. @True@ means that Vault Lock causes delete
-- or update operations on the recovery points stored in the vault to fail.
describeBackupVaultResponse_locked :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Bool)
describeBackupVaultResponse_locked = Lens.lens (\DescribeBackupVaultResponse' {locked} -> locked) (\s@DescribeBackupVaultResponse' {} a -> s {locked = a} :: DescribeBackupVaultResponse)

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
describeBackupVaultResponse_creatorRequestId :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Text)
describeBackupVaultResponse_creatorRequestId = Lens.lens (\DescribeBackupVaultResponse' {creatorRequestId} -> creatorRequestId) (\s@DescribeBackupVaultResponse' {} a -> s {creatorRequestId = a} :: DescribeBackupVaultResponse)

-- | The number of recovery points that are stored in a backup vault.
describeBackupVaultResponse_numberOfRecoveryPoints :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Integer)
describeBackupVaultResponse_numberOfRecoveryPoints = Lens.lens (\DescribeBackupVaultResponse' {numberOfRecoveryPoints} -> numberOfRecoveryPoints) (\s@DescribeBackupVaultResponse' {} a -> s {numberOfRecoveryPoints = a} :: DescribeBackupVaultResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
describeBackupVaultResponse_backupVaultArn :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Text)
describeBackupVaultResponse_backupVaultArn = Lens.lens (\DescribeBackupVaultResponse' {backupVaultArn} -> backupVaultArn) (\s@DescribeBackupVaultResponse' {} a -> s {backupVaultArn = a} :: DescribeBackupVaultResponse)

-- | The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
describeBackupVaultResponse_encryptionKeyArn :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Text)
describeBackupVaultResponse_encryptionKeyArn = Lens.lens (\DescribeBackupVaultResponse' {encryptionKeyArn} -> encryptionKeyArn) (\s@DescribeBackupVaultResponse' {} a -> s {encryptionKeyArn = a} :: DescribeBackupVaultResponse)

-- | The date and time that a backup vault is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeBackupVaultResponse_creationDate :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.UTCTime)
describeBackupVaultResponse_creationDate = Lens.lens (\DescribeBackupVaultResponse' {creationDate} -> creationDate) (\s@DescribeBackupVaultResponse' {} a -> s {creationDate = a} :: DescribeBackupVaultResponse) Prelude.. Lens.mapping Core._Time

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
describeBackupVaultResponse_backupVaultName :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Text)
describeBackupVaultResponse_backupVaultName = Lens.lens (\DescribeBackupVaultResponse' {backupVaultName} -> backupVaultName) (\s@DescribeBackupVaultResponse' {} a -> s {backupVaultName = a} :: DescribeBackupVaultResponse)

-- | The Backup Vault Lock setting that specifies the minimum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a minimum retention period.
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or longer than the minimum
-- retention period. If the job\'s retention period is shorter than that
-- minimum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
describeBackupVaultResponse_minRetentionDays :: Lens.Lens' DescribeBackupVaultResponse (Prelude.Maybe Prelude.Integer)
describeBackupVaultResponse_minRetentionDays = Lens.lens (\DescribeBackupVaultResponse' {minRetentionDays} -> minRetentionDays) (\s@DescribeBackupVaultResponse' {} a -> s {minRetentionDays = a} :: DescribeBackupVaultResponse)

-- | The response's http status code.
describeBackupVaultResponse_httpStatus :: Lens.Lens' DescribeBackupVaultResponse Prelude.Int
describeBackupVaultResponse_httpStatus = Lens.lens (\DescribeBackupVaultResponse' {httpStatus} -> httpStatus) (\s@DescribeBackupVaultResponse' {} a -> s {httpStatus = a} :: DescribeBackupVaultResponse)

instance Prelude.NFData DescribeBackupVaultResponse
