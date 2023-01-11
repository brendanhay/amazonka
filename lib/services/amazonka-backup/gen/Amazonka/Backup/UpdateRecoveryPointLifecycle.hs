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
-- Module      : Amazonka.Backup.UpdateRecoveryPointLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the transition lifecycle of a recovery point.
--
-- The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
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
-- This operation does not support continuous backups.
module Amazonka.Backup.UpdateRecoveryPointLifecycle
  ( -- * Creating a Request
    UpdateRecoveryPointLifecycle (..),
    newUpdateRecoveryPointLifecycle,

    -- * Request Lenses
    updateRecoveryPointLifecycle_lifecycle,
    updateRecoveryPointLifecycle_backupVaultName,
    updateRecoveryPointLifecycle_recoveryPointArn,

    -- * Destructuring the Response
    UpdateRecoveryPointLifecycleResponse (..),
    newUpdateRecoveryPointLifecycleResponse,

    -- * Response Lenses
    updateRecoveryPointLifecycleResponse_backupVaultArn,
    updateRecoveryPointLifecycleResponse_calculatedLifecycle,
    updateRecoveryPointLifecycleResponse_lifecycle,
    updateRecoveryPointLifecycleResponse_recoveryPointArn,
    updateRecoveryPointLifecycleResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRecoveryPointLifecycle' smart constructor.
data UpdateRecoveryPointLifecycle = UpdateRecoveryPointLifecycle'
  { -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup transitions and expires backups
    -- automatically according to the lifecycle that you define.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
    -- greater than the “transition to cold after days” setting. The
    -- “transition to cold after days” setting cannot be changed after a backup
    -- has been transitioned to cold.
    lifecycle :: Prelude.Maybe Lifecycle,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecoveryPointLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'updateRecoveryPointLifecycle_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- 'backupVaultName', 'updateRecoveryPointLifecycle_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'recoveryPointArn', 'updateRecoveryPointLifecycle_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
newUpdateRecoveryPointLifecycle ::
  -- | 'backupVaultName'
  Prelude.Text ->
  -- | 'recoveryPointArn'
  Prelude.Text ->
  UpdateRecoveryPointLifecycle
newUpdateRecoveryPointLifecycle
  pBackupVaultName_
  pRecoveryPointArn_ =
    UpdateRecoveryPointLifecycle'
      { lifecycle =
          Prelude.Nothing,
        backupVaultName = pBackupVaultName_,
        recoveryPointArn = pRecoveryPointArn_
      }

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
updateRecoveryPointLifecycle_lifecycle :: Lens.Lens' UpdateRecoveryPointLifecycle (Prelude.Maybe Lifecycle)
updateRecoveryPointLifecycle_lifecycle = Lens.lens (\UpdateRecoveryPointLifecycle' {lifecycle} -> lifecycle) (\s@UpdateRecoveryPointLifecycle' {} a -> s {lifecycle = a} :: UpdateRecoveryPointLifecycle)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
updateRecoveryPointLifecycle_backupVaultName :: Lens.Lens' UpdateRecoveryPointLifecycle Prelude.Text
updateRecoveryPointLifecycle_backupVaultName = Lens.lens (\UpdateRecoveryPointLifecycle' {backupVaultName} -> backupVaultName) (\s@UpdateRecoveryPointLifecycle' {} a -> s {backupVaultName = a} :: UpdateRecoveryPointLifecycle)

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
updateRecoveryPointLifecycle_recoveryPointArn :: Lens.Lens' UpdateRecoveryPointLifecycle Prelude.Text
updateRecoveryPointLifecycle_recoveryPointArn = Lens.lens (\UpdateRecoveryPointLifecycle' {recoveryPointArn} -> recoveryPointArn) (\s@UpdateRecoveryPointLifecycle' {} a -> s {recoveryPointArn = a} :: UpdateRecoveryPointLifecycle)

instance Core.AWSRequest UpdateRecoveryPointLifecycle where
  type
    AWSResponse UpdateRecoveryPointLifecycle =
      UpdateRecoveryPointLifecycleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecoveryPointLifecycleResponse'
            Prelude.<$> (x Data..?> "BackupVaultArn")
            Prelude.<*> (x Data..?> "CalculatedLifecycle")
            Prelude.<*> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "RecoveryPointArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateRecoveryPointLifecycle
  where
  hashWithSalt _salt UpdateRecoveryPointLifecycle' {..} =
    _salt `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` recoveryPointArn

instance Prelude.NFData UpdateRecoveryPointLifecycle where
  rnf UpdateRecoveryPointLifecycle' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf recoveryPointArn

instance Data.ToHeaders UpdateRecoveryPointLifecycle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRecoveryPointLifecycle where
  toJSON UpdateRecoveryPointLifecycle' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Lifecycle" Data..=) Prelude.<$> lifecycle]
      )

instance Data.ToPath UpdateRecoveryPointLifecycle where
  toPath UpdateRecoveryPointLifecycle' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/recovery-points/",
        Data.toBS recoveryPointArn
      ]

instance Data.ToQuery UpdateRecoveryPointLifecycle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRecoveryPointLifecycleResponse' smart constructor.
data UpdateRecoveryPointLifecycleResponse = UpdateRecoveryPointLifecycleResponse'
  { -- | An ARN that uniquely identifies a backup vault; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
    -- @MoveToColdStorageAt@ timestamps.
    calculatedLifecycle :: Prelude.Maybe CalculatedLifecycle,
    -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup transitions and expires backups
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
    -- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecoveryPointLifecycleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultArn', 'updateRecoveryPointLifecycleResponse_backupVaultArn' - An ARN that uniquely identifies a backup vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'calculatedLifecycle', 'updateRecoveryPointLifecycleResponse_calculatedLifecycle' - A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
--
-- 'lifecycle', 'updateRecoveryPointLifecycleResponse_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
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
-- 'recoveryPointArn', 'updateRecoveryPointLifecycleResponse_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'httpStatus', 'updateRecoveryPointLifecycleResponse_httpStatus' - The response's http status code.
newUpdateRecoveryPointLifecycleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRecoveryPointLifecycleResponse
newUpdateRecoveryPointLifecycleResponse pHttpStatus_ =
  UpdateRecoveryPointLifecycleResponse'
    { backupVaultArn =
        Prelude.Nothing,
      calculatedLifecycle = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ARN that uniquely identifies a backup vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
updateRecoveryPointLifecycleResponse_backupVaultArn :: Lens.Lens' UpdateRecoveryPointLifecycleResponse (Prelude.Maybe Prelude.Text)
updateRecoveryPointLifecycleResponse_backupVaultArn = Lens.lens (\UpdateRecoveryPointLifecycleResponse' {backupVaultArn} -> backupVaultArn) (\s@UpdateRecoveryPointLifecycleResponse' {} a -> s {backupVaultArn = a} :: UpdateRecoveryPointLifecycleResponse)

-- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
updateRecoveryPointLifecycleResponse_calculatedLifecycle :: Lens.Lens' UpdateRecoveryPointLifecycleResponse (Prelude.Maybe CalculatedLifecycle)
updateRecoveryPointLifecycleResponse_calculatedLifecycle = Lens.lens (\UpdateRecoveryPointLifecycleResponse' {calculatedLifecycle} -> calculatedLifecycle) (\s@UpdateRecoveryPointLifecycleResponse' {} a -> s {calculatedLifecycle = a} :: UpdateRecoveryPointLifecycleResponse)

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
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
updateRecoveryPointLifecycleResponse_lifecycle :: Lens.Lens' UpdateRecoveryPointLifecycleResponse (Prelude.Maybe Lifecycle)
updateRecoveryPointLifecycleResponse_lifecycle = Lens.lens (\UpdateRecoveryPointLifecycleResponse' {lifecycle} -> lifecycle) (\s@UpdateRecoveryPointLifecycleResponse' {} a -> s {lifecycle = a} :: UpdateRecoveryPointLifecycleResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
updateRecoveryPointLifecycleResponse_recoveryPointArn :: Lens.Lens' UpdateRecoveryPointLifecycleResponse (Prelude.Maybe Prelude.Text)
updateRecoveryPointLifecycleResponse_recoveryPointArn = Lens.lens (\UpdateRecoveryPointLifecycleResponse' {recoveryPointArn} -> recoveryPointArn) (\s@UpdateRecoveryPointLifecycleResponse' {} a -> s {recoveryPointArn = a} :: UpdateRecoveryPointLifecycleResponse)

-- | The response's http status code.
updateRecoveryPointLifecycleResponse_httpStatus :: Lens.Lens' UpdateRecoveryPointLifecycleResponse Prelude.Int
updateRecoveryPointLifecycleResponse_httpStatus = Lens.lens (\UpdateRecoveryPointLifecycleResponse' {httpStatus} -> httpStatus) (\s@UpdateRecoveryPointLifecycleResponse' {} a -> s {httpStatus = a} :: UpdateRecoveryPointLifecycleResponse)

instance
  Prelude.NFData
    UpdateRecoveryPointLifecycleResponse
  where
  rnf UpdateRecoveryPointLifecycleResponse' {..} =
    Prelude.rnf backupVaultArn
      `Prelude.seq` Prelude.rnf calculatedLifecycle
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf httpStatus
