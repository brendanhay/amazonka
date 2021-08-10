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
-- Module      : Network.AWS.DynamoDB.RestoreTableFromBackup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table from an existing backup. Any number of users can
-- execute up to 4 concurrent restores (any type of restore) in a given
-- account.
--
-- You can call @RestoreTableFromBackup@ at a maximum rate of 10 times per
-- second.
--
-- You must manually set up the following on the restored table:
--
-- -   Auto scaling policies
--
-- -   IAM policies
--
-- -   Amazon CloudWatch metrics and alarms
--
-- -   Tags
--
-- -   Stream settings
--
-- -   Time to Live (TTL) settings
module Network.AWS.DynamoDB.RestoreTableFromBackup
  ( -- * Creating a Request
    RestoreTableFromBackup (..),
    newRestoreTableFromBackup,

    -- * Request Lenses
    restoreTableFromBackup_provisionedThroughputOverride,
    restoreTableFromBackup_globalSecondaryIndexOverride,
    restoreTableFromBackup_billingModeOverride,
    restoreTableFromBackup_sSESpecificationOverride,
    restoreTableFromBackup_localSecondaryIndexOverride,
    restoreTableFromBackup_targetTableName,
    restoreTableFromBackup_backupArn,

    -- * Destructuring the Response
    RestoreTableFromBackupResponse (..),
    newRestoreTableFromBackupResponse,

    -- * Response Lenses
    restoreTableFromBackupResponse_tableDescription,
    restoreTableFromBackupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreTableFromBackup' smart constructor.
data RestoreTableFromBackup = RestoreTableFromBackup'
  { -- | Provisioned throughput settings for the restored table.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughput,
    -- | List of global secondary indexes for the restored table. The indexes
    -- provided should match existing secondary indexes. You can choose to
    -- exclude some or all of the indexes at the time of restore.
    globalSecondaryIndexOverride :: Prelude.Maybe [GlobalSecondaryIndex],
    -- | The billing mode of the restored table.
    billingModeOverride :: Prelude.Maybe BillingMode,
    -- | The new server-side encryption settings for the restored table.
    sSESpecificationOverride :: Prelude.Maybe SSESpecification,
    -- | List of local secondary indexes for the restored table. The indexes
    -- provided should match existing secondary indexes. You can choose to
    -- exclude some or all of the indexes at the time of restore.
    localSecondaryIndexOverride :: Prelude.Maybe [LocalSecondaryIndex],
    -- | The name of the new table to which the backup must be restored.
    targetTableName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the backup.
    backupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableFromBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughputOverride', 'restoreTableFromBackup_provisionedThroughputOverride' - Provisioned throughput settings for the restored table.
--
-- 'globalSecondaryIndexOverride', 'restoreTableFromBackup_globalSecondaryIndexOverride' - List of global secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
--
-- 'billingModeOverride', 'restoreTableFromBackup_billingModeOverride' - The billing mode of the restored table.
--
-- 'sSESpecificationOverride', 'restoreTableFromBackup_sSESpecificationOverride' - The new server-side encryption settings for the restored table.
--
-- 'localSecondaryIndexOverride', 'restoreTableFromBackup_localSecondaryIndexOverride' - List of local secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
--
-- 'targetTableName', 'restoreTableFromBackup_targetTableName' - The name of the new table to which the backup must be restored.
--
-- 'backupArn', 'restoreTableFromBackup_backupArn' - The Amazon Resource Name (ARN) associated with the backup.
newRestoreTableFromBackup ::
  -- | 'targetTableName'
  Prelude.Text ->
  -- | 'backupArn'
  Prelude.Text ->
  RestoreTableFromBackup
newRestoreTableFromBackup
  pTargetTableName_
  pBackupArn_ =
    RestoreTableFromBackup'
      { provisionedThroughputOverride =
          Prelude.Nothing,
        globalSecondaryIndexOverride = Prelude.Nothing,
        billingModeOverride = Prelude.Nothing,
        sSESpecificationOverride = Prelude.Nothing,
        localSecondaryIndexOverride = Prelude.Nothing,
        targetTableName = pTargetTableName_,
        backupArn = pBackupArn_
      }

-- | Provisioned throughput settings for the restored table.
restoreTableFromBackup_provisionedThroughputOverride :: Lens.Lens' RestoreTableFromBackup (Prelude.Maybe ProvisionedThroughput)
restoreTableFromBackup_provisionedThroughputOverride = Lens.lens (\RestoreTableFromBackup' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@RestoreTableFromBackup' {} a -> s {provisionedThroughputOverride = a} :: RestoreTableFromBackup)

-- | List of global secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
restoreTableFromBackup_globalSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Prelude.Maybe [GlobalSecondaryIndex])
restoreTableFromBackup_globalSecondaryIndexOverride = Lens.lens (\RestoreTableFromBackup' {globalSecondaryIndexOverride} -> globalSecondaryIndexOverride) (\s@RestoreTableFromBackup' {} a -> s {globalSecondaryIndexOverride = a} :: RestoreTableFromBackup) Prelude.. Lens.mapping Lens._Coerce

-- | The billing mode of the restored table.
restoreTableFromBackup_billingModeOverride :: Lens.Lens' RestoreTableFromBackup (Prelude.Maybe BillingMode)
restoreTableFromBackup_billingModeOverride = Lens.lens (\RestoreTableFromBackup' {billingModeOverride} -> billingModeOverride) (\s@RestoreTableFromBackup' {} a -> s {billingModeOverride = a} :: RestoreTableFromBackup)

-- | The new server-side encryption settings for the restored table.
restoreTableFromBackup_sSESpecificationOverride :: Lens.Lens' RestoreTableFromBackup (Prelude.Maybe SSESpecification)
restoreTableFromBackup_sSESpecificationOverride = Lens.lens (\RestoreTableFromBackup' {sSESpecificationOverride} -> sSESpecificationOverride) (\s@RestoreTableFromBackup' {} a -> s {sSESpecificationOverride = a} :: RestoreTableFromBackup)

-- | List of local secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
restoreTableFromBackup_localSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Prelude.Maybe [LocalSecondaryIndex])
restoreTableFromBackup_localSecondaryIndexOverride = Lens.lens (\RestoreTableFromBackup' {localSecondaryIndexOverride} -> localSecondaryIndexOverride) (\s@RestoreTableFromBackup' {} a -> s {localSecondaryIndexOverride = a} :: RestoreTableFromBackup) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the new table to which the backup must be restored.
restoreTableFromBackup_targetTableName :: Lens.Lens' RestoreTableFromBackup Prelude.Text
restoreTableFromBackup_targetTableName = Lens.lens (\RestoreTableFromBackup' {targetTableName} -> targetTableName) (\s@RestoreTableFromBackup' {} a -> s {targetTableName = a} :: RestoreTableFromBackup)

-- | The Amazon Resource Name (ARN) associated with the backup.
restoreTableFromBackup_backupArn :: Lens.Lens' RestoreTableFromBackup Prelude.Text
restoreTableFromBackup_backupArn = Lens.lens (\RestoreTableFromBackup' {backupArn} -> backupArn) (\s@RestoreTableFromBackup' {} a -> s {backupArn = a} :: RestoreTableFromBackup)

instance Core.AWSRequest RestoreTableFromBackup where
  type
    AWSResponse RestoreTableFromBackup =
      RestoreTableFromBackupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreTableFromBackupResponse'
            Prelude.<$> (x Core..?> "TableDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreTableFromBackup

instance Prelude.NFData RestoreTableFromBackup

instance Core.ToHeaders RestoreTableFromBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.RestoreTableFromBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RestoreTableFromBackup where
  toJSON RestoreTableFromBackup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProvisionedThroughputOverride" Core..=)
              Prelude.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexOverride" Core..=)
              Prelude.<$> globalSecondaryIndexOverride,
            ("BillingModeOverride" Core..=)
              Prelude.<$> billingModeOverride,
            ("SSESpecificationOverride" Core..=)
              Prelude.<$> sSESpecificationOverride,
            ("LocalSecondaryIndexOverride" Core..=)
              Prelude.<$> localSecondaryIndexOverride,
            Prelude.Just
              ("TargetTableName" Core..= targetTableName),
            Prelude.Just ("BackupArn" Core..= backupArn)
          ]
      )

instance Core.ToPath RestoreTableFromBackup where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreTableFromBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreTableFromBackupResponse' smart constructor.
data RestoreTableFromBackupResponse = RestoreTableFromBackupResponse'
  { -- | The description of the table created from an existing backup.
    tableDescription :: Prelude.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableFromBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableDescription', 'restoreTableFromBackupResponse_tableDescription' - The description of the table created from an existing backup.
--
-- 'httpStatus', 'restoreTableFromBackupResponse_httpStatus' - The response's http status code.
newRestoreTableFromBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreTableFromBackupResponse
newRestoreTableFromBackupResponse pHttpStatus_ =
  RestoreTableFromBackupResponse'
    { tableDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the table created from an existing backup.
restoreTableFromBackupResponse_tableDescription :: Lens.Lens' RestoreTableFromBackupResponse (Prelude.Maybe TableDescription)
restoreTableFromBackupResponse_tableDescription = Lens.lens (\RestoreTableFromBackupResponse' {tableDescription} -> tableDescription) (\s@RestoreTableFromBackupResponse' {} a -> s {tableDescription = a} :: RestoreTableFromBackupResponse)

-- | The response's http status code.
restoreTableFromBackupResponse_httpStatus :: Lens.Lens' RestoreTableFromBackupResponse Prelude.Int
restoreTableFromBackupResponse_httpStatus = Lens.lens (\RestoreTableFromBackupResponse' {httpStatus} -> httpStatus) (\s@RestoreTableFromBackupResponse' {} a -> s {httpStatus = a} :: RestoreTableFromBackupResponse)

instance
  Prelude.NFData
    RestoreTableFromBackupResponse
