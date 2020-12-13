{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.RestoreTableFromBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table from an existing backup. Any number of users can execute up to 4 concurrent restores (any type of restore) in a given account.
--
-- You can call @RestoreTableFromBackup@ at a maximum rate of 10 times per second.
-- You must manually set up the following on the restored table:
--
--     * Auto scaling policies
--
--
--     * IAM policies
--
--
--     * Amazon CloudWatch metrics and alarms
--
--
--     * Tags
--
--
--     * Stream settings
--
--
--     * Time to Live (TTL) settings
module Network.AWS.DynamoDB.RestoreTableFromBackup
  ( -- * Creating a request
    RestoreTableFromBackup (..),
    mkRestoreTableFromBackup,

    -- ** Request lenses
    rtfbBillingModeOverride,
    rtfbGlobalSecondaryIndexOverride,
    rtfbProvisionedThroughputOverride,
    rtfbSSESpecificationOverride,
    rtfbLocalSecondaryIndexOverride,
    rtfbTargetTableName,
    rtfbBackupARN,

    -- * Destructuring the response
    RestoreTableFromBackupResponse (..),
    mkRestoreTableFromBackupResponse,

    -- ** Response lenses
    rtfbrsTableDescription,
    rtfbrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreTableFromBackup' smart constructor.
data RestoreTableFromBackup = RestoreTableFromBackup'
  { -- | The billing mode of the restored table.
    billingModeOverride :: Lude.Maybe BillingMode,
    -- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    globalSecondaryIndexOverride :: Lude.Maybe [GlobalSecondaryIndex],
    -- | Provisioned throughput settings for the restored table.
    provisionedThroughputOverride :: Lude.Maybe ProvisionedThroughput,
    -- | The new server-side encryption settings for the restored table.
    sSESpecificationOverride :: Lude.Maybe SSESpecification,
    -- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    localSecondaryIndexOverride :: Lude.Maybe [LocalSecondaryIndex],
    -- | The name of the new table to which the backup must be restored.
    targetTableName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) associated with the backup.
    backupARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreTableFromBackup' with the minimum fields required to make a request.
--
-- * 'billingModeOverride' - The billing mode of the restored table.
-- * 'globalSecondaryIndexOverride' - List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
-- * 'provisionedThroughputOverride' - Provisioned throughput settings for the restored table.
-- * 'sSESpecificationOverride' - The new server-side encryption settings for the restored table.
-- * 'localSecondaryIndexOverride' - List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
-- * 'targetTableName' - The name of the new table to which the backup must be restored.
-- * 'backupARN' - The Amazon Resource Name (ARN) associated with the backup.
mkRestoreTableFromBackup ::
  -- | 'targetTableName'
  Lude.Text ->
  -- | 'backupARN'
  Lude.Text ->
  RestoreTableFromBackup
mkRestoreTableFromBackup pTargetTableName_ pBackupARN_ =
  RestoreTableFromBackup'
    { billingModeOverride = Lude.Nothing,
      globalSecondaryIndexOverride = Lude.Nothing,
      provisionedThroughputOverride = Lude.Nothing,
      sSESpecificationOverride = Lude.Nothing,
      localSecondaryIndexOverride = Lude.Nothing,
      targetTableName = pTargetTableName_,
      backupARN = pBackupARN_
    }

-- | The billing mode of the restored table.
--
-- /Note:/ Consider using 'billingModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbBillingModeOverride :: Lens.Lens' RestoreTableFromBackup (Lude.Maybe BillingMode)
rtfbBillingModeOverride = Lens.lens (billingModeOverride :: RestoreTableFromBackup -> Lude.Maybe BillingMode) (\s a -> s {billingModeOverride = a} :: RestoreTableFromBackup)
{-# DEPRECATED rtfbBillingModeOverride "Use generic-lens or generic-optics with 'billingModeOverride' instead." #-}

-- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'globalSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbGlobalSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Lude.Maybe [GlobalSecondaryIndex])
rtfbGlobalSecondaryIndexOverride = Lens.lens (globalSecondaryIndexOverride :: RestoreTableFromBackup -> Lude.Maybe [GlobalSecondaryIndex]) (\s a -> s {globalSecondaryIndexOverride = a} :: RestoreTableFromBackup)
{-# DEPRECATED rtfbGlobalSecondaryIndexOverride "Use generic-lens or generic-optics with 'globalSecondaryIndexOverride' instead." #-}

-- | Provisioned throughput settings for the restored table.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbProvisionedThroughputOverride :: Lens.Lens' RestoreTableFromBackup (Lude.Maybe ProvisionedThroughput)
rtfbProvisionedThroughputOverride = Lens.lens (provisionedThroughputOverride :: RestoreTableFromBackup -> Lude.Maybe ProvisionedThroughput) (\s a -> s {provisionedThroughputOverride = a} :: RestoreTableFromBackup)
{-# DEPRECATED rtfbProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | The new server-side encryption settings for the restored table.
--
-- /Note:/ Consider using 'sSESpecificationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbSSESpecificationOverride :: Lens.Lens' RestoreTableFromBackup (Lude.Maybe SSESpecification)
rtfbSSESpecificationOverride = Lens.lens (sSESpecificationOverride :: RestoreTableFromBackup -> Lude.Maybe SSESpecification) (\s a -> s {sSESpecificationOverride = a} :: RestoreTableFromBackup)
{-# DEPRECATED rtfbSSESpecificationOverride "Use generic-lens or generic-optics with 'sSESpecificationOverride' instead." #-}

-- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'localSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbLocalSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Lude.Maybe [LocalSecondaryIndex])
rtfbLocalSecondaryIndexOverride = Lens.lens (localSecondaryIndexOverride :: RestoreTableFromBackup -> Lude.Maybe [LocalSecondaryIndex]) (\s a -> s {localSecondaryIndexOverride = a} :: RestoreTableFromBackup)
{-# DEPRECATED rtfbLocalSecondaryIndexOverride "Use generic-lens or generic-optics with 'localSecondaryIndexOverride' instead." #-}

-- | The name of the new table to which the backup must be restored.
--
-- /Note:/ Consider using 'targetTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbTargetTableName :: Lens.Lens' RestoreTableFromBackup Lude.Text
rtfbTargetTableName = Lens.lens (targetTableName :: RestoreTableFromBackup -> Lude.Text) (\s a -> s {targetTableName = a} :: RestoreTableFromBackup)
{-# DEPRECATED rtfbTargetTableName "Use generic-lens or generic-optics with 'targetTableName' instead." #-}

-- | The Amazon Resource Name (ARN) associated with the backup.
--
-- /Note:/ Consider using 'backupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbBackupARN :: Lens.Lens' RestoreTableFromBackup Lude.Text
rtfbBackupARN = Lens.lens (backupARN :: RestoreTableFromBackup -> Lude.Text) (\s a -> s {backupARN = a} :: RestoreTableFromBackup)
{-# DEPRECATED rtfbBackupARN "Use generic-lens or generic-optics with 'backupARN' instead." #-}

instance Lude.AWSRequest RestoreTableFromBackup where
  type Rs RestoreTableFromBackup = RestoreTableFromBackupResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          RestoreTableFromBackupResponse'
            Lude.<$> (x Lude..?> "TableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreTableFromBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.RestoreTableFromBackup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreTableFromBackup where
  toJSON RestoreTableFromBackup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BillingModeOverride" Lude..=) Lude.<$> billingModeOverride,
            ("GlobalSecondaryIndexOverride" Lude..=)
              Lude.<$> globalSecondaryIndexOverride,
            ("ProvisionedThroughputOverride" Lude..=)
              Lude.<$> provisionedThroughputOverride,
            ("SSESpecificationOverride" Lude..=)
              Lude.<$> sSESpecificationOverride,
            ("LocalSecondaryIndexOverride" Lude..=)
              Lude.<$> localSecondaryIndexOverride,
            Lude.Just ("TargetTableName" Lude..= targetTableName),
            Lude.Just ("BackupArn" Lude..= backupARN)
          ]
      )

instance Lude.ToPath RestoreTableFromBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreTableFromBackup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRestoreTableFromBackupResponse' smart constructor.
data RestoreTableFromBackupResponse = RestoreTableFromBackupResponse'
  { -- | The description of the table created from an existing backup.
    tableDescription :: Lude.Maybe TableDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreTableFromBackupResponse' with the minimum fields required to make a request.
--
-- * 'tableDescription' - The description of the table created from an existing backup.
-- * 'responseStatus' - The response status code.
mkRestoreTableFromBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreTableFromBackupResponse
mkRestoreTableFromBackupResponse pResponseStatus_ =
  RestoreTableFromBackupResponse'
    { tableDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The description of the table created from an existing backup.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbrsTableDescription :: Lens.Lens' RestoreTableFromBackupResponse (Lude.Maybe TableDescription)
rtfbrsTableDescription = Lens.lens (tableDescription :: RestoreTableFromBackupResponse -> Lude.Maybe TableDescription) (\s a -> s {tableDescription = a} :: RestoreTableFromBackupResponse)
{-# DEPRECATED rtfbrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbrsResponseStatus :: Lens.Lens' RestoreTableFromBackupResponse Lude.Int
rtfbrsResponseStatus = Lens.lens (responseStatus :: RestoreTableFromBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreTableFromBackupResponse)
{-# DEPRECATED rtfbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
