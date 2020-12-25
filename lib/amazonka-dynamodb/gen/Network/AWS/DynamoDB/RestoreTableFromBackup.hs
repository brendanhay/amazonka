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
    rtfbTargetTableName,
    rtfbBackupArn,
    rtfbBillingModeOverride,
    rtfbGlobalSecondaryIndexOverride,
    rtfbLocalSecondaryIndexOverride,
    rtfbProvisionedThroughputOverride,
    rtfbSSESpecificationOverride,

    -- * Destructuring the response
    RestoreTableFromBackupResponse (..),
    mkRestoreTableFromBackupResponse,

    -- ** Response lenses
    rtfbrrsTableDescription,
    rtfbrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreTableFromBackup' smart constructor.
data RestoreTableFromBackup = RestoreTableFromBackup'
  { -- | The name of the new table to which the backup must be restored.
    targetTableName :: Types.TargetTableName,
    -- | The Amazon Resource Name (ARN) associated with the backup.
    backupArn :: Types.BackupArn,
    -- | The billing mode of the restored table.
    billingModeOverride :: Core.Maybe Types.BillingMode,
    -- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    globalSecondaryIndexOverride :: Core.Maybe [Types.GlobalSecondaryIndex],
    -- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    localSecondaryIndexOverride :: Core.Maybe [Types.LocalSecondaryIndex],
    -- | Provisioned throughput settings for the restored table.
    provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughput,
    -- | The new server-side encryption settings for the restored table.
    sSESpecificationOverride :: Core.Maybe Types.SSESpecification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreTableFromBackup' value with any optional fields omitted.
mkRestoreTableFromBackup ::
  -- | 'targetTableName'
  Types.TargetTableName ->
  -- | 'backupArn'
  Types.BackupArn ->
  RestoreTableFromBackup
mkRestoreTableFromBackup targetTableName backupArn =
  RestoreTableFromBackup'
    { targetTableName,
      backupArn,
      billingModeOverride = Core.Nothing,
      globalSecondaryIndexOverride = Core.Nothing,
      localSecondaryIndexOverride = Core.Nothing,
      provisionedThroughputOverride = Core.Nothing,
      sSESpecificationOverride = Core.Nothing
    }

-- | The name of the new table to which the backup must be restored.
--
-- /Note:/ Consider using 'targetTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbTargetTableName :: Lens.Lens' RestoreTableFromBackup Types.TargetTableName
rtfbTargetTableName = Lens.field @"targetTableName"
{-# DEPRECATED rtfbTargetTableName "Use generic-lens or generic-optics with 'targetTableName' instead." #-}

-- | The Amazon Resource Name (ARN) associated with the backup.
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbBackupArn :: Lens.Lens' RestoreTableFromBackup Types.BackupArn
rtfbBackupArn = Lens.field @"backupArn"
{-# DEPRECATED rtfbBackupArn "Use generic-lens or generic-optics with 'backupArn' instead." #-}

-- | The billing mode of the restored table.
--
-- /Note:/ Consider using 'billingModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbBillingModeOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe Types.BillingMode)
rtfbBillingModeOverride = Lens.field @"billingModeOverride"
{-# DEPRECATED rtfbBillingModeOverride "Use generic-lens or generic-optics with 'billingModeOverride' instead." #-}

-- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'globalSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbGlobalSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe [Types.GlobalSecondaryIndex])
rtfbGlobalSecondaryIndexOverride = Lens.field @"globalSecondaryIndexOverride"
{-# DEPRECATED rtfbGlobalSecondaryIndexOverride "Use generic-lens or generic-optics with 'globalSecondaryIndexOverride' instead." #-}

-- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'localSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbLocalSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe [Types.LocalSecondaryIndex])
rtfbLocalSecondaryIndexOverride = Lens.field @"localSecondaryIndexOverride"
{-# DEPRECATED rtfbLocalSecondaryIndexOverride "Use generic-lens or generic-optics with 'localSecondaryIndexOverride' instead." #-}

-- | Provisioned throughput settings for the restored table.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbProvisionedThroughputOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe Types.ProvisionedThroughput)
rtfbProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# DEPRECATED rtfbProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | The new server-side encryption settings for the restored table.
--
-- /Note:/ Consider using 'sSESpecificationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbSSESpecificationOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe Types.SSESpecification)
rtfbSSESpecificationOverride = Lens.field @"sSESpecificationOverride"
{-# DEPRECATED rtfbSSESpecificationOverride "Use generic-lens or generic-optics with 'sSESpecificationOverride' instead." #-}

instance Core.FromJSON RestoreTableFromBackup where
  toJSON RestoreTableFromBackup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TargetTableName" Core..= targetTableName),
            Core.Just ("BackupArn" Core..= backupArn),
            ("BillingModeOverride" Core..=) Core.<$> billingModeOverride,
            ("GlobalSecondaryIndexOverride" Core..=)
              Core.<$> globalSecondaryIndexOverride,
            ("LocalSecondaryIndexOverride" Core..=)
              Core.<$> localSecondaryIndexOverride,
            ("ProvisionedThroughputOverride" Core..=)
              Core.<$> provisionedThroughputOverride,
            ("SSESpecificationOverride" Core..=)
              Core.<$> sSESpecificationOverride
          ]
      )

instance Core.AWSRequest RestoreTableFromBackup where
  type Rs RestoreTableFromBackup = RestoreTableFromBackupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DynamoDB_20120810.RestoreTableFromBackup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreTableFromBackupResponse'
            Core.<$> (x Core..:? "TableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreTableFromBackupResponse' smart constructor.
data RestoreTableFromBackupResponse = RestoreTableFromBackupResponse'
  { -- | The description of the table created from an existing backup.
    tableDescription :: Core.Maybe Types.TableDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreTableFromBackupResponse' value with any optional fields omitted.
mkRestoreTableFromBackupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreTableFromBackupResponse
mkRestoreTableFromBackupResponse responseStatus =
  RestoreTableFromBackupResponse'
    { tableDescription = Core.Nothing,
      responseStatus
    }

-- | The description of the table created from an existing backup.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbrrsTableDescription :: Lens.Lens' RestoreTableFromBackupResponse (Core.Maybe Types.TableDescription)
rtfbrrsTableDescription = Lens.field @"tableDescription"
{-# DEPRECATED rtfbrrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbrrsResponseStatus :: Lens.Lens' RestoreTableFromBackupResponse Core.Int
rtfbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtfbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
