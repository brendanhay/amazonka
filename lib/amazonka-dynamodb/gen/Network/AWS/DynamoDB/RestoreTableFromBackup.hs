{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.DynamoDB.RestoreTableFromBackup
    (
    -- * Creating a request
      RestoreTableFromBackup (..)
    , mkRestoreTableFromBackup
    -- ** Request lenses
    , rtfbTargetTableName
    , rtfbBackupArn
    , rtfbBillingModeOverride
    , rtfbGlobalSecondaryIndexOverride
    , rtfbLocalSecondaryIndexOverride
    , rtfbProvisionedThroughputOverride
    , rtfbSSESpecificationOverride

    -- * Destructuring the response
    , RestoreTableFromBackupResponse (..)
    , mkRestoreTableFromBackupResponse
    -- ** Response lenses
    , rtfbrrsTableDescription
    , rtfbrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreTableFromBackup' smart constructor.
data RestoreTableFromBackup = RestoreTableFromBackup'
  { targetTableName :: Types.TargetTableName
    -- ^ The name of the new table to which the backup must be restored.
  , backupArn :: Types.BackupArn
    -- ^ The Amazon Resource Name (ARN) associated with the backup.
  , billingModeOverride :: Core.Maybe Types.BillingMode
    -- ^ The billing mode of the restored table.
  , globalSecondaryIndexOverride :: Core.Maybe [Types.GlobalSecondaryIndex]
    -- ^ List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
  , localSecondaryIndexOverride :: Core.Maybe [Types.LocalSecondaryIndex]
    -- ^ List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
  , provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughput
    -- ^ Provisioned throughput settings for the restored table.
  , sSESpecificationOverride :: Core.Maybe Types.SSESpecification
    -- ^ The new server-side encryption settings for the restored table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreTableFromBackup' value with any optional fields omitted.
mkRestoreTableFromBackup
    :: Types.TargetTableName -- ^ 'targetTableName'
    -> Types.BackupArn -- ^ 'backupArn'
    -> RestoreTableFromBackup
mkRestoreTableFromBackup targetTableName backupArn
  = RestoreTableFromBackup'{targetTableName, backupArn,
                            billingModeOverride = Core.Nothing,
                            globalSecondaryIndexOverride = Core.Nothing,
                            localSecondaryIndexOverride = Core.Nothing,
                            provisionedThroughputOverride = Core.Nothing,
                            sSESpecificationOverride = Core.Nothing}

-- | The name of the new table to which the backup must be restored.
--
-- /Note:/ Consider using 'targetTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbTargetTableName :: Lens.Lens' RestoreTableFromBackup Types.TargetTableName
rtfbTargetTableName = Lens.field @"targetTableName"
{-# INLINEABLE rtfbTargetTableName #-}
{-# DEPRECATED targetTableName "Use generic-lens or generic-optics with 'targetTableName' instead"  #-}

-- | The Amazon Resource Name (ARN) associated with the backup.
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbBackupArn :: Lens.Lens' RestoreTableFromBackup Types.BackupArn
rtfbBackupArn = Lens.field @"backupArn"
{-# INLINEABLE rtfbBackupArn #-}
{-# DEPRECATED backupArn "Use generic-lens or generic-optics with 'backupArn' instead"  #-}

-- | The billing mode of the restored table.
--
-- /Note:/ Consider using 'billingModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbBillingModeOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe Types.BillingMode)
rtfbBillingModeOverride = Lens.field @"billingModeOverride"
{-# INLINEABLE rtfbBillingModeOverride #-}
{-# DEPRECATED billingModeOverride "Use generic-lens or generic-optics with 'billingModeOverride' instead"  #-}

-- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'globalSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbGlobalSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe [Types.GlobalSecondaryIndex])
rtfbGlobalSecondaryIndexOverride = Lens.field @"globalSecondaryIndexOverride"
{-# INLINEABLE rtfbGlobalSecondaryIndexOverride #-}
{-# DEPRECATED globalSecondaryIndexOverride "Use generic-lens or generic-optics with 'globalSecondaryIndexOverride' instead"  #-}

-- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'localSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbLocalSecondaryIndexOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe [Types.LocalSecondaryIndex])
rtfbLocalSecondaryIndexOverride = Lens.field @"localSecondaryIndexOverride"
{-# INLINEABLE rtfbLocalSecondaryIndexOverride #-}
{-# DEPRECATED localSecondaryIndexOverride "Use generic-lens or generic-optics with 'localSecondaryIndexOverride' instead"  #-}

-- | Provisioned throughput settings for the restored table.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbProvisionedThroughputOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe Types.ProvisionedThroughput)
rtfbProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# INLINEABLE rtfbProvisionedThroughputOverride #-}
{-# DEPRECATED provisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead"  #-}

-- | The new server-side encryption settings for the restored table.
--
-- /Note:/ Consider using 'sSESpecificationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbSSESpecificationOverride :: Lens.Lens' RestoreTableFromBackup (Core.Maybe Types.SSESpecification)
rtfbSSESpecificationOverride = Lens.field @"sSESpecificationOverride"
{-# INLINEABLE rtfbSSESpecificationOverride #-}
{-# DEPRECATED sSESpecificationOverride "Use generic-lens or generic-optics with 'sSESpecificationOverride' instead"  #-}

instance Core.ToQuery RestoreTableFromBackup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RestoreTableFromBackup where
        toHeaders RestoreTableFromBackup{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.RestoreTableFromBackup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON RestoreTableFromBackup where
        toJSON RestoreTableFromBackup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TargetTableName" Core..= targetTableName),
                  Core.Just ("BackupArn" Core..= backupArn),
                  ("BillingModeOverride" Core..=) Core.<$> billingModeOverride,
                  ("GlobalSecondaryIndexOverride" Core..=) Core.<$>
                    globalSecondaryIndexOverride,
                  ("LocalSecondaryIndexOverride" Core..=) Core.<$>
                    localSecondaryIndexOverride,
                  ("ProvisionedThroughputOverride" Core..=) Core.<$>
                    provisionedThroughputOverride,
                  ("SSESpecificationOverride" Core..=) Core.<$>
                    sSESpecificationOverride])

instance Core.AWSRequest RestoreTableFromBackup where
        type Rs RestoreTableFromBackup = RestoreTableFromBackupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RestoreTableFromBackupResponse' Core.<$>
                   (x Core..:? "TableDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreTableFromBackupResponse' smart constructor.
data RestoreTableFromBackupResponse = RestoreTableFromBackupResponse'
  { tableDescription :: Core.Maybe Types.TableDescription
    -- ^ The description of the table created from an existing backup.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreTableFromBackupResponse' value with any optional fields omitted.
mkRestoreTableFromBackupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreTableFromBackupResponse
mkRestoreTableFromBackupResponse responseStatus
  = RestoreTableFromBackupResponse'{tableDescription = Core.Nothing,
                                    responseStatus}

-- | The description of the table created from an existing backup.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbrrsTableDescription :: Lens.Lens' RestoreTableFromBackupResponse (Core.Maybe Types.TableDescription)
rtfbrrsTableDescription = Lens.field @"tableDescription"
{-# INLINEABLE rtfbrrsTableDescription #-}
{-# DEPRECATED tableDescription "Use generic-lens or generic-optics with 'tableDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbrrsResponseStatus :: Lens.Lens' RestoreTableFromBackupResponse Core.Int
rtfbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtfbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
