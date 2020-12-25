{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.RestoreTableToPointInTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified table to the specified point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ . You can restore your table to any point in time during the last 35 days. Any number of users can execute up to 4 concurrent restores (any type of restore) in a given account.
--
-- When you restore using point in time recovery, DynamoDB restores your table data to the state based on the selected date and time (day:hour:minute:second) to a new table.
-- Along with data, the following are also included on the new restored table using point in time recovery:
--
--     * Global secondary indexes (GSIs)
--
--
--     * Local secondary indexes (LSIs)
--
--
--     * Provisioned read and write capacity
--
--
--     * Encryption settings
-- /Important:/ All these settings come from the current settings of the source table at the time of restore.
--
--
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
--     * Point in time recovery settings
module Network.AWS.DynamoDB.RestoreTableToPointInTime
  ( -- * Creating a request
    RestoreTableToPointInTime (..),
    mkRestoreTableToPointInTime,

    -- ** Request lenses
    rttpitTargetTableName,
    rttpitBillingModeOverride,
    rttpitGlobalSecondaryIndexOverride,
    rttpitLocalSecondaryIndexOverride,
    rttpitProvisionedThroughputOverride,
    rttpitRestoreDateTime,
    rttpitSSESpecificationOverride,
    rttpitSourceTableArn,
    rttpitSourceTableName,
    rttpitUseLatestRestorableTime,

    -- * Destructuring the response
    RestoreTableToPointInTimeResponse (..),
    mkRestoreTableToPointInTimeResponse,

    -- ** Response lenses
    rttpitrrsTableDescription,
    rttpitrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreTableToPointInTime' smart constructor.
data RestoreTableToPointInTime = RestoreTableToPointInTime'
  { -- | The name of the new table to which it must be restored to.
    targetTableName :: Types.TargetTableName,
    -- | The billing mode of the restored table.
    billingModeOverride :: Core.Maybe Types.BillingMode,
    -- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    globalSecondaryIndexOverride :: Core.Maybe [Types.GlobalSecondaryIndex],
    -- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    localSecondaryIndexOverride :: Core.Maybe [Types.LocalSecondaryIndex],
    -- | Provisioned throughput settings for the restored table.
    provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughput,
    -- | Time in the past to restore the table to.
    restoreDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The new server-side encryption settings for the restored table.
    sSESpecificationOverride :: Core.Maybe Types.SSESpecification,
    -- | The DynamoDB table that will be restored. This value is an Amazon Resource Name (ARN).
    sourceTableArn :: Core.Maybe Types.TableArn,
    -- | Name of the source table that is being restored.
    sourceTableName :: Core.Maybe Types.SourceTableName,
    -- | Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
    useLatestRestorableTime :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreTableToPointInTime' value with any optional fields omitted.
mkRestoreTableToPointInTime ::
  -- | 'targetTableName'
  Types.TargetTableName ->
  RestoreTableToPointInTime
mkRestoreTableToPointInTime targetTableName =
  RestoreTableToPointInTime'
    { targetTableName,
      billingModeOverride = Core.Nothing,
      globalSecondaryIndexOverride = Core.Nothing,
      localSecondaryIndexOverride = Core.Nothing,
      provisionedThroughputOverride = Core.Nothing,
      restoreDateTime = Core.Nothing,
      sSESpecificationOverride = Core.Nothing,
      sourceTableArn = Core.Nothing,
      sourceTableName = Core.Nothing,
      useLatestRestorableTime = Core.Nothing
    }

-- | The name of the new table to which it must be restored to.
--
-- /Note:/ Consider using 'targetTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitTargetTableName :: Lens.Lens' RestoreTableToPointInTime Types.TargetTableName
rttpitTargetTableName = Lens.field @"targetTableName"
{-# DEPRECATED rttpitTargetTableName "Use generic-lens or generic-optics with 'targetTableName' instead." #-}

-- | The billing mode of the restored table.
--
-- /Note:/ Consider using 'billingModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitBillingModeOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Types.BillingMode)
rttpitBillingModeOverride = Lens.field @"billingModeOverride"
{-# DEPRECATED rttpitBillingModeOverride "Use generic-lens or generic-optics with 'billingModeOverride' instead." #-}

-- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'globalSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitGlobalSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe [Types.GlobalSecondaryIndex])
rttpitGlobalSecondaryIndexOverride = Lens.field @"globalSecondaryIndexOverride"
{-# DEPRECATED rttpitGlobalSecondaryIndexOverride "Use generic-lens or generic-optics with 'globalSecondaryIndexOverride' instead." #-}

-- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'localSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitLocalSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe [Types.LocalSecondaryIndex])
rttpitLocalSecondaryIndexOverride = Lens.field @"localSecondaryIndexOverride"
{-# DEPRECATED rttpitLocalSecondaryIndexOverride "Use generic-lens or generic-optics with 'localSecondaryIndexOverride' instead." #-}

-- | Provisioned throughput settings for the restored table.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitProvisionedThroughputOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Types.ProvisionedThroughput)
rttpitProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# DEPRECATED rttpitProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | Time in the past to restore the table to.
--
-- /Note:/ Consider using 'restoreDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitRestoreDateTime :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Core.NominalDiffTime)
rttpitRestoreDateTime = Lens.field @"restoreDateTime"
{-# DEPRECATED rttpitRestoreDateTime "Use generic-lens or generic-optics with 'restoreDateTime' instead." #-}

-- | The new server-side encryption settings for the restored table.
--
-- /Note:/ Consider using 'sSESpecificationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitSSESpecificationOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Types.SSESpecification)
rttpitSSESpecificationOverride = Lens.field @"sSESpecificationOverride"
{-# DEPRECATED rttpitSSESpecificationOverride "Use generic-lens or generic-optics with 'sSESpecificationOverride' instead." #-}

-- | The DynamoDB table that will be restored. This value is an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'sourceTableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitSourceTableArn :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Types.TableArn)
rttpitSourceTableArn = Lens.field @"sourceTableArn"
{-# DEPRECATED rttpitSourceTableArn "Use generic-lens or generic-optics with 'sourceTableArn' instead." #-}

-- | Name of the source table that is being restored.
--
-- /Note:/ Consider using 'sourceTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitSourceTableName :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Types.SourceTableName)
rttpitSourceTableName = Lens.field @"sourceTableName"
{-# DEPRECATED rttpitSourceTableName "Use generic-lens or generic-optics with 'sourceTableName' instead." #-}

-- | Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitUseLatestRestorableTime :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Core.Bool)
rttpitUseLatestRestorableTime = Lens.field @"useLatestRestorableTime"
{-# DEPRECATED rttpitUseLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead." #-}

instance Core.FromJSON RestoreTableToPointInTime where
  toJSON RestoreTableToPointInTime {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TargetTableName" Core..= targetTableName),
            ("BillingModeOverride" Core..=) Core.<$> billingModeOverride,
            ("GlobalSecondaryIndexOverride" Core..=)
              Core.<$> globalSecondaryIndexOverride,
            ("LocalSecondaryIndexOverride" Core..=)
              Core.<$> localSecondaryIndexOverride,
            ("ProvisionedThroughputOverride" Core..=)
              Core.<$> provisionedThroughputOverride,
            ("RestoreDateTime" Core..=) Core.<$> restoreDateTime,
            ("SSESpecificationOverride" Core..=)
              Core.<$> sSESpecificationOverride,
            ("SourceTableArn" Core..=) Core.<$> sourceTableArn,
            ("SourceTableName" Core..=) Core.<$> sourceTableName,
            ("UseLatestRestorableTime" Core..=)
              Core.<$> useLatestRestorableTime
          ]
      )

instance Core.AWSRequest RestoreTableToPointInTime where
  type
    Rs RestoreTableToPointInTime =
      RestoreTableToPointInTimeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DynamoDB_20120810.RestoreTableToPointInTime")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreTableToPointInTimeResponse'
            Core.<$> (x Core..:? "TableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreTableToPointInTimeResponse' smart constructor.
data RestoreTableToPointInTimeResponse = RestoreTableToPointInTimeResponse'
  { -- | Represents the properties of a table.
    tableDescription :: Core.Maybe Types.TableDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreTableToPointInTimeResponse' value with any optional fields omitted.
mkRestoreTableToPointInTimeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreTableToPointInTimeResponse
mkRestoreTableToPointInTimeResponse responseStatus =
  RestoreTableToPointInTimeResponse'
    { tableDescription =
        Core.Nothing,
      responseStatus
    }

-- | Represents the properties of a table.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitrrsTableDescription :: Lens.Lens' RestoreTableToPointInTimeResponse (Core.Maybe Types.TableDescription)
rttpitrrsTableDescription = Lens.field @"tableDescription"
{-# DEPRECATED rttpitrrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitrrsResponseStatus :: Lens.Lens' RestoreTableToPointInTimeResponse Core.Int
rttpitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rttpitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
