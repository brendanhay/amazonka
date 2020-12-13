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
    rttpitBillingModeOverride,
    rttpitUseLatestRestorableTime,
    rttpitGlobalSecondaryIndexOverride,
    rttpitProvisionedThroughputOverride,
    rttpitSourceTableARN,
    rttpitSSESpecificationOverride,
    rttpitSourceTableName,
    rttpitLocalSecondaryIndexOverride,
    rttpitTargetTableName,
    rttpitRestoreDateTime,

    -- * Destructuring the response
    RestoreTableToPointInTimeResponse (..),
    mkRestoreTableToPointInTimeResponse,

    -- ** Response lenses
    rttpitrsTableDescription,
    rttpitrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreTableToPointInTime' smart constructor.
data RestoreTableToPointInTime = RestoreTableToPointInTime'
  { -- | The billing mode of the restored table.
    billingModeOverride :: Lude.Maybe BillingMode,
    -- | Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
    useLatestRestorableTime :: Lude.Maybe Lude.Bool,
    -- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    globalSecondaryIndexOverride :: Lude.Maybe [GlobalSecondaryIndex],
    -- | Provisioned throughput settings for the restored table.
    provisionedThroughputOverride :: Lude.Maybe ProvisionedThroughput,
    -- | The DynamoDB table that will be restored. This value is an Amazon Resource Name (ARN).
    sourceTableARN :: Lude.Maybe Lude.Text,
    -- | The new server-side encryption settings for the restored table.
    sSESpecificationOverride :: Lude.Maybe SSESpecification,
    -- | Name of the source table that is being restored.
    sourceTableName :: Lude.Maybe Lude.Text,
    -- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
    localSecondaryIndexOverride :: Lude.Maybe [LocalSecondaryIndex],
    -- | The name of the new table to which it must be restored to.
    targetTableName :: Lude.Text,
    -- | Time in the past to restore the table to.
    restoreDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreTableToPointInTime' with the minimum fields required to make a request.
--
-- * 'billingModeOverride' - The billing mode of the restored table.
-- * 'useLatestRestorableTime' - Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
-- * 'globalSecondaryIndexOverride' - List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
-- * 'provisionedThroughputOverride' - Provisioned throughput settings for the restored table.
-- * 'sourceTableARN' - The DynamoDB table that will be restored. This value is an Amazon Resource Name (ARN).
-- * 'sSESpecificationOverride' - The new server-side encryption settings for the restored table.
-- * 'sourceTableName' - Name of the source table that is being restored.
-- * 'localSecondaryIndexOverride' - List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
-- * 'targetTableName' - The name of the new table to which it must be restored to.
-- * 'restoreDateTime' - Time in the past to restore the table to.
mkRestoreTableToPointInTime ::
  -- | 'targetTableName'
  Lude.Text ->
  RestoreTableToPointInTime
mkRestoreTableToPointInTime pTargetTableName_ =
  RestoreTableToPointInTime'
    { billingModeOverride = Lude.Nothing,
      useLatestRestorableTime = Lude.Nothing,
      globalSecondaryIndexOverride = Lude.Nothing,
      provisionedThroughputOverride = Lude.Nothing,
      sourceTableARN = Lude.Nothing,
      sSESpecificationOverride = Lude.Nothing,
      sourceTableName = Lude.Nothing,
      localSecondaryIndexOverride = Lude.Nothing,
      targetTableName = pTargetTableName_,
      restoreDateTime = Lude.Nothing
    }

-- | The billing mode of the restored table.
--
-- /Note:/ Consider using 'billingModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitBillingModeOverride :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe BillingMode)
rttpitBillingModeOverride = Lens.lens (billingModeOverride :: RestoreTableToPointInTime -> Lude.Maybe BillingMode) (\s a -> s {billingModeOverride = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitBillingModeOverride "Use generic-lens or generic-optics with 'billingModeOverride' instead." #-}

-- | Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitUseLatestRestorableTime :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe Lude.Bool)
rttpitUseLatestRestorableTime = Lens.lens (useLatestRestorableTime :: RestoreTableToPointInTime -> Lude.Maybe Lude.Bool) (\s a -> s {useLatestRestorableTime = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitUseLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead." #-}

-- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'globalSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitGlobalSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe [GlobalSecondaryIndex])
rttpitGlobalSecondaryIndexOverride = Lens.lens (globalSecondaryIndexOverride :: RestoreTableToPointInTime -> Lude.Maybe [GlobalSecondaryIndex]) (\s a -> s {globalSecondaryIndexOverride = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitGlobalSecondaryIndexOverride "Use generic-lens or generic-optics with 'globalSecondaryIndexOverride' instead." #-}

-- | Provisioned throughput settings for the restored table.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitProvisionedThroughputOverride :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe ProvisionedThroughput)
rttpitProvisionedThroughputOverride = Lens.lens (provisionedThroughputOverride :: RestoreTableToPointInTime -> Lude.Maybe ProvisionedThroughput) (\s a -> s {provisionedThroughputOverride = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | The DynamoDB table that will be restored. This value is an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'sourceTableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitSourceTableARN :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe Lude.Text)
rttpitSourceTableARN = Lens.lens (sourceTableARN :: RestoreTableToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {sourceTableARN = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitSourceTableARN "Use generic-lens or generic-optics with 'sourceTableARN' instead." #-}

-- | The new server-side encryption settings for the restored table.
--
-- /Note:/ Consider using 'sSESpecificationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitSSESpecificationOverride :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe SSESpecification)
rttpitSSESpecificationOverride = Lens.lens (sSESpecificationOverride :: RestoreTableToPointInTime -> Lude.Maybe SSESpecification) (\s a -> s {sSESpecificationOverride = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitSSESpecificationOverride "Use generic-lens or generic-optics with 'sSESpecificationOverride' instead." #-}

-- | Name of the source table that is being restored.
--
-- /Note:/ Consider using 'sourceTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitSourceTableName :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe Lude.Text)
rttpitSourceTableName = Lens.lens (sourceTableName :: RestoreTableToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {sourceTableName = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitSourceTableName "Use generic-lens or generic-optics with 'sourceTableName' instead." #-}

-- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- /Note:/ Consider using 'localSecondaryIndexOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitLocalSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe [LocalSecondaryIndex])
rttpitLocalSecondaryIndexOverride = Lens.lens (localSecondaryIndexOverride :: RestoreTableToPointInTime -> Lude.Maybe [LocalSecondaryIndex]) (\s a -> s {localSecondaryIndexOverride = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitLocalSecondaryIndexOverride "Use generic-lens or generic-optics with 'localSecondaryIndexOverride' instead." #-}

-- | The name of the new table to which it must be restored to.
--
-- /Note:/ Consider using 'targetTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitTargetTableName :: Lens.Lens' RestoreTableToPointInTime Lude.Text
rttpitTargetTableName = Lens.lens (targetTableName :: RestoreTableToPointInTime -> Lude.Text) (\s a -> s {targetTableName = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitTargetTableName "Use generic-lens or generic-optics with 'targetTableName' instead." #-}

-- | Time in the past to restore the table to.
--
-- /Note:/ Consider using 'restoreDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitRestoreDateTime :: Lens.Lens' RestoreTableToPointInTime (Lude.Maybe Lude.Timestamp)
rttpitRestoreDateTime = Lens.lens (restoreDateTime :: RestoreTableToPointInTime -> Lude.Maybe Lude.Timestamp) (\s a -> s {restoreDateTime = a} :: RestoreTableToPointInTime)
{-# DEPRECATED rttpitRestoreDateTime "Use generic-lens or generic-optics with 'restoreDateTime' instead." #-}

instance Lude.AWSRequest RestoreTableToPointInTime where
  type
    Rs RestoreTableToPointInTime =
      RestoreTableToPointInTimeResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          RestoreTableToPointInTimeResponse'
            Lude.<$> (x Lude..?> "TableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreTableToPointInTime where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.RestoreTableToPointInTime" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreTableToPointInTime where
  toJSON RestoreTableToPointInTime' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BillingModeOverride" Lude..=) Lude.<$> billingModeOverride,
            ("UseLatestRestorableTime" Lude..=)
              Lude.<$> useLatestRestorableTime,
            ("GlobalSecondaryIndexOverride" Lude..=)
              Lude.<$> globalSecondaryIndexOverride,
            ("ProvisionedThroughputOverride" Lude..=)
              Lude.<$> provisionedThroughputOverride,
            ("SourceTableArn" Lude..=) Lude.<$> sourceTableARN,
            ("SSESpecificationOverride" Lude..=)
              Lude.<$> sSESpecificationOverride,
            ("SourceTableName" Lude..=) Lude.<$> sourceTableName,
            ("LocalSecondaryIndexOverride" Lude..=)
              Lude.<$> localSecondaryIndexOverride,
            Lude.Just ("TargetTableName" Lude..= targetTableName),
            ("RestoreDateTime" Lude..=) Lude.<$> restoreDateTime
          ]
      )

instance Lude.ToPath RestoreTableToPointInTime where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreTableToPointInTime where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRestoreTableToPointInTimeResponse' smart constructor.
data RestoreTableToPointInTimeResponse = RestoreTableToPointInTimeResponse'
  { -- | Represents the properties of a table.
    tableDescription :: Lude.Maybe TableDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreTableToPointInTimeResponse' with the minimum fields required to make a request.
--
-- * 'tableDescription' - Represents the properties of a table.
-- * 'responseStatus' - The response status code.
mkRestoreTableToPointInTimeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreTableToPointInTimeResponse
mkRestoreTableToPointInTimeResponse pResponseStatus_ =
  RestoreTableToPointInTimeResponse'
    { tableDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the properties of a table.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitrsTableDescription :: Lens.Lens' RestoreTableToPointInTimeResponse (Lude.Maybe TableDescription)
rttpitrsTableDescription = Lens.lens (tableDescription :: RestoreTableToPointInTimeResponse -> Lude.Maybe TableDescription) (\s a -> s {tableDescription = a} :: RestoreTableToPointInTimeResponse)
{-# DEPRECATED rttpitrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rttpitrsResponseStatus :: Lens.Lens' RestoreTableToPointInTimeResponse Lude.Int
rttpitrsResponseStatus = Lens.lens (responseStatus :: RestoreTableToPointInTimeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreTableToPointInTimeResponse)
{-# DEPRECATED rttpitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
