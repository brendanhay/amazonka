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
-- Module      : Network.AWS.DynamoDB.RestoreTableToPointInTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified table to the specified point in time within
-- @EarliestRestorableDateTime@ and @LatestRestorableDateTime@. You can
-- restore your table to any point in time during the last 35 days. Any
-- number of users can execute up to 4 concurrent restores (any type of
-- restore) in a given account.
--
-- When you restore using point in time recovery, DynamoDB restores your
-- table data to the state based on the selected date and time
-- (day:hour:minute:second) to a new table.
--
-- Along with data, the following are also included on the new restored
-- table using point in time recovery:
--
-- -   Global secondary indexes (GSIs)
--
-- -   Local secondary indexes (LSIs)
--
-- -   Provisioned read and write capacity
--
-- -   Encryption settings
--
--     All these settings come from the current settings of the source
--     table at the time of restore.
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
--
-- -   Point in time recovery settings
module Network.AWS.DynamoDB.RestoreTableToPointInTime
  ( -- * Creating a Request
    RestoreTableToPointInTime (..),
    newRestoreTableToPointInTime,

    -- * Request Lenses
    restoreTableToPointInTime_sourceTableName,
    restoreTableToPointInTime_restoreDateTime,
    restoreTableToPointInTime_provisionedThroughputOverride,
    restoreTableToPointInTime_globalSecondaryIndexOverride,
    restoreTableToPointInTime_billingModeOverride,
    restoreTableToPointInTime_sSESpecificationOverride,
    restoreTableToPointInTime_sourceTableArn,
    restoreTableToPointInTime_localSecondaryIndexOverride,
    restoreTableToPointInTime_useLatestRestorableTime,
    restoreTableToPointInTime_targetTableName,

    -- * Destructuring the Response
    RestoreTableToPointInTimeResponse (..),
    newRestoreTableToPointInTimeResponse,

    -- * Response Lenses
    restoreTableToPointInTimeResponse_tableDescription,
    restoreTableToPointInTimeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreTableToPointInTime' smart constructor.
data RestoreTableToPointInTime = RestoreTableToPointInTime'
  { -- | Name of the source table that is being restored.
    sourceTableName :: Core.Maybe Core.Text,
    -- | Time in the past to restore the table to.
    restoreDateTime :: Core.Maybe Core.POSIX,
    -- | Provisioned throughput settings for the restored table.
    provisionedThroughputOverride :: Core.Maybe ProvisionedThroughput,
    -- | List of global secondary indexes for the restored table. The indexes
    -- provided should match existing secondary indexes. You can choose to
    -- exclude some or all of the indexes at the time of restore.
    globalSecondaryIndexOverride :: Core.Maybe [GlobalSecondaryIndex],
    -- | The billing mode of the restored table.
    billingModeOverride :: Core.Maybe BillingMode,
    -- | The new server-side encryption settings for the restored table.
    sSESpecificationOverride :: Core.Maybe SSESpecification,
    -- | The DynamoDB table that will be restored. This value is an Amazon
    -- Resource Name (ARN).
    sourceTableArn :: Core.Maybe Core.Text,
    -- | List of local secondary indexes for the restored table. The indexes
    -- provided should match existing secondary indexes. You can choose to
    -- exclude some or all of the indexes at the time of restore.
    localSecondaryIndexOverride :: Core.Maybe [LocalSecondaryIndex],
    -- | Restore the table to the latest possible time.
    -- @LatestRestorableDateTime@ is typically 5 minutes before the current
    -- time.
    useLatestRestorableTime :: Core.Maybe Core.Bool,
    -- | The name of the new table to which it must be restored to.
    targetTableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreTableToPointInTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceTableName', 'restoreTableToPointInTime_sourceTableName' - Name of the source table that is being restored.
--
-- 'restoreDateTime', 'restoreTableToPointInTime_restoreDateTime' - Time in the past to restore the table to.
--
-- 'provisionedThroughputOverride', 'restoreTableToPointInTime_provisionedThroughputOverride' - Provisioned throughput settings for the restored table.
--
-- 'globalSecondaryIndexOverride', 'restoreTableToPointInTime_globalSecondaryIndexOverride' - List of global secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
--
-- 'billingModeOverride', 'restoreTableToPointInTime_billingModeOverride' - The billing mode of the restored table.
--
-- 'sSESpecificationOverride', 'restoreTableToPointInTime_sSESpecificationOverride' - The new server-side encryption settings for the restored table.
--
-- 'sourceTableArn', 'restoreTableToPointInTime_sourceTableArn' - The DynamoDB table that will be restored. This value is an Amazon
-- Resource Name (ARN).
--
-- 'localSecondaryIndexOverride', 'restoreTableToPointInTime_localSecondaryIndexOverride' - List of local secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
--
-- 'useLatestRestorableTime', 'restoreTableToPointInTime_useLatestRestorableTime' - Restore the table to the latest possible time.
-- @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
--
-- 'targetTableName', 'restoreTableToPointInTime_targetTableName' - The name of the new table to which it must be restored to.
newRestoreTableToPointInTime ::
  -- | 'targetTableName'
  Core.Text ->
  RestoreTableToPointInTime
newRestoreTableToPointInTime pTargetTableName_ =
  RestoreTableToPointInTime'
    { sourceTableName =
        Core.Nothing,
      restoreDateTime = Core.Nothing,
      provisionedThroughputOverride = Core.Nothing,
      globalSecondaryIndexOverride = Core.Nothing,
      billingModeOverride = Core.Nothing,
      sSESpecificationOverride = Core.Nothing,
      sourceTableArn = Core.Nothing,
      localSecondaryIndexOverride = Core.Nothing,
      useLatestRestorableTime = Core.Nothing,
      targetTableName = pTargetTableName_
    }

-- | Name of the source table that is being restored.
restoreTableToPointInTime_sourceTableName :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Core.Text)
restoreTableToPointInTime_sourceTableName = Lens.lens (\RestoreTableToPointInTime' {sourceTableName} -> sourceTableName) (\s@RestoreTableToPointInTime' {} a -> s {sourceTableName = a} :: RestoreTableToPointInTime)

-- | Time in the past to restore the table to.
restoreTableToPointInTime_restoreDateTime :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Core.UTCTime)
restoreTableToPointInTime_restoreDateTime = Lens.lens (\RestoreTableToPointInTime' {restoreDateTime} -> restoreDateTime) (\s@RestoreTableToPointInTime' {} a -> s {restoreDateTime = a} :: RestoreTableToPointInTime) Core.. Lens.mapping Core._Time

-- | Provisioned throughput settings for the restored table.
restoreTableToPointInTime_provisionedThroughputOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe ProvisionedThroughput)
restoreTableToPointInTime_provisionedThroughputOverride = Lens.lens (\RestoreTableToPointInTime' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@RestoreTableToPointInTime' {} a -> s {provisionedThroughputOverride = a} :: RestoreTableToPointInTime)

-- | List of global secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
restoreTableToPointInTime_globalSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe [GlobalSecondaryIndex])
restoreTableToPointInTime_globalSecondaryIndexOverride = Lens.lens (\RestoreTableToPointInTime' {globalSecondaryIndexOverride} -> globalSecondaryIndexOverride) (\s@RestoreTableToPointInTime' {} a -> s {globalSecondaryIndexOverride = a} :: RestoreTableToPointInTime) Core.. Lens.mapping Lens._Coerce

-- | The billing mode of the restored table.
restoreTableToPointInTime_billingModeOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe BillingMode)
restoreTableToPointInTime_billingModeOverride = Lens.lens (\RestoreTableToPointInTime' {billingModeOverride} -> billingModeOverride) (\s@RestoreTableToPointInTime' {} a -> s {billingModeOverride = a} :: RestoreTableToPointInTime)

-- | The new server-side encryption settings for the restored table.
restoreTableToPointInTime_sSESpecificationOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe SSESpecification)
restoreTableToPointInTime_sSESpecificationOverride = Lens.lens (\RestoreTableToPointInTime' {sSESpecificationOverride} -> sSESpecificationOverride) (\s@RestoreTableToPointInTime' {} a -> s {sSESpecificationOverride = a} :: RestoreTableToPointInTime)

-- | The DynamoDB table that will be restored. This value is an Amazon
-- Resource Name (ARN).
restoreTableToPointInTime_sourceTableArn :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Core.Text)
restoreTableToPointInTime_sourceTableArn = Lens.lens (\RestoreTableToPointInTime' {sourceTableArn} -> sourceTableArn) (\s@RestoreTableToPointInTime' {} a -> s {sourceTableArn = a} :: RestoreTableToPointInTime)

-- | List of local secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
restoreTableToPointInTime_localSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe [LocalSecondaryIndex])
restoreTableToPointInTime_localSecondaryIndexOverride = Lens.lens (\RestoreTableToPointInTime' {localSecondaryIndexOverride} -> localSecondaryIndexOverride) (\s@RestoreTableToPointInTime' {} a -> s {localSecondaryIndexOverride = a} :: RestoreTableToPointInTime) Core.. Lens.mapping Lens._Coerce

-- | Restore the table to the latest possible time.
-- @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
restoreTableToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreTableToPointInTime (Core.Maybe Core.Bool)
restoreTableToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreTableToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreTableToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreTableToPointInTime)

-- | The name of the new table to which it must be restored to.
restoreTableToPointInTime_targetTableName :: Lens.Lens' RestoreTableToPointInTime Core.Text
restoreTableToPointInTime_targetTableName = Lens.lens (\RestoreTableToPointInTime' {targetTableName} -> targetTableName) (\s@RestoreTableToPointInTime' {} a -> s {targetTableName = a} :: RestoreTableToPointInTime)

instance Core.AWSRequest RestoreTableToPointInTime where
  type
    AWSResponse RestoreTableToPointInTime =
      RestoreTableToPointInTimeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreTableToPointInTimeResponse'
            Core.<$> (x Core..?> "TableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RestoreTableToPointInTime

instance Core.NFData RestoreTableToPointInTime

instance Core.ToHeaders RestoreTableToPointInTime where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.RestoreTableToPointInTime" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RestoreTableToPointInTime where
  toJSON RestoreTableToPointInTime' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SourceTableName" Core..=)
              Core.<$> sourceTableName,
            ("RestoreDateTime" Core..=) Core.<$> restoreDateTime,
            ("ProvisionedThroughputOverride" Core..=)
              Core.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexOverride" Core..=)
              Core.<$> globalSecondaryIndexOverride,
            ("BillingModeOverride" Core..=)
              Core.<$> billingModeOverride,
            ("SSESpecificationOverride" Core..=)
              Core.<$> sSESpecificationOverride,
            ("SourceTableArn" Core..=) Core.<$> sourceTableArn,
            ("LocalSecondaryIndexOverride" Core..=)
              Core.<$> localSecondaryIndexOverride,
            ("UseLatestRestorableTime" Core..=)
              Core.<$> useLatestRestorableTime,
            Core.Just
              ("TargetTableName" Core..= targetTableName)
          ]
      )

instance Core.ToPath RestoreTableToPointInTime where
  toPath = Core.const "/"

instance Core.ToQuery RestoreTableToPointInTime where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRestoreTableToPointInTimeResponse' smart constructor.
data RestoreTableToPointInTimeResponse = RestoreTableToPointInTimeResponse'
  { -- | Represents the properties of a table.
    tableDescription :: Core.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreTableToPointInTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableDescription', 'restoreTableToPointInTimeResponse_tableDescription' - Represents the properties of a table.
--
-- 'httpStatus', 'restoreTableToPointInTimeResponse_httpStatus' - The response's http status code.
newRestoreTableToPointInTimeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RestoreTableToPointInTimeResponse
newRestoreTableToPointInTimeResponse pHttpStatus_ =
  RestoreTableToPointInTimeResponse'
    { tableDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of a table.
restoreTableToPointInTimeResponse_tableDescription :: Lens.Lens' RestoreTableToPointInTimeResponse (Core.Maybe TableDescription)
restoreTableToPointInTimeResponse_tableDescription = Lens.lens (\RestoreTableToPointInTimeResponse' {tableDescription} -> tableDescription) (\s@RestoreTableToPointInTimeResponse' {} a -> s {tableDescription = a} :: RestoreTableToPointInTimeResponse)

-- | The response's http status code.
restoreTableToPointInTimeResponse_httpStatus :: Lens.Lens' RestoreTableToPointInTimeResponse Core.Int
restoreTableToPointInTimeResponse_httpStatus = Lens.lens (\RestoreTableToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@RestoreTableToPointInTimeResponse' {} a -> s {httpStatus = a} :: RestoreTableToPointInTimeResponse)

instance
  Core.NFData
    RestoreTableToPointInTimeResponse
