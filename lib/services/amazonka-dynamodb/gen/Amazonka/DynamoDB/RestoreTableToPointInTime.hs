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
-- Module      : Amazonka.DynamoDB.RestoreTableToPointInTime
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.DynamoDB.RestoreTableToPointInTime
  ( -- * Creating a Request
    RestoreTableToPointInTime (..),
    newRestoreTableToPointInTime,

    -- * Request Lenses
    restoreTableToPointInTime_billingModeOverride,
    restoreTableToPointInTime_globalSecondaryIndexOverride,
    restoreTableToPointInTime_localSecondaryIndexOverride,
    restoreTableToPointInTime_provisionedThroughputOverride,
    restoreTableToPointInTime_restoreDateTime,
    restoreTableToPointInTime_sSESpecificationOverride,
    restoreTableToPointInTime_sourceTableArn,
    restoreTableToPointInTime_sourceTableName,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreTableToPointInTime' smart constructor.
data RestoreTableToPointInTime = RestoreTableToPointInTime'
  { -- | The billing mode of the restored table.
    billingModeOverride :: Prelude.Maybe BillingMode,
    -- | List of global secondary indexes for the restored table. The indexes
    -- provided should match existing secondary indexes. You can choose to
    -- exclude some or all of the indexes at the time of restore.
    globalSecondaryIndexOverride :: Prelude.Maybe [GlobalSecondaryIndex],
    -- | List of local secondary indexes for the restored table. The indexes
    -- provided should match existing secondary indexes. You can choose to
    -- exclude some or all of the indexes at the time of restore.
    localSecondaryIndexOverride :: Prelude.Maybe [LocalSecondaryIndex],
    -- | Provisioned throughput settings for the restored table.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughput,
    -- | Time in the past to restore the table to.
    restoreDateTime :: Prelude.Maybe Data.POSIX,
    -- | The new server-side encryption settings for the restored table.
    sSESpecificationOverride :: Prelude.Maybe SSESpecification,
    -- | The DynamoDB table that will be restored. This value is an Amazon
    -- Resource Name (ARN).
    sourceTableArn :: Prelude.Maybe Prelude.Text,
    -- | Name of the source table that is being restored.
    sourceTableName :: Prelude.Maybe Prelude.Text,
    -- | Restore the table to the latest possible time.
    -- @LatestRestorableDateTime@ is typically 5 minutes before the current
    -- time.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | The name of the new table to which it must be restored to.
    targetTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableToPointInTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingModeOverride', 'restoreTableToPointInTime_billingModeOverride' - The billing mode of the restored table.
--
-- 'globalSecondaryIndexOverride', 'restoreTableToPointInTime_globalSecondaryIndexOverride' - List of global secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
--
-- 'localSecondaryIndexOverride', 'restoreTableToPointInTime_localSecondaryIndexOverride' - List of local secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
--
-- 'provisionedThroughputOverride', 'restoreTableToPointInTime_provisionedThroughputOverride' - Provisioned throughput settings for the restored table.
--
-- 'restoreDateTime', 'restoreTableToPointInTime_restoreDateTime' - Time in the past to restore the table to.
--
-- 'sSESpecificationOverride', 'restoreTableToPointInTime_sSESpecificationOverride' - The new server-side encryption settings for the restored table.
--
-- 'sourceTableArn', 'restoreTableToPointInTime_sourceTableArn' - The DynamoDB table that will be restored. This value is an Amazon
-- Resource Name (ARN).
--
-- 'sourceTableName', 'restoreTableToPointInTime_sourceTableName' - Name of the source table that is being restored.
--
-- 'useLatestRestorableTime', 'restoreTableToPointInTime_useLatestRestorableTime' - Restore the table to the latest possible time.
-- @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
--
-- 'targetTableName', 'restoreTableToPointInTime_targetTableName' - The name of the new table to which it must be restored to.
newRestoreTableToPointInTime ::
  -- | 'targetTableName'
  Prelude.Text ->
  RestoreTableToPointInTime
newRestoreTableToPointInTime pTargetTableName_ =
  RestoreTableToPointInTime'
    { billingModeOverride =
        Prelude.Nothing,
      globalSecondaryIndexOverride = Prelude.Nothing,
      localSecondaryIndexOverride = Prelude.Nothing,
      provisionedThroughputOverride = Prelude.Nothing,
      restoreDateTime = Prelude.Nothing,
      sSESpecificationOverride = Prelude.Nothing,
      sourceTableArn = Prelude.Nothing,
      sourceTableName = Prelude.Nothing,
      useLatestRestorableTime = Prelude.Nothing,
      targetTableName = pTargetTableName_
    }

-- | The billing mode of the restored table.
restoreTableToPointInTime_billingModeOverride :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe BillingMode)
restoreTableToPointInTime_billingModeOverride = Lens.lens (\RestoreTableToPointInTime' {billingModeOverride} -> billingModeOverride) (\s@RestoreTableToPointInTime' {} a -> s {billingModeOverride = a} :: RestoreTableToPointInTime)

-- | List of global secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
restoreTableToPointInTime_globalSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe [GlobalSecondaryIndex])
restoreTableToPointInTime_globalSecondaryIndexOverride = Lens.lens (\RestoreTableToPointInTime' {globalSecondaryIndexOverride} -> globalSecondaryIndexOverride) (\s@RestoreTableToPointInTime' {} a -> s {globalSecondaryIndexOverride = a} :: RestoreTableToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | List of local secondary indexes for the restored table. The indexes
-- provided should match existing secondary indexes. You can choose to
-- exclude some or all of the indexes at the time of restore.
restoreTableToPointInTime_localSecondaryIndexOverride :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe [LocalSecondaryIndex])
restoreTableToPointInTime_localSecondaryIndexOverride = Lens.lens (\RestoreTableToPointInTime' {localSecondaryIndexOverride} -> localSecondaryIndexOverride) (\s@RestoreTableToPointInTime' {} a -> s {localSecondaryIndexOverride = a} :: RestoreTableToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | Provisioned throughput settings for the restored table.
restoreTableToPointInTime_provisionedThroughputOverride :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe ProvisionedThroughput)
restoreTableToPointInTime_provisionedThroughputOverride = Lens.lens (\RestoreTableToPointInTime' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@RestoreTableToPointInTime' {} a -> s {provisionedThroughputOverride = a} :: RestoreTableToPointInTime)

-- | Time in the past to restore the table to.
restoreTableToPointInTime_restoreDateTime :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe Prelude.UTCTime)
restoreTableToPointInTime_restoreDateTime = Lens.lens (\RestoreTableToPointInTime' {restoreDateTime} -> restoreDateTime) (\s@RestoreTableToPointInTime' {} a -> s {restoreDateTime = a} :: RestoreTableToPointInTime) Prelude.. Lens.mapping Data._Time

-- | The new server-side encryption settings for the restored table.
restoreTableToPointInTime_sSESpecificationOverride :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe SSESpecification)
restoreTableToPointInTime_sSESpecificationOverride = Lens.lens (\RestoreTableToPointInTime' {sSESpecificationOverride} -> sSESpecificationOverride) (\s@RestoreTableToPointInTime' {} a -> s {sSESpecificationOverride = a} :: RestoreTableToPointInTime)

-- | The DynamoDB table that will be restored. This value is an Amazon
-- Resource Name (ARN).
restoreTableToPointInTime_sourceTableArn :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe Prelude.Text)
restoreTableToPointInTime_sourceTableArn = Lens.lens (\RestoreTableToPointInTime' {sourceTableArn} -> sourceTableArn) (\s@RestoreTableToPointInTime' {} a -> s {sourceTableArn = a} :: RestoreTableToPointInTime)

-- | Name of the source table that is being restored.
restoreTableToPointInTime_sourceTableName :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe Prelude.Text)
restoreTableToPointInTime_sourceTableName = Lens.lens (\RestoreTableToPointInTime' {sourceTableName} -> sourceTableName) (\s@RestoreTableToPointInTime' {} a -> s {sourceTableName = a} :: RestoreTableToPointInTime)

-- | Restore the table to the latest possible time.
-- @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
restoreTableToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreTableToPointInTime (Prelude.Maybe Prelude.Bool)
restoreTableToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreTableToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreTableToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreTableToPointInTime)

-- | The name of the new table to which it must be restored to.
restoreTableToPointInTime_targetTableName :: Lens.Lens' RestoreTableToPointInTime Prelude.Text
restoreTableToPointInTime_targetTableName = Lens.lens (\RestoreTableToPointInTime' {targetTableName} -> targetTableName) (\s@RestoreTableToPointInTime' {} a -> s {targetTableName = a} :: RestoreTableToPointInTime)

instance Core.AWSRequest RestoreTableToPointInTime where
  type
    AWSResponse RestoreTableToPointInTime =
      RestoreTableToPointInTimeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreTableToPointInTimeResponse'
            Prelude.<$> (x Data..?> "TableDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreTableToPointInTime where
  hashWithSalt _salt RestoreTableToPointInTime' {..} =
    _salt
      `Prelude.hashWithSalt` billingModeOverride
      `Prelude.hashWithSalt` globalSecondaryIndexOverride
      `Prelude.hashWithSalt` localSecondaryIndexOverride
      `Prelude.hashWithSalt` provisionedThroughputOverride
      `Prelude.hashWithSalt` restoreDateTime
      `Prelude.hashWithSalt` sSESpecificationOverride
      `Prelude.hashWithSalt` sourceTableArn
      `Prelude.hashWithSalt` sourceTableName
      `Prelude.hashWithSalt` useLatestRestorableTime
      `Prelude.hashWithSalt` targetTableName

instance Prelude.NFData RestoreTableToPointInTime where
  rnf RestoreTableToPointInTime' {..} =
    Prelude.rnf billingModeOverride
      `Prelude.seq` Prelude.rnf globalSecondaryIndexOverride
      `Prelude.seq` Prelude.rnf localSecondaryIndexOverride
      `Prelude.seq` Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf restoreDateTime
      `Prelude.seq` Prelude.rnf sSESpecificationOverride
      `Prelude.seq` Prelude.rnf sourceTableArn
      `Prelude.seq` Prelude.rnf sourceTableName
      `Prelude.seq` Prelude.rnf useLatestRestorableTime
      `Prelude.seq` Prelude.rnf targetTableName

instance Data.ToHeaders RestoreTableToPointInTime where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.RestoreTableToPointInTime" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreTableToPointInTime where
  toJSON RestoreTableToPointInTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingModeOverride" Data..=)
              Prelude.<$> billingModeOverride,
            ("GlobalSecondaryIndexOverride" Data..=)
              Prelude.<$> globalSecondaryIndexOverride,
            ("LocalSecondaryIndexOverride" Data..=)
              Prelude.<$> localSecondaryIndexOverride,
            ("ProvisionedThroughputOverride" Data..=)
              Prelude.<$> provisionedThroughputOverride,
            ("RestoreDateTime" Data..=)
              Prelude.<$> restoreDateTime,
            ("SSESpecificationOverride" Data..=)
              Prelude.<$> sSESpecificationOverride,
            ("SourceTableArn" Data..=)
              Prelude.<$> sourceTableArn,
            ("SourceTableName" Data..=)
              Prelude.<$> sourceTableName,
            ("UseLatestRestorableTime" Data..=)
              Prelude.<$> useLatestRestorableTime,
            Prelude.Just
              ("TargetTableName" Data..= targetTableName)
          ]
      )

instance Data.ToPath RestoreTableToPointInTime where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreTableToPointInTime where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreTableToPointInTimeResponse' smart constructor.
data RestoreTableToPointInTimeResponse = RestoreTableToPointInTimeResponse'
  { -- | Represents the properties of a table.
    tableDescription :: Prelude.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RestoreTableToPointInTimeResponse
newRestoreTableToPointInTimeResponse pHttpStatus_ =
  RestoreTableToPointInTimeResponse'
    { tableDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of a table.
restoreTableToPointInTimeResponse_tableDescription :: Lens.Lens' RestoreTableToPointInTimeResponse (Prelude.Maybe TableDescription)
restoreTableToPointInTimeResponse_tableDescription = Lens.lens (\RestoreTableToPointInTimeResponse' {tableDescription} -> tableDescription) (\s@RestoreTableToPointInTimeResponse' {} a -> s {tableDescription = a} :: RestoreTableToPointInTimeResponse)

-- | The response's http status code.
restoreTableToPointInTimeResponse_httpStatus :: Lens.Lens' RestoreTableToPointInTimeResponse Prelude.Int
restoreTableToPointInTimeResponse_httpStatus = Lens.lens (\RestoreTableToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@RestoreTableToPointInTimeResponse' {} a -> s {httpStatus = a} :: RestoreTableToPointInTimeResponse)

instance
  Prelude.NFData
    RestoreTableToPointInTimeResponse
  where
  rnf RestoreTableToPointInTimeResponse' {..} =
    Prelude.rnf tableDescription
      `Prelude.seq` Prelude.rnf httpStatus
