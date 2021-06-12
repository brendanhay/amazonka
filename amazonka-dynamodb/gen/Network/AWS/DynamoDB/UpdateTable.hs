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
-- Module      : Network.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the provisioned throughput settings, global secondary indexes,
-- or DynamoDB Streams settings for a given table.
--
-- You can only perform one of the following operations at once:
--
-- -   Modify the provisioned throughput settings of the table.
--
-- -   Enable or disable DynamoDB Streams on the table.
--
-- -   Remove a global secondary index from the table.
--
-- -   Create a new global secondary index on the table. After the index
--     begins backfilling, you can use @UpdateTable@ to perform other
--     operations.
--
-- @UpdateTable@ is an asynchronous operation; while it is executing, the
-- table status changes from @ACTIVE@ to @UPDATING@. While it is
-- @UPDATING@, you cannot issue another @UpdateTable@ request. When the
-- table returns to the @ACTIVE@ state, the @UpdateTable@ operation is
-- complete.
module Network.AWS.DynamoDB.UpdateTable
  ( -- * Creating a Request
    UpdateTable (..),
    newUpdateTable,

    -- * Request Lenses
    updateTable_streamSpecification,
    updateTable_sSESpecification,
    updateTable_billingMode,
    updateTable_attributeDefinitions,
    updateTable_globalSecondaryIndexUpdates,
    updateTable_provisionedThroughput,
    updateTable_replicaUpdates,
    updateTable_tableName,

    -- * Destructuring the Response
    UpdateTableResponse (..),
    newUpdateTableResponse,

    -- * Response Lenses
    updateTableResponse_tableDescription,
    updateTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateTable@ operation.
--
-- /See:/ 'newUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | Represents the DynamoDB Streams configuration for the table.
    --
    -- You receive a @ResourceInUseException@ if you try to enable a stream on
    -- a table that already has a stream, or if you try to disable a stream on
    -- a table that doesn\'t have a stream.
    streamSpecification :: Core.Maybe StreamSpecification,
    -- | The new server-side encryption settings for the specified table.
    sSESpecification :: Core.Maybe SSESpecification,
    -- | Controls how you are charged for read and write throughput and how you
    -- manage capacity. When switching from pay-per-request to provisioned
    -- capacity, initial provisioned capacity values must be set. The initial
    -- provisioned capacity values are estimated based on the consumed read and
    -- write capacity of your table and global secondary indexes over the past
    -- 30 minutes.
    --
    -- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
    --     workloads. @PROVISIONED@ sets the billing mode to
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
    --
    -- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
    --     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
    billingMode :: Core.Maybe BillingMode,
    -- | An array of attributes that describe the key schema for the table and
    -- indexes. If you are adding a new global secondary index to the table,
    -- @AttributeDefinitions@ must include the key element(s) of the new index.
    attributeDefinitions :: Core.Maybe [AttributeDefinition],
    -- | An array of one or more global secondary indexes for the table. For each
    -- index in the array, you can request one action:
    --
    -- -   @Create@ - add a new global secondary index to the table.
    --
    -- -   @Update@ - modify the provisioned throughput settings of an existing
    --     global secondary index.
    --
    -- -   @Delete@ - remove a global secondary index from the table.
    --
    -- You can create or delete only one global secondary index per
    -- @UpdateTable@ operation.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes>
    -- in the /Amazon DynamoDB Developer Guide/.
    globalSecondaryIndexUpdates :: Core.Maybe [GlobalSecondaryIndexUpdate],
    -- | The new provisioned throughput settings for the specified table or
    -- index.
    provisionedThroughput :: Core.Maybe ProvisionedThroughput,
    -- | A list of replica update actions (create, delete, or update) for the
    -- table.
    --
    -- This property only applies to
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
    -- of global tables.
    replicaUpdates :: Core.Maybe (Core.NonEmpty ReplicationGroupUpdate),
    -- | The name of the table to be updated.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamSpecification', 'updateTable_streamSpecification' - Represents the DynamoDB Streams configuration for the table.
--
-- You receive a @ResourceInUseException@ if you try to enable a stream on
-- a table that already has a stream, or if you try to disable a stream on
-- a table that doesn\'t have a stream.
--
-- 'sSESpecification', 'updateTable_sSESpecification' - The new server-side encryption settings for the specified table.
--
-- 'billingMode', 'updateTable_billingMode' - Controls how you are charged for read and write throughput and how you
-- manage capacity. When switching from pay-per-request to provisioned
-- capacity, initial provisioned capacity values must be set. The initial
-- provisioned capacity values are estimated based on the consumed read and
-- write capacity of your table and global secondary indexes over the past
-- 30 minutes.
--
-- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
--     workloads. @PROVISIONED@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
--
-- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
--
-- 'attributeDefinitions', 'updateTable_attributeDefinitions' - An array of attributes that describe the key schema for the table and
-- indexes. If you are adding a new global secondary index to the table,
-- @AttributeDefinitions@ must include the key element(s) of the new index.
--
-- 'globalSecondaryIndexUpdates', 'updateTable_globalSecondaryIndexUpdates' - An array of one or more global secondary indexes for the table. For each
-- index in the array, you can request one action:
--
-- -   @Create@ - add a new global secondary index to the table.
--
-- -   @Update@ - modify the provisioned throughput settings of an existing
--     global secondary index.
--
-- -   @Delete@ - remove a global secondary index from the table.
--
-- You can create or delete only one global secondary index per
-- @UpdateTable@ operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'provisionedThroughput', 'updateTable_provisionedThroughput' - The new provisioned throughput settings for the specified table or
-- index.
--
-- 'replicaUpdates', 'updateTable_replicaUpdates' - A list of replica update actions (create, delete, or update) for the
-- table.
--
-- This property only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
--
-- 'tableName', 'updateTable_tableName' - The name of the table to be updated.
newUpdateTable ::
  -- | 'tableName'
  Core.Text ->
  UpdateTable
newUpdateTable pTableName_ =
  UpdateTable'
    { streamSpecification = Core.Nothing,
      sSESpecification = Core.Nothing,
      billingMode = Core.Nothing,
      attributeDefinitions = Core.Nothing,
      globalSecondaryIndexUpdates = Core.Nothing,
      provisionedThroughput = Core.Nothing,
      replicaUpdates = Core.Nothing,
      tableName = pTableName_
    }

-- | Represents the DynamoDB Streams configuration for the table.
--
-- You receive a @ResourceInUseException@ if you try to enable a stream on
-- a table that already has a stream, or if you try to disable a stream on
-- a table that doesn\'t have a stream.
updateTable_streamSpecification :: Lens.Lens' UpdateTable (Core.Maybe StreamSpecification)
updateTable_streamSpecification = Lens.lens (\UpdateTable' {streamSpecification} -> streamSpecification) (\s@UpdateTable' {} a -> s {streamSpecification = a} :: UpdateTable)

-- | The new server-side encryption settings for the specified table.
updateTable_sSESpecification :: Lens.Lens' UpdateTable (Core.Maybe SSESpecification)
updateTable_sSESpecification = Lens.lens (\UpdateTable' {sSESpecification} -> sSESpecification) (\s@UpdateTable' {} a -> s {sSESpecification = a} :: UpdateTable)

-- | Controls how you are charged for read and write throughput and how you
-- manage capacity. When switching from pay-per-request to provisioned
-- capacity, initial provisioned capacity values must be set. The initial
-- provisioned capacity values are estimated based on the consumed read and
-- write capacity of your table and global secondary indexes over the past
-- 30 minutes.
--
-- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
--     workloads. @PROVISIONED@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
--
-- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
updateTable_billingMode :: Lens.Lens' UpdateTable (Core.Maybe BillingMode)
updateTable_billingMode = Lens.lens (\UpdateTable' {billingMode} -> billingMode) (\s@UpdateTable' {} a -> s {billingMode = a} :: UpdateTable)

-- | An array of attributes that describe the key schema for the table and
-- indexes. If you are adding a new global secondary index to the table,
-- @AttributeDefinitions@ must include the key element(s) of the new index.
updateTable_attributeDefinitions :: Lens.Lens' UpdateTable (Core.Maybe [AttributeDefinition])
updateTable_attributeDefinitions = Lens.lens (\UpdateTable' {attributeDefinitions} -> attributeDefinitions) (\s@UpdateTable' {} a -> s {attributeDefinitions = a} :: UpdateTable) Core.. Lens.mapping Lens._Coerce

-- | An array of one or more global secondary indexes for the table. For each
-- index in the array, you can request one action:
--
-- -   @Create@ - add a new global secondary index to the table.
--
-- -   @Update@ - modify the provisioned throughput settings of an existing
--     global secondary index.
--
-- -   @Delete@ - remove a global secondary index from the table.
--
-- You can create or delete only one global secondary index per
-- @UpdateTable@ operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes>
-- in the /Amazon DynamoDB Developer Guide/.
updateTable_globalSecondaryIndexUpdates :: Lens.Lens' UpdateTable (Core.Maybe [GlobalSecondaryIndexUpdate])
updateTable_globalSecondaryIndexUpdates = Lens.lens (\UpdateTable' {globalSecondaryIndexUpdates} -> globalSecondaryIndexUpdates) (\s@UpdateTable' {} a -> s {globalSecondaryIndexUpdates = a} :: UpdateTable) Core.. Lens.mapping Lens._Coerce

-- | The new provisioned throughput settings for the specified table or
-- index.
updateTable_provisionedThroughput :: Lens.Lens' UpdateTable (Core.Maybe ProvisionedThroughput)
updateTable_provisionedThroughput = Lens.lens (\UpdateTable' {provisionedThroughput} -> provisionedThroughput) (\s@UpdateTable' {} a -> s {provisionedThroughput = a} :: UpdateTable)

-- | A list of replica update actions (create, delete, or update) for the
-- table.
--
-- This property only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
updateTable_replicaUpdates :: Lens.Lens' UpdateTable (Core.Maybe (Core.NonEmpty ReplicationGroupUpdate))
updateTable_replicaUpdates = Lens.lens (\UpdateTable' {replicaUpdates} -> replicaUpdates) (\s@UpdateTable' {} a -> s {replicaUpdates = a} :: UpdateTable) Core.. Lens.mapping Lens._Coerce

-- | The name of the table to be updated.
updateTable_tableName :: Lens.Lens' UpdateTable Core.Text
updateTable_tableName = Lens.lens (\UpdateTable' {tableName} -> tableName) (\s@UpdateTable' {} a -> s {tableName = a} :: UpdateTable)

instance Core.AWSRequest UpdateTable where
  type AWSResponse UpdateTable = UpdateTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableResponse'
            Core.<$> (x Core..?> "TableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTable

instance Core.NFData UpdateTable

instance Core.ToHeaders UpdateTable where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DynamoDB_20120810.UpdateTable" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StreamSpecification" Core..=)
              Core.<$> streamSpecification,
            ("SSESpecification" Core..=)
              Core.<$> sSESpecification,
            ("BillingMode" Core..=) Core.<$> billingMode,
            ("AttributeDefinitions" Core..=)
              Core.<$> attributeDefinitions,
            ("GlobalSecondaryIndexUpdates" Core..=)
              Core.<$> globalSecondaryIndexUpdates,
            ("ProvisionedThroughput" Core..=)
              Core.<$> provisionedThroughput,
            ("ReplicaUpdates" Core..=) Core.<$> replicaUpdates,
            Core.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath UpdateTable where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTable where
  toQuery = Core.const Core.mempty

-- | Represents the output of an @UpdateTable@ operation.
--
-- /See:/ 'newUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { -- | Represents the properties of the table.
    tableDescription :: Core.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableDescription', 'updateTableResponse_tableDescription' - Represents the properties of the table.
--
-- 'httpStatus', 'updateTableResponse_httpStatus' - The response's http status code.
newUpdateTableResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTableResponse
newUpdateTableResponse pHttpStatus_ =
  UpdateTableResponse'
    { tableDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of the table.
updateTableResponse_tableDescription :: Lens.Lens' UpdateTableResponse (Core.Maybe TableDescription)
updateTableResponse_tableDescription = Lens.lens (\UpdateTableResponse' {tableDescription} -> tableDescription) (\s@UpdateTableResponse' {} a -> s {tableDescription = a} :: UpdateTableResponse)

-- | The response's http status code.
updateTableResponse_httpStatus :: Lens.Lens' UpdateTableResponse Core.Int
updateTableResponse_httpStatus = Lens.lens (\UpdateTableResponse' {httpStatus} -> httpStatus) (\s@UpdateTableResponse' {} a -> s {httpStatus = a} :: UpdateTableResponse)

instance Core.NFData UpdateTableResponse
