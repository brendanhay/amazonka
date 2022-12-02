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
-- Module      : Amazonka.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.DynamoDB.UpdateTable
  ( -- * Creating a Request
    UpdateTable (..),
    newUpdateTable,

    -- * Request Lenses
    updateTable_globalSecondaryIndexUpdates,
    updateTable_replicaUpdates,
    updateTable_billingMode,
    updateTable_provisionedThroughput,
    updateTable_sSESpecification,
    updateTable_tableClass,
    updateTable_streamSpecification,
    updateTable_attributeDefinitions,
    updateTable_tableName,

    -- * Destructuring the Response
    UpdateTableResponse (..),
    newUpdateTableResponse,

    -- * Response Lenses
    updateTableResponse_tableDescription,
    updateTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of an @UpdateTable@ operation.
--
-- /See:/ 'newUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | An array of one or more global secondary indexes for the table. For each
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
    globalSecondaryIndexUpdates :: Prelude.Maybe [GlobalSecondaryIndexUpdate],
    -- | A list of replica update actions (create, delete, or update) for the
    -- table.
    --
    -- This property only applies to
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
    -- of global tables.
    replicaUpdates :: Prelude.Maybe (Prelude.NonEmpty ReplicationGroupUpdate),
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
    billingMode :: Prelude.Maybe BillingMode,
    -- | The new provisioned throughput settings for the specified table or
    -- index.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
    -- | The new server-side encryption settings for the specified table.
    sSESpecification :: Prelude.Maybe SSESpecification,
    -- | The table class of the table to be updated. Valid values are @STANDARD@
    -- and @STANDARD_INFREQUENT_ACCESS@.
    tableClass :: Prelude.Maybe TableClass,
    -- | Represents the DynamoDB Streams configuration for the table.
    --
    -- You receive a @ResourceInUseException@ if you try to enable a stream on
    -- a table that already has a stream, or if you try to disable a stream on
    -- a table that doesn\'t have a stream.
    streamSpecification :: Prelude.Maybe StreamSpecification,
    -- | An array of attributes that describe the key schema for the table and
    -- indexes. If you are adding a new global secondary index to the table,
    -- @AttributeDefinitions@ must include the key element(s) of the new index.
    attributeDefinitions :: Prelude.Maybe [AttributeDefinition],
    -- | The name of the table to be updated.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'replicaUpdates', 'updateTable_replicaUpdates' - A list of replica update actions (create, delete, or update) for the
-- table.
--
-- This property only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
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
-- 'provisionedThroughput', 'updateTable_provisionedThroughput' - The new provisioned throughput settings for the specified table or
-- index.
--
-- 'sSESpecification', 'updateTable_sSESpecification' - The new server-side encryption settings for the specified table.
--
-- 'tableClass', 'updateTable_tableClass' - The table class of the table to be updated. Valid values are @STANDARD@
-- and @STANDARD_INFREQUENT_ACCESS@.
--
-- 'streamSpecification', 'updateTable_streamSpecification' - Represents the DynamoDB Streams configuration for the table.
--
-- You receive a @ResourceInUseException@ if you try to enable a stream on
-- a table that already has a stream, or if you try to disable a stream on
-- a table that doesn\'t have a stream.
--
-- 'attributeDefinitions', 'updateTable_attributeDefinitions' - An array of attributes that describe the key schema for the table and
-- indexes. If you are adding a new global secondary index to the table,
-- @AttributeDefinitions@ must include the key element(s) of the new index.
--
-- 'tableName', 'updateTable_tableName' - The name of the table to be updated.
newUpdateTable ::
  -- | 'tableName'
  Prelude.Text ->
  UpdateTable
newUpdateTable pTableName_ =
  UpdateTable'
    { globalSecondaryIndexUpdates =
        Prelude.Nothing,
      replicaUpdates = Prelude.Nothing,
      billingMode = Prelude.Nothing,
      provisionedThroughput = Prelude.Nothing,
      sSESpecification = Prelude.Nothing,
      tableClass = Prelude.Nothing,
      streamSpecification = Prelude.Nothing,
      attributeDefinitions = Prelude.Nothing,
      tableName = pTableName_
    }

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
updateTable_globalSecondaryIndexUpdates :: Lens.Lens' UpdateTable (Prelude.Maybe [GlobalSecondaryIndexUpdate])
updateTable_globalSecondaryIndexUpdates = Lens.lens (\UpdateTable' {globalSecondaryIndexUpdates} -> globalSecondaryIndexUpdates) (\s@UpdateTable' {} a -> s {globalSecondaryIndexUpdates = a} :: UpdateTable) Prelude.. Lens.mapping Lens.coerced

-- | A list of replica update actions (create, delete, or update) for the
-- table.
--
-- This property only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
updateTable_replicaUpdates :: Lens.Lens' UpdateTable (Prelude.Maybe (Prelude.NonEmpty ReplicationGroupUpdate))
updateTable_replicaUpdates = Lens.lens (\UpdateTable' {replicaUpdates} -> replicaUpdates) (\s@UpdateTable' {} a -> s {replicaUpdates = a} :: UpdateTable) Prelude.. Lens.mapping Lens.coerced

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
updateTable_billingMode :: Lens.Lens' UpdateTable (Prelude.Maybe BillingMode)
updateTable_billingMode = Lens.lens (\UpdateTable' {billingMode} -> billingMode) (\s@UpdateTable' {} a -> s {billingMode = a} :: UpdateTable)

-- | The new provisioned throughput settings for the specified table or
-- index.
updateTable_provisionedThroughput :: Lens.Lens' UpdateTable (Prelude.Maybe ProvisionedThroughput)
updateTable_provisionedThroughput = Lens.lens (\UpdateTable' {provisionedThroughput} -> provisionedThroughput) (\s@UpdateTable' {} a -> s {provisionedThroughput = a} :: UpdateTable)

-- | The new server-side encryption settings for the specified table.
updateTable_sSESpecification :: Lens.Lens' UpdateTable (Prelude.Maybe SSESpecification)
updateTable_sSESpecification = Lens.lens (\UpdateTable' {sSESpecification} -> sSESpecification) (\s@UpdateTable' {} a -> s {sSESpecification = a} :: UpdateTable)

-- | The table class of the table to be updated. Valid values are @STANDARD@
-- and @STANDARD_INFREQUENT_ACCESS@.
updateTable_tableClass :: Lens.Lens' UpdateTable (Prelude.Maybe TableClass)
updateTable_tableClass = Lens.lens (\UpdateTable' {tableClass} -> tableClass) (\s@UpdateTable' {} a -> s {tableClass = a} :: UpdateTable)

-- | Represents the DynamoDB Streams configuration for the table.
--
-- You receive a @ResourceInUseException@ if you try to enable a stream on
-- a table that already has a stream, or if you try to disable a stream on
-- a table that doesn\'t have a stream.
updateTable_streamSpecification :: Lens.Lens' UpdateTable (Prelude.Maybe StreamSpecification)
updateTable_streamSpecification = Lens.lens (\UpdateTable' {streamSpecification} -> streamSpecification) (\s@UpdateTable' {} a -> s {streamSpecification = a} :: UpdateTable)

-- | An array of attributes that describe the key schema for the table and
-- indexes. If you are adding a new global secondary index to the table,
-- @AttributeDefinitions@ must include the key element(s) of the new index.
updateTable_attributeDefinitions :: Lens.Lens' UpdateTable (Prelude.Maybe [AttributeDefinition])
updateTable_attributeDefinitions = Lens.lens (\UpdateTable' {attributeDefinitions} -> attributeDefinitions) (\s@UpdateTable' {} a -> s {attributeDefinitions = a} :: UpdateTable) Prelude.. Lens.mapping Lens.coerced

-- | The name of the table to be updated.
updateTable_tableName :: Lens.Lens' UpdateTable Prelude.Text
updateTable_tableName = Lens.lens (\UpdateTable' {tableName} -> tableName) (\s@UpdateTable' {} a -> s {tableName = a} :: UpdateTable)

instance Core.AWSRequest UpdateTable where
  type AWSResponse UpdateTable = UpdateTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableResponse'
            Prelude.<$> (x Data..?> "TableDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTable where
  hashWithSalt _salt UpdateTable' {..} =
    _salt
      `Prelude.hashWithSalt` globalSecondaryIndexUpdates
      `Prelude.hashWithSalt` replicaUpdates
      `Prelude.hashWithSalt` billingMode
      `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` sSESpecification
      `Prelude.hashWithSalt` tableClass
      `Prelude.hashWithSalt` streamSpecification
      `Prelude.hashWithSalt` attributeDefinitions
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData UpdateTable where
  rnf UpdateTable' {..} =
    Prelude.rnf globalSecondaryIndexUpdates
      `Prelude.seq` Prelude.rnf replicaUpdates
      `Prelude.seq` Prelude.rnf billingMode
      `Prelude.seq` Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf sSESpecification
      `Prelude.seq` Prelude.rnf tableClass
      `Prelude.seq` Prelude.rnf streamSpecification
      `Prelude.seq` Prelude.rnf attributeDefinitions
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders UpdateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.UpdateTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GlobalSecondaryIndexUpdates" Data..=)
              Prelude.<$> globalSecondaryIndexUpdates,
            ("ReplicaUpdates" Data..=)
              Prelude.<$> replicaUpdates,
            ("BillingMode" Data..=) Prelude.<$> billingMode,
            ("ProvisionedThroughput" Data..=)
              Prelude.<$> provisionedThroughput,
            ("SSESpecification" Data..=)
              Prelude.<$> sSESpecification,
            ("TableClass" Data..=) Prelude.<$> tableClass,
            ("StreamSpecification" Data..=)
              Prelude.<$> streamSpecification,
            ("AttributeDefinitions" Data..=)
              Prelude.<$> attributeDefinitions,
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath UpdateTable where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTable where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @UpdateTable@ operation.
--
-- /See:/ 'newUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { -- | Represents the properties of the table.
    tableDescription :: Prelude.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTableResponse
newUpdateTableResponse pHttpStatus_ =
  UpdateTableResponse'
    { tableDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of the table.
updateTableResponse_tableDescription :: Lens.Lens' UpdateTableResponse (Prelude.Maybe TableDescription)
updateTableResponse_tableDescription = Lens.lens (\UpdateTableResponse' {tableDescription} -> tableDescription) (\s@UpdateTableResponse' {} a -> s {tableDescription = a} :: UpdateTableResponse)

-- | The response's http status code.
updateTableResponse_httpStatus :: Lens.Lens' UpdateTableResponse Prelude.Int
updateTableResponse_httpStatus = Lens.lens (\UpdateTableResponse' {httpStatus} -> httpStatus) (\s@UpdateTableResponse' {} a -> s {httpStatus = a} :: UpdateTableResponse)

instance Prelude.NFData UpdateTableResponse where
  rnf UpdateTableResponse' {..} =
    Prelude.rnf tableDescription
      `Prelude.seq` Prelude.rnf httpStatus
