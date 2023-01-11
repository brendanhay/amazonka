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
-- Module      : Amazonka.DynamoDB.CreateTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateTable@ operation adds a new table to your account. In an
-- Amazon Web Services account, table names must be unique within each
-- Region. That is, you can have two tables with same name if you create
-- the tables in different Regions.
--
-- @CreateTable@ is an asynchronous operation. Upon receiving a
-- @CreateTable@ request, DynamoDB immediately returns a response with a
-- @TableStatus@ of @CREATING@. After the table is created, DynamoDB sets
-- the @TableStatus@ to @ACTIVE@. You can perform read and write operations
-- only on an @ACTIVE@ table.
--
-- You can optionally define secondary indexes on the new table, as part of
-- the @CreateTable@ operation. If you want to create multiple tables with
-- secondary indexes on them, you must create the tables sequentially. Only
-- one table with secondary indexes can be in the @CREATING@ state at any
-- given time.
--
-- You can use the @DescribeTable@ action to check the table status.
module Amazonka.DynamoDB.CreateTable
  ( -- * Creating a Request
    CreateTable (..),
    newCreateTable,

    -- * Request Lenses
    createTable_billingMode,
    createTable_globalSecondaryIndexes,
    createTable_localSecondaryIndexes,
    createTable_provisionedThroughput,
    createTable_sSESpecification,
    createTable_streamSpecification,
    createTable_tableClass,
    createTable_tags,
    createTable_attributeDefinitions,
    createTable_tableName,
    createTable_keySchema,

    -- * Destructuring the Response
    CreateTableResponse (..),
    newCreateTableResponse,

    -- * Response Lenses
    createTableResponse_tableDescription,
    createTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateTable@ operation.
--
-- /See:/ 'newCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | Controls how you are charged for read and write throughput and how you
    -- manage capacity. This setting can be changed later.
    --
    -- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
    --     workloads. @PROVISIONED@ sets the billing mode to
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
    --
    -- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
    --     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
    billingMode :: Prelude.Maybe BillingMode,
    -- | One or more global secondary indexes (the maximum is 20) to be created
    -- on the table. Each global secondary index in the array includes the
    -- following:
    --
    -- -   @IndexName@ - The name of the global secondary index. Must be unique
    --     only for this table.
    --
    -- -   @KeySchema@ - Specifies the key schema for the global secondary
    --     index.
    --
    -- -   @Projection@ - Specifies attributes that are copied (projected) from
    --     the table into the index. These are in addition to the primary key
    --     attributes and index key attributes, which are automatically
    --     projected. Each attribute specification is composed of:
    --
    --     -   @ProjectionType@ - One of the following:
    --
    --         -   @KEYS_ONLY@ - Only the index and primary keys are projected
    --             into the index.
    --
    --         -   @INCLUDE@ - Only the specified table attributes are
    --             projected into the index. The list of projected attributes
    --             is in @NonKeyAttributes@.
    --
    --         -   @ALL@ - All of the table attributes are projected into the
    --             index.
    --
    --     -   @NonKeyAttributes@ - A list of one or more non-key attribute
    --         names that are projected into the secondary index. The total
    --         count of attributes provided in @NonKeyAttributes@, summed
    --         across all of the secondary indexes, must not exceed 100. If you
    --         project the same attribute into two different indexes, this
    --         counts as two distinct attributes when determining the total.
    --
    -- -   @ProvisionedThroughput@ - The provisioned throughput settings for
    --     the global secondary index, consisting of read and write capacity
    --     units.
    globalSecondaryIndexes :: Prelude.Maybe [GlobalSecondaryIndex],
    -- | One or more local secondary indexes (the maximum is 5) to be created on
    -- the table. Each index is scoped to a given partition key value. There is
    -- a 10 GB size limit per partition key value; otherwise, the size of a
    -- local secondary index is unconstrained.
    --
    -- Each local secondary index in the array includes the following:
    --
    -- -   @IndexName@ - The name of the local secondary index. Must be unique
    --     only for this table.
    --
    -- -   @KeySchema@ - Specifies the key schema for the local secondary
    --     index. The key schema must begin with the same partition key as the
    --     table.
    --
    -- -   @Projection@ - Specifies attributes that are copied (projected) from
    --     the table into the index. These are in addition to the primary key
    --     attributes and index key attributes, which are automatically
    --     projected. Each attribute specification is composed of:
    --
    --     -   @ProjectionType@ - One of the following:
    --
    --         -   @KEYS_ONLY@ - Only the index and primary keys are projected
    --             into the index.
    --
    --         -   @INCLUDE@ - Only the specified table attributes are
    --             projected into the index. The list of projected attributes
    --             is in @NonKeyAttributes@.
    --
    --         -   @ALL@ - All of the table attributes are projected into the
    --             index.
    --
    --     -   @NonKeyAttributes@ - A list of one or more non-key attribute
    --         names that are projected into the secondary index. The total
    --         count of attributes provided in @NonKeyAttributes@, summed
    --         across all of the secondary indexes, must not exceed 100. If you
    --         project the same attribute into two different indexes, this
    --         counts as two distinct attributes when determining the total.
    localSecondaryIndexes :: Prelude.Maybe [LocalSecondaryIndex],
    -- | Represents the provisioned throughput settings for a specified table or
    -- index. The settings can be modified using the @UpdateTable@ operation.
    --
    -- If you set BillingMode as @PROVISIONED@, you must specify this property.
    -- If you set BillingMode as @PAY_PER_REQUEST@, you cannot specify this
    -- property.
    --
    -- For current minimum and maximum provisioned throughput values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
    -- in the /Amazon DynamoDB Developer Guide/.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
    -- | Represents the settings used to enable server-side encryption.
    sSESpecification :: Prelude.Maybe SSESpecification,
    -- | The settings for DynamoDB Streams on the table. These settings consist
    -- of:
    --
    -- -   @StreamEnabled@ - Indicates whether DynamoDB Streams is to be
    --     enabled (true) or disabled (false).
    --
    -- -   @StreamViewType@ - When an item in the table is modified,
    --     @StreamViewType@ determines what information is written to the
    --     table\'s stream. Valid values for @StreamViewType@ are:
    --
    --     -   @KEYS_ONLY@ - Only the key attributes of the modified item are
    --         written to the stream.
    --
    --     -   @NEW_IMAGE@ - The entire item, as it appears after it was
    --         modified, is written to the stream.
    --
    --     -   @OLD_IMAGE@ - The entire item, as it appeared before it was
    --         modified, is written to the stream.
    --
    --     -   @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of
    --         the item are written to the stream.
    streamSpecification :: Prelude.Maybe StreamSpecification,
    -- | The table class of the new table. Valid values are @STANDARD@ and
    -- @STANDARD_INFREQUENT_ACCESS@.
    tableClass :: Prelude.Maybe TableClass,
    -- | A list of key-value pairs to label the table. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB>.
    tags :: Prelude.Maybe [Tag],
    -- | An array of attributes that describe the key schema for the table and
    -- indexes.
    attributeDefinitions :: [AttributeDefinition],
    -- | The name of the table to create.
    tableName :: Prelude.Text,
    -- | Specifies the attributes that make up the primary key for a table or an
    -- index. The attributes in @KeySchema@ must also be defined in the
    -- @AttributeDefinitions@ array. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html Data Model>
    -- in the /Amazon DynamoDB Developer Guide/.
    --
    -- Each @KeySchemaElement@ in the array is composed of:
    --
    -- -   @AttributeName@ - The name of this key attribute.
    --
    -- -   @KeyType@ - The role that the key attribute will assume:
    --
    --     -   @HASH@ - partition key
    --
    --     -   @RANGE@ - sort key
    --
    -- The partition key of an item is also known as its /hash attribute/. The
    -- term \"hash attribute\" derives from the DynamoDB usage of an internal
    -- hash function to evenly distribute data items across partitions, based
    -- on their partition key values.
    --
    -- The sort key of an item is also known as its /range attribute/. The term
    -- \"range attribute\" derives from the way DynamoDB stores items with the
    -- same partition key physically close together, in sorted order by the
    -- sort key value.
    --
    -- For a simple primary key (partition key), you must provide exactly one
    -- element with a @KeyType@ of @HASH@.
    --
    -- For a composite primary key (partition key and sort key), you must
    -- provide exactly two elements, in this order: The first element must have
    -- a @KeyType@ of @HASH@, and the second element must have a @KeyType@ of
    -- @RANGE@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#WorkingWithTables.primary.key Working with Tables>
    -- in the /Amazon DynamoDB Developer Guide/.
    keySchema :: Prelude.NonEmpty KeySchemaElement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingMode', 'createTable_billingMode' - Controls how you are charged for read and write throughput and how you
-- manage capacity. This setting can be changed later.
--
-- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
--     workloads. @PROVISIONED@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
--
-- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
--
-- 'globalSecondaryIndexes', 'createTable_globalSecondaryIndexes' - One or more global secondary indexes (the maximum is 20) to be created
-- on the table. Each global secondary index in the array includes the
-- following:
--
-- -   @IndexName@ - The name of the global secondary index. Must be unique
--     only for this table.
--
-- -   @KeySchema@ - Specifies the key schema for the global secondary
--     index.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             is in @NonKeyAttributes@.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 100. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   @ProvisionedThroughput@ - The provisioned throughput settings for
--     the global secondary index, consisting of read and write capacity
--     units.
--
-- 'localSecondaryIndexes', 'createTable_localSecondaryIndexes' - One or more local secondary indexes (the maximum is 5) to be created on
-- the table. Each index is scoped to a given partition key value. There is
-- a 10 GB size limit per partition key value; otherwise, the size of a
-- local secondary index is unconstrained.
--
-- Each local secondary index in the array includes the following:
--
-- -   @IndexName@ - The name of the local secondary index. Must be unique
--     only for this table.
--
-- -   @KeySchema@ - Specifies the key schema for the local secondary
--     index. The key schema must begin with the same partition key as the
--     table.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             is in @NonKeyAttributes@.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 100. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- 'provisionedThroughput', 'createTable_provisionedThroughput' - Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the @UpdateTable@ operation.
--
-- If you set BillingMode as @PROVISIONED@, you must specify this property.
-- If you set BillingMode as @PAY_PER_REQUEST@, you cannot specify this
-- property.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'sSESpecification', 'createTable_sSESpecification' - Represents the settings used to enable server-side encryption.
--
-- 'streamSpecification', 'createTable_streamSpecification' - The settings for DynamoDB Streams on the table. These settings consist
-- of:
--
-- -   @StreamEnabled@ - Indicates whether DynamoDB Streams is to be
--     enabled (true) or disabled (false).
--
-- -   @StreamViewType@ - When an item in the table is modified,
--     @StreamViewType@ determines what information is written to the
--     table\'s stream. Valid values for @StreamViewType@ are:
--
--     -   @KEYS_ONLY@ - Only the key attributes of the modified item are
--         written to the stream.
--
--     -   @NEW_IMAGE@ - The entire item, as it appears after it was
--         modified, is written to the stream.
--
--     -   @OLD_IMAGE@ - The entire item, as it appeared before it was
--         modified, is written to the stream.
--
--     -   @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of
--         the item are written to the stream.
--
-- 'tableClass', 'createTable_tableClass' - The table class of the new table. Valid values are @STANDARD@ and
-- @STANDARD_INFREQUENT_ACCESS@.
--
-- 'tags', 'createTable_tags' - A list of key-value pairs to label the table. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB>.
--
-- 'attributeDefinitions', 'createTable_attributeDefinitions' - An array of attributes that describe the key schema for the table and
-- indexes.
--
-- 'tableName', 'createTable_tableName' - The name of the table to create.
--
-- 'keySchema', 'createTable_keySchema' - Specifies the attributes that make up the primary key for a table or an
-- index. The attributes in @KeySchema@ must also be defined in the
-- @AttributeDefinitions@ array. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html Data Model>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- Each @KeySchemaElement@ in the array is composed of:
--
-- -   @AttributeName@ - The name of this key attribute.
--
-- -   @KeyType@ - The role that the key attribute will assume:
--
--     -   @HASH@ - partition key
--
--     -   @RANGE@ - sort key
--
-- The partition key of an item is also known as its /hash attribute/. The
-- term \"hash attribute\" derives from the DynamoDB usage of an internal
-- hash function to evenly distribute data items across partitions, based
-- on their partition key values.
--
-- The sort key of an item is also known as its /range attribute/. The term
-- \"range attribute\" derives from the way DynamoDB stores items with the
-- same partition key physically close together, in sorted order by the
-- sort key value.
--
-- For a simple primary key (partition key), you must provide exactly one
-- element with a @KeyType@ of @HASH@.
--
-- For a composite primary key (partition key and sort key), you must
-- provide exactly two elements, in this order: The first element must have
-- a @KeyType@ of @HASH@, and the second element must have a @KeyType@ of
-- @RANGE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#WorkingWithTables.primary.key Working with Tables>
-- in the /Amazon DynamoDB Developer Guide/.
newCreateTable ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'keySchema'
  Prelude.NonEmpty KeySchemaElement ->
  CreateTable
newCreateTable pTableName_ pKeySchema_ =
  CreateTable'
    { billingMode = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      localSecondaryIndexes = Prelude.Nothing,
      provisionedThroughput = Prelude.Nothing,
      sSESpecification = Prelude.Nothing,
      streamSpecification = Prelude.Nothing,
      tableClass = Prelude.Nothing,
      tags = Prelude.Nothing,
      attributeDefinitions = Prelude.mempty,
      tableName = pTableName_,
      keySchema = Lens.coerced Lens.# pKeySchema_
    }

-- | Controls how you are charged for read and write throughput and how you
-- manage capacity. This setting can be changed later.
--
-- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
--     workloads. @PROVISIONED@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
--
-- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
createTable_billingMode :: Lens.Lens' CreateTable (Prelude.Maybe BillingMode)
createTable_billingMode = Lens.lens (\CreateTable' {billingMode} -> billingMode) (\s@CreateTable' {} a -> s {billingMode = a} :: CreateTable)

-- | One or more global secondary indexes (the maximum is 20) to be created
-- on the table. Each global secondary index in the array includes the
-- following:
--
-- -   @IndexName@ - The name of the global secondary index. Must be unique
--     only for this table.
--
-- -   @KeySchema@ - Specifies the key schema for the global secondary
--     index.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             is in @NonKeyAttributes@.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 100. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   @ProvisionedThroughput@ - The provisioned throughput settings for
--     the global secondary index, consisting of read and write capacity
--     units.
createTable_globalSecondaryIndexes :: Lens.Lens' CreateTable (Prelude.Maybe [GlobalSecondaryIndex])
createTable_globalSecondaryIndexes = Lens.lens (\CreateTable' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@CreateTable' {} a -> s {globalSecondaryIndexes = a} :: CreateTable) Prelude.. Lens.mapping Lens.coerced

-- | One or more local secondary indexes (the maximum is 5) to be created on
-- the table. Each index is scoped to a given partition key value. There is
-- a 10 GB size limit per partition key value; otherwise, the size of a
-- local secondary index is unconstrained.
--
-- Each local secondary index in the array includes the following:
--
-- -   @IndexName@ - The name of the local secondary index. Must be unique
--     only for this table.
--
-- -   @KeySchema@ - Specifies the key schema for the local secondary
--     index. The key schema must begin with the same partition key as the
--     table.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             is in @NonKeyAttributes@.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 100. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
createTable_localSecondaryIndexes :: Lens.Lens' CreateTable (Prelude.Maybe [LocalSecondaryIndex])
createTable_localSecondaryIndexes = Lens.lens (\CreateTable' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@CreateTable' {} a -> s {localSecondaryIndexes = a} :: CreateTable) Prelude.. Lens.mapping Lens.coerced

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the @UpdateTable@ operation.
--
-- If you set BillingMode as @PROVISIONED@, you must specify this property.
-- If you set BillingMode as @PAY_PER_REQUEST@, you cannot specify this
-- property.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
createTable_provisionedThroughput :: Lens.Lens' CreateTable (Prelude.Maybe ProvisionedThroughput)
createTable_provisionedThroughput = Lens.lens (\CreateTable' {provisionedThroughput} -> provisionedThroughput) (\s@CreateTable' {} a -> s {provisionedThroughput = a} :: CreateTable)

-- | Represents the settings used to enable server-side encryption.
createTable_sSESpecification :: Lens.Lens' CreateTable (Prelude.Maybe SSESpecification)
createTable_sSESpecification = Lens.lens (\CreateTable' {sSESpecification} -> sSESpecification) (\s@CreateTable' {} a -> s {sSESpecification = a} :: CreateTable)

-- | The settings for DynamoDB Streams on the table. These settings consist
-- of:
--
-- -   @StreamEnabled@ - Indicates whether DynamoDB Streams is to be
--     enabled (true) or disabled (false).
--
-- -   @StreamViewType@ - When an item in the table is modified,
--     @StreamViewType@ determines what information is written to the
--     table\'s stream. Valid values for @StreamViewType@ are:
--
--     -   @KEYS_ONLY@ - Only the key attributes of the modified item are
--         written to the stream.
--
--     -   @NEW_IMAGE@ - The entire item, as it appears after it was
--         modified, is written to the stream.
--
--     -   @OLD_IMAGE@ - The entire item, as it appeared before it was
--         modified, is written to the stream.
--
--     -   @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of
--         the item are written to the stream.
createTable_streamSpecification :: Lens.Lens' CreateTable (Prelude.Maybe StreamSpecification)
createTable_streamSpecification = Lens.lens (\CreateTable' {streamSpecification} -> streamSpecification) (\s@CreateTable' {} a -> s {streamSpecification = a} :: CreateTable)

-- | The table class of the new table. Valid values are @STANDARD@ and
-- @STANDARD_INFREQUENT_ACCESS@.
createTable_tableClass :: Lens.Lens' CreateTable (Prelude.Maybe TableClass)
createTable_tableClass = Lens.lens (\CreateTable' {tableClass} -> tableClass) (\s@CreateTable' {} a -> s {tableClass = a} :: CreateTable)

-- | A list of key-value pairs to label the table. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB>.
createTable_tags :: Lens.Lens' CreateTable (Prelude.Maybe [Tag])
createTable_tags = Lens.lens (\CreateTable' {tags} -> tags) (\s@CreateTable' {} a -> s {tags = a} :: CreateTable) Prelude.. Lens.mapping Lens.coerced

-- | An array of attributes that describe the key schema for the table and
-- indexes.
createTable_attributeDefinitions :: Lens.Lens' CreateTable [AttributeDefinition]
createTable_attributeDefinitions = Lens.lens (\CreateTable' {attributeDefinitions} -> attributeDefinitions) (\s@CreateTable' {} a -> s {attributeDefinitions = a} :: CreateTable) Prelude.. Lens.coerced

-- | The name of the table to create.
createTable_tableName :: Lens.Lens' CreateTable Prelude.Text
createTable_tableName = Lens.lens (\CreateTable' {tableName} -> tableName) (\s@CreateTable' {} a -> s {tableName = a} :: CreateTable)

-- | Specifies the attributes that make up the primary key for a table or an
-- index. The attributes in @KeySchema@ must also be defined in the
-- @AttributeDefinitions@ array. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html Data Model>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- Each @KeySchemaElement@ in the array is composed of:
--
-- -   @AttributeName@ - The name of this key attribute.
--
-- -   @KeyType@ - The role that the key attribute will assume:
--
--     -   @HASH@ - partition key
--
--     -   @RANGE@ - sort key
--
-- The partition key of an item is also known as its /hash attribute/. The
-- term \"hash attribute\" derives from the DynamoDB usage of an internal
-- hash function to evenly distribute data items across partitions, based
-- on their partition key values.
--
-- The sort key of an item is also known as its /range attribute/. The term
-- \"range attribute\" derives from the way DynamoDB stores items with the
-- same partition key physically close together, in sorted order by the
-- sort key value.
--
-- For a simple primary key (partition key), you must provide exactly one
-- element with a @KeyType@ of @HASH@.
--
-- For a composite primary key (partition key and sort key), you must
-- provide exactly two elements, in this order: The first element must have
-- a @KeyType@ of @HASH@, and the second element must have a @KeyType@ of
-- @RANGE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#WorkingWithTables.primary.key Working with Tables>
-- in the /Amazon DynamoDB Developer Guide/.
createTable_keySchema :: Lens.Lens' CreateTable (Prelude.NonEmpty KeySchemaElement)
createTable_keySchema = Lens.lens (\CreateTable' {keySchema} -> keySchema) (\s@CreateTable' {} a -> s {keySchema = a} :: CreateTable) Prelude.. Lens.coerced

instance Core.AWSRequest CreateTable where
  type AWSResponse CreateTable = CreateTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTableResponse'
            Prelude.<$> (x Data..?> "TableDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTable where
  hashWithSalt _salt CreateTable' {..} =
    _salt `Prelude.hashWithSalt` billingMode
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` localSecondaryIndexes
      `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` sSESpecification
      `Prelude.hashWithSalt` streamSpecification
      `Prelude.hashWithSalt` tableClass
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` attributeDefinitions
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` keySchema

instance Prelude.NFData CreateTable where
  rnf CreateTable' {..} =
    Prelude.rnf billingMode
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf localSecondaryIndexes
      `Prelude.seq` Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf sSESpecification
      `Prelude.seq` Prelude.rnf streamSpecification
      `Prelude.seq` Prelude.rnf tableClass
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf attributeDefinitions
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf keySchema

instance Data.ToHeaders CreateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.CreateTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTable where
  toJSON CreateTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingMode" Data..=) Prelude.<$> billingMode,
            ("GlobalSecondaryIndexes" Data..=)
              Prelude.<$> globalSecondaryIndexes,
            ("LocalSecondaryIndexes" Data..=)
              Prelude.<$> localSecondaryIndexes,
            ("ProvisionedThroughput" Data..=)
              Prelude.<$> provisionedThroughput,
            ("SSESpecification" Data..=)
              Prelude.<$> sSESpecification,
            ("StreamSpecification" Data..=)
              Prelude.<$> streamSpecification,
            ("TableClass" Data..=) Prelude.<$> tableClass,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "AttributeDefinitions"
                  Data..= attributeDefinitions
              ),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("KeySchema" Data..= keySchema)
          ]
      )

instance Data.ToPath CreateTable where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTable where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateTable@ operation.
--
-- /See:/ 'newCreateTableResponse' smart constructor.
data CreateTableResponse = CreateTableResponse'
  { -- | Represents the properties of the table.
    tableDescription :: Prelude.Maybe TableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableDescription', 'createTableResponse_tableDescription' - Represents the properties of the table.
--
-- 'httpStatus', 'createTableResponse_httpStatus' - The response's http status code.
newCreateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTableResponse
newCreateTableResponse pHttpStatus_ =
  CreateTableResponse'
    { tableDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of the table.
createTableResponse_tableDescription :: Lens.Lens' CreateTableResponse (Prelude.Maybe TableDescription)
createTableResponse_tableDescription = Lens.lens (\CreateTableResponse' {tableDescription} -> tableDescription) (\s@CreateTableResponse' {} a -> s {tableDescription = a} :: CreateTableResponse)

-- | The response's http status code.
createTableResponse_httpStatus :: Lens.Lens' CreateTableResponse Prelude.Int
createTableResponse_httpStatus = Lens.lens (\CreateTableResponse' {httpStatus} -> httpStatus) (\s@CreateTableResponse' {} a -> s {httpStatus = a} :: CreateTableResponse)

instance Prelude.NFData CreateTableResponse where
  rnf CreateTableResponse' {..} =
    Prelude.rnf tableDescription
      `Prelude.seq` Prelude.rnf httpStatus
