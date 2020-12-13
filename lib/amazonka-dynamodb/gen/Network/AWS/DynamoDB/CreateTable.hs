{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.CreateTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateTable@ operation adds a new table to your account. In an AWS account, table names must be unique within each Region. That is, you can have two tables with same name if you create the tables in different Regions.
--
-- @CreateTable@ is an asynchronous operation. Upon receiving a @CreateTable@ request, DynamoDB immediately returns a response with a @TableStatus@ of @CREATING@ . After the table is created, DynamoDB sets the @TableStatus@ to @ACTIVE@ . You can perform read and write operations only on an @ACTIVE@ table.
-- You can optionally define secondary indexes on the new table, as part of the @CreateTable@ operation. If you want to create multiple tables with secondary indexes on them, you must create the tables sequentially. Only one table with secondary indexes can be in the @CREATING@ state at any given time.
-- You can use the @DescribeTable@ action to check the table status.
module Network.AWS.DynamoDB.CreateTable
  ( -- * Creating a request
    CreateTable (..),
    mkCreateTable,

    -- ** Request lenses
    ctAttributeDefinitions,
    ctProvisionedThroughput,
    ctSSESpecification,
    ctKeySchema,
    ctGlobalSecondaryIndexes,
    ctLocalSecondaryIndexes,
    ctBillingMode,
    ctTableName,
    ctTags,
    ctStreamSpecification,

    -- * Destructuring the response
    CreateTableResponse (..),
    mkCreateTableResponse,

    -- ** Response lenses
    ctrsTableDescription,
    ctrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateTable@ operation.
--
-- /See:/ 'mkCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | An array of attributes that describe the key schema for the table and indexes.
    attributeDefinitions :: [AttributeDefinition],
    -- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
    --
    -- If you set BillingMode as @PROVISIONED@ , you must specify this property. If you set BillingMode as @PAY_PER_REQUEST@ , you cannot specify this property.
    -- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
    provisionedThroughput :: Lude.Maybe ProvisionedThroughput,
    -- | Represents the settings used to enable server-side encryption.
    sSESpecification :: Lude.Maybe SSESpecification,
    -- | Specifies the attributes that make up the primary key for a table or an index. The attributes in @KeySchema@ must also be defined in the @AttributeDefinitions@ array. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html Data Model> in the /Amazon DynamoDB Developer Guide/ .
    --
    -- Each @KeySchemaElement@ in the array is composed of:
    --
    --     * @AttributeName@ - The name of this key attribute.
    --
    --
    --     * @KeyType@ - The role that the key attribute will assume:
    --
    --     * @HASH@ - partition key
    --
    --
    --     * @RANGE@ - sort key
    --
    --
    --
    --
    -- For a simple primary key (partition key), you must provide exactly one element with a @KeyType@ of @HASH@ .
    -- For a composite primary key (partition key and sort key), you must provide exactly two elements, in this order: The first element must have a @KeyType@ of @HASH@ , and the second element must have a @KeyType@ of @RANGE@ .
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#WorkingWithTables.primary.key Working with Tables> in the /Amazon DynamoDB Developer Guide/ .
    keySchema :: Lude.NonEmpty KeySchemaElement,
    -- | One or more global secondary indexes (the maximum is 20) to be created on the table. Each global secondary index in the array includes the following:
    --
    --
    --     * @IndexName@ - The name of the global secondary index. Must be unique only for this table.
    --
    --
    --
    --     * @KeySchema@ - Specifies the key schema for the global secondary index.
    --
    --
    --     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
    --
    --     * @ProjectionType@ - One of the following:
    --
    --     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
    --
    --
    --     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .
    --
    --
    --     * @ALL@ - All of the table attributes are projected into the index.
    --
    --
    --
    --
    --     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 100. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
    --
    --
    --
    --
    --     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units.
    globalSecondaryIndexes :: Lude.Maybe [GlobalSecondaryIndex],
    -- | One or more local secondary indexes (the maximum is 5) to be created on the table. Each index is scoped to a given partition key value. There is a 10 GB size limit per partition key value; otherwise, the size of a local secondary index is unconstrained.
    --
    -- Each local secondary index in the array includes the following:
    --
    --     * @IndexName@ - The name of the local secondary index. Must be unique only for this table.
    --
    --
    --
    --     * @KeySchema@ - Specifies the key schema for the local secondary index. The key schema must begin with the same partition key as the table.
    --
    --
    --     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
    --
    --     * @ProjectionType@ - One of the following:
    --
    --     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
    --
    --
    --     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .
    --
    --
    --     * @ALL@ - All of the table attributes are projected into the index.
    --
    --
    --
    --
    --     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 100. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
    localSecondaryIndexes :: Lude.Maybe [LocalSecondaryIndex],
    -- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
    --
    --
    --     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
    --
    --
    --     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
    billingMode :: Lude.Maybe BillingMode,
    -- | The name of the table to create.
    tableName :: Lude.Text,
    -- | A list of key-value pairs to label the table. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> .
    tags :: Lude.Maybe [Tag],
    -- | The settings for DynamoDB Streams on the table. These settings consist of:
    --
    --
    --     * @StreamEnabled@ - Indicates whether DynamoDB Streams is to be enabled (true) or disabled (false).
    --
    --
    --     * @StreamViewType@ - When an item in the table is modified, @StreamViewType@ determines what information is written to the table's stream. Valid values for @StreamViewType@ are:
    --
    --     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.
    --
    --
    --     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.
    --
    --
    --     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.
    --
    --
    --     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
    streamSpecification :: Lude.Maybe StreamSpecification
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTable' with the minimum fields required to make a request.
--
-- * 'attributeDefinitions' - An array of attributes that describe the key schema for the table and indexes.
-- * 'provisionedThroughput' - Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
--
-- If you set BillingMode as @PROVISIONED@ , you must specify this property. If you set BillingMode as @PAY_PER_REQUEST@ , you cannot specify this property.
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
-- * 'sSESpecification' - Represents the settings used to enable server-side encryption.
-- * 'keySchema' - Specifies the attributes that make up the primary key for a table or an index. The attributes in @KeySchema@ must also be defined in the @AttributeDefinitions@ array. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html Data Model> in the /Amazon DynamoDB Developer Guide/ .
--
-- Each @KeySchemaElement@ in the array is composed of:
--
--     * @AttributeName@ - The name of this key attribute.
--
--
--     * @KeyType@ - The role that the key attribute will assume:
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
--
--
-- For a simple primary key (partition key), you must provide exactly one element with a @KeyType@ of @HASH@ .
-- For a composite primary key (partition key and sort key), you must provide exactly two elements, in this order: The first element must have a @KeyType@ of @HASH@ , and the second element must have a @KeyType@ of @RANGE@ .
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#WorkingWithTables.primary.key Working with Tables> in the /Amazon DynamoDB Developer Guide/ .
-- * 'globalSecondaryIndexes' - One or more global secondary indexes (the maximum is 20) to be created on the table. Each global secondary index in the array includes the following:
--
--
--     * @IndexName@ - The name of the global secondary index. Must be unique only for this table.
--
--
--
--     * @KeySchema@ - Specifies the key schema for the global secondary index.
--
--
--     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
--
--     * @ProjectionType@ - One of the following:
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
--
--
--
--
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 100. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
--     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units.
--
--
-- * 'localSecondaryIndexes' - One or more local secondary indexes (the maximum is 5) to be created on the table. Each index is scoped to a given partition key value. There is a 10 GB size limit per partition key value; otherwise, the size of a local secondary index is unconstrained.
--
-- Each local secondary index in the array includes the following:
--
--     * @IndexName@ - The name of the local secondary index. Must be unique only for this table.
--
--
--
--     * @KeySchema@ - Specifies the key schema for the local secondary index. The key schema must begin with the same partition key as the table.
--
--
--     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
--
--     * @ProjectionType@ - One of the following:
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
--
--
--
--
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 100. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
-- * 'billingMode' - Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
--
--
--     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
--
--
--     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
--
--
-- * 'tableName' - The name of the table to create.
-- * 'tags' - A list of key-value pairs to label the table. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> .
-- * 'streamSpecification' - The settings for DynamoDB Streams on the table. These settings consist of:
--
--
--     * @StreamEnabled@ - Indicates whether DynamoDB Streams is to be enabled (true) or disabled (false).
--
--
--     * @StreamViewType@ - When an item in the table is modified, @StreamViewType@ determines what information is written to the table's stream. Valid values for @StreamViewType@ are:
--
--     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.
--
--
--     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.
--
--
--     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.
--
--
--     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
mkCreateTable ::
  -- | 'keySchema'
  Lude.NonEmpty KeySchemaElement ->
  -- | 'tableName'
  Lude.Text ->
  CreateTable
mkCreateTable pKeySchema_ pTableName_ =
  CreateTable'
    { attributeDefinitions = Lude.mempty,
      provisionedThroughput = Lude.Nothing,
      sSESpecification = Lude.Nothing,
      keySchema = pKeySchema_,
      globalSecondaryIndexes = Lude.Nothing,
      localSecondaryIndexes = Lude.Nothing,
      billingMode = Lude.Nothing,
      tableName = pTableName_,
      tags = Lude.Nothing,
      streamSpecification = Lude.Nothing
    }

-- | An array of attributes that describe the key schema for the table and indexes.
--
-- /Note:/ Consider using 'attributeDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctAttributeDefinitions :: Lens.Lens' CreateTable [AttributeDefinition]
ctAttributeDefinitions = Lens.lens (attributeDefinitions :: CreateTable -> [AttributeDefinition]) (\s a -> s {attributeDefinitions = a} :: CreateTable)
{-# DEPRECATED ctAttributeDefinitions "Use generic-lens or generic-optics with 'attributeDefinitions' instead." #-}

-- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
--
-- If you set BillingMode as @PROVISIONED@ , you must specify this property. If you set BillingMode as @PAY_PER_REQUEST@ , you cannot specify this property.
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctProvisionedThroughput :: Lens.Lens' CreateTable (Lude.Maybe ProvisionedThroughput)
ctProvisionedThroughput = Lens.lens (provisionedThroughput :: CreateTable -> Lude.Maybe ProvisionedThroughput) (\s a -> s {provisionedThroughput = a} :: CreateTable)
{-# DEPRECATED ctProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | Represents the settings used to enable server-side encryption.
--
-- /Note:/ Consider using 'sSESpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSSESpecification :: Lens.Lens' CreateTable (Lude.Maybe SSESpecification)
ctSSESpecification = Lens.lens (sSESpecification :: CreateTable -> Lude.Maybe SSESpecification) (\s a -> s {sSESpecification = a} :: CreateTable)
{-# DEPRECATED ctSSESpecification "Use generic-lens or generic-optics with 'sSESpecification' instead." #-}

-- | Specifies the attributes that make up the primary key for a table or an index. The attributes in @KeySchema@ must also be defined in the @AttributeDefinitions@ array. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html Data Model> in the /Amazon DynamoDB Developer Guide/ .
--
-- Each @KeySchemaElement@ in the array is composed of:
--
--     * @AttributeName@ - The name of this key attribute.
--
--
--     * @KeyType@ - The role that the key attribute will assume:
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
--
--
-- For a simple primary key (partition key), you must provide exactly one element with a @KeyType@ of @HASH@ .
-- For a composite primary key (partition key and sort key), you must provide exactly two elements, in this order: The first element must have a @KeyType@ of @HASH@ , and the second element must have a @KeyType@ of @RANGE@ .
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#WorkingWithTables.primary.key Working with Tables> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctKeySchema :: Lens.Lens' CreateTable (Lude.NonEmpty KeySchemaElement)
ctKeySchema = Lens.lens (keySchema :: CreateTable -> Lude.NonEmpty KeySchemaElement) (\s a -> s {keySchema = a} :: CreateTable)
{-# DEPRECATED ctKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | One or more global secondary indexes (the maximum is 20) to be created on the table. Each global secondary index in the array includes the following:
--
--
--     * @IndexName@ - The name of the global secondary index. Must be unique only for this table.
--
--
--
--     * @KeySchema@ - Specifies the key schema for the global secondary index.
--
--
--     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
--
--     * @ProjectionType@ - One of the following:
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
--
--
--
--
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 100. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
--     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units.
--
--
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctGlobalSecondaryIndexes :: Lens.Lens' CreateTable (Lude.Maybe [GlobalSecondaryIndex])
ctGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: CreateTable -> Lude.Maybe [GlobalSecondaryIndex]) (\s a -> s {globalSecondaryIndexes = a} :: CreateTable)
{-# DEPRECATED ctGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | One or more local secondary indexes (the maximum is 5) to be created on the table. Each index is scoped to a given partition key value. There is a 10 GB size limit per partition key value; otherwise, the size of a local secondary index is unconstrained.
--
-- Each local secondary index in the array includes the following:
--
--     * @IndexName@ - The name of the local secondary index. Must be unique only for this table.
--
--
--
--     * @KeySchema@ - Specifies the key schema for the local secondary index. The key schema must begin with the same partition key as the table.
--
--
--     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
--
--     * @ProjectionType@ - One of the following:
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
--
--
--
--
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 100. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
--
-- /Note:/ Consider using 'localSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctLocalSecondaryIndexes :: Lens.Lens' CreateTable (Lude.Maybe [LocalSecondaryIndex])
ctLocalSecondaryIndexes = Lens.lens (localSecondaryIndexes :: CreateTable -> Lude.Maybe [LocalSecondaryIndex]) (\s a -> s {localSecondaryIndexes = a} :: CreateTable)
{-# DEPRECATED ctLocalSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead." #-}

-- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
--
--
--     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
--
--
--     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
--
--
--
-- /Note:/ Consider using 'billingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctBillingMode :: Lens.Lens' CreateTable (Lude.Maybe BillingMode)
ctBillingMode = Lens.lens (billingMode :: CreateTable -> Lude.Maybe BillingMode) (\s a -> s {billingMode = a} :: CreateTable)
{-# DEPRECATED ctBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

-- | The name of the table to create.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTableName :: Lens.Lens' CreateTable Lude.Text
ctTableName = Lens.lens (tableName :: CreateTable -> Lude.Text) (\s a -> s {tableName = a} :: CreateTable)
{-# DEPRECATED ctTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of key-value pairs to label the table. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTable (Lude.Maybe [Tag])
ctTags = Lens.lens (tags :: CreateTable -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTable)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The settings for DynamoDB Streams on the table. These settings consist of:
--
--
--     * @StreamEnabled@ - Indicates whether DynamoDB Streams is to be enabled (true) or disabled (false).
--
--
--     * @StreamViewType@ - When an item in the table is modified, @StreamViewType@ determines what information is written to the table's stream. Valid values for @StreamViewType@ are:
--
--     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.
--
--
--     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.
--
--
--     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.
--
--
--     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
--
--
--
--
--
-- /Note:/ Consider using 'streamSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctStreamSpecification :: Lens.Lens' CreateTable (Lude.Maybe StreamSpecification)
ctStreamSpecification = Lens.lens (streamSpecification :: CreateTable -> Lude.Maybe StreamSpecification) (\s a -> s {streamSpecification = a} :: CreateTable)
{-# DEPRECATED ctStreamSpecification "Use generic-lens or generic-optics with 'streamSpecification' instead." #-}

instance Lude.AWSRequest CreateTable where
  type Rs CreateTable = CreateTableResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTableResponse'
            Lude.<$> (x Lude..?> "TableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.CreateTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTable where
  toJSON CreateTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeDefinitions" Lude..= attributeDefinitions),
            ("ProvisionedThroughput" Lude..=) Lude.<$> provisionedThroughput,
            ("SSESpecification" Lude..=) Lude.<$> sSESpecification,
            Lude.Just ("KeySchema" Lude..= keySchema),
            ("GlobalSecondaryIndexes" Lude..=) Lude.<$> globalSecondaryIndexes,
            ("LocalSecondaryIndexes" Lude..=) Lude.<$> localSecondaryIndexes,
            ("BillingMode" Lude..=) Lude.<$> billingMode,
            Lude.Just ("TableName" Lude..= tableName),
            ("Tags" Lude..=) Lude.<$> tags,
            ("StreamSpecification" Lude..=) Lude.<$> streamSpecification
          ]
      )

instance Lude.ToPath CreateTable where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTable where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateTable@ operation.
--
-- /See:/ 'mkCreateTableResponse' smart constructor.
data CreateTableResponse = CreateTableResponse'
  { -- | Represents the properties of the table.
    tableDescription :: Lude.Maybe TableDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTableResponse' with the minimum fields required to make a request.
--
-- * 'tableDescription' - Represents the properties of the table.
-- * 'responseStatus' - The response status code.
mkCreateTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTableResponse
mkCreateTableResponse pResponseStatus_ =
  CreateTableResponse'
    { tableDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the properties of the table.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsTableDescription :: Lens.Lens' CreateTableResponse (Lude.Maybe TableDescription)
ctrsTableDescription = Lens.lens (tableDescription :: CreateTableResponse -> Lude.Maybe TableDescription) (\s a -> s {tableDescription = a} :: CreateTableResponse)
{-# DEPRECATED ctrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTableResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTableResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
