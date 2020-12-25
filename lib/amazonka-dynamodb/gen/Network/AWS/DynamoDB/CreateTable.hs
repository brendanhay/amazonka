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
    ctTableName,
    ctKeySchema,
    ctBillingMode,
    ctGlobalSecondaryIndexes,
    ctLocalSecondaryIndexes,
    ctProvisionedThroughput,
    ctSSESpecification,
    ctStreamSpecification,
    ctTags,

    -- * Destructuring the response
    CreateTableResponse (..),
    mkCreateTableResponse,

    -- ** Response lenses
    ctrrsTableDescription,
    ctrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateTable@ operation.
--
-- /See:/ 'mkCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | An array of attributes that describe the key schema for the table and indexes.
    attributeDefinitions :: [Types.AttributeDefinition],
    -- | The name of the table to create.
    tableName :: Types.TableName,
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
    keySchema :: Core.NonEmpty Types.KeySchemaElement,
    -- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
    --
    --
    --     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
    --
    --
    --     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
    billingMode :: Core.Maybe Types.BillingMode,
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
    globalSecondaryIndexes :: Core.Maybe [Types.GlobalSecondaryIndex],
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
    localSecondaryIndexes :: Core.Maybe [Types.LocalSecondaryIndex],
    -- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
    --
    -- If you set BillingMode as @PROVISIONED@ , you must specify this property. If you set BillingMode as @PAY_PER_REQUEST@ , you cannot specify this property.
    -- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
    provisionedThroughput :: Core.Maybe Types.ProvisionedThroughput,
    -- | Represents the settings used to enable server-side encryption.
    sSESpecification :: Core.Maybe Types.SSESpecification,
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
    streamSpecification :: Core.Maybe Types.StreamSpecification,
    -- | A list of key-value pairs to label the table. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTable' value with any optional fields omitted.
mkCreateTable ::
  -- | 'tableName'
  Types.TableName ->
  -- | 'keySchema'
  Core.NonEmpty Types.KeySchemaElement ->
  CreateTable
mkCreateTable tableName keySchema =
  CreateTable'
    { attributeDefinitions = Core.mempty,
      tableName,
      keySchema,
      billingMode = Core.Nothing,
      globalSecondaryIndexes = Core.Nothing,
      localSecondaryIndexes = Core.Nothing,
      provisionedThroughput = Core.Nothing,
      sSESpecification = Core.Nothing,
      streamSpecification = Core.Nothing,
      tags = Core.Nothing
    }

-- | An array of attributes that describe the key schema for the table and indexes.
--
-- /Note:/ Consider using 'attributeDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctAttributeDefinitions :: Lens.Lens' CreateTable [Types.AttributeDefinition]
ctAttributeDefinitions = Lens.field @"attributeDefinitions"
{-# DEPRECATED ctAttributeDefinitions "Use generic-lens or generic-optics with 'attributeDefinitions' instead." #-}

-- | The name of the table to create.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTableName :: Lens.Lens' CreateTable Types.TableName
ctTableName = Lens.field @"tableName"
{-# DEPRECATED ctTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

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
ctKeySchema :: Lens.Lens' CreateTable (Core.NonEmpty Types.KeySchemaElement)
ctKeySchema = Lens.field @"keySchema"
{-# DEPRECATED ctKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

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
ctBillingMode :: Lens.Lens' CreateTable (Core.Maybe Types.BillingMode)
ctBillingMode = Lens.field @"billingMode"
{-# DEPRECATED ctBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

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
ctGlobalSecondaryIndexes :: Lens.Lens' CreateTable (Core.Maybe [Types.GlobalSecondaryIndex])
ctGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
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
ctLocalSecondaryIndexes :: Lens.Lens' CreateTable (Core.Maybe [Types.LocalSecondaryIndex])
ctLocalSecondaryIndexes = Lens.field @"localSecondaryIndexes"
{-# DEPRECATED ctLocalSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead." #-}

-- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
--
-- If you set BillingMode as @PROVISIONED@ , you must specify this property. If you set BillingMode as @PAY_PER_REQUEST@ , you cannot specify this property.
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctProvisionedThroughput :: Lens.Lens' CreateTable (Core.Maybe Types.ProvisionedThroughput)
ctProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# DEPRECATED ctProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | Represents the settings used to enable server-side encryption.
--
-- /Note:/ Consider using 'sSESpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSSESpecification :: Lens.Lens' CreateTable (Core.Maybe Types.SSESpecification)
ctSSESpecification = Lens.field @"sSESpecification"
{-# DEPRECATED ctSSESpecification "Use generic-lens or generic-optics with 'sSESpecification' instead." #-}

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
ctStreamSpecification :: Lens.Lens' CreateTable (Core.Maybe Types.StreamSpecification)
ctStreamSpecification = Lens.field @"streamSpecification"
{-# DEPRECATED ctStreamSpecification "Use generic-lens or generic-optics with 'streamSpecification' instead." #-}

-- | A list of key-value pairs to label the table. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTable (Core.Maybe [Types.Tag])
ctTags = Lens.field @"tags"
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateTable where
  toJSON CreateTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AttributeDefinitions" Core..= attributeDefinitions),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("KeySchema" Core..= keySchema),
            ("BillingMode" Core..=) Core.<$> billingMode,
            ("GlobalSecondaryIndexes" Core..=) Core.<$> globalSecondaryIndexes,
            ("LocalSecondaryIndexes" Core..=) Core.<$> localSecondaryIndexes,
            ("ProvisionedThroughput" Core..=) Core.<$> provisionedThroughput,
            ("SSESpecification" Core..=) Core.<$> sSESpecification,
            ("StreamSpecification" Core..=) Core.<$> streamSpecification,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateTable where
  type Rs CreateTable = CreateTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.CreateTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTableResponse'
            Core.<$> (x Core..:? "TableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @CreateTable@ operation.
--
-- /See:/ 'mkCreateTableResponse' smart constructor.
data CreateTableResponse = CreateTableResponse'
  { -- | Represents the properties of the table.
    tableDescription :: Core.Maybe Types.TableDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTableResponse' value with any optional fields omitted.
mkCreateTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTableResponse
mkCreateTableResponse responseStatus =
  CreateTableResponse'
    { tableDescription = Core.Nothing,
      responseStatus
    }

-- | Represents the properties of the table.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsTableDescription :: Lens.Lens' CreateTableResponse (Core.Maybe Types.TableDescription)
ctrrsTableDescription = Lens.field @"tableDescription"
{-# DEPRECATED ctrrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTableResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
