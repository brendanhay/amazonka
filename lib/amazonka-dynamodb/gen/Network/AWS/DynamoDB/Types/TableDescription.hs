{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TableDescription
  ( TableDescription (..),

    -- * Smart constructor
    mkTableDescription,

    -- * Lenses
    tdRestoreSummary,
    tdGlobalTableVersion,
    tdTableSizeBytes,
    tdAttributeDefinitions,
    tdLatestStreamARN,
    tdProvisionedThroughput,
    tdTableStatus,
    tdTableARN,
    tdKeySchema,
    tdGlobalSecondaryIndexes,
    tdLatestStreamLabel,
    tdBillingModeSummary,
    tdLocalSecondaryIndexes,
    tdCreationDateTime,
    tdSSEDescription,
    tdTableId,
    tdReplicas,
    tdItemCount,
    tdArchivalSummary,
    tdTableName,
    tdStreamSpecification,
  )
where

import Network.AWS.DynamoDB.Types.ArchivalSummary
import Network.AWS.DynamoDB.Types.AttributeDefinition
import Network.AWS.DynamoDB.Types.BillingModeSummary
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
import Network.AWS.DynamoDB.Types.ReplicaDescription
import Network.AWS.DynamoDB.Types.RestoreSummary
import Network.AWS.DynamoDB.Types.SSEDescription
import Network.AWS.DynamoDB.Types.StreamSpecification
import Network.AWS.DynamoDB.Types.TableStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a table.
--
-- /See:/ 'mkTableDescription' smart constructor.
data TableDescription = TableDescription'
  { restoreSummary ::
      Lude.Maybe RestoreSummary,
    globalTableVersion :: Lude.Maybe Lude.Text,
    tableSizeBytes :: Lude.Maybe Lude.Integer,
    attributeDefinitions :: Lude.Maybe [AttributeDefinition],
    latestStreamARN :: Lude.Maybe Lude.Text,
    provisionedThroughput ::
      Lude.Maybe ProvisionedThroughputDescription,
    tableStatus :: Lude.Maybe TableStatus,
    tableARN :: Lude.Maybe Lude.Text,
    keySchema :: Lude.Maybe (Lude.NonEmpty KeySchemaElement),
    globalSecondaryIndexes ::
      Lude.Maybe [GlobalSecondaryIndexDescription],
    latestStreamLabel :: Lude.Maybe Lude.Text,
    billingModeSummary :: Lude.Maybe BillingModeSummary,
    localSecondaryIndexes ::
      Lude.Maybe [LocalSecondaryIndexDescription],
    creationDateTime :: Lude.Maybe Lude.Timestamp,
    sSEDescription :: Lude.Maybe SSEDescription,
    tableId :: Lude.Maybe Lude.Text,
    replicas :: Lude.Maybe [ReplicaDescription],
    itemCount :: Lude.Maybe Lude.Integer,
    archivalSummary :: Lude.Maybe ArchivalSummary,
    tableName :: Lude.Maybe Lude.Text,
    streamSpecification :: Lude.Maybe StreamSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableDescription' with the minimum fields required to make a request.
--
-- * 'archivalSummary' - Contains information about the table archive.
-- * 'attributeDefinitions' - An array of @AttributeDefinition@ objects. Each of these objects describes one attribute in the table and index key schema.
--
-- Each @AttributeDefinition@ object in this array is composed of:
--
--     * @AttributeName@ - The name of the attribute.
--
--
--     * @AttributeType@ - The data type for the attribute.
--
--
-- * 'billingModeSummary' - Contains the details for the read/write capacity mode.
-- * 'creationDateTime' - The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
-- * 'globalSecondaryIndexes' - The global secondary indexes, if any, on the table. Each index is scoped to a given partition key value. Each element is composed of:
--
--
--     * @Backfilling@ - If true, then the index is currently in the backfilling phase. Backfilling occurs only when a new global secondary index is added to the table. It is the process by which DynamoDB populates the new index with data from the table. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)
-- You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)
--
--
--     * @IndexName@ - The name of the global secondary index.
--
--
--     * @IndexSizeBytes@ - The total size of the global secondary index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
--     * @IndexStatus@ - The current status of the global secondary index:
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
--
--
--     * @ItemCount@ - The number of items in the global secondary index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
--     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.
--
--
--     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
--
--     * @ProjectionType@ - One of the following:
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
--
--
--
--
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
--     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units, along with data about increases and decreases.
--
--
-- If the table is in the @DELETING@ state, no information about indexes will be returned.
-- * 'globalTableVersion' - Represents the version of <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables> in use, if the table is replicated across AWS Regions.
-- * 'itemCount' - The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
-- * 'keySchema' - The primary key structure for the table. Each @KeySchemaElement@ consists of:
--
--
--     * @AttributeName@ - The name of the attribute.
--
--
--     * @KeyType@ - The role of the attribute:
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
--
--
-- For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
-- * 'latestStreamARN' - The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
-- * 'latestStreamLabel' - A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:
--
--     * AWS customer ID
--
--
--     * Table name
--
--
--     * @StreamLabel@
--
--
-- * 'localSecondaryIndexes' - Represents one or more local secondary indexes on the table. Each index is scoped to a given partition key value. Tables with one or more local secondary indexes are subject to an item collection size limit, where the amount of data within a given item collection cannot exceed 10 GB. Each element is composed of:
--
--
--     * @IndexName@ - The name of the local secondary index.
--
--
--     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.
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
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
--     * @IndexSizeBytes@ - Represents the total size of the index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
--     * @ItemCount@ - Represents the number of items in the index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
-- If the table is in the @DELETING@ state, no information about indexes will be returned.
-- * 'provisionedThroughput' - The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
-- * 'replicas' - Represents replicas of the table.
-- * 'restoreSummary' - Contains details for the restore.
-- * 'sSEDescription' - The description of the server-side encryption status on the specified table.
-- * 'streamSpecification' - The current DynamoDB Streams configuration for the table.
-- * 'tableARN' - The Amazon Resource Name (ARN) that uniquely identifies the table.
-- * 'tableId' - Unique identifier for the table for which the backup was created.
-- * 'tableName' - The name of the table.
-- * 'tableSizeBytes' - The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
-- * 'tableStatus' - The current state of the table:
--
--
--     * @CREATING@ - The table is being created.
--
--
--     * @UPDATING@ - The table is being updated.
--
--
--     * @DELETING@ - The table is being deleted.
--
--
--     * @ACTIVE@ - The table is ready for use.
--
--
--     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The AWS KMS key used to encrypt the table in inaccessible. Table operations may fail due to failure to use the AWS KMS key. DynamoDB will initiate the table archival process when a table's AWS KMS key remains inaccessible for more than seven days.
--
--
--     * @ARCHIVING@ - The table is being archived. Operations are not allowed until archival is complete.
--
--
--     * @ARCHIVED@ - The table has been archived. See the ArchivalReason for more information.
mkTableDescription ::
  TableDescription
mkTableDescription =
  TableDescription'
    { restoreSummary = Lude.Nothing,
      globalTableVersion = Lude.Nothing,
      tableSizeBytes = Lude.Nothing,
      attributeDefinitions = Lude.Nothing,
      latestStreamARN = Lude.Nothing,
      provisionedThroughput = Lude.Nothing,
      tableStatus = Lude.Nothing,
      tableARN = Lude.Nothing,
      keySchema = Lude.Nothing,
      globalSecondaryIndexes = Lude.Nothing,
      latestStreamLabel = Lude.Nothing,
      billingModeSummary = Lude.Nothing,
      localSecondaryIndexes = Lude.Nothing,
      creationDateTime = Lude.Nothing,
      sSEDescription = Lude.Nothing,
      tableId = Lude.Nothing,
      replicas = Lude.Nothing,
      itemCount = Lude.Nothing,
      archivalSummary = Lude.Nothing,
      tableName = Lude.Nothing,
      streamSpecification = Lude.Nothing
    }

-- | Contains details for the restore.
--
-- /Note:/ Consider using 'restoreSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRestoreSummary :: Lens.Lens' TableDescription (Lude.Maybe RestoreSummary)
tdRestoreSummary = Lens.lens (restoreSummary :: TableDescription -> Lude.Maybe RestoreSummary) (\s a -> s {restoreSummary = a} :: TableDescription)
{-# DEPRECATED tdRestoreSummary "Use generic-lens or generic-optics with 'restoreSummary' instead." #-}

-- | Represents the version of <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables> in use, if the table is replicated across AWS Regions.
--
-- /Note:/ Consider using 'globalTableVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdGlobalTableVersion :: Lens.Lens' TableDescription (Lude.Maybe Lude.Text)
tdGlobalTableVersion = Lens.lens (globalTableVersion :: TableDescription -> Lude.Maybe Lude.Text) (\s a -> s {globalTableVersion = a} :: TableDescription)
{-# DEPRECATED tdGlobalTableVersion "Use generic-lens or generic-optics with 'globalTableVersion' instead." #-}

-- | The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'tableSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableSizeBytes :: Lens.Lens' TableDescription (Lude.Maybe Lude.Integer)
tdTableSizeBytes = Lens.lens (tableSizeBytes :: TableDescription -> Lude.Maybe Lude.Integer) (\s a -> s {tableSizeBytes = a} :: TableDescription)
{-# DEPRECATED tdTableSizeBytes "Use generic-lens or generic-optics with 'tableSizeBytes' instead." #-}

-- | An array of @AttributeDefinition@ objects. Each of these objects describes one attribute in the table and index key schema.
--
-- Each @AttributeDefinition@ object in this array is composed of:
--
--     * @AttributeName@ - The name of the attribute.
--
--
--     * @AttributeType@ - The data type for the attribute.
--
--
--
-- /Note:/ Consider using 'attributeDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAttributeDefinitions :: Lens.Lens' TableDescription (Lude.Maybe [AttributeDefinition])
tdAttributeDefinitions = Lens.lens (attributeDefinitions :: TableDescription -> Lude.Maybe [AttributeDefinition]) (\s a -> s {attributeDefinitions = a} :: TableDescription)
{-# DEPRECATED tdAttributeDefinitions "Use generic-lens or generic-optics with 'attributeDefinitions' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
--
-- /Note:/ Consider using 'latestStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdLatestStreamARN :: Lens.Lens' TableDescription (Lude.Maybe Lude.Text)
tdLatestStreamARN = Lens.lens (latestStreamARN :: TableDescription -> Lude.Maybe Lude.Text) (\s a -> s {latestStreamARN = a} :: TableDescription)
{-# DEPRECATED tdLatestStreamARN "Use generic-lens or generic-optics with 'latestStreamARN' instead." #-}

-- | The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdProvisionedThroughput :: Lens.Lens' TableDescription (Lude.Maybe ProvisionedThroughputDescription)
tdProvisionedThroughput = Lens.lens (provisionedThroughput :: TableDescription -> Lude.Maybe ProvisionedThroughputDescription) (\s a -> s {provisionedThroughput = a} :: TableDescription)
{-# DEPRECATED tdProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | The current state of the table:
--
--
--     * @CREATING@ - The table is being created.
--
--
--     * @UPDATING@ - The table is being updated.
--
--
--     * @DELETING@ - The table is being deleted.
--
--
--     * @ACTIVE@ - The table is ready for use.
--
--
--     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The AWS KMS key used to encrypt the table in inaccessible. Table operations may fail due to failure to use the AWS KMS key. DynamoDB will initiate the table archival process when a table's AWS KMS key remains inaccessible for more than seven days.
--
--
--     * @ARCHIVING@ - The table is being archived. Operations are not allowed until archival is complete.
--
--
--     * @ARCHIVED@ - The table has been archived. See the ArchivalReason for more information.
--
--
--
-- /Note:/ Consider using 'tableStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableStatus :: Lens.Lens' TableDescription (Lude.Maybe TableStatus)
tdTableStatus = Lens.lens (tableStatus :: TableDescription -> Lude.Maybe TableStatus) (\s a -> s {tableStatus = a} :: TableDescription)
{-# DEPRECATED tdTableStatus "Use generic-lens or generic-optics with 'tableStatus' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the table.
--
-- /Note:/ Consider using 'tableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableARN :: Lens.Lens' TableDescription (Lude.Maybe Lude.Text)
tdTableARN = Lens.lens (tableARN :: TableDescription -> Lude.Maybe Lude.Text) (\s a -> s {tableARN = a} :: TableDescription)
{-# DEPRECATED tdTableARN "Use generic-lens or generic-optics with 'tableARN' instead." #-}

-- | The primary key structure for the table. Each @KeySchemaElement@ consists of:
--
--
--     * @AttributeName@ - The name of the attribute.
--
--
--     * @KeyType@ - The role of the attribute:
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
--
--
-- For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdKeySchema :: Lens.Lens' TableDescription (Lude.Maybe (Lude.NonEmpty KeySchemaElement))
tdKeySchema = Lens.lens (keySchema :: TableDescription -> Lude.Maybe (Lude.NonEmpty KeySchemaElement)) (\s a -> s {keySchema = a} :: TableDescription)
{-# DEPRECATED tdKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | The global secondary indexes, if any, on the table. Each index is scoped to a given partition key value. Each element is composed of:
--
--
--     * @Backfilling@ - If true, then the index is currently in the backfilling phase. Backfilling occurs only when a new global secondary index is added to the table. It is the process by which DynamoDB populates the new index with data from the table. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)
-- You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)
--
--
--     * @IndexName@ - The name of the global secondary index.
--
--
--     * @IndexSizeBytes@ - The total size of the global secondary index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
--     * @IndexStatus@ - The current status of the global secondary index:
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
--
--
--     * @ItemCount@ - The number of items in the global secondary index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
--     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.
--
--
--     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:
--
--     * @ProjectionType@ - One of the following:
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
--
--
--
--
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
--     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units, along with data about increases and decreases.
--
--
-- If the table is in the @DELETING@ state, no information about indexes will be returned.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdGlobalSecondaryIndexes :: Lens.Lens' TableDescription (Lude.Maybe [GlobalSecondaryIndexDescription])
tdGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: TableDescription -> Lude.Maybe [GlobalSecondaryIndexDescription]) (\s a -> s {globalSecondaryIndexes = a} :: TableDescription)
{-# DEPRECATED tdGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:
--
--     * AWS customer ID
--
--
--     * Table name
--
--
--     * @StreamLabel@
--
--
--
-- /Note:/ Consider using 'latestStreamLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdLatestStreamLabel :: Lens.Lens' TableDescription (Lude.Maybe Lude.Text)
tdLatestStreamLabel = Lens.lens (latestStreamLabel :: TableDescription -> Lude.Maybe Lude.Text) (\s a -> s {latestStreamLabel = a} :: TableDescription)
{-# DEPRECATED tdLatestStreamLabel "Use generic-lens or generic-optics with 'latestStreamLabel' instead." #-}

-- | Contains the details for the read/write capacity mode.
--
-- /Note:/ Consider using 'billingModeSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdBillingModeSummary :: Lens.Lens' TableDescription (Lude.Maybe BillingModeSummary)
tdBillingModeSummary = Lens.lens (billingModeSummary :: TableDescription -> Lude.Maybe BillingModeSummary) (\s a -> s {billingModeSummary = a} :: TableDescription)
{-# DEPRECATED tdBillingModeSummary "Use generic-lens or generic-optics with 'billingModeSummary' instead." #-}

-- | Represents one or more local secondary indexes on the table. Each index is scoped to a given partition key value. Tables with one or more local secondary indexes are subject to an item collection size limit, where the amount of data within a given item collection cannot exceed 10 GB. Each element is composed of:
--
--
--     * @IndexName@ - The name of the local secondary index.
--
--
--     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.
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
--     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
--
--
--
--     * @IndexSizeBytes@ - Represents the total size of the index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
--     * @ItemCount@ - Represents the number of items in the index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
--
-- If the table is in the @DELETING@ state, no information about indexes will be returned.
--
-- /Note:/ Consider using 'localSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdLocalSecondaryIndexes :: Lens.Lens' TableDescription (Lude.Maybe [LocalSecondaryIndexDescription])
tdLocalSecondaryIndexes = Lens.lens (localSecondaryIndexes :: TableDescription -> Lude.Maybe [LocalSecondaryIndexDescription]) (\s a -> s {localSecondaryIndexes = a} :: TableDescription)
{-# DEPRECATED tdLocalSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead." #-}

-- | The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdCreationDateTime :: Lens.Lens' TableDescription (Lude.Maybe Lude.Timestamp)
tdCreationDateTime = Lens.lens (creationDateTime :: TableDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: TableDescription)
{-# DEPRECATED tdCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The description of the server-side encryption status on the specified table.
--
-- /Note:/ Consider using 'sSEDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdSSEDescription :: Lens.Lens' TableDescription (Lude.Maybe SSEDescription)
tdSSEDescription = Lens.lens (sSEDescription :: TableDescription -> Lude.Maybe SSEDescription) (\s a -> s {sSEDescription = a} :: TableDescription)
{-# DEPRECATED tdSSEDescription "Use generic-lens or generic-optics with 'sSEDescription' instead." #-}

-- | Unique identifier for the table for which the backup was created.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableId :: Lens.Lens' TableDescription (Lude.Maybe Lude.Text)
tdTableId = Lens.lens (tableId :: TableDescription -> Lude.Maybe Lude.Text) (\s a -> s {tableId = a} :: TableDescription)
{-# DEPRECATED tdTableId "Use generic-lens or generic-optics with 'tableId' instead." #-}

-- | Represents replicas of the table.
--
-- /Note:/ Consider using 'replicas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdReplicas :: Lens.Lens' TableDescription (Lude.Maybe [ReplicaDescription])
tdReplicas = Lens.lens (replicas :: TableDescription -> Lude.Maybe [ReplicaDescription]) (\s a -> s {replicas = a} :: TableDescription)
{-# DEPRECATED tdReplicas "Use generic-lens or generic-optics with 'replicas' instead." #-}

-- | The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdItemCount :: Lens.Lens' TableDescription (Lude.Maybe Lude.Integer)
tdItemCount = Lens.lens (itemCount :: TableDescription -> Lude.Maybe Lude.Integer) (\s a -> s {itemCount = a} :: TableDescription)
{-# DEPRECATED tdItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | Contains information about the table archive.
--
-- /Note:/ Consider using 'archivalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdArchivalSummary :: Lens.Lens' TableDescription (Lude.Maybe ArchivalSummary)
tdArchivalSummary = Lens.lens (archivalSummary :: TableDescription -> Lude.Maybe ArchivalSummary) (\s a -> s {archivalSummary = a} :: TableDescription)
{-# DEPRECATED tdArchivalSummary "Use generic-lens or generic-optics with 'archivalSummary' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableName :: Lens.Lens' TableDescription (Lude.Maybe Lude.Text)
tdTableName = Lens.lens (tableName :: TableDescription -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: TableDescription)
{-# DEPRECATED tdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The current DynamoDB Streams configuration for the table.
--
-- /Note:/ Consider using 'streamSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdStreamSpecification :: Lens.Lens' TableDescription (Lude.Maybe StreamSpecification)
tdStreamSpecification = Lens.lens (streamSpecification :: TableDescription -> Lude.Maybe StreamSpecification) (\s a -> s {streamSpecification = a} :: TableDescription)
{-# DEPRECATED tdStreamSpecification "Use generic-lens or generic-optics with 'streamSpecification' instead." #-}

instance Lude.FromJSON TableDescription where
  parseJSON =
    Lude.withObject
      "TableDescription"
      ( \x ->
          TableDescription'
            Lude.<$> (x Lude..:? "RestoreSummary")
            Lude.<*> (x Lude..:? "GlobalTableVersion")
            Lude.<*> (x Lude..:? "TableSizeBytes")
            Lude.<*> (x Lude..:? "AttributeDefinitions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LatestStreamArn")
            Lude.<*> (x Lude..:? "ProvisionedThroughput")
            Lude.<*> (x Lude..:? "TableStatus")
            Lude.<*> (x Lude..:? "TableArn")
            Lude.<*> (x Lude..:? "KeySchema")
            Lude.<*> (x Lude..:? "GlobalSecondaryIndexes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LatestStreamLabel")
            Lude.<*> (x Lude..:? "BillingModeSummary")
            Lude.<*> (x Lude..:? "LocalSecondaryIndexes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreationDateTime")
            Lude.<*> (x Lude..:? "SSEDescription")
            Lude.<*> (x Lude..:? "TableId")
            Lude.<*> (x Lude..:? "Replicas" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ItemCount")
            Lude.<*> (x Lude..:? "ArchivalSummary")
            Lude.<*> (x Lude..:? "TableName")
            Lude.<*> (x Lude..:? "StreamSpecification")
      )
