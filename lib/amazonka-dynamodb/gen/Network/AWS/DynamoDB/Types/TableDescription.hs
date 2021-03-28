{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.TableDescription
  ( TableDescription (..)
  -- * Smart constructor
  , mkTableDescription
  -- * Lenses
  , tdArchivalSummary
  , tdAttributeDefinitions
  , tdBillingModeSummary
  , tdCreationDateTime
  , tdGlobalSecondaryIndexes
  , tdGlobalTableVersion
  , tdItemCount
  , tdKeySchema
  , tdLatestStreamArn
  , tdLatestStreamLabel
  , tdLocalSecondaryIndexes
  , tdProvisionedThroughput
  , tdReplicas
  , tdRestoreSummary
  , tdSSEDescription
  , tdStreamSpecification
  , tdTableArn
  , tdTableId
  , tdTableName
  , tdTableSizeBytes
  , tdTableStatus
  ) where

import qualified Network.AWS.DynamoDB.Types.ArchivalSummary as Types
import qualified Network.AWS.DynamoDB.Types.AttributeDefinition as Types
import qualified Network.AWS.DynamoDB.Types.BillingModeSummary as Types
import qualified Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.LatestStreamArn as Types
import qualified Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaDescription as Types
import qualified Network.AWS.DynamoDB.Types.RestoreSummary as Types
import qualified Network.AWS.DynamoDB.Types.SSEDescription as Types
import qualified Network.AWS.DynamoDB.Types.StreamSpecification as Types
import qualified Network.AWS.DynamoDB.Types.TableId as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.DynamoDB.Types.TableStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a table.
--
-- /See:/ 'mkTableDescription' smart constructor.
data TableDescription = TableDescription'
  { archivalSummary :: Core.Maybe Types.ArchivalSummary
    -- ^ Contains information about the table archive.
  , attributeDefinitions :: Core.Maybe [Types.AttributeDefinition]
    -- ^ An array of @AttributeDefinition@ objects. Each of these objects describes one attribute in the table and index key schema.
--
-- Each @AttributeDefinition@ object in this array is composed of:
--
--     * @AttributeName@ - The name of the attribute.
--
--
--     * @AttributeType@ - The data type for the attribute.
--
--
  , billingModeSummary :: Core.Maybe Types.BillingModeSummary
    -- ^ Contains the details for the read/write capacity mode.
  , creationDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
  , globalSecondaryIndexes :: Core.Maybe [Types.GlobalSecondaryIndexDescription]
    -- ^ The global secondary indexes, if any, on the table. Each index is scoped to a given partition key value. Each element is composed of:
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
  , globalTableVersion :: Core.Maybe Core.Text
    -- ^ Represents the version of <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables> in use, if the table is replicated across AWS Regions.
  , itemCount :: Core.Maybe Core.Integer
    -- ^ The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
  , keySchema :: Core.Maybe (Core.NonEmpty Types.KeySchemaElement)
    -- ^ The primary key structure for the table. Each @KeySchemaElement@ consists of:
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
  , latestStreamArn :: Core.Maybe Types.LatestStreamArn
    -- ^ The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
  , latestStreamLabel :: Core.Maybe Core.Text
    -- ^ A timestamp, in ISO 8601 format, for this stream.
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
  , localSecondaryIndexes :: Core.Maybe [Types.LocalSecondaryIndexDescription]
    -- ^ Represents one or more local secondary indexes on the table. Each index is scoped to a given partition key value. Tables with one or more local secondary indexes are subject to an item collection size limit, where the amount of data within a given item collection cannot exceed 10 GB. Each element is composed of:
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
  , provisionedThroughput :: Core.Maybe Types.ProvisionedThroughputDescription
    -- ^ The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
  , replicas :: Core.Maybe [Types.ReplicaDescription]
    -- ^ Represents replicas of the table.
  , restoreSummary :: Core.Maybe Types.RestoreSummary
    -- ^ Contains details for the restore.
  , sSEDescription :: Core.Maybe Types.SSEDescription
    -- ^ The description of the server-side encryption status on the specified table.
  , streamSpecification :: Core.Maybe Types.StreamSpecification
    -- ^ The current DynamoDB Streams configuration for the table.
  , tableArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) that uniquely identifies the table.
  , tableId :: Core.Maybe Types.TableId
    -- ^ Unique identifier for the table for which the backup was created. 
  , tableName :: Core.Maybe Types.TableName
    -- ^ The name of the table.
  , tableSizeBytes :: Core.Maybe Core.Integer
    -- ^ The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
  , tableStatus :: Core.Maybe Types.TableStatus
    -- ^ The current state of the table:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TableDescription' value with any optional fields omitted.
mkTableDescription
    :: TableDescription
mkTableDescription
  = TableDescription'{archivalSummary = Core.Nothing,
                      attributeDefinitions = Core.Nothing,
                      billingModeSummary = Core.Nothing, creationDateTime = Core.Nothing,
                      globalSecondaryIndexes = Core.Nothing,
                      globalTableVersion = Core.Nothing, itemCount = Core.Nothing,
                      keySchema = Core.Nothing, latestStreamArn = Core.Nothing,
                      latestStreamLabel = Core.Nothing,
                      localSecondaryIndexes = Core.Nothing,
                      provisionedThroughput = Core.Nothing, replicas = Core.Nothing,
                      restoreSummary = Core.Nothing, sSEDescription = Core.Nothing,
                      streamSpecification = Core.Nothing, tableArn = Core.Nothing,
                      tableId = Core.Nothing, tableName = Core.Nothing,
                      tableSizeBytes = Core.Nothing, tableStatus = Core.Nothing}

-- | Contains information about the table archive.
--
-- /Note:/ Consider using 'archivalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdArchivalSummary :: Lens.Lens' TableDescription (Core.Maybe Types.ArchivalSummary)
tdArchivalSummary = Lens.field @"archivalSummary"
{-# INLINEABLE tdArchivalSummary #-}
{-# DEPRECATED archivalSummary "Use generic-lens or generic-optics with 'archivalSummary' instead"  #-}

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
tdAttributeDefinitions :: Lens.Lens' TableDescription (Core.Maybe [Types.AttributeDefinition])
tdAttributeDefinitions = Lens.field @"attributeDefinitions"
{-# INLINEABLE tdAttributeDefinitions #-}
{-# DEPRECATED attributeDefinitions "Use generic-lens or generic-optics with 'attributeDefinitions' instead"  #-}

-- | Contains the details for the read/write capacity mode.
--
-- /Note:/ Consider using 'billingModeSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdBillingModeSummary :: Lens.Lens' TableDescription (Core.Maybe Types.BillingModeSummary)
tdBillingModeSummary = Lens.field @"billingModeSummary"
{-# INLINEABLE tdBillingModeSummary #-}
{-# DEPRECATED billingModeSummary "Use generic-lens or generic-optics with 'billingModeSummary' instead"  #-}

-- | The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdCreationDateTime :: Lens.Lens' TableDescription (Core.Maybe Core.NominalDiffTime)
tdCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE tdCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

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
tdGlobalSecondaryIndexes :: Lens.Lens' TableDescription (Core.Maybe [Types.GlobalSecondaryIndexDescription])
tdGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
{-# INLINEABLE tdGlobalSecondaryIndexes #-}
{-# DEPRECATED globalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead"  #-}

-- | Represents the version of <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables> in use, if the table is replicated across AWS Regions.
--
-- /Note:/ Consider using 'globalTableVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdGlobalTableVersion :: Lens.Lens' TableDescription (Core.Maybe Core.Text)
tdGlobalTableVersion = Lens.field @"globalTableVersion"
{-# INLINEABLE tdGlobalTableVersion #-}
{-# DEPRECATED globalTableVersion "Use generic-lens or generic-optics with 'globalTableVersion' instead"  #-}

-- | The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdItemCount :: Lens.Lens' TableDescription (Core.Maybe Core.Integer)
tdItemCount = Lens.field @"itemCount"
{-# INLINEABLE tdItemCount #-}
{-# DEPRECATED itemCount "Use generic-lens or generic-optics with 'itemCount' instead"  #-}

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
tdKeySchema :: Lens.Lens' TableDescription (Core.Maybe (Core.NonEmpty Types.KeySchemaElement))
tdKeySchema = Lens.field @"keySchema"
{-# INLINEABLE tdKeySchema #-}
{-# DEPRECATED keySchema "Use generic-lens or generic-optics with 'keySchema' instead"  #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
--
-- /Note:/ Consider using 'latestStreamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdLatestStreamArn :: Lens.Lens' TableDescription (Core.Maybe Types.LatestStreamArn)
tdLatestStreamArn = Lens.field @"latestStreamArn"
{-# INLINEABLE tdLatestStreamArn #-}
{-# DEPRECATED latestStreamArn "Use generic-lens or generic-optics with 'latestStreamArn' instead"  #-}

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
tdLatestStreamLabel :: Lens.Lens' TableDescription (Core.Maybe Core.Text)
tdLatestStreamLabel = Lens.field @"latestStreamLabel"
{-# INLINEABLE tdLatestStreamLabel #-}
{-# DEPRECATED latestStreamLabel "Use generic-lens or generic-optics with 'latestStreamLabel' instead"  #-}

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
tdLocalSecondaryIndexes :: Lens.Lens' TableDescription (Core.Maybe [Types.LocalSecondaryIndexDescription])
tdLocalSecondaryIndexes = Lens.field @"localSecondaryIndexes"
{-# INLINEABLE tdLocalSecondaryIndexes #-}
{-# DEPRECATED localSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead"  #-}

-- | The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdProvisionedThroughput :: Lens.Lens' TableDescription (Core.Maybe Types.ProvisionedThroughputDescription)
tdProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# INLINEABLE tdProvisionedThroughput #-}
{-# DEPRECATED provisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead"  #-}

-- | Represents replicas of the table.
--
-- /Note:/ Consider using 'replicas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdReplicas :: Lens.Lens' TableDescription (Core.Maybe [Types.ReplicaDescription])
tdReplicas = Lens.field @"replicas"
{-# INLINEABLE tdReplicas #-}
{-# DEPRECATED replicas "Use generic-lens or generic-optics with 'replicas' instead"  #-}

-- | Contains details for the restore.
--
-- /Note:/ Consider using 'restoreSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRestoreSummary :: Lens.Lens' TableDescription (Core.Maybe Types.RestoreSummary)
tdRestoreSummary = Lens.field @"restoreSummary"
{-# INLINEABLE tdRestoreSummary #-}
{-# DEPRECATED restoreSummary "Use generic-lens or generic-optics with 'restoreSummary' instead"  #-}

-- | The description of the server-side encryption status on the specified table.
--
-- /Note:/ Consider using 'sSEDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdSSEDescription :: Lens.Lens' TableDescription (Core.Maybe Types.SSEDescription)
tdSSEDescription = Lens.field @"sSEDescription"
{-# INLINEABLE tdSSEDescription #-}
{-# DEPRECATED sSEDescription "Use generic-lens or generic-optics with 'sSEDescription' instead"  #-}

-- | The current DynamoDB Streams configuration for the table.
--
-- /Note:/ Consider using 'streamSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdStreamSpecification :: Lens.Lens' TableDescription (Core.Maybe Types.StreamSpecification)
tdStreamSpecification = Lens.field @"streamSpecification"
{-# INLINEABLE tdStreamSpecification #-}
{-# DEPRECATED streamSpecification "Use generic-lens or generic-optics with 'streamSpecification' instead"  #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the table.
--
-- /Note:/ Consider using 'tableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableArn :: Lens.Lens' TableDescription (Core.Maybe Core.Text)
tdTableArn = Lens.field @"tableArn"
{-# INLINEABLE tdTableArn #-}
{-# DEPRECATED tableArn "Use generic-lens or generic-optics with 'tableArn' instead"  #-}

-- | Unique identifier for the table for which the backup was created. 
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableId :: Lens.Lens' TableDescription (Core.Maybe Types.TableId)
tdTableId = Lens.field @"tableId"
{-# INLINEABLE tdTableId #-}
{-# DEPRECATED tableId "Use generic-lens or generic-optics with 'tableId' instead"  #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableName :: Lens.Lens' TableDescription (Core.Maybe Types.TableName)
tdTableName = Lens.field @"tableName"
{-# INLINEABLE tdTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'tableSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTableSizeBytes :: Lens.Lens' TableDescription (Core.Maybe Core.Integer)
tdTableSizeBytes = Lens.field @"tableSizeBytes"
{-# INLINEABLE tdTableSizeBytes #-}
{-# DEPRECATED tableSizeBytes "Use generic-lens or generic-optics with 'tableSizeBytes' instead"  #-}

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
tdTableStatus :: Lens.Lens' TableDescription (Core.Maybe Types.TableStatus)
tdTableStatus = Lens.field @"tableStatus"
{-# INLINEABLE tdTableStatus #-}
{-# DEPRECATED tableStatus "Use generic-lens or generic-optics with 'tableStatus' instead"  #-}

instance Core.FromJSON TableDescription where
        parseJSON
          = Core.withObject "TableDescription" Core.$
              \ x ->
                TableDescription' Core.<$>
                  (x Core..:? "ArchivalSummary") Core.<*>
                    x Core..:? "AttributeDefinitions"
                    Core.<*> x Core..:? "BillingModeSummary"
                    Core.<*> x Core..:? "CreationDateTime"
                    Core.<*> x Core..:? "GlobalSecondaryIndexes"
                    Core.<*> x Core..:? "GlobalTableVersion"
                    Core.<*> x Core..:? "ItemCount"
                    Core.<*> x Core..:? "KeySchema"
                    Core.<*> x Core..:? "LatestStreamArn"
                    Core.<*> x Core..:? "LatestStreamLabel"
                    Core.<*> x Core..:? "LocalSecondaryIndexes"
                    Core.<*> x Core..:? "ProvisionedThroughput"
                    Core.<*> x Core..:? "Replicas"
                    Core.<*> x Core..:? "RestoreSummary"
                    Core.<*> x Core..:? "SSEDescription"
                    Core.<*> x Core..:? "StreamSpecification"
                    Core.<*> x Core..:? "TableArn"
                    Core.<*> x Core..:? "TableId"
                    Core.<*> x Core..:? "TableName"
                    Core.<*> x Core..:? "TableSizeBytes"
                    Core.<*> x Core..:? "TableStatus"
