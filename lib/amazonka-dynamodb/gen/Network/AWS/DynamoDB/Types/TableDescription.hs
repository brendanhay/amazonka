{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TableDescription where

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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a table.
--
--
--
-- /See:/ 'tableDescription' smart constructor.
data TableDescription = TableDescription'
  { _tdRestoreSummary ::
      !(Maybe RestoreSummary),
    _tdGlobalTableVersion :: !(Maybe Text),
    _tdTableSizeBytes :: !(Maybe Integer),
    _tdAttributeDefinitions :: !(Maybe [AttributeDefinition]),
    _tdLatestStreamARN :: !(Maybe Text),
    _tdProvisionedThroughput ::
      !(Maybe ProvisionedThroughputDescription),
    _tdTableStatus :: !(Maybe TableStatus),
    _tdTableARN :: !(Maybe Text),
    _tdKeySchema :: !(Maybe (List1 KeySchemaElement)),
    _tdGlobalSecondaryIndexes ::
      !(Maybe [GlobalSecondaryIndexDescription]),
    _tdLatestStreamLabel :: !(Maybe Text),
    _tdBillingModeSummary :: !(Maybe BillingModeSummary),
    _tdLocalSecondaryIndexes ::
      !(Maybe [LocalSecondaryIndexDescription]),
    _tdCreationDateTime :: !(Maybe POSIX),
    _tdSSEDescription :: !(Maybe SSEDescription),
    _tdTableId :: !(Maybe Text),
    _tdReplicas :: !(Maybe [ReplicaDescription]),
    _tdItemCount :: !(Maybe Integer),
    _tdArchivalSummary :: !(Maybe ArchivalSummary),
    _tdTableName :: !(Maybe Text),
    _tdStreamSpecification :: !(Maybe StreamSpecification)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdRestoreSummary' - Contains details for the restore.
--
-- * 'tdGlobalTableVersion' - Represents the version of <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables> in use, if the table is replicated across AWS Regions.
--
-- * 'tdTableSizeBytes' - The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'tdAttributeDefinitions' - An array of @AttributeDefinition@ objects. Each of these objects describes one attribute in the table and index key schema. Each @AttributeDefinition@ object in this array is composed of:     * @AttributeName@ - The name of the attribute.     * @AttributeType@ - The data type for the attribute.
--
-- * 'tdLatestStreamARN' - The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
--
-- * 'tdProvisionedThroughput' - The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
-- * 'tdTableStatus' - The current state of the table:     * @CREATING@ - The table is being created.     * @UPDATING@ - The table is being updated.     * @DELETING@ - The table is being deleted.     * @ACTIVE@ - The table is ready for use.     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The AWS KMS key used to encrypt the table in inaccessible. Table operations may fail due to failure to use the AWS KMS key. DynamoDB will initiate the table archival process when a table's AWS KMS key remains inaccessible for more than seven days.      * @ARCHIVING@ - The table is being archived. Operations are not allowed until archival is complete.      * @ARCHIVED@ - The table has been archived. See the ArchivalReason for more information.
--
-- * 'tdTableARN' - The Amazon Resource Name (ARN) that uniquely identifies the table.
--
-- * 'tdKeySchema' - The primary key structure for the table. Each @KeySchemaElement@ consists of:     * @AttributeName@ - The name of the attribute.     * @KeyType@ - The role of the attribute:     * @HASH@ - partition key     * @RANGE@ - sort key For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'tdGlobalSecondaryIndexes' - The global secondary indexes, if any, on the table. Each index is scoped to a given partition key value. Each element is composed of:     * @Backfilling@ - If true, then the index is currently in the backfilling phase. Backfilling occurs only when a new global secondary index is added to the table. It is the process by which DynamoDB populates the new index with data from the table. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)  You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)     * @IndexName@ - The name of the global secondary index.     * @IndexSizeBytes@ - The total size of the global secondary index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @IndexStatus@ - The current status of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.     * @ItemCount@ - The number of items in the global secondary index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units, along with data about increases and decreases.  If the table is in the @DELETING@ state, no information about indexes will be returned.
--
-- * 'tdLatestStreamLabel' - A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * AWS customer ID     * Table name     * @StreamLabel@
--
-- * 'tdBillingModeSummary' - Contains the details for the read/write capacity mode.
--
-- * 'tdLocalSecondaryIndexes' - Represents one or more local secondary indexes on the table. Each index is scoped to a given partition key value. Tables with one or more local secondary indexes are subject to an item collection size limit, where the amount of data within a given item collection cannot exceed 10 GB. Each element is composed of:     * @IndexName@ - The name of the local secondary index.     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @IndexSizeBytes@ - Represents the total size of the index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.     * @ItemCount@ - Represents the number of items in the index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value. If the table is in the @DELETING@ state, no information about indexes will be returned.
--
-- * 'tdCreationDateTime' - The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- * 'tdSSEDescription' - The description of the server-side encryption status on the specified table.
--
-- * 'tdTableId' - Unique identifier for the table for which the backup was created.
--
-- * 'tdReplicas' - Represents replicas of the table.
--
-- * 'tdItemCount' - The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'tdArchivalSummary' - Contains information about the table archive.
--
-- * 'tdTableName' - The name of the table.
--
-- * 'tdStreamSpecification' - The current DynamoDB Streams configuration for the table.
tableDescription ::
  TableDescription
tableDescription =
  TableDescription'
    { _tdRestoreSummary = Nothing,
      _tdGlobalTableVersion = Nothing,
      _tdTableSizeBytes = Nothing,
      _tdAttributeDefinitions = Nothing,
      _tdLatestStreamARN = Nothing,
      _tdProvisionedThroughput = Nothing,
      _tdTableStatus = Nothing,
      _tdTableARN = Nothing,
      _tdKeySchema = Nothing,
      _tdGlobalSecondaryIndexes = Nothing,
      _tdLatestStreamLabel = Nothing,
      _tdBillingModeSummary = Nothing,
      _tdLocalSecondaryIndexes = Nothing,
      _tdCreationDateTime = Nothing,
      _tdSSEDescription = Nothing,
      _tdTableId = Nothing,
      _tdReplicas = Nothing,
      _tdItemCount = Nothing,
      _tdArchivalSummary = Nothing,
      _tdTableName = Nothing,
      _tdStreamSpecification = Nothing
    }

-- | Contains details for the restore.
tdRestoreSummary :: Lens' TableDescription (Maybe RestoreSummary)
tdRestoreSummary = lens _tdRestoreSummary (\s a -> s {_tdRestoreSummary = a})

-- | Represents the version of <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables> in use, if the table is replicated across AWS Regions.
tdGlobalTableVersion :: Lens' TableDescription (Maybe Text)
tdGlobalTableVersion = lens _tdGlobalTableVersion (\s a -> s {_tdGlobalTableVersion = a})

-- | The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
tdTableSizeBytes :: Lens' TableDescription (Maybe Integer)
tdTableSizeBytes = lens _tdTableSizeBytes (\s a -> s {_tdTableSizeBytes = a})

-- | An array of @AttributeDefinition@ objects. Each of these objects describes one attribute in the table and index key schema. Each @AttributeDefinition@ object in this array is composed of:     * @AttributeName@ - The name of the attribute.     * @AttributeType@ - The data type for the attribute.
tdAttributeDefinitions :: Lens' TableDescription [AttributeDefinition]
tdAttributeDefinitions = lens _tdAttributeDefinitions (\s a -> s {_tdAttributeDefinitions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
tdLatestStreamARN :: Lens' TableDescription (Maybe Text)
tdLatestStreamARN = lens _tdLatestStreamARN (\s a -> s {_tdLatestStreamARN = a})

-- | The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
tdProvisionedThroughput :: Lens' TableDescription (Maybe ProvisionedThroughputDescription)
tdProvisionedThroughput = lens _tdProvisionedThroughput (\s a -> s {_tdProvisionedThroughput = a})

-- | The current state of the table:     * @CREATING@ - The table is being created.     * @UPDATING@ - The table is being updated.     * @DELETING@ - The table is being deleted.     * @ACTIVE@ - The table is ready for use.     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The AWS KMS key used to encrypt the table in inaccessible. Table operations may fail due to failure to use the AWS KMS key. DynamoDB will initiate the table archival process when a table's AWS KMS key remains inaccessible for more than seven days.      * @ARCHIVING@ - The table is being archived. Operations are not allowed until archival is complete.      * @ARCHIVED@ - The table has been archived. See the ArchivalReason for more information.
tdTableStatus :: Lens' TableDescription (Maybe TableStatus)
tdTableStatus = lens _tdTableStatus (\s a -> s {_tdTableStatus = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the table.
tdTableARN :: Lens' TableDescription (Maybe Text)
tdTableARN = lens _tdTableARN (\s a -> s {_tdTableARN = a})

-- | The primary key structure for the table. Each @KeySchemaElement@ consists of:     * @AttributeName@ - The name of the attribute.     * @KeyType@ - The role of the attribute:     * @HASH@ - partition key     * @RANGE@ - sort key For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
tdKeySchema :: Lens' TableDescription (Maybe (NonEmpty KeySchemaElement))
tdKeySchema = lens _tdKeySchema (\s a -> s {_tdKeySchema = a}) . mapping _List1

-- | The global secondary indexes, if any, on the table. Each index is scoped to a given partition key value. Each element is composed of:     * @Backfilling@ - If true, then the index is currently in the backfilling phase. Backfilling occurs only when a new global secondary index is added to the table. It is the process by which DynamoDB populates the new index with data from the table. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)  You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)     * @IndexName@ - The name of the global secondary index.     * @IndexSizeBytes@ - The total size of the global secondary index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @IndexStatus@ - The current status of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.     * @ItemCount@ - The number of items in the global secondary index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units, along with data about increases and decreases.  If the table is in the @DELETING@ state, no information about indexes will be returned.
tdGlobalSecondaryIndexes :: Lens' TableDescription [GlobalSecondaryIndexDescription]
tdGlobalSecondaryIndexes = lens _tdGlobalSecondaryIndexes (\s a -> s {_tdGlobalSecondaryIndexes = a}) . _Default . _Coerce

-- | A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * AWS customer ID     * Table name     * @StreamLabel@
tdLatestStreamLabel :: Lens' TableDescription (Maybe Text)
tdLatestStreamLabel = lens _tdLatestStreamLabel (\s a -> s {_tdLatestStreamLabel = a})

-- | Contains the details for the read/write capacity mode.
tdBillingModeSummary :: Lens' TableDescription (Maybe BillingModeSummary)
tdBillingModeSummary = lens _tdBillingModeSummary (\s a -> s {_tdBillingModeSummary = a})

-- | Represents one or more local secondary indexes on the table. Each index is scoped to a given partition key value. Tables with one or more local secondary indexes are subject to an item collection size limit, where the amount of data within a given item collection cannot exceed 10 GB. Each element is composed of:     * @IndexName@ - The name of the local secondary index.     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes is in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @IndexSizeBytes@ - Represents the total size of the index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.     * @ItemCount@ - Represents the number of items in the index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value. If the table is in the @DELETING@ state, no information about indexes will be returned.
tdLocalSecondaryIndexes :: Lens' TableDescription [LocalSecondaryIndexDescription]
tdLocalSecondaryIndexes = lens _tdLocalSecondaryIndexes (\s a -> s {_tdLocalSecondaryIndexes = a}) . _Default . _Coerce

-- | The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
tdCreationDateTime :: Lens' TableDescription (Maybe UTCTime)
tdCreationDateTime = lens _tdCreationDateTime (\s a -> s {_tdCreationDateTime = a}) . mapping _Time

-- | The description of the server-side encryption status on the specified table.
tdSSEDescription :: Lens' TableDescription (Maybe SSEDescription)
tdSSEDescription = lens _tdSSEDescription (\s a -> s {_tdSSEDescription = a})

-- | Unique identifier for the table for which the backup was created.
tdTableId :: Lens' TableDescription (Maybe Text)
tdTableId = lens _tdTableId (\s a -> s {_tdTableId = a})

-- | Represents replicas of the table.
tdReplicas :: Lens' TableDescription [ReplicaDescription]
tdReplicas = lens _tdReplicas (\s a -> s {_tdReplicas = a}) . _Default . _Coerce

-- | The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
tdItemCount :: Lens' TableDescription (Maybe Integer)
tdItemCount = lens _tdItemCount (\s a -> s {_tdItemCount = a})

-- | Contains information about the table archive.
tdArchivalSummary :: Lens' TableDescription (Maybe ArchivalSummary)
tdArchivalSummary = lens _tdArchivalSummary (\s a -> s {_tdArchivalSummary = a})

-- | The name of the table.
tdTableName :: Lens' TableDescription (Maybe Text)
tdTableName = lens _tdTableName (\s a -> s {_tdTableName = a})

-- | The current DynamoDB Streams configuration for the table.
tdStreamSpecification :: Lens' TableDescription (Maybe StreamSpecification)
tdStreamSpecification = lens _tdStreamSpecification (\s a -> s {_tdStreamSpecification = a})

instance FromJSON TableDescription where
  parseJSON =
    withObject
      "TableDescription"
      ( \x ->
          TableDescription'
            <$> (x .:? "RestoreSummary")
            <*> (x .:? "GlobalTableVersion")
            <*> (x .:? "TableSizeBytes")
            <*> (x .:? "AttributeDefinitions" .!= mempty)
            <*> (x .:? "LatestStreamArn")
            <*> (x .:? "ProvisionedThroughput")
            <*> (x .:? "TableStatus")
            <*> (x .:? "TableArn")
            <*> (x .:? "KeySchema")
            <*> (x .:? "GlobalSecondaryIndexes" .!= mempty)
            <*> (x .:? "LatestStreamLabel")
            <*> (x .:? "BillingModeSummary")
            <*> (x .:? "LocalSecondaryIndexes" .!= mempty)
            <*> (x .:? "CreationDateTime")
            <*> (x .:? "SSEDescription")
            <*> (x .:? "TableId")
            <*> (x .:? "Replicas" .!= mempty)
            <*> (x .:? "ItemCount")
            <*> (x .:? "ArchivalSummary")
            <*> (x .:? "TableName")
            <*> (x .:? "StreamSpecification")
      )

instance Hashable TableDescription

instance NFData TableDescription
