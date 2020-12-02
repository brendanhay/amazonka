{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription where

import Network.AWS.DynamoDB.Types.IndexStatus
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a global secondary index.
--
--
--
-- /See:/ 'globalSecondaryIndexDescription' smart constructor.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription'
  { _gsidBackfilling ::
      !(Maybe Bool),
    _gsidIndexSizeBytes ::
      !(Maybe Integer),
    _gsidIndexStatus ::
      !(Maybe IndexStatus),
    _gsidProvisionedThroughput ::
      !( Maybe
           ProvisionedThroughputDescription
       ),
    _gsidIndexARN ::
      !(Maybe Text),
    _gsidKeySchema ::
      !( Maybe
           ( List1
               KeySchemaElement
           )
       ),
    _gsidProjection ::
      !(Maybe Projection),
    _gsidItemCount ::
      !(Maybe Integer),
    _gsidIndexName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsidBackfilling' - Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false. You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false.
--
-- * 'gsidIndexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'gsidIndexStatus' - The current state of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.
--
-- * 'gsidProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'gsidIndexARN' - The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- * 'gsidKeySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'gsidProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'gsidItemCount' - The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'gsidIndexName' - The name of the global secondary index.
globalSecondaryIndexDescription ::
  GlobalSecondaryIndexDescription
globalSecondaryIndexDescription =
  GlobalSecondaryIndexDescription'
    { _gsidBackfilling = Nothing,
      _gsidIndexSizeBytes = Nothing,
      _gsidIndexStatus = Nothing,
      _gsidProvisionedThroughput = Nothing,
      _gsidIndexARN = Nothing,
      _gsidKeySchema = Nothing,
      _gsidProjection = Nothing,
      _gsidItemCount = Nothing,
      _gsidIndexName = Nothing
    }

-- | Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false. You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false.
gsidBackfilling :: Lens' GlobalSecondaryIndexDescription (Maybe Bool)
gsidBackfilling = lens _gsidBackfilling (\s a -> s {_gsidBackfilling = a})

-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
gsidIndexSizeBytes :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidIndexSizeBytes = lens _gsidIndexSizeBytes (\s a -> s {_gsidIndexSizeBytes = a})

-- | The current state of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.
gsidIndexStatus :: Lens' GlobalSecondaryIndexDescription (Maybe IndexStatus)
gsidIndexStatus = lens _gsidIndexStatus (\s a -> s {_gsidIndexStatus = a})

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
gsidProvisionedThroughput :: Lens' GlobalSecondaryIndexDescription (Maybe ProvisionedThroughputDescription)
gsidProvisionedThroughput = lens _gsidProvisionedThroughput (\s a -> s {_gsidProvisionedThroughput = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
gsidIndexARN :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexARN = lens _gsidIndexARN (\s a -> s {_gsidIndexARN = a})

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
gsidKeySchema :: Lens' GlobalSecondaryIndexDescription (Maybe (NonEmpty KeySchemaElement))
gsidKeySchema = lens _gsidKeySchema (\s a -> s {_gsidKeySchema = a}) . mapping _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
gsidProjection :: Lens' GlobalSecondaryIndexDescription (Maybe Projection)
gsidProjection = lens _gsidProjection (\s a -> s {_gsidProjection = a})

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
gsidItemCount :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidItemCount = lens _gsidItemCount (\s a -> s {_gsidItemCount = a})

-- | The name of the global secondary index.
gsidIndexName :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexName = lens _gsidIndexName (\s a -> s {_gsidIndexName = a})

instance FromJSON GlobalSecondaryIndexDescription where
  parseJSON =
    withObject
      "GlobalSecondaryIndexDescription"
      ( \x ->
          GlobalSecondaryIndexDescription'
            <$> (x .:? "Backfilling")
            <*> (x .:? "IndexSizeBytes")
            <*> (x .:? "IndexStatus")
            <*> (x .:? "ProvisionedThroughput")
            <*> (x .:? "IndexArn")
            <*> (x .:? "KeySchema")
            <*> (x .:? "Projection")
            <*> (x .:? "ItemCount")
            <*> (x .:? "IndexName")
      )

instance Hashable GlobalSecondaryIndexDescription

instance NFData GlobalSecondaryIndexDescription
