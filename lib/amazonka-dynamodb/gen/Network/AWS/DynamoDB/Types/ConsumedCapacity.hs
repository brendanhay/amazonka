{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ConsumedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ConsumedCapacity where

import Network.AWS.DynamoDB.Types.Capacity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The capacity units consumed by an operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the request asked for it. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
--
--
-- /See:/ 'consumedCapacity' smart constructor.
data ConsumedCapacity = ConsumedCapacity'
  { _cReadCapacityUnits ::
      !(Maybe Double),
    _cGlobalSecondaryIndexes ::
      !(Maybe (Map Text (Capacity))),
    _cCapacityUnits :: !(Maybe Double),
    _cWriteCapacityUnits :: !(Maybe Double),
    _cLocalSecondaryIndexes :: !(Maybe (Map Text (Capacity))),
    _cTable :: !(Maybe Capacity),
    _cTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConsumedCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cReadCapacityUnits' - The total number of read capacity units consumed by the operation.
--
-- * 'cGlobalSecondaryIndexes' - The amount of throughput consumed on each global index affected by the operation.
--
-- * 'cCapacityUnits' - The total number of capacity units consumed by the operation.
--
-- * 'cWriteCapacityUnits' - The total number of write capacity units consumed by the operation.
--
-- * 'cLocalSecondaryIndexes' - The amount of throughput consumed on each local index affected by the operation.
--
-- * 'cTable' - The amount of throughput consumed on the table affected by the operation.
--
-- * 'cTableName' - The name of the table that was affected by the operation.
consumedCapacity ::
  ConsumedCapacity
consumedCapacity =
  ConsumedCapacity'
    { _cReadCapacityUnits = Nothing,
      _cGlobalSecondaryIndexes = Nothing,
      _cCapacityUnits = Nothing,
      _cWriteCapacityUnits = Nothing,
      _cLocalSecondaryIndexes = Nothing,
      _cTable = Nothing,
      _cTableName = Nothing
    }

-- | The total number of read capacity units consumed by the operation.
cReadCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
cReadCapacityUnits = lens _cReadCapacityUnits (\s a -> s {_cReadCapacityUnits = a})

-- | The amount of throughput consumed on each global index affected by the operation.
cGlobalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text (Capacity))
cGlobalSecondaryIndexes = lens _cGlobalSecondaryIndexes (\s a -> s {_cGlobalSecondaryIndexes = a}) . _Default . _Map

-- | The total number of capacity units consumed by the operation.
cCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
cCapacityUnits = lens _cCapacityUnits (\s a -> s {_cCapacityUnits = a})

-- | The total number of write capacity units consumed by the operation.
cWriteCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
cWriteCapacityUnits = lens _cWriteCapacityUnits (\s a -> s {_cWriteCapacityUnits = a})

-- | The amount of throughput consumed on each local index affected by the operation.
cLocalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text (Capacity))
cLocalSecondaryIndexes = lens _cLocalSecondaryIndexes (\s a -> s {_cLocalSecondaryIndexes = a}) . _Default . _Map

-- | The amount of throughput consumed on the table affected by the operation.
cTable :: Lens' ConsumedCapacity (Maybe Capacity)
cTable = lens _cTable (\s a -> s {_cTable = a})

-- | The name of the table that was affected by the operation.
cTableName :: Lens' ConsumedCapacity (Maybe Text)
cTableName = lens _cTableName (\s a -> s {_cTableName = a})

instance FromJSON ConsumedCapacity where
  parseJSON =
    withObject
      "ConsumedCapacity"
      ( \x ->
          ConsumedCapacity'
            <$> (x .:? "ReadCapacityUnits")
            <*> (x .:? "GlobalSecondaryIndexes" .!= mempty)
            <*> (x .:? "CapacityUnits")
            <*> (x .:? "WriteCapacityUnits")
            <*> (x .:? "LocalSecondaryIndexes" .!= mempty)
            <*> (x .:? "Table")
            <*> (x .:? "TableName")
      )

instance Hashable ConsumedCapacity

instance NFData ConsumedCapacity
