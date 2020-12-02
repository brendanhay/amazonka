{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndex where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a global secondary index.
--
--
--
-- /See:/ 'globalSecondaryIndex' smart constructor.
data GlobalSecondaryIndex = GlobalSecondaryIndex'
  { _gsiProvisionedThroughput ::
      !(Maybe ProvisionedThroughput),
    _gsiIndexName :: !Text,
    _gsiKeySchema :: !(List1 KeySchemaElement),
    _gsiProjection :: !Projection
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalSecondaryIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'gsiIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- * 'gsiKeySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'gsiProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
globalSecondaryIndex ::
  -- | 'gsiIndexName'
  Text ->
  -- | 'gsiKeySchema'
  NonEmpty KeySchemaElement ->
  -- | 'gsiProjection'
  Projection ->
  GlobalSecondaryIndex
globalSecondaryIndex pIndexName_ pKeySchema_ pProjection_ =
  GlobalSecondaryIndex'
    { _gsiProvisionedThroughput = Nothing,
      _gsiIndexName = pIndexName_,
      _gsiKeySchema = _List1 # pKeySchema_,
      _gsiProjection = pProjection_
    }

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
gsiProvisionedThroughput :: Lens' GlobalSecondaryIndex (Maybe ProvisionedThroughput)
gsiProvisionedThroughput = lens _gsiProvisionedThroughput (\s a -> s {_gsiProvisionedThroughput = a})

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
gsiIndexName :: Lens' GlobalSecondaryIndex Text
gsiIndexName = lens _gsiIndexName (\s a -> s {_gsiIndexName = a})

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
gsiKeySchema :: Lens' GlobalSecondaryIndex (NonEmpty KeySchemaElement)
gsiKeySchema = lens _gsiKeySchema (\s a -> s {_gsiKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
gsiProjection :: Lens' GlobalSecondaryIndex Projection
gsiProjection = lens _gsiProjection (\s a -> s {_gsiProjection = a})

instance Hashable GlobalSecondaryIndex

instance NFData GlobalSecondaryIndex

instance ToJSON GlobalSecondaryIndex where
  toJSON GlobalSecondaryIndex' {..} =
    object
      ( catMaybes
          [ ("ProvisionedThroughput" .=) <$> _gsiProvisionedThroughput,
            Just ("IndexName" .= _gsiIndexName),
            Just ("KeySchema" .= _gsiKeySchema),
            Just ("Projection" .= _gsiProjection)
          ]
      )
