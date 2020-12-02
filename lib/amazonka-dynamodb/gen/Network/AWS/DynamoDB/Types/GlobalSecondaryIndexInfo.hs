{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a global secondary index for the table when the backup was created.
--
--
--
-- /See:/ 'globalSecondaryIndexInfo' smart constructor.
data GlobalSecondaryIndexInfo = GlobalSecondaryIndexInfo'
  { _gsiiProvisionedThroughput ::
      !(Maybe ProvisionedThroughput),
    _gsiiKeySchema ::
      !(Maybe (List1 KeySchemaElement)),
    _gsiiProjection :: !(Maybe Projection),
    _gsiiIndexName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalSecondaryIndexInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiiProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index.
--
-- * 'gsiiKeySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'gsiiProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'gsiiIndexName' - The name of the global secondary index.
globalSecondaryIndexInfo ::
  GlobalSecondaryIndexInfo
globalSecondaryIndexInfo =
  GlobalSecondaryIndexInfo'
    { _gsiiProvisionedThroughput = Nothing,
      _gsiiKeySchema = Nothing,
      _gsiiProjection = Nothing,
      _gsiiIndexName = Nothing
    }

-- | Represents the provisioned throughput settings for the specified global secondary index.
gsiiProvisionedThroughput :: Lens' GlobalSecondaryIndexInfo (Maybe ProvisionedThroughput)
gsiiProvisionedThroughput = lens _gsiiProvisionedThroughput (\s a -> s {_gsiiProvisionedThroughput = a})

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
gsiiKeySchema :: Lens' GlobalSecondaryIndexInfo (Maybe (NonEmpty KeySchemaElement))
gsiiKeySchema = lens _gsiiKeySchema (\s a -> s {_gsiiKeySchema = a}) . mapping _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
gsiiProjection :: Lens' GlobalSecondaryIndexInfo (Maybe Projection)
gsiiProjection = lens _gsiiProjection (\s a -> s {_gsiiProjection = a})

-- | The name of the global secondary index.
gsiiIndexName :: Lens' GlobalSecondaryIndexInfo (Maybe Text)
gsiiIndexName = lens _gsiiIndexName (\s a -> s {_gsiiIndexName = a})

instance FromJSON GlobalSecondaryIndexInfo where
  parseJSON =
    withObject
      "GlobalSecondaryIndexInfo"
      ( \x ->
          GlobalSecondaryIndexInfo'
            <$> (x .:? "ProvisionedThroughput")
            <*> (x .:? "KeySchema")
            <*> (x .:? "Projection")
            <*> (x .:? "IndexName")
      )

instance Hashable GlobalSecondaryIndexInfo

instance NFData GlobalSecondaryIndexInfo
