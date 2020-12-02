{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a local secondary index.
--
--
--
-- /See:/ 'localSecondaryIndexDescription' smart constructor.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription'
  { _lsidIndexSizeBytes ::
      !(Maybe Integer),
    _lsidIndexARN ::
      !(Maybe Text),
    _lsidKeySchema ::
      !( Maybe
           (List1 KeySchemaElement)
       ),
    _lsidProjection ::
      !(Maybe Projection),
    _lsidItemCount ::
      !(Maybe Integer),
    _lsidIndexName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsidIndexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'lsidIndexARN' - The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- * 'lsidKeySchema' - The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'lsidProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'lsidItemCount' - The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'lsidIndexName' - Represents the name of the local secondary index.
localSecondaryIndexDescription ::
  LocalSecondaryIndexDescription
localSecondaryIndexDescription =
  LocalSecondaryIndexDescription'
    { _lsidIndexSizeBytes = Nothing,
      _lsidIndexARN = Nothing,
      _lsidKeySchema = Nothing,
      _lsidProjection = Nothing,
      _lsidItemCount = Nothing,
      _lsidIndexName = Nothing
    }

-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
lsidIndexSizeBytes :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidIndexSizeBytes = lens _lsidIndexSizeBytes (\s a -> s {_lsidIndexSizeBytes = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
lsidIndexARN :: Lens' LocalSecondaryIndexDescription (Maybe Text)
lsidIndexARN = lens _lsidIndexARN (\s a -> s {_lsidIndexARN = a})

-- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
lsidKeySchema :: Lens' LocalSecondaryIndexDescription (Maybe (NonEmpty KeySchemaElement))
lsidKeySchema = lens _lsidKeySchema (\s a -> s {_lsidKeySchema = a}) . mapping _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
lsidProjection :: Lens' LocalSecondaryIndexDescription (Maybe Projection)
lsidProjection = lens _lsidProjection (\s a -> s {_lsidProjection = a})

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
lsidItemCount :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidItemCount = lens _lsidItemCount (\s a -> s {_lsidItemCount = a})

-- | Represents the name of the local secondary index.
lsidIndexName :: Lens' LocalSecondaryIndexDescription (Maybe Text)
lsidIndexName = lens _lsidIndexName (\s a -> s {_lsidIndexName = a})

instance FromJSON LocalSecondaryIndexDescription where
  parseJSON =
    withObject
      "LocalSecondaryIndexDescription"
      ( \x ->
          LocalSecondaryIndexDescription'
            <$> (x .:? "IndexSizeBytes")
            <*> (x .:? "IndexArn")
            <*> (x .:? "KeySchema")
            <*> (x .:? "Projection")
            <*> (x .:? "ItemCount")
            <*> (x .:? "IndexName")
      )

instance Hashable LocalSecondaryIndexDescription

instance NFData LocalSecondaryIndexDescription
