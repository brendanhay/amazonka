{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.LocalSecondaryIndex where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a local secondary index.
--
--
--
-- /See:/ 'localSecondaryIndex' smart constructor.
data LocalSecondaryIndex = LocalSecondaryIndex'
  { _lsiIndexName ::
      !Text,
    _lsiKeySchema :: !(List1 KeySchemaElement),
    _lsiProjection :: !Projection
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalSecondaryIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsiIndexName' - The name of the local secondary index. The name must be unique among all other indexes on this table.
--
-- * 'lsiKeySchema' - The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'lsiProjection' - Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
localSecondaryIndex ::
  -- | 'lsiIndexName'
  Text ->
  -- | 'lsiKeySchema'
  NonEmpty KeySchemaElement ->
  -- | 'lsiProjection'
  Projection ->
  LocalSecondaryIndex
localSecondaryIndex pIndexName_ pKeySchema_ pProjection_ =
  LocalSecondaryIndex'
    { _lsiIndexName = pIndexName_,
      _lsiKeySchema = _List1 # pKeySchema_,
      _lsiProjection = pProjection_
    }

-- | The name of the local secondary index. The name must be unique among all other indexes on this table.
lsiIndexName :: Lens' LocalSecondaryIndex Text
lsiIndexName = lens _lsiIndexName (\s a -> s {_lsiIndexName = a})

-- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
lsiKeySchema :: Lens' LocalSecondaryIndex (NonEmpty KeySchemaElement)
lsiKeySchema = lens _lsiKeySchema (\s a -> s {_lsiKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
lsiProjection :: Lens' LocalSecondaryIndex Projection
lsiProjection = lens _lsiProjection (\s a -> s {_lsiProjection = a})

instance Hashable LocalSecondaryIndex

instance NFData LocalSecondaryIndex

instance ToJSON LocalSecondaryIndex where
  toJSON LocalSecondaryIndex' {..} =
    object
      ( catMaybes
          [ Just ("IndexName" .= _lsiIndexName),
            Just ("KeySchema" .= _lsiKeySchema),
            Just ("Projection" .= _lsiProjection)
          ]
      )
