{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KeySchemaElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KeySchemaElement where

import Network.AWS.DynamoDB.Types.KeyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents /a single element/ of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.
--
--
-- A @KeySchemaElement@ represents exactly one attribute of the primary key. For example, a simple primary key would be represented by one @KeySchemaElement@ (for the partition key). A composite primary key would require one @KeySchemaElement@ for the partition key, and another @KeySchemaElement@ for the sort key.
--
-- A @KeySchemaElement@ must be a scalar, top-level attribute (not a nested attribute). The data type must be one of String, Number, or Binary. The attribute cannot be nested within a List or a Map.
--
--
-- /See:/ 'keySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { _kseAttributeName ::
      !Text,
    _kseKeyType :: !KeyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeySchemaElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kseAttributeName' - The name of a key attribute.
--
-- * 'kseKeyType' - The role that this key attribute will assume:     * @HASH@ - partition key     * @RANGE@ - sort key
keySchemaElement ::
  -- | 'kseAttributeName'
  Text ->
  -- | 'kseKeyType'
  KeyType ->
  KeySchemaElement
keySchemaElement pAttributeName_ pKeyType_ =
  KeySchemaElement'
    { _kseAttributeName = pAttributeName_,
      _kseKeyType = pKeyType_
    }

-- | The name of a key attribute.
kseAttributeName :: Lens' KeySchemaElement Text
kseAttributeName = lens _kseAttributeName (\s a -> s {_kseAttributeName = a})

-- | The role that this key attribute will assume:     * @HASH@ - partition key     * @RANGE@ - sort key
kseKeyType :: Lens' KeySchemaElement KeyType
kseKeyType = lens _kseKeyType (\s a -> s {_kseKeyType = a})

instance FromJSON KeySchemaElement where
  parseJSON =
    withObject
      "KeySchemaElement"
      ( \x ->
          KeySchemaElement' <$> (x .: "AttributeName") <*> (x .: "KeyType")
      )

instance Hashable KeySchemaElement

instance NFData KeySchemaElement

instance ToJSON KeySchemaElement where
  toJSON KeySchemaElement' {..} =
    object
      ( catMaybes
          [ Just ("AttributeName" .= _kseAttributeName),
            Just ("KeyType" .= _kseKeyType)
          ]
      )
