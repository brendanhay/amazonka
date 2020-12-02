{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PutRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PutRequest where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a request to perform a @PutItem@ operation on an item.
--
--
--
-- /See:/ 'putRequest' smart constructor.
newtype PutRequest = PutRequest'
  { _prItem ::
      Map Text (AttributeValue)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prItem' - A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
putRequest ::
  PutRequest
putRequest = PutRequest' {_prItem = mempty}

-- | A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
prItem :: Lens' PutRequest (HashMap Text (AttributeValue))
prItem = lens _prItem (\s a -> s {_prItem = a}) . _Map

instance FromJSON PutRequest where
  parseJSON =
    withObject
      "PutRequest"
      (\x -> PutRequest' <$> (x .:? "Item" .!= mempty))

instance Hashable PutRequest

instance NFData PutRequest

instance ToJSON PutRequest where
  toJSON PutRequest' {..} =
    object (catMaybes [Just ("Item" .= _prItem)])
