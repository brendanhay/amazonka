{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteRequest where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a request to perform a @DeleteItem@ operation on an item.
--
--
--
-- /See:/ 'deleteRequest' smart constructor.
newtype DeleteRequest = DeleteRequest'
  { _drKey ::
      Map Text (AttributeValue)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drKey' - A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
deleteRequest ::
  DeleteRequest
deleteRequest = DeleteRequest' {_drKey = mempty}

-- | A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
drKey :: Lens' DeleteRequest (HashMap Text (AttributeValue))
drKey = lens _drKey (\s a -> s {_drKey = a}) . _Map

instance FromJSON DeleteRequest where
  parseJSON =
    withObject
      "DeleteRequest"
      (\x -> DeleteRequest' <$> (x .:? "Key" .!= mempty))

instance Hashable DeleteRequest

instance NFData DeleteRequest

instance ToJSON DeleteRequest where
  toJSON DeleteRequest' {..} =
    object (catMaybes [Just ("Key" .= _drKey)])
