{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Get
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Get where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an item and related attribute values to retrieve in a @TransactGetItem@ object.
--
--
--
-- /See:/ 'get'' smart constructor.
data Get = Get'
  { _getProjectionExpression :: !(Maybe Text),
    _getExpressionAttributeNames :: !(Maybe (Map Text (Text))),
    _getKey :: !(Map Text (AttributeValue)),
    _getTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Get' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getProjectionExpression' - A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
--
-- * 'getExpressionAttributeNames' - One or more substitution tokens for attribute names in the ProjectionExpression parameter.
--
-- * 'getKey' - A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
--
-- * 'getTableName' - The name of the table from which to retrieve the specified item.
get' ::
  -- | 'getTableName'
  Text ->
  Get
get' pTableName_ =
  Get'
    { _getProjectionExpression = Nothing,
      _getExpressionAttributeNames = Nothing,
      _getKey = mempty,
      _getTableName = pTableName_
    }

-- | A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
getProjectionExpression :: Lens' Get (Maybe Text)
getProjectionExpression = lens _getProjectionExpression (\s a -> s {_getProjectionExpression = a})

-- | One or more substitution tokens for attribute names in the ProjectionExpression parameter.
getExpressionAttributeNames :: Lens' Get (HashMap Text (Text))
getExpressionAttributeNames = lens _getExpressionAttributeNames (\s a -> s {_getExpressionAttributeNames = a}) . _Default . _Map

-- | A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
getKey :: Lens' Get (HashMap Text (AttributeValue))
getKey = lens _getKey (\s a -> s {_getKey = a}) . _Map

-- | The name of the table from which to retrieve the specified item.
getTableName :: Lens' Get Text
getTableName = lens _getTableName (\s a -> s {_getTableName = a})

instance Hashable Get

instance NFData Get

instance ToJSON Get where
  toJSON Get' {..} =
    object
      ( catMaybes
          [ ("ProjectionExpression" .=) <$> _getProjectionExpression,
            ("ExpressionAttributeNames" .=) <$> _getExpressionAttributeNames,
            Just ("Key" .= _getKey),
            Just ("TableName" .= _getTableName)
          ]
      )
