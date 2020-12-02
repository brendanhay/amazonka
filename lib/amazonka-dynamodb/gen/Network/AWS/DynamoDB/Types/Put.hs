{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Put
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Put where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a request to perform a @PutItem@ operation.
--
--
--
-- /See:/ 'put' smart constructor.
data Put = Put'
  { _pExpressionAttributeNames ::
      !(Maybe (Map Text (Text))),
    _pExpressionAttributeValues ::
      !(Maybe (Map Text (AttributeValue))),
    _pReturnValuesOnConditionCheckFailure ::
      !(Maybe ReturnValuesOnConditionCheckFailure),
    _pConditionExpression :: !(Maybe Text),
    _pItem :: !(Map Text (AttributeValue)),
    _pTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Put' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'pExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'pReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- * 'pConditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
--
-- * 'pItem' - A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
--
-- * 'pTableName' - Name of the table in which to write the item.
put ::
  -- | 'pTableName'
  Text ->
  Put
put pTableName_ =
  Put'
    { _pExpressionAttributeNames = Nothing,
      _pExpressionAttributeValues = Nothing,
      _pReturnValuesOnConditionCheckFailure = Nothing,
      _pConditionExpression = Nothing,
      _pItem = mempty,
      _pTableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression.
pExpressionAttributeNames :: Lens' Put (HashMap Text (Text))
pExpressionAttributeNames = lens _pExpressionAttributeNames (\s a -> s {_pExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
pExpressionAttributeValues :: Lens' Put (HashMap Text (AttributeValue))
pExpressionAttributeValues = lens _pExpressionAttributeValues (\s a -> s {_pExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
pReturnValuesOnConditionCheckFailure :: Lens' Put (Maybe ReturnValuesOnConditionCheckFailure)
pReturnValuesOnConditionCheckFailure = lens _pReturnValuesOnConditionCheckFailure (\s a -> s {_pReturnValuesOnConditionCheckFailure = a})

-- | A condition that must be satisfied in order for a conditional update to succeed.
pConditionExpression :: Lens' Put (Maybe Text)
pConditionExpression = lens _pConditionExpression (\s a -> s {_pConditionExpression = a})

-- | A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
pItem :: Lens' Put (HashMap Text (AttributeValue))
pItem = lens _pItem (\s a -> s {_pItem = a}) . _Map

-- | Name of the table in which to write the item.
pTableName :: Lens' Put Text
pTableName = lens _pTableName (\s a -> s {_pTableName = a})

instance Hashable Put

instance NFData Put

instance ToJSON Put where
  toJSON Put' {..} =
    object
      ( catMaybes
          [ ("ExpressionAttributeNames" .=) <$> _pExpressionAttributeNames,
            ("ExpressionAttributeValues" .=) <$> _pExpressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" .=)
              <$> _pReturnValuesOnConditionCheckFailure,
            ("ConditionExpression" .=) <$> _pConditionExpression,
            Just ("Item" .= _pItem),
            Just ("TableName" .= _pTableName)
          ]
      )
