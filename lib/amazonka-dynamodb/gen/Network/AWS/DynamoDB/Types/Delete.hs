{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Delete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Delete where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a request to perform a @DeleteItem@ operation.
--
--
--
-- /See:/ 'delete'' smart constructor.
data Delete = Delete'
  { _dExpressionAttributeNames ::
      !(Maybe (Map Text (Text))),
    _dExpressionAttributeValues ::
      !(Maybe (Map Text (AttributeValue))),
    _dReturnValuesOnConditionCheckFailure ::
      !(Maybe ReturnValuesOnConditionCheckFailure),
    _dConditionExpression :: !(Maybe Text),
    _dKey :: !(Map Text (AttributeValue)),
    _dTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'dExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'dReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- * 'dConditionExpression' - A condition that must be satisfied in order for a conditional delete to succeed.
--
-- * 'dKey' - The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
--
-- * 'dTableName' - Name of the table in which the item to be deleted resides.
delete' ::
  -- | 'dTableName'
  Text ->
  Delete
delete' pTableName_ =
  Delete'
    { _dExpressionAttributeNames = Nothing,
      _dExpressionAttributeValues = Nothing,
      _dReturnValuesOnConditionCheckFailure = Nothing,
      _dConditionExpression = Nothing,
      _dKey = mempty,
      _dTableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression.
dExpressionAttributeNames :: Lens' Delete (HashMap Text (Text))
dExpressionAttributeNames = lens _dExpressionAttributeNames (\s a -> s {_dExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
dExpressionAttributeValues :: Lens' Delete (HashMap Text (AttributeValue))
dExpressionAttributeValues = lens _dExpressionAttributeValues (\s a -> s {_dExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
dReturnValuesOnConditionCheckFailure :: Lens' Delete (Maybe ReturnValuesOnConditionCheckFailure)
dReturnValuesOnConditionCheckFailure = lens _dReturnValuesOnConditionCheckFailure (\s a -> s {_dReturnValuesOnConditionCheckFailure = a})

-- | A condition that must be satisfied in order for a conditional delete to succeed.
dConditionExpression :: Lens' Delete (Maybe Text)
dConditionExpression = lens _dConditionExpression (\s a -> s {_dConditionExpression = a})

-- | The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
dKey :: Lens' Delete (HashMap Text (AttributeValue))
dKey = lens _dKey (\s a -> s {_dKey = a}) . _Map

-- | Name of the table in which the item to be deleted resides.
dTableName :: Lens' Delete Text
dTableName = lens _dTableName (\s a -> s {_dTableName = a})

instance Hashable Delete

instance NFData Delete

instance ToJSON Delete where
  toJSON Delete' {..} =
    object
      ( catMaybes
          [ ("ExpressionAttributeNames" .=) <$> _dExpressionAttributeNames,
            ("ExpressionAttributeValues" .=) <$> _dExpressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" .=)
              <$> _dReturnValuesOnConditionCheckFailure,
            ("ConditionExpression" .=) <$> _dConditionExpression,
            Just ("Key" .= _dKey),
            Just ("TableName" .= _dTableName)
          ]
      )
