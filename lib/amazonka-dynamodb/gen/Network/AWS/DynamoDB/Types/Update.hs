{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Update
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Update where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a request to perform an @UpdateItem@ operation.
--
--
--
-- /See:/ 'update' smart constructor.
data Update = Update'
  { _uExpressionAttributeNames ::
      !(Maybe (Map Text (Text))),
    _uExpressionAttributeValues ::
      !(Maybe (Map Text (AttributeValue))),
    _uReturnValuesOnConditionCheckFailure ::
      !(Maybe ReturnValuesOnConditionCheckFailure),
    _uConditionExpression :: !(Maybe Text),
    _uKey :: !(Map Text (AttributeValue)),
    _uUpdateExpression :: !Text,
    _uTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Update' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'uExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'uReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
--
-- * 'uConditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
--
-- * 'uKey' - The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
--
-- * 'uUpdateExpression' - An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
--
-- * 'uTableName' - Name of the table for the @UpdateItem@ request.
update ::
  -- | 'uUpdateExpression'
  Text ->
  -- | 'uTableName'
  Text ->
  Update
update pUpdateExpression_ pTableName_ =
  Update'
    { _uExpressionAttributeNames = Nothing,
      _uExpressionAttributeValues = Nothing,
      _uReturnValuesOnConditionCheckFailure = Nothing,
      _uConditionExpression = Nothing,
      _uKey = mempty,
      _uUpdateExpression = pUpdateExpression_,
      _uTableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression.
uExpressionAttributeNames :: Lens' Update (HashMap Text (Text))
uExpressionAttributeNames = lens _uExpressionAttributeNames (\s a -> s {_uExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
uExpressionAttributeValues :: Lens' Update (HashMap Text (AttributeValue))
uExpressionAttributeValues = lens _uExpressionAttributeValues (\s a -> s {_uExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
uReturnValuesOnConditionCheckFailure :: Lens' Update (Maybe ReturnValuesOnConditionCheckFailure)
uReturnValuesOnConditionCheckFailure = lens _uReturnValuesOnConditionCheckFailure (\s a -> s {_uReturnValuesOnConditionCheckFailure = a})

-- | A condition that must be satisfied in order for a conditional update to succeed.
uConditionExpression :: Lens' Update (Maybe Text)
uConditionExpression = lens _uConditionExpression (\s a -> s {_uConditionExpression = a})

-- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
uKey :: Lens' Update (HashMap Text (AttributeValue))
uKey = lens _uKey (\s a -> s {_uKey = a}) . _Map

-- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
uUpdateExpression :: Lens' Update Text
uUpdateExpression = lens _uUpdateExpression (\s a -> s {_uUpdateExpression = a})

-- | Name of the table for the @UpdateItem@ request.
uTableName :: Lens' Update Text
uTableName = lens _uTableName (\s a -> s {_uTableName = a})

instance Hashable Update

instance NFData Update

instance ToJSON Update where
  toJSON Update' {..} =
    object
      ( catMaybes
          [ ("ExpressionAttributeNames" .=) <$> _uExpressionAttributeNames,
            ("ExpressionAttributeValues" .=) <$> _uExpressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" .=)
              <$> _uReturnValuesOnConditionCheckFailure,
            ("ConditionExpression" .=) <$> _uConditionExpression,
            Just ("Key" .= _uKey),
            Just ("UpdateExpression" .= _uUpdateExpression),
            Just ("TableName" .= _uTableName)
          ]
      )
