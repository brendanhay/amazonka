{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ConditionCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ConditionCheck where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a request to perform a check that an item exists or to check the condition of specific attributes of the item.
--
--
--
-- /See:/ 'conditionCheck' smart constructor.
data ConditionCheck = ConditionCheck'
  { _ccExpressionAttributeNames ::
      !(Maybe (Map Text (Text))),
    _ccExpressionAttributeValues ::
      !(Maybe (Map Text (AttributeValue))),
    _ccReturnValuesOnConditionCheckFailure ::
      !(Maybe ReturnValuesOnConditionCheckFailure),
    _ccKey :: !(Map Text (AttributeValue)),
    _ccTableName :: !Text,
    _ccConditionExpression :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConditionCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'ccExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'ccReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- * 'ccKey' - The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
--
-- * 'ccTableName' - Name of the table for the check item request.
--
-- * 'ccConditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
conditionCheck ::
  -- | 'ccTableName'
  Text ->
  -- | 'ccConditionExpression'
  Text ->
  ConditionCheck
conditionCheck pTableName_ pConditionExpression_ =
  ConditionCheck'
    { _ccExpressionAttributeNames = Nothing,
      _ccExpressionAttributeValues = Nothing,
      _ccReturnValuesOnConditionCheckFailure = Nothing,
      _ccKey = mempty,
      _ccTableName = pTableName_,
      _ccConditionExpression = pConditionExpression_
    }

-- | One or more substitution tokens for attribute names in an expression.
ccExpressionAttributeNames :: Lens' ConditionCheck (HashMap Text (Text))
ccExpressionAttributeNames = lens _ccExpressionAttributeNames (\s a -> s {_ccExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
ccExpressionAttributeValues :: Lens' ConditionCheck (HashMap Text (AttributeValue))
ccExpressionAttributeValues = lens _ccExpressionAttributeValues (\s a -> s {_ccExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
ccReturnValuesOnConditionCheckFailure :: Lens' ConditionCheck (Maybe ReturnValuesOnConditionCheckFailure)
ccReturnValuesOnConditionCheckFailure = lens _ccReturnValuesOnConditionCheckFailure (\s a -> s {_ccReturnValuesOnConditionCheckFailure = a})

-- | The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
ccKey :: Lens' ConditionCheck (HashMap Text (AttributeValue))
ccKey = lens _ccKey (\s a -> s {_ccKey = a}) . _Map

-- | Name of the table for the check item request.
ccTableName :: Lens' ConditionCheck Text
ccTableName = lens _ccTableName (\s a -> s {_ccTableName = a})

-- | A condition that must be satisfied in order for a conditional update to succeed.
ccConditionExpression :: Lens' ConditionCheck Text
ccConditionExpression = lens _ccConditionExpression (\s a -> s {_ccConditionExpression = a})

instance Hashable ConditionCheck

instance NFData ConditionCheck

instance ToJSON ConditionCheck where
  toJSON ConditionCheck' {..} =
    object
      ( catMaybes
          [ ("ExpressionAttributeNames" .=) <$> _ccExpressionAttributeNames,
            ("ExpressionAttributeValues" .=) <$> _ccExpressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" .=)
              <$> _ccReturnValuesOnConditionCheckFailure,
            Just ("Key" .= _ccKey),
            Just ("TableName" .= _ccTableName),
            Just ("ConditionExpression" .= _ccConditionExpression)
          ]
      )
