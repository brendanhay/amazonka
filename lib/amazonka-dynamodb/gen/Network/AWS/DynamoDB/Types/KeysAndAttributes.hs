{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KeysAndAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KeysAndAttributes where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a set of primary keys and, for each key, the attributes to retrieve from the table.
--
--
-- For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide the partition key. For a composite primary key, you must provide /both/ the partition key and the sort key.
--
--
-- /See:/ 'keysAndAttributes' smart constructor.
data KeysAndAttributes = KeysAndAttributes'
  { _kaaProjectionExpression ::
      !(Maybe Text),
    _kaaAttributesToGet :: !(Maybe (List1 Text)),
    _kaaExpressionAttributeNames ::
      !(Maybe (Map Text (Text))),
    _kaaConsistentRead :: !(Maybe Bool),
    _kaaKeys :: !(List1 (Map Text (AttributeValue)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeysAndAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kaaProjectionExpression' - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas. If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'kaaAttributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'kaaExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'kaaConsistentRead' - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
-- * 'kaaKeys' - The primary key attribute values that define the items and the attributes associated with the items.
keysAndAttributes ::
  -- | 'kaaKeys'
  NonEmpty (HashMap Text (AttributeValue)) ->
  KeysAndAttributes
keysAndAttributes pKeys_ =
  KeysAndAttributes'
    { _kaaProjectionExpression = Nothing,
      _kaaAttributesToGet = Nothing,
      _kaaExpressionAttributeNames = Nothing,
      _kaaConsistentRead = Nothing,
      _kaaKeys = _List1 # pKeys_
    }

-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas. If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
kaaProjectionExpression :: Lens' KeysAndAttributes (Maybe Text)
kaaProjectionExpression = lens _kaaProjectionExpression (\s a -> s {_kaaProjectionExpression = a})

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
kaaAttributesToGet :: Lens' KeysAndAttributes (Maybe (NonEmpty Text))
kaaAttributesToGet = lens _kaaAttributesToGet (\s a -> s {_kaaAttributesToGet = a}) . mapping _List1

-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
kaaExpressionAttributeNames :: Lens' KeysAndAttributes (HashMap Text (Text))
kaaExpressionAttributeNames = lens _kaaExpressionAttributeNames (\s a -> s {_kaaExpressionAttributeNames = a}) . _Default . _Map

-- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
kaaConsistentRead :: Lens' KeysAndAttributes (Maybe Bool)
kaaConsistentRead = lens _kaaConsistentRead (\s a -> s {_kaaConsistentRead = a})

-- | The primary key attribute values that define the items and the attributes associated with the items.
kaaKeys :: Lens' KeysAndAttributes (NonEmpty (HashMap Text (AttributeValue)))
kaaKeys = lens _kaaKeys (\s a -> s {_kaaKeys = a}) . _List1

instance FromJSON KeysAndAttributes where
  parseJSON =
    withObject
      "KeysAndAttributes"
      ( \x ->
          KeysAndAttributes'
            <$> (x .:? "ProjectionExpression")
            <*> (x .:? "AttributesToGet")
            <*> (x .:? "ExpressionAttributeNames" .!= mempty)
            <*> (x .:? "ConsistentRead")
            <*> (x .: "Keys")
      )

instance Hashable KeysAndAttributes

instance NFData KeysAndAttributes

instance ToJSON KeysAndAttributes where
  toJSON KeysAndAttributes' {..} =
    object
      ( catMaybes
          [ ("ProjectionExpression" .=) <$> _kaaProjectionExpression,
            ("AttributesToGet" .=) <$> _kaaAttributesToGet,
            ("ExpressionAttributeNames" .=) <$> _kaaExpressionAttributeNames,
            ("ConsistentRead" .=) <$> _kaaConsistentRead,
            Just ("Keys" .= _kaaKeys)
          ]
      )
