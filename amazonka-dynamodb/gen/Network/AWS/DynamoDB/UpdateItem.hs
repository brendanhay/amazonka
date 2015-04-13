{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Edits an existing item's attributes, or adds a new item to the table if it
-- does not already exist. You can put, delete, or add attribute values. You can
-- also perform a conditional update on an existing item (insert a new attribute
-- name-value pair if it doesn't exist, or replace an existing name-value pair
-- if it has certain expected attribute values). If conditions are specified and
-- the item does not exist, then the operation fails and a new item is not
-- created.
--
-- You can also return the item's attribute values in the same /UpdateItem/
-- operation using the /ReturnValues/ parameter.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html>
module Network.AWS.DynamoDB.UpdateItem
    (
    -- * Request
      UpdateItem
    -- ** Request constructor
    , updateItem
    -- ** Request lenses
    , uiAttributeUpdates
    , uiConditionExpression
    , uiConditionalOperator
    , uiExpected
    , uiExpressionAttributeNames
    , uiExpressionAttributeValues
    , uiKey
    , uiReturnConsumedCapacity
    , uiReturnItemCollectionMetrics
    , uiReturnValues
    , uiTableName
    , uiUpdateExpression

    -- * Response
    , UpdateItemResponse
    -- ** Response constructor
    , updateItemResponse
    -- ** Response lenses
    , uirAttributes
    , uirConsumedCapacity
    , uirItemCollectionMetrics
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data UpdateItem = UpdateItem
    { _uiAttributeUpdates            :: Map Text AttributeValueUpdate
    , _uiConditionExpression         :: Maybe Text
    , _uiConditionalOperator         :: Maybe ConditionalOperator
    , _uiExpected                    :: Map Text ExpectedAttributeValue
    , _uiExpressionAttributeNames    :: Map Text Text
    , _uiExpressionAttributeValues   :: Map Text AttributeValue
    , _uiKey                         :: Map Text AttributeValue
    , _uiReturnConsumedCapacity      :: Maybe ReturnConsumedCapacity
    , _uiReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics
    , _uiReturnValues                :: Maybe ReturnValue
    , _uiTableName                   :: Text
    , _uiUpdateExpression            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'UpdateItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiAttributeUpdates' @::@ 'HashMap' 'Text' 'AttributeValueUpdate'
--
-- * 'uiConditionExpression' @::@ 'Maybe' 'Text'
--
-- * 'uiConditionalOperator' @::@ 'Maybe' 'ConditionalOperator'
--
-- * 'uiExpected' @::@ 'HashMap' 'Text' 'ExpectedAttributeValue'
--
-- * 'uiExpressionAttributeNames' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'uiExpressionAttributeValues' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'uiKey' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'uiReturnConsumedCapacity' @::@ 'Maybe' 'ReturnConsumedCapacity'
--
-- * 'uiReturnItemCollectionMetrics' @::@ 'Maybe' 'ReturnItemCollectionMetrics'
--
-- * 'uiReturnValues' @::@ 'Maybe' 'ReturnValue'
--
-- * 'uiTableName' @::@ 'Text'
--
-- * 'uiUpdateExpression' @::@ 'Maybe' 'Text'
--
updateItem :: Text -- ^ 'uiTableName'
           -> UpdateItem
updateItem p1 = UpdateItem
    { _uiTableName                   = p1
    , _uiKey                         = mempty
    , _uiAttributeUpdates            = mempty
    , _uiExpected                    = mempty
    , _uiConditionalOperator         = Nothing
    , _uiReturnValues                = Nothing
    , _uiReturnConsumedCapacity      = Nothing
    , _uiReturnItemCollectionMetrics = Nothing
    , _uiUpdateExpression            = Nothing
    , _uiConditionExpression         = Nothing
    , _uiExpressionAttributeNames    = mempty
    , _uiExpressionAttributeValues   = mempty
    }

-- | There is a newer parameter available. Use /UpdateExpression/ instead. Note
-- that if you use /AttributeUpdates/ and /UpdateExpression/ at the same time,
-- DynamoDB will return a /ValidationException/ exception.
--
-- This parameter can be used for modifying top-level attributes; however, it
-- does not support individual list or map elements.
--
-- The names of attributes to be modified, the action to perform on each, and
-- the new value for each. If you are updating an attribute that is an index key
-- attribute for any indexes on that table, the attribute type must match the
-- index key type defined in the /AttributesDefinition/ of the table description.
-- You can use /UpdateItem/ to update any nonkey attributes.
--
-- Attribute values cannot be null. String and Binary type attributes must have
-- lengths greater than zero. Set type attributes must not be empty. Requests
-- with empty values will be rejected with a /ValidationException/ exception.
--
-- Each /AttributeUpdates/ element consists of an attribute name to modify, along
-- with the following:
--
-- /Value/ - The new value, if applicable, for this attribute.
--
-- /Action/ - A value that specifies how to perform the update. This action is
-- only valid for an existing attribute whose data type is Number or is a set;
-- do not use 'ADD' for other data types.
--
-- If an item with the specified primary key is found in the table, the
-- following values perform the following actions:
--
-- 'PUT' - Adds the specified attribute to the item. If the attribute already
-- exists, it is replaced by the new value.
--
-- 'DELETE' - Removes the attribute and its value, if no value is specified for 'DELETE'. The data type of the specified value must match the existing value's
-- data type.
--
-- If a set of values is specified, then those values are subtracted from the
-- old set. For example, if the attribute value was the set '[a,b,c]' and the 'DELETE' action specifies '[a,c]', then the final attribute value is '[b]'. Specifying an
-- empty set is an error.
--
-- 'ADD' - Adds the specified value to the item, if the attribute does not
-- already exist. If the attribute does exist, then the behavior of 'ADD' depends
-- on the data type of the attribute:
--
-- If the existing attribute is a number, and if /Value/ is also a number, then /Value/ is mathematically added to the existing attribute. If /Value/ is a
-- negative number, then it is subtracted from the existing attribute.
--
-- If you use 'ADD' to increment or decrement a number value for an item that
-- doesn't exist before the update, DynamoDB uses 0 as the initial value.
--
-- Similarly, if you use 'ADD' for an existing item to increment or decrement an
-- attribute value that doesn't exist before the update, DynamoDB uses '0' as the
-- initial value. For example, suppose that the item you want to update doesn't
-- have an attribute named /itemcount/, but you decide to 'ADD' the number '3' to this
-- attribute anyway. DynamoDB will create the /itemcount/ attribute, set its
-- initial value to '0', and finally add '3' to it. The result will be a new /itemcount/ attribute, with a value of '3'.
--
-- If the existing data type is a set, and if /Value/ is also a set, then /Value/
-- is appended to the existing set. For example, if the attribute value is the
-- set '[1,2]', and the 'ADD' action specified '[3]', then the final attribute value
-- is '[1,2,3]'. An error occurs if an 'ADD' action is specified for a set attribute
-- and the attribute type specified does not match the existing set type.
--
-- Both sets must have the same primitive data type. For example, if the
-- existing data type is a set of strings, /Value/ must also be a set of strings.
--
-- If no item with the specified key is found in the table, the following
-- values perform the following actions:
--
-- 'PUT' - Causes DynamoDB to create a new item with the specified primary key,
-- and then adds the attribute.
--
-- 'DELETE' - Nothing happens, because attributes cannot be deleted from a
-- nonexistent item. The operation succeeds, but DynamoDB does not create a new
-- item.
--
-- 'ADD' - Causes DynamoDB to create an item with the supplied primary key and
-- number (or set of numbers) for the attribute value. The only data types
-- allowed are Number and Number Set.
--
-- If you provide any attributes that are part of an index key, then the
-- data types for those attributes must match those of the schema in the table's
-- attribute definition.
uiAttributeUpdates :: Lens' UpdateItem (HashMap Text AttributeValueUpdate)
uiAttributeUpdates =
    lens _uiAttributeUpdates (\s a -> s { _uiAttributeUpdates = a })
        . _Map

-- | A condition that must be satisfied in order for a conditional update to
-- succeed.
--
-- An expression can contain any of the following:
--
-- Boolean functions: 'attribute_exists | attribute_not_exists | contains |begins_with'
--
-- These function names are case-sensitive.
--
-- Comparison operators: ' = | <> | < | > | <= | >= | BETWEEN | IN'
--
-- Logical operators: 'AND | OR | NOT'
--
-- For more information on condition expressions, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
uiConditionExpression :: Lens' UpdateItem (Maybe Text)
uiConditionExpression =
    lens _uiConditionExpression (\s a -> s { _uiConditionExpression = a })

-- | There is a newer parameter available. Use /ConditionExpression/ instead. Note
-- that if you use /ConditionalOperator/ and / ConditionExpression / at the same
-- time, DynamoDB will return a /ValidationException/ exception.
--
-- A logical operator to apply to the conditions in the /Expected/ map:
--
-- 'AND' - If all of the conditions evaluate to true, then the entire map
-- evaluates to true.
--
-- 'OR' - If at least one of the conditions evaluate to true, then the entire map
-- evaluates to true.
--
-- If you omit /ConditionalOperator/, then 'AND' is the default.
--
-- The operation will succeed only if the entire map evaluates to true.
--
-- This parameter does not support attributes of type List or Map.
--
uiConditionalOperator :: Lens' UpdateItem (Maybe ConditionalOperator)
uiConditionalOperator =
    lens _uiConditionalOperator (\s a -> s { _uiConditionalOperator = a })

-- | There is a newer parameter available. Use / ConditionExpression / instead.
-- Note that if you use /Expected/ and / ConditionExpression / at the same time,
-- DynamoDB will return a /ValidationException/ exception.
--
-- A map of attribute/condition pairs. /Expected/ provides a conditional block
-- for the /UpdateItem/ operation.
--
-- Each element of /Expected/ consists of an attribute name, a comparison
-- operator, and one or more values. DynamoDB compares the attribute with the
-- value(s) you supplied, using the comparison operator. For each /Expected/
-- element, the result of the evaluation is either true or false.
--
-- If you specify more than one element in the /Expected/ map, then by default
-- all of the conditions must evaluate to true. In other words, the conditions
-- are ANDed together. (You can use the /ConditionalOperator/ parameter to OR the
-- conditions instead. If you do this, then at least one of the conditions must
-- evaluate to true, rather than all of them.)
--
-- If the /Expected/ map evaluates to true, then the conditional operation
-- succeeds; otherwise, it fails.
--
-- /Expected/ contains the following:
--
-- /AttributeValueList/ - One or more values to evaluate against the supplied
-- attribute. The number of values in the list depends on the /ComparisonOperator/
-- being used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are based on
-- ASCII character code values. For example, 'a' is greater than 'A', and 'a' is
-- greater than 'B'. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For type Binary, DynamoDB treats each byte of the binary data as unsigned
-- when it compares binary values.
--
-- /ComparisonOperator/ - A comparator for evaluating attributes in the /AttributeValueList/. When performing the comparison, DynamoDB uses strongly consistent reads.
--
-- The following comparison operators are available:
--
-- 'EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS |BEGINS_WITH | IN | BETWEEN'
--
-- The following are descriptions of each comparison operator.
--
-- 'EQ' : Equal. 'EQ' is supported for all datatypes, including lists and maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, Binary, String Set, Number Set, or Binary Set. If an item
-- contains an /AttributeValue/ element of a different type than the one provided
-- in the request, the value does not match. For example, '{"S":"6"}' does not
-- equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NE' : Not equal. 'NE' is supported for all datatypes, including lists and
-- maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, Binary, String Set, Number Set, or Binary Set. If an item contains an /AttributeValue/ of a different type than the one provided in the request, the
-- value does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LE' : Less than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LT' : Less than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, or Binary (not a set type). If an item contains an /AttributeValue/
-- element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GE' : Greater than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GT' : Greater than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NOT_NULL' : The attribute exists. 'NOT_NULL' is supported for all datatypes,
-- including lists and maps.
--
-- This operator tests for the existence of an attribute, not its data type. If
-- the data type of attribute "'a'" is null, and you evaluate it using 'NOT_NULL',
-- the result is a Boolean /true/. This result is because the attribute "'a'"
-- exists; its data type is not relevant to the 'NOT_NULL' comparison operator.
--
-- 'NULL' : The attribute does not exist. 'NULL' is supported for all datatypes,
-- including lists and maps.
--
-- This operator tests for the nonexistence of an attribute, not its data type.
-- If the data type of attribute "'a'" is null, and you evaluate it using 'NULL',
-- the result is a Boolean /false/. This is because the attribute "'a'" exists; its
-- data type is not relevant to the 'NULL' comparison operator.
--
-- 'CONTAINS' : Checks for a subsequence, or value in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is of type String, then the operator checks for a substring match.
-- If the target attribute of the comparison is of type Binary, then the
-- operator looks for a subsequence of the target that matches the input. If the
-- target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the
-- operator evaluates to true if it finds an exact match with any member of the
-- set.
--
-- CONTAINS is supported for lists: When evaluating "'a CONTAINS b'", "'a'" can be
-- a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'NOT_CONTAINS' : Checks for absence of a subsequence, or absence of a value
-- in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is a String, then the operator checks for the absence of a
-- substring match. If the target attribute of the comparison is Binary, then
-- the operator checks for the absence of a subsequence of the target that
-- matches the input. If the target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the operator evaluates to true if it /does not/ find an
-- exact match with any member of the set.
--
-- NOT_CONTAINS is supported for lists: When evaluating "'a NOT CONTAINS b'", "'a'"
-- can be a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'BEGINS_WITH' : Checks for a prefix.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String or
-- Binary (not a Number or a set type). The target attribute of the comparison
-- must be of type String or Binary (not a Number or a set type).
--
--
--
-- 'IN' : Checks for matching elements within two sets.
--
-- /AttributeValueList/ can contain one or more /AttributeValue/ elements of type
-- String, Number, or Binary (not a set type). These attributes are compared
-- against an existing set type attribute of an item. If any elements of the
-- input set are present in the item attribute, the expression evaluates to true.
--
-- 'BETWEEN' : Greater than or equal to the first value, and less than or equal
-- to the second value.
--
-- /AttributeValueList/ must contain two /AttributeValue/ elements of the same
-- type, either String, Number, or Binary (not a set type). A target attribute
-- matches if the target value is greater than, or equal to, the first element
-- and less than, or equal to, the second element. If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value
-- does not match. For example, '{"S":"6"}' does not compare to '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/.
--
-- For backward compatibility with previous DynamoDB releases, the following
-- parameters can be used instead of /AttributeValueList/ and /ComparisonOperator/:
--
-- /Value/ - A value for DynamoDB to compare with an attribute.
--
-- /Exists/ - A Boolean value that causes DynamoDB to evaluate the value before
-- attempting the conditional operation:
--
-- If /Exists/ is 'true', DynamoDB will check to see if that attribute value
-- already exists in the table. If it is found, then the condition evaluates to
-- true; otherwise the condition evaluate to false.
--
-- If /Exists/ is 'false', DynamoDB assumes that the attribute value does /not/
-- exist in the table. If in fact the value does not exist, then the assumption
-- is valid and the condition evaluates to true. If the value is found, despite
-- the assumption that it does not exist, the condition evaluates to false.
--
-- Note that the default value for /Exists/ is 'true'.
--
-- The /Value/ and /Exists/ parameters are incompatible with /AttributeValueList/
-- and /ComparisonOperator/. Note that if you use both sets of parameters at once,
-- DynamoDB will return a /ValidationException/ exception.
--
-- This parameter does not support attributes of type List or Map.
--
uiExpected :: Lens' UpdateItem (HashMap Text ExpectedAttributeValue)
uiExpected = lens _uiExpected (\s a -> s { _uiExpected = a }) . _Map

-- | One or more substitution tokens for attribute names in an expression. The
-- following are some use cases for using /ExpressionAttributeNames/:
--
-- To access an attribute whose name conflicts with a DynamoDB reserved word.
--
-- To create a placeholder for repeating occurrences of an attribute name in
-- an expression.
--
-- To prevent special characters in an attribute name from being
-- misinterpreted in an expression.
--
-- Use the # character in an expression to dereference an attribute name. For
-- example, consider the following attribute name:
--
-- 'Percentile'
--
-- The name of this attribute conflicts with a reserved word, so it cannot be
-- used directly in an expression. (For the complete list of reserved words, go
-- to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/). To work around
-- this, you could specify the following for /ExpressionAttributeNames/:
--
-- '{"#P":"Percentile"}'
--
-- You could then use this substitution in an expression, as in this example:
--
-- '#P = :val'
--
-- Tokens that begin with the : character are /expression attribute values/,
-- which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing ItemAttributes> in the /Amazon DynamoDB Developer Guide/.
uiExpressionAttributeNames :: Lens' UpdateItem (HashMap Text Text)
uiExpressionAttributeNames =
    lens _uiExpressionAttributeNames
        (\s a -> s { _uiExpressionAttributeNames = a })
            . _Map

-- | One or more values that can be substituted in an expression.
--
-- Use the : (colon) character in an expression to dereference an attribute
-- value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
--
-- 'Available | Backordered | Discontinued'
--
-- You would first need to specify /ExpressionAttributeValues/ as follows:
--
-- '{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"},":disc":{"S":"Discontinued"} }'
--
-- You could then use these values in an expression, such as this:
--
-- 'ProductStatus IN (:avail, :back, :disc)'
--
-- For more information on expression attribute values, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html SpecifyingConditions> in the /Amazon DynamoDB Developer Guide/.
uiExpressionAttributeValues :: Lens' UpdateItem (HashMap Text AttributeValue)
uiExpressionAttributeValues =
    lens _uiExpressionAttributeValues
        (\s a -> s { _uiExpressionAttributeValues = a })
            . _Map

-- | The primary key of the item to be updated. Each element consists of an
-- attribute name and a value for that attribute.
--
-- For the primary key, you must provide all of the attributes. For example,
-- with a hash type primary key, you only need to provide the hash attribute.
-- For a hash-and-range type primary key, you must provide both the hash
-- attribute and the range attribute.
uiKey :: Lens' UpdateItem (HashMap Text AttributeValue)
uiKey = lens _uiKey (\s a -> s { _uiKey = a }) . _Map

uiReturnConsumedCapacity :: Lens' UpdateItem (Maybe ReturnConsumedCapacity)
uiReturnConsumedCapacity =
    lens _uiReturnConsumedCapacity
        (\s a -> s { _uiReturnConsumedCapacity = a })

-- | A value that if set to 'SIZE', the response includes statistics about item
-- collections, if any, that were modified during the operation are returned in
-- the response. If set to 'NONE' (the default), no statistics are returned.
uiReturnItemCollectionMetrics :: Lens' UpdateItem (Maybe ReturnItemCollectionMetrics)
uiReturnItemCollectionMetrics =
    lens _uiReturnItemCollectionMetrics
        (\s a -> s { _uiReturnItemCollectionMetrics = a })

-- | Use /ReturnValues/ if you want to get the item attributes as they appeared
-- either before or after they were updated. For /UpdateItem/, the valid values
-- are:
--
-- 'NONE' - If /ReturnValues/ is not specified, or if its value is 'NONE', then
-- nothing is returned. (This setting is the default for /ReturnValues/.)
--
-- 'ALL_OLD' - If /UpdateItem/ overwrote an attribute name-value pair, then the
-- content of the old item is returned.
--
-- 'UPDATED_OLD' - The old versions of only the updated attributes are returned.
--
-- 'ALL_NEW' - All of the attributes of the new version of the item are
-- returned.
--
-- 'UPDATED_NEW' - The new versions of only the updated attributes are returned.
--
--
uiReturnValues :: Lens' UpdateItem (Maybe ReturnValue)
uiReturnValues = lens _uiReturnValues (\s a -> s { _uiReturnValues = a })

-- | The name of the table containing the item to update.
uiTableName :: Lens' UpdateItem Text
uiTableName = lens _uiTableName (\s a -> s { _uiTableName = a })

-- | An expression that defines one or more attributes to be updated, the action
-- to be performed on them, and new value(s) for them.
--
-- The following action values are available for /UpdateExpression/.
--
-- 'SET' - Adds one or more attributes and values to an item. If any of these
-- attribute already exist, they are replaced by the new values. You can also
-- use 'SET' to add or subtract from an attribute that is of type Number.
--
-- 'SET' supports the following functions:
--
-- 'if_not_exists (path, operand)' - if the item does not contain an attribute
-- at the specified path, then 'if_not_exists' evaluates to operand; otherwise, it
-- evaluates to path. You can use this function to avoid overwriting an
-- attribute that may already be present in the item.
--
-- 'list_append (operand, operand)' - evaluates to a list with a new element
-- added to it. You can append the new element to the start or the end of the
-- list by reversing the order of the operands.
--
-- These function names are case-sensitive.
--
-- 'REMOVE' - Removes one or more attributes from an item.
--
-- 'ADD' - Adds the specified value to the item, if the attribute does not
-- already exist. If the attribute does exist, then the behavior of 'ADD' depends
-- on the data type of the attribute:
--
-- If the existing attribute is a number, and if /Value/ is also a number, then /Value/ is mathematically added to the existing attribute. If /Value/ is a
-- negative number, then it is subtracted from the existing attribute.
--
-- If you use 'ADD' to increment or decrement a number value for an item that
-- doesn't exist before the update, DynamoDB uses '0' as the initial value.
--
-- Similarly, if you use 'ADD' for an existing item to increment or decrement an
-- attribute value that doesn't exist before the update, DynamoDB uses '0' as the
-- initial value. For example, suppose that the item you want to update doesn't
-- have an attribute named /itemcount/, but you decide to 'ADD' the number '3' to this
-- attribute anyway. DynamoDB will create the /itemcount/ attribute, set its
-- initial value to '0', and finally add '3' to it. The result will be a new /itemcount/ attribute in the item, with a value of '3'.
--
-- If the existing data type is a set and if /Value/ is also a set, then /Value/
-- is added to the existing set. For example, if the attribute value is the set '[1,2]', and the 'ADD' action specified '[3]', then the final attribute value is '[1,2,3]'. An error occurs if an
-- 'ADD' action is specified for a set attribute and the attribute type specified
-- does not match the existing set type.
--
-- Both sets must have the same primitive data type. For example, if the
-- existing data type is a set of strings, the /Value/ must also be a set of
-- strings.
--
-- The 'ADD' action only supports Number and set data types. In addition, 'ADD'
-- can only be used on top-level attributes, not nested attributes.
--
-- 'DELETE' - Deletes an element from a set.
--
-- If a set of values is specified, then those values are subtracted from the
-- old set. For example, if the attribute value was the set '[a,b,c]' and the 'DELETE' action specifies '[a,c]', then the final attribute value is '[b]'. Specifying an
-- empty set is an error.
--
-- The 'DELETE' action only supports Number and set data types. In addition, 'DELETE' can only be used on top-level attributes, not nested attributes.
--
-- You can have many actions in a single expression, such as the following: 'SET a=:value1, b=:value2 DELETE :value3, :value4, :value5'
--
-- For more information on update expressions, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items andAttributes> in the /Amazon DynamoDB Developer Guide/.
uiUpdateExpression :: Lens' UpdateItem (Maybe Text)
uiUpdateExpression =
    lens _uiUpdateExpression (\s a -> s { _uiUpdateExpression = a })

data UpdateItemResponse = UpdateItemResponse
    { _uirAttributes            :: Map Text AttributeValue
    , _uirConsumedCapacity      :: Maybe ConsumedCapacity
    , _uirItemCollectionMetrics :: Maybe ItemCollectionMetrics
    } deriving (Eq, Read, Show)

-- | 'UpdateItemResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uirAttributes' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'uirConsumedCapacity' @::@ 'Maybe' 'ConsumedCapacity'
--
-- * 'uirItemCollectionMetrics' @::@ 'Maybe' 'ItemCollectionMetrics'
--
updateItemResponse :: UpdateItemResponse
updateItemResponse = UpdateItemResponse
    { _uirAttributes            = mempty
    , _uirConsumedCapacity      = Nothing
    , _uirItemCollectionMetrics = Nothing
    }

-- | A map of attribute values as they appeared before the /UpdateItem/ operation.
-- This map only appears if /ReturnValues/ was specified as something other than 'NONE' in the request. Each element represents one attribute.
uirAttributes :: Lens' UpdateItemResponse (HashMap Text AttributeValue)
uirAttributes = lens _uirAttributes (\s a -> s { _uirAttributes = a }) . _Map

uirConsumedCapacity :: Lens' UpdateItemResponse (Maybe ConsumedCapacity)
uirConsumedCapacity =
    lens _uirConsumedCapacity (\s a -> s { _uirConsumedCapacity = a })

uirItemCollectionMetrics :: Lens' UpdateItemResponse (Maybe ItemCollectionMetrics)
uirItemCollectionMetrics =
    lens _uirItemCollectionMetrics
        (\s a -> s { _uirItemCollectionMetrics = a })

instance ToPath UpdateItem where
    toPath = const "/"

instance ToQuery UpdateItem where
    toQuery = const mempty

instance ToHeaders UpdateItem

instance ToJSON UpdateItem where
    toJSON UpdateItem{..} = object
        [ "TableName"                   .= _uiTableName
        , "Key"                         .= _uiKey
        , "AttributeUpdates"            .= _uiAttributeUpdates
        , "Expected"                    .= _uiExpected
        , "ConditionalOperator"         .= _uiConditionalOperator
        , "ReturnValues"                .= _uiReturnValues
        , "ReturnConsumedCapacity"      .= _uiReturnConsumedCapacity
        , "ReturnItemCollectionMetrics" .= _uiReturnItemCollectionMetrics
        , "UpdateExpression"            .= _uiUpdateExpression
        , "ConditionExpression"         .= _uiConditionExpression
        , "ExpressionAttributeNames"    .= _uiExpressionAttributeNames
        , "ExpressionAttributeValues"   .= _uiExpressionAttributeValues
        ]

instance AWSRequest UpdateItem where
    type Sv UpdateItem = DynamoDB
    type Rs UpdateItem = UpdateItemResponse

    request  = post "UpdateItem"
    response = jsonResponse

instance FromJSON UpdateItemResponse where
    parseJSON = withObject "UpdateItemResponse" $ \o -> UpdateItemResponse
        <$> o .:? "Attributes" .!= mempty
        <*> o .:? "ConsumedCapacity"
        <*> o .:? "ItemCollectionMetrics"
