{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Edits an existing item\'s attributes, or adds a new item to the table if
-- it does not already exist. You can put, delete, or add attribute values.
-- You can also perform a conditional update on an existing item (insert a
-- new attribute name-value pair if it doesn\'t exist, or replace an
-- existing name-value pair if it has certain expected attribute values).
-- If conditions are specified and the item does not exist, then the
-- operation fails and a new item is not created.
--
-- You can also return the item\'s attribute values in the same
-- /UpdateItem/ operation using the /ReturnValues/ parameter.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html>
module Network.AWS.DynamoDB.UpdateItem
    (
    -- * Request
      UpdateItem
    -- ** Request constructor
    , updateItem
    -- ** Request lenses
    , uirqReturnValues
    , uirqExpressionAttributeNames
    , uirqUpdateExpression
    , uirqAttributeUpdates
    , uirqReturnConsumedCapacity
    , uirqExpressionAttributeValues
    , uirqReturnItemCollectionMetrics
    , uirqConditionExpression
    , uirqConditionalOperator
    , uirqExpected
    , uirqTableName
    , uirqKey

    -- * Response
    , UpdateItemResponse
    -- ** Response constructor
    , updateItemResponse
    -- ** Response lenses
    , uirsConsumedCapacity
    , uirsItemCollectionMetrics
    , uirsAttributes
    , uirsStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an /UpdateItem/ operation.
--
-- /See:/ 'updateItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uirqReturnValues'
--
-- * 'uirqExpressionAttributeNames'
--
-- * 'uirqUpdateExpression'
--
-- * 'uirqAttributeUpdates'
--
-- * 'uirqReturnConsumedCapacity'
--
-- * 'uirqExpressionAttributeValues'
--
-- * 'uirqReturnItemCollectionMetrics'
--
-- * 'uirqConditionExpression'
--
-- * 'uirqConditionalOperator'
--
-- * 'uirqExpected'
--
-- * 'uirqTableName'
--
-- * 'uirqKey'
data UpdateItem = UpdateItem'
    { _uirqReturnValues                :: !(Maybe ReturnValue)
    , _uirqExpressionAttributeNames    :: !(Maybe (Map Text Text))
    , _uirqUpdateExpression            :: !(Maybe Text)
    , _uirqAttributeUpdates            :: !(Maybe (Map Text AttributeValueUpdate))
    , _uirqReturnConsumedCapacity      :: !(Maybe ReturnConsumedCapacity)
    , _uirqExpressionAttributeValues   :: !(Maybe (Map Text AttributeValue))
    , _uirqReturnItemCollectionMetrics :: !(Maybe ReturnItemCollectionMetrics)
    , _uirqConditionExpression         :: !(Maybe Text)
    , _uirqConditionalOperator         :: !(Maybe ConditionalOperator)
    , _uirqExpected                    :: !(Maybe (Map Text ExpectedAttributeValue))
    , _uirqTableName                   :: !Text
    , _uirqKey                         :: !(Map Text AttributeValue)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UpdateItem' smart constructor.
updateItem :: Text -> UpdateItem
updateItem pTableName =
    UpdateItem'
    { _uirqReturnValues = Nothing
    , _uirqExpressionAttributeNames = Nothing
    , _uirqUpdateExpression = Nothing
    , _uirqAttributeUpdates = Nothing
    , _uirqReturnConsumedCapacity = Nothing
    , _uirqExpressionAttributeValues = Nothing
    , _uirqReturnItemCollectionMetrics = Nothing
    , _uirqConditionExpression = Nothing
    , _uirqConditionalOperator = Nothing
    , _uirqExpected = Nothing
    , _uirqTableName = pTableName
    , _uirqKey = mempty
    }

-- | Use /ReturnValues/ if you want to get the item attributes as they
-- appeared either before or after they were updated. For /UpdateItem/, the
-- valid values are:
--
-- -   @NONE@ - If /ReturnValues/ is not specified, or if its value is
--     @NONE@, then nothing is returned. (This setting is the default for
--     /ReturnValues/.)
--
-- -   @ALL_OLD@ - If /UpdateItem/ overwrote an attribute name-value pair,
--     then the content of the old item is returned.
--
-- -   @UPDATED_OLD@ - The old versions of only the updated attributes are
--     returned.
--
-- -   @ALL_NEW@ - All of the attributes of the new version of the item are
--     returned.
--
-- -   @UPDATED_NEW@ - The new versions of only the updated attributes are
--     returned.
--
uirqReturnValues :: Lens' UpdateItem (Maybe ReturnValue)
uirqReturnValues = lens _uirqReturnValues (\ s a -> s{_uirqReturnValues = a});

-- | One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using /ExpressionAttributeNames/:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved
--     word.
--
-- -   To create a placeholder for repeating occurrences of an attribute
--     name in an expression.
--
-- -   To prevent special characters in an attribute name from being
--     misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute
-- name. For example, consider the following attribute name:
--
-- -   @Percentile@
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for /ExpressionAttributeNames/:
--
-- -   @{\"#P\":\"Percentile\"}@
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   @#P = :val@
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
uirqExpressionAttributeNames :: Lens' UpdateItem (HashMap Text Text)
uirqExpressionAttributeNames = lens _uirqExpressionAttributeNames (\ s a -> s{_uirqExpressionAttributeNames = a}) . _Default . _Map;

-- | An expression that defines one or more attributes to be updated, the
-- action to be performed on them, and new value(s) for them.
--
-- The following action values are available for /UpdateExpression/.
--
-- -   @SET@ - Adds one or more attributes and values to an item. If any of
--     these attribute already exist, they are replaced by the new values.
--     You can also use @SET@ to add or subtract from an attribute that is
--     of type Number. For example: @SET myNum = myNum + :val@
--
--     @SET@ supports the following functions:
--
--     -   @if_not_exists (path, operand)@ - if the item does not contain
--         an attribute at the specified path, then @if_not_exists@
--         evaluates to operand; otherwise, it evaluates to path. You can
--         use this function to avoid overwriting an attribute that may
--         already be present in the item.
--
--     -   @list_append (operand, operand)@ - evaluates to a list with a
--         new element added to it. You can append the new element to the
--         start or the end of the list by reversing the order of the
--         operands.
--
--     These function names are case-sensitive.
--
-- -   @REMOVE@ - Removes one or more attributes from an item.
--
-- -   @ADD@ - Adds the specified value to the item, if the attribute does
--     not already exist. If the attribute does exist, then the behavior of
--     @ADD@ depends on the data type of the attribute:
--
--     -   If the existing attribute is a number, and if /Value/ is also a
--         number, then /Value/ is mathematically added to the existing
--         attribute. If /Value/ is a negative number, then it is
--         subtracted from the existing attribute.
--
--         If you use @ADD@ to increment or decrement a number value for an
--         item that doesn\'t exist before the update, DynamoDB uses @0@ as
--         the initial value.
--
--         Similarly, if you use @ADD@ for an existing item to increment or
--         decrement an attribute value that doesn\'t exist before the
--         update, DynamoDB uses @0@ as the initial value. For example,
--         suppose that the item you want to update doesn\'t have an
--         attribute named /itemcount/, but you decide to @ADD@ the number
--         @3@ to this attribute anyway. DynamoDB will create the
--         /itemcount/ attribute, set its initial value to @0@, and finally
--         add @3@ to it. The result will be a new /itemcount/ attribute in
--         the item, with a value of @3@.
--
--     -   If the existing data type is a set and if /Value/ is also a set,
--         then /Value/ is added to the existing set. For example, if the
--         attribute value is the set @[1,2]@, and the @ADD@ action
--         specified @[3]@, then the final attribute value is @[1,2,3]@. An
--         error occurs if an @ADD@ action is specified for a set attribute
--         and the attribute type specified does not match the existing set
--         type.
--
--         Both sets must have the same primitive data type. For example,
--         if the existing data type is a set of strings, the /Value/ must
--         also be a set of strings.
--
--     The @ADD@ action only supports Number and set data types. In
--     addition, @ADD@ can only be used on top-level attributes, not nested
--     attributes.
--
-- -   @DELETE@ - Deletes an element from a set.
--
--     If a set of values is specified, then those values are subtracted
--     from the old set. For example, if the attribute value was the set
--     @[a,b,c]@ and the @DELETE@ action specifies @[a,c]@, then the final
--     attribute value is @[b]@. Specifying an empty set is an error.
--
--     The @DELETE@ action only supports set data types. In addition,
--     @DELETE@ can only be used on top-level attributes, not nested
--     attributes.
--
-- You can have many actions in a single expression, such as the following:
-- @SET a=:value1, b=:value2 DELETE :value3, :value4, :value5@
--
-- For more information on update expressions, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /UpdateExpression/ replaces the legacy /AttributeUpdates/ parameter.
uirqUpdateExpression :: Lens' UpdateItem (Maybe Text)
uirqUpdateExpression = lens _uirqUpdateExpression (\ s a -> s{_uirqUpdateExpression = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /UpdateExpression/ instead. Do not combine legacy parameters
-- and expression parameters in a single API call; otherwise, DynamoDB will
-- return a /ValidationException/ exception.
--
-- This parameter can be used for modifying top-level attributes; however,
-- it does not support individual list or map elements.
--
-- The names of attributes to be modified, the action to perform on each,
-- and the new value for each. If you are updating an attribute that is an
-- index key attribute for any indexes on that table, the attribute type
-- must match the index key type defined in the /AttributesDefinition/ of
-- the table description. You can use /UpdateItem/ to update any nonkey
-- attributes.
--
-- Attribute values cannot be null. String and Binary type attributes must
-- have lengths greater than zero. Set type attributes must not be empty.
-- Requests with empty values will be rejected with a /ValidationException/
-- exception.
--
-- Each /AttributeUpdates/ element consists of an attribute name to modify,
-- along with the following:
--
-- -   /Value/ - The new value, if applicable, for this attribute.
--
-- -   /Action/ - A value that specifies how to perform the update. This
--     action is only valid for an existing attribute whose data type is
--     Number or is a set; do not use @ADD@ for other data types.
--
--     If an item with the specified primary key is found in the table, the
--     following values perform the following actions:
--
--     -   @PUT@ - Adds the specified attribute to the item. If the
--         attribute already exists, it is replaced by the new value.
--
--     -   @DELETE@ - Removes the attribute and its value, if no value is
--         specified for @DELETE@. The data type of the specified value
--         must match the existing value\'s data type.
--
--         If a set of values is specified, then those values are
--         subtracted from the old set. For example, if the attribute value
--         was the set @[a,b,c]@ and the @DELETE@ action specifies @[a,c]@,
--         then the final attribute value is @[b]@. Specifying an empty set
--         is an error.
--
--     -   @ADD@ - Adds the specified value to the item, if the attribute
--         does not already exist. If the attribute does exist, then the
--         behavior of @ADD@ depends on the data type of the attribute:
--
--         -   If the existing attribute is a number, and if /Value/ is
--             also a number, then /Value/ is mathematically added to the
--             existing attribute. If /Value/ is a negative number, then it
--             is subtracted from the existing attribute.
--
--             If you use @ADD@ to increment or decrement a number value
--             for an item that doesn\'t exist before the update, DynamoDB
--             uses 0 as the initial value.
--
--             Similarly, if you use @ADD@ for an existing item to
--             increment or decrement an attribute value that doesn\'t
--             exist before the update, DynamoDB uses @0@ as the initial
--             value. For example, suppose that the item you want to update
--             doesn\'t have an attribute named /itemcount/, but you decide
--             to @ADD@ the number @3@ to this attribute anyway. DynamoDB
--             will create the /itemcount/ attribute, set its initial value
--             to @0@, and finally add @3@ to it. The result will be a new
--             /itemcount/ attribute, with a value of @3@.
--
--         -   If the existing data type is a set, and if /Value/ is also a
--             set, then /Value/ is appended to the existing set. For
--             example, if the attribute value is the set @[1,2]@, and the
--             @ADD@ action specified @[3]@, then the final attribute value
--             is @[1,2,3]@. An error occurs if an @ADD@ action is
--             specified for a set attribute and the attribute type
--             specified does not match the existing set type.
--
--             Both sets must have the same primitive data type. For
--             example, if the existing data type is a set of strings,
--             /Value/ must also be a set of strings.
--
--     If no item with the specified key is found in the table, the
--     following values perform the following actions:
--
--     -   @PUT@ - Causes DynamoDB to create a new item with the specified
--         primary key, and then adds the attribute.
--
--     -   @DELETE@ - Nothing happens, because attributes cannot be deleted
--         from a nonexistent item. The operation succeeds, but DynamoDB
--         does not create a new item.
--
--     -   @ADD@ - Causes DynamoDB to create an item with the supplied
--         primary key and number (or set of numbers) for the attribute
--         value. The only data types allowed are Number and Number Set.
--
-- If you provide any attributes that are part of an index key, then the
-- data types for those attributes must match those of the schema in the
-- table\'s attribute definition.
uirqAttributeUpdates :: Lens' UpdateItem (HashMap Text AttributeValueUpdate)
uirqAttributeUpdates = lens _uirqAttributeUpdates (\ s a -> s{_uirqAttributeUpdates = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
uirqReturnConsumedCapacity :: Lens' UpdateItem (Maybe ReturnConsumedCapacity)
uirqReturnConsumedCapacity = lens _uirqReturnConsumedCapacity (\ s a -> s{_uirqReturnConsumedCapacity = a});

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify /ExpressionAttributeValues/ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
uirqExpressionAttributeValues :: Lens' UpdateItem (HashMap Text AttributeValue)
uirqExpressionAttributeValues = lens _uirqExpressionAttributeValues (\ s a -> s{_uirqExpressionAttributeValues = a}) . _Default . _Map;

-- | Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
uirqReturnItemCollectionMetrics :: Lens' UpdateItem (Maybe ReturnItemCollectionMetrics)
uirqReturnItemCollectionMetrics = lens _uirqReturnItemCollectionMetrics (\ s a -> s{_uirqReturnItemCollectionMetrics = a});

-- | A condition that must be satisfied in order for a conditional update to
-- succeed.
--
-- An expression can contain any of the following:
--
-- -   Functions:
--     @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
--
--     These function names are case-sensitive.
--
-- -   Comparison operators: @ = | \<> | \< | > | \<= | >= | BETWEEN | IN@
--
-- -   Logical operators: @AND | OR | NOT@
--
-- For more information on condition expressions, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /ConditionExpression/ replaces the legacy /ConditionalOperator/ and
-- /Expected/ parameters.
uirqConditionExpression :: Lens' UpdateItem (Maybe Text)
uirqConditionExpression = lens _uirqConditionExpression (\ s a -> s{_uirqConditionExpression = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /ConditionExpression/ instead. Do not combine legacy
-- parameters and expression parameters in a single API call; otherwise,
-- DynamoDB will return a /ValidationException/ exception.
--
-- A logical operator to apply to the conditions in the /Expected/ map:
--
-- -   @AND@ - If all of the conditions evaluate to true, then the entire
--     map evaluates to true.
--
-- -   @OR@ - If at least one of the conditions evaluate to true, then the
--     entire map evaluates to true.
--
-- If you omit /ConditionalOperator/, then @AND@ is the default.
--
-- The operation will succeed only if the entire map evaluates to true.
--
-- This parameter does not support attributes of type List or Map.
uirqConditionalOperator :: Lens' UpdateItem (Maybe ConditionalOperator)
uirqConditionalOperator = lens _uirqConditionalOperator (\ s a -> s{_uirqConditionalOperator = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /ConditionExpression/ instead. Do not combine legacy
-- parameters and expression parameters in a single API call; otherwise,
-- DynamoDB will return a /ValidationException/ exception.
--
-- A map of attribute\/condition pairs. /Expected/ provides a conditional
-- block for the /UpdateItem/ operation.
--
-- Each element of /Expected/ consists of an attribute name, a comparison
-- operator, and one or more values. DynamoDB compares the attribute with
-- the value(s) you supplied, using the comparison operator. For each
-- /Expected/ element, the result of the evaluation is either true or
-- false.
--
-- If you specify more than one element in the /Expected/ map, then by
-- default all of the conditions must evaluate to true. In other words, the
-- conditions are ANDed together. (You can use the /ConditionalOperator/
-- parameter to OR the conditions instead. If you do this, then at least
-- one of the conditions must evaluate to true, rather than all of them.)
--
-- If the /Expected/ map evaluates to true, then the conditional operation
-- succeeds; otherwise, it fails.
--
-- /Expected/ contains the following:
--
-- -   /AttributeValueList/ - One or more values to evaluate against the
--     supplied attribute. The number of values in the list depends on the
--     /ComparisonOperator/ being used.
--
--     For type Number, value comparisons are numeric.
--
--     String value comparisons for greater than, equals, or less than are
--     based on ASCII character code values. For example, @a@ is greater
--     than @A@, and @a@ is greater than @B@. For a list of code values,
--     see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
--     For type Binary, DynamoDB treats each byte of the binary data as
--     unsigned when it compares binary values.
--
-- -   /ComparisonOperator/ - A comparator for evaluating attributes in the
--     /AttributeValueList/. When performing the comparison, DynamoDB uses
--     strongly consistent reads.
--
--     The following comparison operators are available:
--
--     @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
--
--     The following are descriptions of each comparison operator.
--
--     -   @EQ@ : Equal. @EQ@ is supported for all datatypes, including
--         lists and maps.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, Binary, String Set, Number Set,
--         or Binary Set. If an item contains an /AttributeValue/ element
--         of a different type than the one provided in the request, the
--         value does not match. For example, @{\"S\":\"6\"}@ does not
--         equal @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does not equal
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @NE@ : Not equal. @NE@ is supported for all datatypes, including
--         lists and maps.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String, Number, Binary, String Set, Number Set, or Binary
--         Set. If an item contains an /AttributeValue/ of a different type
--         than the one provided in the request, the value does not match.
--         For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@.
--         Also, @{\"N\":\"6\"}@ does not equal
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @LE@ : Less than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@.
--         Also, @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @LT@ : Less than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String, Number, or Binary (not a set type). If an item
--         contains an /AttributeValue/ element of a different type than
--         the one provided in the request, the value does not match. For
--         example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--         @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @GE@ : Greater than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@.
--         Also, @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @GT@ : Greater than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@.
--         Also, @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for
--         all datatypes, including lists and maps.
--
--         This operator tests for the existence of an attribute, not its
--         data type. If the data type of attribute \"@a@\" is null, and
--         you evaluate it using @NOT_NULL@, the result is a Boolean
--         /true/. This result is because the attribute \"@a@\" exists; its
--         data type is not relevant to the @NOT_NULL@ comparison operator.
--
--     -   @NULL@ : The attribute does not exist. @NULL@ is supported for
--         all datatypes, including lists and maps.
--
--         This operator tests for the nonexistence of an attribute, not
--         its data type. If the data type of attribute \"@a@\" is null,
--         and you evaluate it using @NULL@, the result is a Boolean
--         /false/. This is because the attribute \"@a@\" exists; its data
--         type is not relevant to the @NULL@ comparison operator.
--
--     -   @CONTAINS@ : Checks for a subsequence, or value in a set.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         the target attribute of the comparison is of type String, then
--         the operator checks for a substring match. If the target
--         attribute of the comparison is of type Binary, then the operator
--         looks for a subsequence of the target that matches the input. If
--         the target attribute of the comparison is a set (\"@SS@\",
--         \"@NS@\", or \"@BS@\"), then the operator evaluates to true if
--         it finds an exact match with any member of the set.
--
--         CONTAINS is supported for lists: When evaluating
--         \"@a CONTAINS b@\", \"@a@\" can be a list; however, \"@b@\"
--         cannot be a set, a map, or a list.
--
--     -   @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence
--         of a value in a set.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         the target attribute of the comparison is a String, then the
--         operator checks for the absence of a substring match. If the
--         target attribute of the comparison is Binary, then the operator
--         checks for the absence of a subsequence of the target that
--         matches the input. If the target attribute of the comparison is
--         a set (\"@SS@\", \"@NS@\", or \"@BS@\"), then the operator
--         evaluates to true if it /does not/ find an exact match with any
--         member of the set.
--
--         NOT_CONTAINS is supported for lists: When evaluating
--         \"@a NOT CONTAINS b@\", \"@a@\" can be a list; however, \"@b@\"
--         cannot be a set, a map, or a list.
--
--     -   @BEGINS_WITH@ : Checks for a prefix.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String or Binary (not a Number or a set type). The target
--         attribute of the comparison must be of type String or Binary
--         (not a Number or a set type).
--
--     -   @IN@ : Checks for matching elements within two sets.
--
--         /AttributeValueList/ can contain one or more /AttributeValue/
--         elements of type String, Number, or Binary (not a set type).
--         These attributes are compared against an existing set type
--         attribute of an item. If any elements of the input set are
--         present in the item attribute, the expression evaluates to true.
--
--     -   @BETWEEN@ : Greater than or equal to the first value, and less
--         than or equal to the second value.
--
--         /AttributeValueList/ must contain two /AttributeValue/ elements
--         of the same type, either String, Number, or Binary (not a set
--         type). A target attribute matches if the target value is greater
--         than, or equal to, the first element and less than, or equal to,
--         the second element. If an item contains an /AttributeValue/
--         element of a different type than the one provided in the
--         request, the value does not match. For example, @{\"S\":\"6\"}@
--         does not compare to @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does
--         not compare to @{\"NS\":[\"6\", \"2\", \"1\"]}@
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- For backward compatibility with previous DynamoDB releases, the
-- following parameters can be used instead of /AttributeValueList/ and
-- /ComparisonOperator/:
--
-- -   /Value/ - A value for DynamoDB to compare with an attribute.
--
-- -   /Exists/ - A Boolean value that causes DynamoDB to evaluate the
--     value before attempting the conditional operation:
--
--     -   If /Exists/ is @true@, DynamoDB will check to see if that
--         attribute value already exists in the table. If it is found,
--         then the condition evaluates to true; otherwise the condition
--         evaluate to false.
--
--     -   If /Exists/ is @false@, DynamoDB assumes that the attribute
--         value does /not/ exist in the table. If in fact the value does
--         not exist, then the assumption is valid and the condition
--         evaluates to true. If the value is found, despite the assumption
--         that it does not exist, the condition evaluates to false.
--
--     Note that the default value for /Exists/ is @true@.
--
-- The /Value/ and /Exists/ parameters are incompatible with
-- /AttributeValueList/ and /ComparisonOperator/. Note that if you use both
-- sets of parameters at once, DynamoDB will return a /ValidationException/
-- exception.
--
-- This parameter does not support attributes of type List or Map.
uirqExpected :: Lens' UpdateItem (HashMap Text ExpectedAttributeValue)
uirqExpected = lens _uirqExpected (\ s a -> s{_uirqExpected = a}) . _Default . _Map;

-- | The name of the table containing the item to update.
uirqTableName :: Lens' UpdateItem Text
uirqTableName = lens _uirqTableName (\ s a -> s{_uirqTableName = a});

-- | The primary key of the item to be updated. Each element consists of an
-- attribute name and a value for that attribute.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a hash type primary key, you only need to provide the hash
-- attribute. For a hash-and-range type primary key, you must provide both
-- the hash attribute and the range attribute.
uirqKey :: Lens' UpdateItem (HashMap Text AttributeValue)
uirqKey = lens _uirqKey (\ s a -> s{_uirqKey = a}) . _Map;

instance AWSRequest UpdateItem where
        type Sv UpdateItem = DynamoDB
        type Rs UpdateItem = UpdateItemResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateItemResponse' <$>
                   (x .?> "ConsumedCapacity") <*>
                     (x .?> "ItemCollectionMetrics")
                     <*> (x .?> "Attributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders UpdateItem where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.UpdateItem" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UpdateItem where
        toJSON UpdateItem'{..}
          = object
              ["ReturnValues" .= _uirqReturnValues,
               "ExpressionAttributeNames" .=
                 _uirqExpressionAttributeNames,
               "UpdateExpression" .= _uirqUpdateExpression,
               "AttributeUpdates" .= _uirqAttributeUpdates,
               "ReturnConsumedCapacity" .=
                 _uirqReturnConsumedCapacity,
               "ExpressionAttributeValues" .=
                 _uirqExpressionAttributeValues,
               "ReturnItemCollectionMetrics" .=
                 _uirqReturnItemCollectionMetrics,
               "ConditionExpression" .= _uirqConditionExpression,
               "ConditionalOperator" .= _uirqConditionalOperator,
               "Expected" .= _uirqExpected,
               "TableName" .= _uirqTableName, "Key" .= _uirqKey]

instance ToPath UpdateItem where
        toPath = const "/"

instance ToQuery UpdateItem where
        toQuery = const mempty

-- | Represents the output of an /UpdateItem/ operation.
--
-- /See:/ 'updateItemResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uirsConsumedCapacity'
--
-- * 'uirsItemCollectionMetrics'
--
-- * 'uirsAttributes'
--
-- * 'uirsStatus'
data UpdateItemResponse = UpdateItemResponse'
    { _uirsConsumedCapacity      :: !(Maybe ConsumedCapacity)
    , _uirsItemCollectionMetrics :: !(Maybe ItemCollectionMetrics)
    , _uirsAttributes            :: !(Maybe (Map Text AttributeValue))
    , _uirsStatus                :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UpdateItemResponse' smart constructor.
updateItemResponse :: Int -> UpdateItemResponse
updateItemResponse pStatus =
    UpdateItemResponse'
    { _uirsConsumedCapacity = Nothing
    , _uirsItemCollectionMetrics = Nothing
    , _uirsAttributes = Nothing
    , _uirsStatus = pStatus
    }

-- | FIXME: Undocumented member.
uirsConsumedCapacity :: Lens' UpdateItemResponse (Maybe ConsumedCapacity)
uirsConsumedCapacity = lens _uirsConsumedCapacity (\ s a -> s{_uirsConsumedCapacity = a});

-- | FIXME: Undocumented member.
uirsItemCollectionMetrics :: Lens' UpdateItemResponse (Maybe ItemCollectionMetrics)
uirsItemCollectionMetrics = lens _uirsItemCollectionMetrics (\ s a -> s{_uirsItemCollectionMetrics = a});

-- | A map of attribute values as they appeared before the /UpdateItem/
-- operation. This map only appears if /ReturnValues/ was specified as
-- something other than @NONE@ in the request. Each element represents one
-- attribute.
uirsAttributes :: Lens' UpdateItemResponse (HashMap Text AttributeValue)
uirsAttributes = lens _uirsAttributes (\ s a -> s{_uirsAttributes = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
uirsStatus :: Lens' UpdateItemResponse Int
uirsStatus = lens _uirsStatus (\ s a -> s{_uirsStatus = a});
