{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edits an existing item\'s attributes, or adds a new item to the table if
-- it does not already exist. You can put, delete, or add attribute values.
-- You can also perform a conditional update on an existing item (insert a
-- new attribute name-value pair if it doesn\'t exist, or replace an
-- existing name-value pair if it has certain expected attribute values).
--
-- You can also return the item\'s attribute values in the same
-- @UpdateItem@ operation using the @ReturnValues@ parameter.
module Network.AWS.DynamoDB.UpdateItem
  ( -- * Creating a Request
    UpdateItem (..),
    newUpdateItem,

    -- * Request Lenses
    updateItem_expected,
    updateItem_expressionAttributeValues,
    updateItem_returnItemCollectionMetrics,
    updateItem_updateExpression,
    updateItem_expressionAttributeNames,
    updateItem_returnValues,
    updateItem_conditionExpression,
    updateItem_attributeUpdates,
    updateItem_returnConsumedCapacity,
    updateItem_conditionalOperator,
    updateItem_tableName,
    updateItem_key,

    -- * Destructuring the Response
    UpdateItemResponse (..),
    newUpdateItemResponse,

    -- * Response Lenses
    updateItemResponse_itemCollectionMetrics,
    updateItemResponse_attributes,
    updateItemResponse_consumedCapacity,
    updateItemResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateItem@ operation.
--
-- /See:/ 'newUpdateItem' smart constructor.
data UpdateItem = UpdateItem'
  { -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
    -- in the /Amazon DynamoDB Developer Guide/.
    expected :: Core.Maybe (Core.HashMap Core.Text ExpectedAttributeValue),
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an
    -- attribute value. For example, suppose that you wanted to check whether
    -- the value of the @ProductStatus@ attribute was one of the following:
    --
    -- @Available | Backordered | Discontinued@
    --
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    --
    -- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
    --
    -- You could then use these values in an expression, such as this:
    --
    -- @ProductStatus IN (:avail, :back, :disc)@
    --
    -- For more information on expression attribute values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeValues :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | Determines whether item collection metrics are returned. If set to
    -- @SIZE@, the response includes statistics about item collections, if any,
    -- that were modified during the operation are returned in the response. If
    -- set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Core.Maybe ReturnItemCollectionMetrics,
    -- | An expression that defines one or more attributes to be updated, the
    -- action to be performed on them, and new values for them.
    --
    -- The following action values are available for @UpdateExpression@.
    --
    -- -   @SET@ - Adds one or more attributes and values to an item. If any of
    --     these attributes already exist, they are replaced by the new values.
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
    --     -   If the existing attribute is a number, and if @Value@ is also a
    --         number, then @Value@ is mathematically added to the existing
    --         attribute. If @Value@ is a negative number, then it is
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
    --         attribute named @itemcount@, but you decide to @ADD@ the number
    --         @3@ to this attribute anyway. DynamoDB will create the
    --         @itemcount@ attribute, set its initial value to @0@, and finally
    --         add @3@ to it. The result will be a new @itemcount@ attribute in
    --         the item, with a value of @3@.
    --
    --     -   If the existing data type is a set and if @Value@ is also a set,
    --         then @Value@ is added to the existing set. For example, if the
    --         attribute value is the set @[1,2]@, and the @ADD@ action
    --         specified @[3]@, then the final attribute value is @[1,2,3]@. An
    --         error occurs if an @ADD@ action is specified for a set attribute
    --         and the attribute type specified does not match the existing set
    --         type.
    --
    --         Both sets must have the same primitive data type. For example,
    --         if the existing data type is a set of strings, the @Value@ must
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    updateExpression :: Core.Maybe Core.Text,
    -- | One or more substitution tokens for attribute names in an expression.
    -- The following are some use cases for using @ExpressionAttributeNames@:
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
    -- in the /Amazon DynamoDB Developer Guide/.) To work around this, you
    -- could specify the following for @ExpressionAttributeNames@:
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
    -- For more information about expression attribute names, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Use @ReturnValues@ if you want to get the item attributes as they appear
    -- before or after they are updated. For @UpdateItem@, the valid values
    -- are:
    --
    -- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
    --     @NONE@, then nothing is returned. (This setting is the default for
    --     @ReturnValues@.)
    --
    -- -   @ALL_OLD@ - Returns all of the attributes of the item, as they
    --     appeared before the UpdateItem operation.
    --
    -- -   @UPDATED_OLD@ - Returns only the updated attributes, as they
    --     appeared before the UpdateItem operation.
    --
    -- -   @ALL_NEW@ - Returns all of the attributes of the item, as they
    --     appear after the UpdateItem operation.
    --
    -- -   @UPDATED_NEW@ - Returns only the updated attributes, as they appear
    --     after the UpdateItem operation.
    --
    -- There is no additional cost associated with requesting a return value
    -- aside from the small network and processing overhead of receiving a
    -- larger response. No read capacity units are consumed.
    --
    -- The values returned are strongly consistent.
    returnValues :: Core.Maybe ReturnValue,
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
    -- -   Comparison operators: @= | \<> | \< | > | \<= | >= | BETWEEN | IN @
    --
    -- -   Logical operators: @AND | OR | NOT@
    --
    -- For more information about condition expressions, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionExpression :: Core.Maybe Core.Text,
    -- | This is a legacy parameter. Use @UpdateExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributeUpdates.html AttributeUpdates>
    -- in the /Amazon DynamoDB Developer Guide/.
    attributeUpdates :: Core.Maybe (Core.HashMap Core.Text AttributeValueUpdate),
    returnConsumedCapacity :: Core.Maybe ReturnConsumedCapacity,
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionalOperator :: Core.Maybe ConditionalOperator,
    -- | The name of the table containing the item to update.
    tableName :: Core.Text,
    -- | The primary key of the item to be updated. Each element consists of an
    -- attribute name and a value for that attribute.
    --
    -- For the primary key, you must provide all of the attributes. For
    -- example, with a simple primary key, you only need to provide a value for
    -- the partition key. For a composite primary key, you must provide values
    -- for both the partition key and the sort key.
    key :: Core.HashMap Core.Text AttributeValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expected', 'updateItem_expected' - This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'expressionAttributeValues', 'updateItem_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the @ProductStatus@ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify @ExpressionAttributeValues@ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnItemCollectionMetrics', 'updateItem_returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
--
-- 'updateExpression', 'updateItem_updateExpression' - An expression that defines one or more attributes to be updated, the
-- action to be performed on them, and new values for them.
--
-- The following action values are available for @UpdateExpression@.
--
-- -   @SET@ - Adds one or more attributes and values to an item. If any of
--     these attributes already exist, they are replaced by the new values.
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
--     -   If the existing attribute is a number, and if @Value@ is also a
--         number, then @Value@ is mathematically added to the existing
--         attribute. If @Value@ is a negative number, then it is
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
--         attribute named @itemcount@, but you decide to @ADD@ the number
--         @3@ to this attribute anyway. DynamoDB will create the
--         @itemcount@ attribute, set its initial value to @0@, and finally
--         add @3@ to it. The result will be a new @itemcount@ attribute in
--         the item, with a value of @3@.
--
--     -   If the existing data type is a set and if @Value@ is also a set,
--         then @Value@ is added to the existing set. For example, if the
--         attribute value is the set @[1,2]@, and the @ADD@ action
--         specified @[3]@, then the final attribute value is @[1,2,3]@. An
--         error occurs if an @ADD@ action is specified for a set attribute
--         and the attribute type specified does not match the existing set
--         type.
--
--         Both sets must have the same primitive data type. For example,
--         if the existing data type is a set of strings, the @Value@ must
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'expressionAttributeNames', 'updateItem_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/.) To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
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
-- For more information about expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnValues', 'updateItem_returnValues' - Use @ReturnValues@ if you want to get the item attributes as they appear
-- before or after they are updated. For @UpdateItem@, the valid values
-- are:
--
-- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
--     @NONE@, then nothing is returned. (This setting is the default for
--     @ReturnValues@.)
--
-- -   @ALL_OLD@ - Returns all of the attributes of the item, as they
--     appeared before the UpdateItem operation.
--
-- -   @UPDATED_OLD@ - Returns only the updated attributes, as they
--     appeared before the UpdateItem operation.
--
-- -   @ALL_NEW@ - Returns all of the attributes of the item, as they
--     appear after the UpdateItem operation.
--
-- -   @UPDATED_NEW@ - Returns only the updated attributes, as they appear
--     after the UpdateItem operation.
--
-- There is no additional cost associated with requesting a return value
-- aside from the small network and processing overhead of receiving a
-- larger response. No read capacity units are consumed.
--
-- The values returned are strongly consistent.
--
-- 'conditionExpression', 'updateItem_conditionExpression' - A condition that must be satisfied in order for a conditional update to
-- succeed.
--
-- An expression can contain any of the following:
--
-- -   Functions:
--     @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
--
--     These function names are case-sensitive.
--
-- -   Comparison operators: @= | \<> | \< | > | \<= | >= | BETWEEN | IN @
--
-- -   Logical operators: @AND | OR | NOT@
--
-- For more information about condition expressions, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'attributeUpdates', 'updateItem_attributeUpdates' - This is a legacy parameter. Use @UpdateExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributeUpdates.html AttributeUpdates>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnConsumedCapacity', 'updateItem_returnConsumedCapacity' - Undocumented member.
--
-- 'conditionalOperator', 'updateItem_conditionalOperator' - This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'tableName', 'updateItem_tableName' - The name of the table containing the item to update.
--
-- 'key', 'updateItem_key' - The primary key of the item to be updated. Each element consists of an
-- attribute name and a value for that attribute.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a simple primary key, you only need to provide a value for
-- the partition key. For a composite primary key, you must provide values
-- for both the partition key and the sort key.
newUpdateItem ::
  -- | 'tableName'
  Core.Text ->
  UpdateItem
newUpdateItem pTableName_ =
  UpdateItem'
    { expected = Core.Nothing,
      expressionAttributeValues = Core.Nothing,
      returnItemCollectionMetrics = Core.Nothing,
      updateExpression = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      returnValues = Core.Nothing,
      conditionExpression = Core.Nothing,
      attributeUpdates = Core.Nothing,
      returnConsumedCapacity = Core.Nothing,
      conditionalOperator = Core.Nothing,
      tableName = pTableName_,
      key = Core.mempty
    }

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
-- in the /Amazon DynamoDB Developer Guide/.
updateItem_expected :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Core.Text ExpectedAttributeValue))
updateItem_expected = Lens.lens (\UpdateItem' {expected} -> expected) (\s@UpdateItem' {} a -> s {expected = a} :: UpdateItem) Core.. Lens.mapping Lens._Coerce

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the @ProductStatus@ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify @ExpressionAttributeValues@ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
updateItem_expressionAttributeValues :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Core.Text AttributeValue))
updateItem_expressionAttributeValues = Lens.lens (\UpdateItem' {expressionAttributeValues} -> expressionAttributeValues) (\s@UpdateItem' {} a -> s {expressionAttributeValues = a} :: UpdateItem) Core.. Lens.mapping Lens._Coerce

-- | Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
updateItem_returnItemCollectionMetrics :: Lens.Lens' UpdateItem (Core.Maybe ReturnItemCollectionMetrics)
updateItem_returnItemCollectionMetrics = Lens.lens (\UpdateItem' {returnItemCollectionMetrics} -> returnItemCollectionMetrics) (\s@UpdateItem' {} a -> s {returnItemCollectionMetrics = a} :: UpdateItem)

-- | An expression that defines one or more attributes to be updated, the
-- action to be performed on them, and new values for them.
--
-- The following action values are available for @UpdateExpression@.
--
-- -   @SET@ - Adds one or more attributes and values to an item. If any of
--     these attributes already exist, they are replaced by the new values.
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
--     -   If the existing attribute is a number, and if @Value@ is also a
--         number, then @Value@ is mathematically added to the existing
--         attribute. If @Value@ is a negative number, then it is
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
--         attribute named @itemcount@, but you decide to @ADD@ the number
--         @3@ to this attribute anyway. DynamoDB will create the
--         @itemcount@ attribute, set its initial value to @0@, and finally
--         add @3@ to it. The result will be a new @itemcount@ attribute in
--         the item, with a value of @3@.
--
--     -   If the existing data type is a set and if @Value@ is also a set,
--         then @Value@ is added to the existing set. For example, if the
--         attribute value is the set @[1,2]@, and the @ADD@ action
--         specified @[3]@, then the final attribute value is @[1,2,3]@. An
--         error occurs if an @ADD@ action is specified for a set attribute
--         and the attribute type specified does not match the existing set
--         type.
--
--         Both sets must have the same primitive data type. For example,
--         if the existing data type is a set of strings, the @Value@ must
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
updateItem_updateExpression :: Lens.Lens' UpdateItem (Core.Maybe Core.Text)
updateItem_updateExpression = Lens.lens (\UpdateItem' {updateExpression} -> updateExpression) (\s@UpdateItem' {} a -> s {updateExpression = a} :: UpdateItem)

-- | One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/.) To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
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
-- For more information about expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
updateItem_expressionAttributeNames :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateItem_expressionAttributeNames = Lens.lens (\UpdateItem' {expressionAttributeNames} -> expressionAttributeNames) (\s@UpdateItem' {} a -> s {expressionAttributeNames = a} :: UpdateItem) Core.. Lens.mapping Lens._Coerce

-- | Use @ReturnValues@ if you want to get the item attributes as they appear
-- before or after they are updated. For @UpdateItem@, the valid values
-- are:
--
-- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
--     @NONE@, then nothing is returned. (This setting is the default for
--     @ReturnValues@.)
--
-- -   @ALL_OLD@ - Returns all of the attributes of the item, as they
--     appeared before the UpdateItem operation.
--
-- -   @UPDATED_OLD@ - Returns only the updated attributes, as they
--     appeared before the UpdateItem operation.
--
-- -   @ALL_NEW@ - Returns all of the attributes of the item, as they
--     appear after the UpdateItem operation.
--
-- -   @UPDATED_NEW@ - Returns only the updated attributes, as they appear
--     after the UpdateItem operation.
--
-- There is no additional cost associated with requesting a return value
-- aside from the small network and processing overhead of receiving a
-- larger response. No read capacity units are consumed.
--
-- The values returned are strongly consistent.
updateItem_returnValues :: Lens.Lens' UpdateItem (Core.Maybe ReturnValue)
updateItem_returnValues = Lens.lens (\UpdateItem' {returnValues} -> returnValues) (\s@UpdateItem' {} a -> s {returnValues = a} :: UpdateItem)

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
-- -   Comparison operators: @= | \<> | \< | > | \<= | >= | BETWEEN | IN @
--
-- -   Logical operators: @AND | OR | NOT@
--
-- For more information about condition expressions, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
updateItem_conditionExpression :: Lens.Lens' UpdateItem (Core.Maybe Core.Text)
updateItem_conditionExpression = Lens.lens (\UpdateItem' {conditionExpression} -> conditionExpression) (\s@UpdateItem' {} a -> s {conditionExpression = a} :: UpdateItem)

-- | This is a legacy parameter. Use @UpdateExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributeUpdates.html AttributeUpdates>
-- in the /Amazon DynamoDB Developer Guide/.
updateItem_attributeUpdates :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Core.Text AttributeValueUpdate))
updateItem_attributeUpdates = Lens.lens (\UpdateItem' {attributeUpdates} -> attributeUpdates) (\s@UpdateItem' {} a -> s {attributeUpdates = a} :: UpdateItem) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
updateItem_returnConsumedCapacity :: Lens.Lens' UpdateItem (Core.Maybe ReturnConsumedCapacity)
updateItem_returnConsumedCapacity = Lens.lens (\UpdateItem' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@UpdateItem' {} a -> s {returnConsumedCapacity = a} :: UpdateItem)

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
updateItem_conditionalOperator :: Lens.Lens' UpdateItem (Core.Maybe ConditionalOperator)
updateItem_conditionalOperator = Lens.lens (\UpdateItem' {conditionalOperator} -> conditionalOperator) (\s@UpdateItem' {} a -> s {conditionalOperator = a} :: UpdateItem)

-- | The name of the table containing the item to update.
updateItem_tableName :: Lens.Lens' UpdateItem Core.Text
updateItem_tableName = Lens.lens (\UpdateItem' {tableName} -> tableName) (\s@UpdateItem' {} a -> s {tableName = a} :: UpdateItem)

-- | The primary key of the item to be updated. Each element consists of an
-- attribute name and a value for that attribute.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a simple primary key, you only need to provide a value for
-- the partition key. For a composite primary key, you must provide values
-- for both the partition key and the sort key.
updateItem_key :: Lens.Lens' UpdateItem (Core.HashMap Core.Text AttributeValue)
updateItem_key = Lens.lens (\UpdateItem' {key} -> key) (\s@UpdateItem' {} a -> s {key = a} :: UpdateItem) Core.. Lens._Coerce

instance Core.AWSRequest UpdateItem where
  type AWSResponse UpdateItem = UpdateItemResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateItemResponse'
            Core.<$> (x Core..?> "ItemCollectionMetrics")
            Core.<*> (x Core..?> "Attributes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ConsumedCapacity")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateItem

instance Core.NFData UpdateItem

instance Core.ToHeaders UpdateItem where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DynamoDB_20120810.UpdateItem" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateItem where
  toJSON UpdateItem' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Expected" Core..=) Core.<$> expected,
            ("ExpressionAttributeValues" Core..=)
              Core.<$> expressionAttributeValues,
            ("ReturnItemCollectionMetrics" Core..=)
              Core.<$> returnItemCollectionMetrics,
            ("UpdateExpression" Core..=)
              Core.<$> updateExpression,
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ReturnValues" Core..=) Core.<$> returnValues,
            ("ConditionExpression" Core..=)
              Core.<$> conditionExpression,
            ("AttributeUpdates" Core..=)
              Core.<$> attributeUpdates,
            ("ReturnConsumedCapacity" Core..=)
              Core.<$> returnConsumedCapacity,
            ("ConditionalOperator" Core..=)
              Core.<$> conditionalOperator,
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("Key" Core..= key)
          ]
      )

instance Core.ToPath UpdateItem where
  toPath = Core.const "/"

instance Core.ToQuery UpdateItem where
  toQuery = Core.const Core.mempty

-- | Represents the output of an @UpdateItem@ operation.
--
-- /See:/ 'newUpdateItemResponse' smart constructor.
data UpdateItemResponse = UpdateItemResponse'
  { -- | Information about item collections, if any, that were affected by the
    -- @UpdateItem@ operation. @ItemCollectionMetrics@ is only returned if the
    -- @ReturnItemCollectionMetrics@ parameter was specified. If the table does
    -- not have any local secondary indexes, this information is not returned
    -- in the response.
    --
    -- Each @ItemCollectionMetrics@ element consists of:
    --
    -- -   @ItemCollectionKey@ - The partition key value of the item
    --     collection. This is the same as the partition key value of the item
    --     itself.
    --
    -- -   @SizeEstimateRangeGB@ - An estimate of item collection size, in
    --     gigabytes. This value is a two-element array containing a lower
    --     bound and an upper bound for the estimate. The estimate includes the
    --     size of all the items in the table, plus the size of all attributes
    --     projected into all of the local secondary indexes on that table. Use
    --     this estimate to measure whether a local secondary index is
    --     approaching its size limit.
    --
    --     The estimate is subject to change over time; therefore, do not rely
    --     on the precision or accuracy of the estimate.
    itemCollectionMetrics :: Core.Maybe ItemCollectionMetrics,
    -- | A map of attribute values as they appear before or after the
    -- @UpdateItem@ operation, as determined by the @ReturnValues@ parameter.
    --
    -- The @Attributes@ map is only present if @ReturnValues@ was specified as
    -- something other than @NONE@ in the request. Each element represents one
    -- attribute.
    attributes :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | The capacity units consumed by the @UpdateItem@ operation. The data
    -- returned includes the total provisioned throughput consumed, along with
    -- statistics for the table and any indexes involved in the operation.
    -- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
    -- parameter was specified. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
    -- in the /Amazon DynamoDB Developer Guide/.
    consumedCapacity :: Core.Maybe ConsumedCapacity,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemCollectionMetrics', 'updateItemResponse_itemCollectionMetrics' - Information about item collections, if any, that were affected by the
-- @UpdateItem@ operation. @ItemCollectionMetrics@ is only returned if the
-- @ReturnItemCollectionMetrics@ parameter was specified. If the table does
-- not have any local secondary indexes, this information is not returned
-- in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
-- -   @ItemCollectionKey@ - The partition key value of the item
--     collection. This is the same as the partition key value of the item
--     itself.
--
-- -   @SizeEstimateRangeGB@ - An estimate of item collection size, in
--     gigabytes. This value is a two-element array containing a lower
--     bound and an upper bound for the estimate. The estimate includes the
--     size of all the items in the table, plus the size of all attributes
--     projected into all of the local secondary indexes on that table. Use
--     this estimate to measure whether a local secondary index is
--     approaching its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely
--     on the precision or accuracy of the estimate.
--
-- 'attributes', 'updateItemResponse_attributes' - A map of attribute values as they appear before or after the
-- @UpdateItem@ operation, as determined by the @ReturnValues@ parameter.
--
-- The @Attributes@ map is only present if @ReturnValues@ was specified as
-- something other than @NONE@ in the request. Each element represents one
-- attribute.
--
-- 'consumedCapacity', 'updateItemResponse_consumedCapacity' - The capacity units consumed by the @UpdateItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'httpStatus', 'updateItemResponse_httpStatus' - The response's http status code.
newUpdateItemResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateItemResponse
newUpdateItemResponse pHttpStatus_ =
  UpdateItemResponse'
    { itemCollectionMetrics =
        Core.Nothing,
      attributes = Core.Nothing,
      consumedCapacity = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about item collections, if any, that were affected by the
-- @UpdateItem@ operation. @ItemCollectionMetrics@ is only returned if the
-- @ReturnItemCollectionMetrics@ parameter was specified. If the table does
-- not have any local secondary indexes, this information is not returned
-- in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
-- -   @ItemCollectionKey@ - The partition key value of the item
--     collection. This is the same as the partition key value of the item
--     itself.
--
-- -   @SizeEstimateRangeGB@ - An estimate of item collection size, in
--     gigabytes. This value is a two-element array containing a lower
--     bound and an upper bound for the estimate. The estimate includes the
--     size of all the items in the table, plus the size of all attributes
--     projected into all of the local secondary indexes on that table. Use
--     this estimate to measure whether a local secondary index is
--     approaching its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely
--     on the precision or accuracy of the estimate.
updateItemResponse_itemCollectionMetrics :: Lens.Lens' UpdateItemResponse (Core.Maybe ItemCollectionMetrics)
updateItemResponse_itemCollectionMetrics = Lens.lens (\UpdateItemResponse' {itemCollectionMetrics} -> itemCollectionMetrics) (\s@UpdateItemResponse' {} a -> s {itemCollectionMetrics = a} :: UpdateItemResponse)

-- | A map of attribute values as they appear before or after the
-- @UpdateItem@ operation, as determined by the @ReturnValues@ parameter.
--
-- The @Attributes@ map is only present if @ReturnValues@ was specified as
-- something other than @NONE@ in the request. Each element represents one
-- attribute.
updateItemResponse_attributes :: Lens.Lens' UpdateItemResponse (Core.Maybe (Core.HashMap Core.Text AttributeValue))
updateItemResponse_attributes = Lens.lens (\UpdateItemResponse' {attributes} -> attributes) (\s@UpdateItemResponse' {} a -> s {attributes = a} :: UpdateItemResponse) Core.. Lens.mapping Lens._Coerce

-- | The capacity units consumed by the @UpdateItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
updateItemResponse_consumedCapacity :: Lens.Lens' UpdateItemResponse (Core.Maybe ConsumedCapacity)
updateItemResponse_consumedCapacity = Lens.lens (\UpdateItemResponse' {consumedCapacity} -> consumedCapacity) (\s@UpdateItemResponse' {} a -> s {consumedCapacity = a} :: UpdateItemResponse)

-- | The response's http status code.
updateItemResponse_httpStatus :: Lens.Lens' UpdateItemResponse Core.Int
updateItemResponse_httpStatus = Lens.lens (\UpdateItemResponse' {httpStatus} -> httpStatus) (\s@UpdateItemResponse' {} a -> s {httpStatus = a} :: UpdateItemResponse)

instance Core.NFData UpdateItemResponse
