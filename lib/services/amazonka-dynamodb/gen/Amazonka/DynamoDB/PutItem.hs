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
-- Module      : Amazonka.DynamoDB.PutItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new item, or replaces an old item with a new item. If an item
-- that has the same primary key as the new item already exists in the
-- specified table, the new item completely replaces the existing item. You
-- can perform a conditional put operation (add a new item if one with the
-- specified primary key doesn\'t exist), or replace an existing item if it
-- has certain attribute values. You can return the item\'s attribute
-- values in the same operation, using the @ReturnValues@ parameter.
--
-- When you add an item, the primary key attributes are the only required
-- attributes.
--
-- Empty String and Binary attribute values are allowed. Attribute values
-- of type String and Binary must have a length greater than zero if the
-- attribute is used as a key attribute for a table or index. Set type
-- attributes cannot be empty.
--
-- Invalid Requests with empty values will be rejected with a
-- @ValidationException@ exception.
--
-- To prevent a new item from replacing an existing item, use a conditional
-- expression that contains the @attribute_not_exists@ function with the
-- name of the attribute being used as the partition key for the table.
-- Since every record must contain that attribute, the
-- @attribute_not_exists@ function will only succeed if no matching item
-- exists.
--
-- For more information about @PutItem@, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html Working with Items>
-- in the /Amazon DynamoDB Developer Guide/.
module Amazonka.DynamoDB.PutItem
  ( -- * Creating a Request
    PutItem (..),
    newPutItem,

    -- * Request Lenses
    putItem_returnValues,
    putItem_expressionAttributeValues,
    putItem_expressionAttributeNames,
    putItem_conditionalOperator,
    putItem_returnConsumedCapacity,
    putItem_expected,
    putItem_returnItemCollectionMetrics,
    putItem_conditionExpression,
    putItem_tableName,
    putItem_item,

    -- * Destructuring the Response
    PutItemResponse (..),
    newPutItemResponse,

    -- * Response Lenses
    putItemResponse_consumedCapacity,
    putItemResponse_attributes,
    putItemResponse_itemCollectionMetrics,
    putItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @PutItem@ operation.
--
-- /See:/ 'newPutItem' smart constructor.
data PutItem = PutItem'
  { -- | Use @ReturnValues@ if you want to get the item attributes as they
    -- appeared before they were updated with the @PutItem@ request. For
    -- @PutItem@, the valid values are:
    --
    -- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
    --     @NONE@, then nothing is returned. (This setting is the default for
    --     @ReturnValues@.)
    --
    -- -   @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair,
    --     then the content of the old item is returned.
    --
    -- The values returned are strongly consistent.
    --
    -- There is no additional cost associated with requesting a return value
    -- aside from the small network and processing overhead of receiving a
    -- larger response. No read capacity units are consumed.
    --
    -- The @ReturnValues@ parameter is used by several DynamoDB operations;
    -- however, @PutItem@ does not recognize any values other than @NONE@ or
    -- @ALL_OLD@.
    returnValues :: Prelude.Maybe ReturnValue,
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an
    -- attribute value. For example, suppose that you wanted to check whether
    -- the value of the /ProductStatus/ attribute was one of the following:
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
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
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
    -- in the /Amazon DynamoDB Developer Guide/). To work around this, you
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
    -- For more information on expression attribute names, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionalOperator :: Prelude.Maybe ConditionalOperator,
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
    -- in the /Amazon DynamoDB Developer Guide/.
    expected :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExpectedAttributeValue),
    -- | Determines whether item collection metrics are returned. If set to
    -- @SIZE@, the response includes statistics about item collections, if any,
    -- that were modified during the operation are returned in the response. If
    -- set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Prelude.Maybe ReturnItemCollectionMetrics,
    -- | A condition that must be satisfied in order for a conditional @PutItem@
    -- operation to succeed.
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
    -- For more information on condition expressions, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of the table to contain the item.
    tableName :: Prelude.Text,
    -- | A map of attribute name\/value pairs, one for each attribute. Only the
    -- primary key attributes are required; you can optionally provide other
    -- attribute name-value pairs for the item.
    --
    -- You must provide all of the attributes for the primary key. For example,
    -- with a simple primary key, you only need to provide a value for the
    -- partition key. For a composite primary key, you must provide both values
    -- for both the partition key and the sort key.
    --
    -- If you specify any attributes that are part of an index key, then the
    -- data types for those attributes must match those of the schema in the
    -- table\'s attribute definition.
    --
    -- Empty String and Binary attribute values are allowed. Attribute values
    -- of type String and Binary must have a length greater than zero if the
    -- attribute is used as a key attribute for a table or index.
    --
    -- For more information about primary keys, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key>
    -- in the /Amazon DynamoDB Developer Guide/.
    --
    -- Each element in the @Item@ map is an @AttributeValue@ object.
    item :: Prelude.HashMap Prelude.Text AttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValues', 'putItem_returnValues' - Use @ReturnValues@ if you want to get the item attributes as they
-- appeared before they were updated with the @PutItem@ request. For
-- @PutItem@, the valid values are:
--
-- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
--     @NONE@, then nothing is returned. (This setting is the default for
--     @ReturnValues@.)
--
-- -   @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair,
--     then the content of the old item is returned.
--
-- The values returned are strongly consistent.
--
-- There is no additional cost associated with requesting a return value
-- aside from the small network and processing overhead of receiving a
-- larger response. No read capacity units are consumed.
--
-- The @ReturnValues@ parameter is used by several DynamoDB operations;
-- however, @PutItem@ does not recognize any values other than @NONE@ or
-- @ALL_OLD@.
--
-- 'expressionAttributeValues', 'putItem_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
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
-- 'expressionAttributeNames', 'putItem_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
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
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
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
-- For more information on expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'conditionalOperator', 'putItem_conditionalOperator' - This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnConsumedCapacity', 'putItem_returnConsumedCapacity' - Undocumented member.
--
-- 'expected', 'putItem_expected' - This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnItemCollectionMetrics', 'putItem_returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
--
-- 'conditionExpression', 'putItem_conditionExpression' - A condition that must be satisfied in order for a conditional @PutItem@
-- operation to succeed.
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
-- For more information on condition expressions, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'tableName', 'putItem_tableName' - The name of the table to contain the item.
--
-- 'item', 'putItem_item' - A map of attribute name\/value pairs, one for each attribute. Only the
-- primary key attributes are required; you can optionally provide other
-- attribute name-value pairs for the item.
--
-- You must provide all of the attributes for the primary key. For example,
-- with a simple primary key, you only need to provide a value for the
-- partition key. For a composite primary key, you must provide both values
-- for both the partition key and the sort key.
--
-- If you specify any attributes that are part of an index key, then the
-- data types for those attributes must match those of the schema in the
-- table\'s attribute definition.
--
-- Empty String and Binary attribute values are allowed. Attribute values
-- of type String and Binary must have a length greater than zero if the
-- attribute is used as a key attribute for a table or index.
--
-- For more information about primary keys, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- Each element in the @Item@ map is an @AttributeValue@ object.
newPutItem ::
  -- | 'tableName'
  Prelude.Text ->
  PutItem
newPutItem pTableName_ =
  PutItem'
    { returnValues = Prelude.Nothing,
      expressionAttributeValues = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      conditionalOperator = Prelude.Nothing,
      returnConsumedCapacity = Prelude.Nothing,
      expected = Prelude.Nothing,
      returnItemCollectionMetrics = Prelude.Nothing,
      conditionExpression = Prelude.Nothing,
      tableName = pTableName_,
      item = Prelude.mempty
    }

-- | Use @ReturnValues@ if you want to get the item attributes as they
-- appeared before they were updated with the @PutItem@ request. For
-- @PutItem@, the valid values are:
--
-- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
--     @NONE@, then nothing is returned. (This setting is the default for
--     @ReturnValues@.)
--
-- -   @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair,
--     then the content of the old item is returned.
--
-- The values returned are strongly consistent.
--
-- There is no additional cost associated with requesting a return value
-- aside from the small network and processing overhead of receiving a
-- larger response. No read capacity units are consumed.
--
-- The @ReturnValues@ parameter is used by several DynamoDB operations;
-- however, @PutItem@ does not recognize any values other than @NONE@ or
-- @ALL_OLD@.
putItem_returnValues :: Lens.Lens' PutItem (Prelude.Maybe ReturnValue)
putItem_returnValues = Lens.lens (\PutItem' {returnValues} -> returnValues) (\s@PutItem' {} a -> s {returnValues = a} :: PutItem)

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
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
putItem_expressionAttributeValues :: Lens.Lens' PutItem (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
putItem_expressionAttributeValues = Lens.lens (\PutItem' {expressionAttributeValues} -> expressionAttributeValues) (\s@PutItem' {} a -> s {expressionAttributeValues = a} :: PutItem) Prelude.. Lens.mapping Lens.coerced

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
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
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
-- For more information on expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
putItem_expressionAttributeNames :: Lens.Lens' PutItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putItem_expressionAttributeNames = Lens.lens (\PutItem' {expressionAttributeNames} -> expressionAttributeNames) (\s@PutItem' {} a -> s {expressionAttributeNames = a} :: PutItem) Prelude.. Lens.mapping Lens.coerced

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
putItem_conditionalOperator :: Lens.Lens' PutItem (Prelude.Maybe ConditionalOperator)
putItem_conditionalOperator = Lens.lens (\PutItem' {conditionalOperator} -> conditionalOperator) (\s@PutItem' {} a -> s {conditionalOperator = a} :: PutItem)

-- | Undocumented member.
putItem_returnConsumedCapacity :: Lens.Lens' PutItem (Prelude.Maybe ReturnConsumedCapacity)
putItem_returnConsumedCapacity = Lens.lens (\PutItem' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@PutItem' {} a -> s {returnConsumedCapacity = a} :: PutItem)

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
-- in the /Amazon DynamoDB Developer Guide/.
putItem_expected :: Lens.Lens' PutItem (Prelude.Maybe (Prelude.HashMap Prelude.Text ExpectedAttributeValue))
putItem_expected = Lens.lens (\PutItem' {expected} -> expected) (\s@PutItem' {} a -> s {expected = a} :: PutItem) Prelude.. Lens.mapping Lens.coerced

-- | Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
putItem_returnItemCollectionMetrics :: Lens.Lens' PutItem (Prelude.Maybe ReturnItemCollectionMetrics)
putItem_returnItemCollectionMetrics = Lens.lens (\PutItem' {returnItemCollectionMetrics} -> returnItemCollectionMetrics) (\s@PutItem' {} a -> s {returnItemCollectionMetrics = a} :: PutItem)

-- | A condition that must be satisfied in order for a conditional @PutItem@
-- operation to succeed.
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
-- For more information on condition expressions, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
putItem_conditionExpression :: Lens.Lens' PutItem (Prelude.Maybe Prelude.Text)
putItem_conditionExpression = Lens.lens (\PutItem' {conditionExpression} -> conditionExpression) (\s@PutItem' {} a -> s {conditionExpression = a} :: PutItem)

-- | The name of the table to contain the item.
putItem_tableName :: Lens.Lens' PutItem Prelude.Text
putItem_tableName = Lens.lens (\PutItem' {tableName} -> tableName) (\s@PutItem' {} a -> s {tableName = a} :: PutItem)

-- | A map of attribute name\/value pairs, one for each attribute. Only the
-- primary key attributes are required; you can optionally provide other
-- attribute name-value pairs for the item.
--
-- You must provide all of the attributes for the primary key. For example,
-- with a simple primary key, you only need to provide a value for the
-- partition key. For a composite primary key, you must provide both values
-- for both the partition key and the sort key.
--
-- If you specify any attributes that are part of an index key, then the
-- data types for those attributes must match those of the schema in the
-- table\'s attribute definition.
--
-- Empty String and Binary attribute values are allowed. Attribute values
-- of type String and Binary must have a length greater than zero if the
-- attribute is used as a key attribute for a table or index.
--
-- For more information about primary keys, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- Each element in the @Item@ map is an @AttributeValue@ object.
putItem_item :: Lens.Lens' PutItem (Prelude.HashMap Prelude.Text AttributeValue)
putItem_item = Lens.lens (\PutItem' {item} -> item) (\s@PutItem' {} a -> s {item = a} :: PutItem) Prelude.. Lens.coerced

instance Core.AWSRequest PutItem where
  type AWSResponse PutItem = PutItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutItemResponse'
            Prelude.<$> (x Data..?> "ConsumedCapacity")
            Prelude.<*> (x Data..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ItemCollectionMetrics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutItem where
  hashWithSalt _salt PutItem' {..} =
    _salt `Prelude.hashWithSalt` returnValues
      `Prelude.hashWithSalt` expressionAttributeValues
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` conditionalOperator
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` expected
      `Prelude.hashWithSalt` returnItemCollectionMetrics
      `Prelude.hashWithSalt` conditionExpression
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` item

instance Prelude.NFData PutItem where
  rnf PutItem' {..} =
    Prelude.rnf returnValues
      `Prelude.seq` Prelude.rnf expressionAttributeValues
      `Prelude.seq` Prelude.rnf expressionAttributeNames
      `Prelude.seq` Prelude.rnf conditionalOperator
      `Prelude.seq` Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf expected
      `Prelude.seq` Prelude.rnf returnItemCollectionMetrics
      `Prelude.seq` Prelude.rnf conditionExpression
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf item

instance Data.ToHeaders PutItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DynamoDB_20120810.PutItem" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutItem where
  toJSON PutItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReturnValues" Data..=) Prelude.<$> returnValues,
            ("ExpressionAttributeValues" Data..=)
              Prelude.<$> expressionAttributeValues,
            ("ExpressionAttributeNames" Data..=)
              Prelude.<$> expressionAttributeNames,
            ("ConditionalOperator" Data..=)
              Prelude.<$> conditionalOperator,
            ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            ("Expected" Data..=) Prelude.<$> expected,
            ("ReturnItemCollectionMetrics" Data..=)
              Prelude.<$> returnItemCollectionMetrics,
            ("ConditionExpression" Data..=)
              Prelude.<$> conditionExpression,
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("Item" Data..= item)
          ]
      )

instance Data.ToPath PutItem where
  toPath = Prelude.const "/"

instance Data.ToQuery PutItem where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @PutItem@ operation.
--
-- /See:/ 'newPutItemResponse' smart constructor.
data PutItemResponse = PutItemResponse'
  { -- | The capacity units consumed by the @PutItem@ operation. The data
    -- returned includes the total provisioned throughput consumed, along with
    -- statistics for the table and any indexes involved in the operation.
    -- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
    -- parameter was specified. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read\/Write Capacity Mode>
    -- in the /Amazon DynamoDB Developer Guide/.
    consumedCapacity :: Prelude.Maybe ConsumedCapacity,
    -- | The attribute values as they appeared before the @PutItem@ operation,
    -- but only if @ReturnValues@ is specified as @ALL_OLD@ in the request.
    -- Each element consists of an attribute name and an attribute value.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | Information about item collections, if any, that were affected by the
    -- @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the
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
    itemCollectionMetrics :: Prelude.Maybe ItemCollectionMetrics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'putItemResponse_consumedCapacity' - The capacity units consumed by the @PutItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read\/Write Capacity Mode>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'attributes', 'putItemResponse_attributes' - The attribute values as they appeared before the @PutItem@ operation,
-- but only if @ReturnValues@ is specified as @ALL_OLD@ in the request.
-- Each element consists of an attribute name and an attribute value.
--
-- 'itemCollectionMetrics', 'putItemResponse_itemCollectionMetrics' - Information about item collections, if any, that were affected by the
-- @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the
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
-- 'httpStatus', 'putItemResponse_httpStatus' - The response's http status code.
newPutItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutItemResponse
newPutItemResponse pHttpStatus_ =
  PutItemResponse'
    { consumedCapacity =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      itemCollectionMetrics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The capacity units consumed by the @PutItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read\/Write Capacity Mode>
-- in the /Amazon DynamoDB Developer Guide/.
putItemResponse_consumedCapacity :: Lens.Lens' PutItemResponse (Prelude.Maybe ConsumedCapacity)
putItemResponse_consumedCapacity = Lens.lens (\PutItemResponse' {consumedCapacity} -> consumedCapacity) (\s@PutItemResponse' {} a -> s {consumedCapacity = a} :: PutItemResponse)

-- | The attribute values as they appeared before the @PutItem@ operation,
-- but only if @ReturnValues@ is specified as @ALL_OLD@ in the request.
-- Each element consists of an attribute name and an attribute value.
putItemResponse_attributes :: Lens.Lens' PutItemResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
putItemResponse_attributes = Lens.lens (\PutItemResponse' {attributes} -> attributes) (\s@PutItemResponse' {} a -> s {attributes = a} :: PutItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about item collections, if any, that were affected by the
-- @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the
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
putItemResponse_itemCollectionMetrics :: Lens.Lens' PutItemResponse (Prelude.Maybe ItemCollectionMetrics)
putItemResponse_itemCollectionMetrics = Lens.lens (\PutItemResponse' {itemCollectionMetrics} -> itemCollectionMetrics) (\s@PutItemResponse' {} a -> s {itemCollectionMetrics = a} :: PutItemResponse)

-- | The response's http status code.
putItemResponse_httpStatus :: Lens.Lens' PutItemResponse Prelude.Int
putItemResponse_httpStatus = Lens.lens (\PutItemResponse' {httpStatus} -> httpStatus) (\s@PutItemResponse' {} a -> s {httpStatus = a} :: PutItemResponse)

instance Prelude.NFData PutItemResponse where
  rnf PutItemResponse' {..} =
    Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf itemCollectionMetrics
      `Prelude.seq` Prelude.rnf httpStatus
