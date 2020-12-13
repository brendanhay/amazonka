{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.PutItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new item, or replaces an old item with a new item. If an item that has the same primary key as the new item already exists in the specified table, the new item completely replaces the existing item. You can perform a conditional put operation (add a new item if one with the specified primary key doesn't exist), or replace an existing item if it has certain attribute values. You can return the item's attribute values in the same operation, using the @ReturnValues@ parameter.
--
-- /Important:/ This topic provides general information about the @PutItem@ API.
-- For information on how to call the @PutItem@ API using the AWS SDK in specific languages, see the following:
--
--     * <http://docs.aws.amazon.com/goto/aws-cli/dynamodb-2012-08-10/PutItem PutItem in the AWS Command Line Interface>
--
--
--     * <http://docs.aws.amazon.com/goto/DotNetSDKV3/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for .NET>
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForCpp/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for C++>
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForGoV1/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Go>
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForJava/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Java>
--
--
--     * <http://docs.aws.amazon.com/goto/AWSJavaScriptSDK/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for JavaScript>
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForPHPV3/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for PHP V3>
--
--
--     * <http://docs.aws.amazon.com/goto/boto3/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Python>
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForRubyV2/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Ruby V2>
--
--
-- When you add an item, the primary key attributes are the only required attributes. Attribute values cannot be null.
-- Empty String and Binary attribute values are allowed. Attribute values of type String and Binary must have a length greater than zero if the attribute is used as a key attribute for a table or index. Set type attributes cannot be empty.
-- Invalid Requests with empty values will be rejected with a @ValidationException@ exception.
-- For more information about @PutItem@ , see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html Working with Items> in the /Amazon DynamoDB Developer Guide/ .
module Network.AWS.DynamoDB.PutItem
  ( -- * Creating a request
    PutItem (..),
    mkPutItem,

    -- ** Request lenses
    piExpressionAttributeNames,
    piReturnValues,
    piExpressionAttributeValues,
    piReturnConsumedCapacity,
    piReturnItemCollectionMetrics,
    piConditionExpression,
    piItem,
    piConditionalOperator,
    piExpected,
    piTableName,

    -- * Destructuring the response
    PutItemResponse (..),
    mkPutItemResponse,

    -- ** Response lenses
    pirsItemCollectionMetrics,
    pirsConsumedCapacity,
    pirsAttributes,
    pirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PutItem@ operation.
--
-- /See:/ 'mkPutItem' smart constructor.
data PutItem = PutItem'
  { -- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
    --
    --
    --     * To access an attribute whose name conflicts with a DynamoDB reserved word.
    --
    --
    --     * To create a placeholder for repeating occurrences of an attribute name in an expression.
    --
    --
    --     * To prevent special characters in an attribute name from being misinterpreted in an expression.
    --
    --
    -- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
    --
    --     * @Percentile@
    --
    --
    -- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
    --
    --     * @{"#P":"Percentile"}@
    --
    --
    -- You could then use this substitution in an expression, as in this example:
    --
    --     * @#P = :val@
    --
    --
    -- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeNames :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Use @ReturnValues@ if you want to get the item attributes as they appeared before they were updated with the @PutItem@ request. For @PutItem@ , the valid values are:
    --
    --
    --     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
    --
    --
    --     * @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair, then the content of the old item is returned.
    returnValues :: Lude.Maybe ReturnValue,
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
    -- @Available | Backordered | Discontinued@
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    -- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
    -- You could then use these values in an expression, such as this:
    -- @ProductStatus IN (:avail, :back, :disc)@
    -- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeValues :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    returnConsumedCapacity :: Lude.Maybe ReturnConsumedCapacity,
    -- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Lude.Maybe ReturnItemCollectionMetrics,
    -- | A condition that must be satisfied in order for a conditional @PutItem@ operation to succeed.
    --
    -- An expression can contain any of the following:
    --
    --     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
    -- These function names are case-sensitive.
    --
    --
    --     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @
    --
    --
    --     * Logical operators: @AND | OR | NOT@
    --
    --
    -- For more information on condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
    conditionExpression :: Lude.Maybe Lude.Text,
    -- | A map of attribute name/value pairs, one for each attribute. Only the primary key attributes are required; you can optionally provide other attribute name-value pairs for the item.
    --
    -- You must provide all of the attributes for the primary key. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide both values for both the partition key and the sort key.
    -- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
    -- Empty String and Binary attribute values are allowed. Attribute values of type String and Binary must have a length greater than zero if the attribute is used as a key attribute for a table or index.
    -- For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
    -- Each element in the @Item@ map is an @AttributeValue@ object.
    item :: Lude.HashMap Lude.Text (AttributeValue),
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
    conditionalOperator :: Lude.Maybe ConditionalOperator,
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
    expected :: Lude.Maybe (Lude.HashMap Lude.Text (ExpectedAttributeValue)),
    -- | The name of the table to contain the item.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutItem' with the minimum fields required to make a request.
--
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
--
--
--     * To access an attribute whose name conflicts with a DynamoDB reserved word.
--
--
--     * To create a placeholder for repeating occurrences of an attribute name in an expression.
--
--
--     * To prevent special characters in an attribute name from being misinterpreted in an expression.
--
--
-- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
--
--     * @Percentile@
--
--
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
--
--     * @{"#P":"Percentile"}@
--
--
-- You could then use this substitution in an expression, as in this example:
--
--     * @#P = :val@
--
--
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'returnValues' - Use @ReturnValues@ if you want to get the item attributes as they appeared before they were updated with the @PutItem@ request. For @PutItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair, then the content of the old item is returned.
--
--
-- * 'expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
-- * 'returnConsumedCapacity' -
-- * 'returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
-- * 'conditionExpression' - A condition that must be satisfied in order for a conditional @PutItem@ operation to succeed.
--
-- An expression can contain any of the following:
--
--     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
-- These function names are case-sensitive.
--
--
--     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @
--
--
--     * Logical operators: @AND | OR | NOT@
--
--
-- For more information on condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
-- * 'item' - A map of attribute name/value pairs, one for each attribute. Only the primary key attributes are required; you can optionally provide other attribute name-value pairs for the item.
--
-- You must provide all of the attributes for the primary key. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide both values for both the partition key and the sort key.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
-- Empty String and Binary attribute values are allowed. Attribute values of type String and Binary must have a length greater than zero if the attribute is used as a key attribute for a table or index.
-- For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
-- Each element in the @Item@ map is an @AttributeValue@ object.
-- * 'conditionalOperator' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
-- * 'expected' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
-- * 'tableName' - The name of the table to contain the item.
mkPutItem ::
  -- | 'tableName'
  Lude.Text ->
  PutItem
mkPutItem pTableName_ =
  PutItem'
    { expressionAttributeNames = Lude.Nothing,
      returnValues = Lude.Nothing,
      expressionAttributeValues = Lude.Nothing,
      returnConsumedCapacity = Lude.Nothing,
      returnItemCollectionMetrics = Lude.Nothing,
      conditionExpression = Lude.Nothing,
      item = Lude.mempty,
      conditionalOperator = Lude.Nothing,
      expected = Lude.Nothing,
      tableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
--
--
--     * To access an attribute whose name conflicts with a DynamoDB reserved word.
--
--
--     * To create a placeholder for repeating occurrences of an attribute name in an expression.
--
--
--     * To prevent special characters in an attribute name from being misinterpreted in an expression.
--
--
-- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
--
--     * @Percentile@
--
--
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
--
--     * @{"#P":"Percentile"}@
--
--
-- You could then use this substitution in an expression, as in this example:
--
--     * @#P = :val@
--
--
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piExpressionAttributeNames :: Lens.Lens' PutItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
piExpressionAttributeNames = Lens.lens (expressionAttributeNames :: PutItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: PutItem)
{-# DEPRECATED piExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | Use @ReturnValues@ if you want to get the item attributes as they appeared before they were updated with the @PutItem@ request. For @PutItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair, then the content of the old item is returned.
--
--
--
-- /Note:/ Consider using 'returnValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piReturnValues :: Lens.Lens' PutItem (Lude.Maybe ReturnValue)
piReturnValues = Lens.lens (returnValues :: PutItem -> Lude.Maybe ReturnValue) (\s a -> s {returnValues = a} :: PutItem)
{-# DEPRECATED piReturnValues "Use generic-lens or generic-optics with 'returnValues' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piExpressionAttributeValues :: Lens.Lens' PutItem (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
piExpressionAttributeValues = Lens.lens (expressionAttributeValues :: PutItem -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {expressionAttributeValues = a} :: PutItem)
{-# DEPRECATED piExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piReturnConsumedCapacity :: Lens.Lens' PutItem (Lude.Maybe ReturnConsumedCapacity)
piReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: PutItem -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: PutItem)
{-# DEPRECATED piReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piReturnItemCollectionMetrics :: Lens.Lens' PutItem (Lude.Maybe ReturnItemCollectionMetrics)
piReturnItemCollectionMetrics = Lens.lens (returnItemCollectionMetrics :: PutItem -> Lude.Maybe ReturnItemCollectionMetrics) (\s a -> s {returnItemCollectionMetrics = a} :: PutItem)
{-# DEPRECATED piReturnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead." #-}

-- | A condition that must be satisfied in order for a conditional @PutItem@ operation to succeed.
--
-- An expression can contain any of the following:
--
--     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
-- These function names are case-sensitive.
--
--
--     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @
--
--
--     * Logical operators: @AND | OR | NOT@
--
--
-- For more information on condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConditionExpression :: Lens.Lens' PutItem (Lude.Maybe Lude.Text)
piConditionExpression = Lens.lens (conditionExpression :: PutItem -> Lude.Maybe Lude.Text) (\s a -> s {conditionExpression = a} :: PutItem)
{-# DEPRECATED piConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | A map of attribute name/value pairs, one for each attribute. Only the primary key attributes are required; you can optionally provide other attribute name-value pairs for the item.
--
-- You must provide all of the attributes for the primary key. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide both values for both the partition key and the sort key.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
-- Empty String and Binary attribute values are allowed. Attribute values of type String and Binary must have a length greater than zero if the attribute is used as a key attribute for a table or index.
-- For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
-- Each element in the @Item@ map is an @AttributeValue@ object.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piItem :: Lens.Lens' PutItem (Lude.HashMap Lude.Text (AttributeValue))
piItem = Lens.lens (item :: PutItem -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {item = a} :: PutItem)
{-# DEPRECATED piItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConditionalOperator :: Lens.Lens' PutItem (Lude.Maybe ConditionalOperator)
piConditionalOperator = Lens.lens (conditionalOperator :: PutItem -> Lude.Maybe ConditionalOperator) (\s a -> s {conditionalOperator = a} :: PutItem)
{-# DEPRECATED piConditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead." #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piExpected :: Lens.Lens' PutItem (Lude.Maybe (Lude.HashMap Lude.Text (ExpectedAttributeValue)))
piExpected = Lens.lens (expected :: PutItem -> Lude.Maybe (Lude.HashMap Lude.Text (ExpectedAttributeValue))) (\s a -> s {expected = a} :: PutItem)
{-# DEPRECATED piExpected "Use generic-lens or generic-optics with 'expected' instead." #-}

-- | The name of the table to contain the item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piTableName :: Lens.Lens' PutItem Lude.Text
piTableName = Lens.lens (tableName :: PutItem -> Lude.Text) (\s a -> s {tableName = a} :: PutItem)
{-# DEPRECATED piTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest PutItem where
  type Rs PutItem = PutItemResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutItemResponse'
            Lude.<$> (x Lude..?> "ItemCollectionMetrics")
            Lude.<*> (x Lude..?> "ConsumedCapacity")
            Lude.<*> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.PutItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutItem where
  toJSON PutItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("ReturnValues" Lude..=) Lude.<$> returnValues,
            ("ExpressionAttributeValues" Lude..=)
              Lude.<$> expressionAttributeValues,
            ("ReturnConsumedCapacity" Lude..=) Lude.<$> returnConsumedCapacity,
            ("ReturnItemCollectionMetrics" Lude..=)
              Lude.<$> returnItemCollectionMetrics,
            ("ConditionExpression" Lude..=) Lude.<$> conditionExpression,
            Lude.Just ("Item" Lude..= item),
            ("ConditionalOperator" Lude..=) Lude.<$> conditionalOperator,
            ("Expected" Lude..=) Lude.<$> expected,
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath PutItem where
  toPath = Lude.const "/"

instance Lude.ToQuery PutItem where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @PutItem@ operation.
--
-- /See:/ 'mkPutItemResponse' smart constructor.
data PutItemResponse = PutItemResponse'
  { -- | Information about item collections, if any, that were affected by the @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
    --
    -- Each @ItemCollectionMetrics@ element consists of:
    --
    --     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
    --
    --
    --     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
    -- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
    itemCollectionMetrics :: Lude.Maybe ItemCollectionMetrics,
    -- | The capacity units consumed by the @PutItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
    consumedCapacity :: Lude.Maybe ConsumedCapacity,
    -- | The attribute values as they appeared before the @PutItem@ operation, but only if @ReturnValues@ is specified as @ALL_OLD@ in the request. Each element consists of an attribute name and an attribute value.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutItemResponse' with the minimum fields required to make a request.
--
-- * 'itemCollectionMetrics' - Information about item collections, if any, that were affected by the @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
--     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
--
--
--     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
--
-- * 'consumedCapacity' - The capacity units consumed by the @PutItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
-- * 'attributes' - The attribute values as they appeared before the @PutItem@ operation, but only if @ReturnValues@ is specified as @ALL_OLD@ in the request. Each element consists of an attribute name and an attribute value.
-- * 'responseStatus' - The response status code.
mkPutItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutItemResponse
mkPutItemResponse pResponseStatus_ =
  PutItemResponse'
    { itemCollectionMetrics = Lude.Nothing,
      consumedCapacity = Lude.Nothing,
      attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about item collections, if any, that were affected by the @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
--     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
--
--
--     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
--
--
-- /Note:/ Consider using 'itemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsItemCollectionMetrics :: Lens.Lens' PutItemResponse (Lude.Maybe ItemCollectionMetrics)
pirsItemCollectionMetrics = Lens.lens (itemCollectionMetrics :: PutItemResponse -> Lude.Maybe ItemCollectionMetrics) (\s a -> s {itemCollectionMetrics = a} :: PutItemResponse)
{-# DEPRECATED pirsItemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead." #-}

-- | The capacity units consumed by the @PutItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsConsumedCapacity :: Lens.Lens' PutItemResponse (Lude.Maybe ConsumedCapacity)
pirsConsumedCapacity = Lens.lens (consumedCapacity :: PutItemResponse -> Lude.Maybe ConsumedCapacity) (\s a -> s {consumedCapacity = a} :: PutItemResponse)
{-# DEPRECATED pirsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | The attribute values as they appeared before the @PutItem@ operation, but only if @ReturnValues@ is specified as @ALL_OLD@ in the request. Each element consists of an attribute name and an attribute value.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsAttributes :: Lens.Lens' PutItemResponse (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
pirsAttributes = Lens.lens (attributes :: PutItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {attributes = a} :: PutItemResponse)
{-# DEPRECATED pirsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsResponseStatus :: Lens.Lens' PutItemResponse Lude.Int
pirsResponseStatus = Lens.lens (responseStatus :: PutItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutItemResponse)
{-# DEPRECATED pirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
