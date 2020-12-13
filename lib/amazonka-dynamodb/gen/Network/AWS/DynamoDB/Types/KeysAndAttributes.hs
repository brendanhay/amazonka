{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KeysAndAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KeysAndAttributes
  ( KeysAndAttributes (..),

    -- * Smart constructor
    mkKeysAndAttributes,

    -- * Lenses
    kaaProjectionExpression,
    kaaAttributesToGet,
    kaaExpressionAttributeNames,
    kaaConsistentRead,
    kaaKeys,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a set of primary keys and, for each key, the attributes to retrieve from the table.
--
-- For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide the partition key. For a composite primary key, you must provide /both/ the partition key and the sort key.
--
-- /See:/ 'mkKeysAndAttributes' smart constructor.
data KeysAndAttributes = KeysAndAttributes'
  { -- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas.
    --
    -- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    projectionExpression :: Lude.Maybe Lude.Text,
    -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
    attributesToGet :: Lude.Maybe (Lude.NonEmpty Lude.Text),
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
    -- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeNames :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
    consistentRead :: Lude.Maybe Lude.Bool,
    -- | The primary key attribute values that define the items and the attributes associated with the items.
    keys :: Lude.NonEmpty (Lude.HashMap Lude.Text (AttributeValue))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeysAndAttributes' with the minimum fields required to make a request.
--
-- * 'projectionExpression' - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'attributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
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
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'consistentRead' - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
-- * 'keys' - The primary key attribute values that define the items and the attributes associated with the items.
mkKeysAndAttributes ::
  -- | 'keys'
  Lude.NonEmpty (Lude.HashMap Lude.Text (AttributeValue)) ->
  KeysAndAttributes
mkKeysAndAttributes pKeys_ =
  KeysAndAttributes'
    { projectionExpression = Lude.Nothing,
      attributesToGet = Lude.Nothing,
      expressionAttributeNames = Lude.Nothing,
      consistentRead = Lude.Nothing,
      keys = pKeys_
    }

-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaProjectionExpression :: Lens.Lens' KeysAndAttributes (Lude.Maybe Lude.Text)
kaaProjectionExpression = Lens.lens (projectionExpression :: KeysAndAttributes -> Lude.Maybe Lude.Text) (\s a -> s {projectionExpression = a} :: KeysAndAttributes)
{-# DEPRECATED kaaProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaAttributesToGet :: Lens.Lens' KeysAndAttributes (Lude.Maybe (Lude.NonEmpty Lude.Text))
kaaAttributesToGet = Lens.lens (attributesToGet :: KeysAndAttributes -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {attributesToGet = a} :: KeysAndAttributes)
{-# DEPRECATED kaaAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

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
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaExpressionAttributeNames :: Lens.Lens' KeysAndAttributes (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
kaaExpressionAttributeNames = Lens.lens (expressionAttributeNames :: KeysAndAttributes -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: KeysAndAttributes)
{-# DEPRECATED kaaExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaConsistentRead :: Lens.Lens' KeysAndAttributes (Lude.Maybe Lude.Bool)
kaaConsistentRead = Lens.lens (consistentRead :: KeysAndAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: KeysAndAttributes)
{-# DEPRECATED kaaConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | The primary key attribute values that define the items and the attributes associated with the items.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaKeys :: Lens.Lens' KeysAndAttributes (Lude.NonEmpty (Lude.HashMap Lude.Text (AttributeValue)))
kaaKeys = Lens.lens (keys :: KeysAndAttributes -> Lude.NonEmpty (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {keys = a} :: KeysAndAttributes)
{-# DEPRECATED kaaKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

instance Lude.FromJSON KeysAndAttributes where
  parseJSON =
    Lude.withObject
      "KeysAndAttributes"
      ( \x ->
          KeysAndAttributes'
            Lude.<$> (x Lude..:? "ProjectionExpression")
            Lude.<*> (x Lude..:? "AttributesToGet")
            Lude.<*> (x Lude..:? "ExpressionAttributeNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ConsistentRead")
            Lude.<*> (x Lude..: "Keys")
      )

instance Lude.ToJSON KeysAndAttributes where
  toJSON KeysAndAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProjectionExpression" Lude..=) Lude.<$> projectionExpression,
            ("AttributesToGet" Lude..=) Lude.<$> attributesToGet,
            ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("ConsistentRead" Lude..=) Lude.<$> consistentRead,
            Lude.Just ("Keys" Lude..= keys)
          ]
      )
