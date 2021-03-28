{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KeysAndAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.KeysAndAttributes
  ( KeysAndAttributes (..)
  -- * Smart constructor
  , mkKeysAndAttributes
  -- * Lenses
  , kaaKeys
  , kaaAttributesToGet
  , kaaConsistentRead
  , kaaExpressionAttributeNames
  , kaaProjectionExpression
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeNameVariable as Types
import qualified Network.AWS.DynamoDB.Types.ProjectionExpression as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a set of primary keys and, for each key, the attributes to retrieve from the table.
--
-- For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide the partition key. For a composite primary key, you must provide /both/ the partition key and the sort key.
--
-- /See:/ 'mkKeysAndAttributes' smart constructor.
data KeysAndAttributes = KeysAndAttributes'
  { keys :: Core.NonEmpty (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ The primary key attribute values that define the items and the attributes associated with the items.
  , attributesToGet :: Core.Maybe (Core.NonEmpty Types.AttributeName)
    -- ^ This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
  , consistentRead :: Core.Maybe Core.Bool
    -- ^ The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
  , expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName)
    -- ^ One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
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
  , projectionExpression :: Core.Maybe Types.ProjectionExpression
    -- ^ A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeysAndAttributes' value with any optional fields omitted.
mkKeysAndAttributes
    :: Core.NonEmpty (Core.HashMap Types.AttributeName Types.AttributeValue) -- ^ 'keys'
    -> KeysAndAttributes
mkKeysAndAttributes keys
  = KeysAndAttributes'{keys, attributesToGet = Core.Nothing,
                       consistentRead = Core.Nothing,
                       expressionAttributeNames = Core.Nothing,
                       projectionExpression = Core.Nothing}

-- | The primary key attribute values that define the items and the attributes associated with the items.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaKeys :: Lens.Lens' KeysAndAttributes (Core.NonEmpty (Core.HashMap Types.AttributeName Types.AttributeValue))
kaaKeys = Lens.field @"keys"
{-# INLINEABLE kaaKeys #-}
{-# DEPRECATED keys "Use generic-lens or generic-optics with 'keys' instead"  #-}

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaAttributesToGet :: Lens.Lens' KeysAndAttributes (Core.Maybe (Core.NonEmpty Types.AttributeName))
kaaAttributesToGet = Lens.field @"attributesToGet"
{-# INLINEABLE kaaAttributesToGet #-}
{-# DEPRECATED attributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead"  #-}

-- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaConsistentRead :: Lens.Lens' KeysAndAttributes (Core.Maybe Core.Bool)
kaaConsistentRead = Lens.field @"consistentRead"
{-# INLINEABLE kaaConsistentRead #-}
{-# DEPRECATED consistentRead "Use generic-lens or generic-optics with 'consistentRead' instead"  #-}

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
kaaExpressionAttributeNames :: Lens.Lens' KeysAndAttributes (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
kaaExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# INLINEABLE kaaExpressionAttributeNames #-}
{-# DEPRECATED expressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead"  #-}

-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaaProjectionExpression :: Lens.Lens' KeysAndAttributes (Core.Maybe Types.ProjectionExpression)
kaaProjectionExpression = Lens.field @"projectionExpression"
{-# INLINEABLE kaaProjectionExpression #-}
{-# DEPRECATED projectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead"  #-}

instance Core.FromJSON KeysAndAttributes where
        toJSON KeysAndAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Keys" Core..= keys),
                  ("AttributesToGet" Core..=) Core.<$> attributesToGet,
                  ("ConsistentRead" Core..=) Core.<$> consistentRead,
                  ("ExpressionAttributeNames" Core..=) Core.<$>
                    expressionAttributeNames,
                  ("ProjectionExpression" Core..=) Core.<$> projectionExpression])

instance Core.FromJSON KeysAndAttributes where
        parseJSON
          = Core.withObject "KeysAndAttributes" Core.$
              \ x ->
                KeysAndAttributes' Core.<$>
                  (x Core..: "Keys") Core.<*> x Core..:? "AttributesToGet" Core.<*>
                    x Core..:? "ConsistentRead"
                    Core.<*> x Core..:? "ExpressionAttributeNames"
                    Core.<*> x Core..:? "ProjectionExpression"
