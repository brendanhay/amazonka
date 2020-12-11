-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Put
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Put
  ( Put (..),

    -- * Smart constructor
    mkPut,

    -- * Lenses
    pExpressionAttributeNames,
    pExpressionAttributeValues,
    pReturnValuesOnConditionCheckFailure,
    pConditionExpression,
    pItem,
    pTableName,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a request to perform a @PutItem@ operation.
--
-- /See:/ 'mkPut' smart constructor.
data Put = Put'
  { expressionAttributeNames ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    expressionAttributeValues ::
      Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    returnValuesOnConditionCheckFailure ::
      Lude.Maybe ReturnValuesOnConditionCheckFailure,
    conditionExpression :: Lude.Maybe Lude.Text,
    item :: Lude.HashMap Lude.Text (AttributeValue),
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Put' with the minimum fields required to make a request.
--
-- * 'conditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- * 'expressionAttributeValues' - One or more values that can be substituted in an expression.
-- * 'item' - A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
-- * 'returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
-- * 'tableName' - Name of the table in which to write the item.
mkPut ::
  -- | 'tableName'
  Lude.Text ->
  Put
mkPut pTableName_ =
  Put'
    { expressionAttributeNames = Lude.Nothing,
      expressionAttributeValues = Lude.Nothing,
      returnValuesOnConditionCheckFailure = Lude.Nothing,
      conditionExpression = Lude.Nothing,
      item = Lude.mempty,
      tableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExpressionAttributeNames :: Lens.Lens' Put (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pExpressionAttributeNames = Lens.lens (expressionAttributeNames :: Put -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: Put)
{-# DEPRECATED pExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExpressionAttributeValues :: Lens.Lens' Put (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
pExpressionAttributeValues = Lens.lens (expressionAttributeValues :: Put -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {expressionAttributeValues = a} :: Put)
{-# DEPRECATED pExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pReturnValuesOnConditionCheckFailure :: Lens.Lens' Put (Lude.Maybe ReturnValuesOnConditionCheckFailure)
pReturnValuesOnConditionCheckFailure = Lens.lens (returnValuesOnConditionCheckFailure :: Put -> Lude.Maybe ReturnValuesOnConditionCheckFailure) (\s a -> s {returnValuesOnConditionCheckFailure = a} :: Put)
{-# DEPRECATED pReturnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead." #-}

-- | A condition that must be satisfied in order for a conditional update to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pConditionExpression :: Lens.Lens' Put (Lude.Maybe Lude.Text)
pConditionExpression = Lens.lens (conditionExpression :: Put -> Lude.Maybe Lude.Text) (\s a -> s {conditionExpression = a} :: Put)
{-# DEPRECATED pConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pItem :: Lens.Lens' Put (Lude.HashMap Lude.Text (AttributeValue))
pItem = Lens.lens (item :: Put -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {item = a} :: Put)
{-# DEPRECATED pItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | Name of the table in which to write the item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTableName :: Lens.Lens' Put Lude.Text
pTableName = Lens.lens (tableName :: Put -> Lude.Text) (\s a -> s {tableName = a} :: Put)
{-# DEPRECATED pTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON Put where
  toJSON Put' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Lude..=)
              Lude.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Lude..=)
              Lude.<$> returnValuesOnConditionCheckFailure,
            ("ConditionExpression" Lude..=) Lude.<$> conditionExpression,
            Lude.Just ("Item" Lude..= item),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )
