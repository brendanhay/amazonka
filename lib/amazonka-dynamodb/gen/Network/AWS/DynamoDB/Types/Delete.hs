-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Delete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Delete
  ( Delete (..),

    -- * Smart constructor
    mkDelete,

    -- * Lenses
    dExpressionAttributeNames,
    dExpressionAttributeValues,
    dReturnValuesOnConditionCheckFailure,
    dConditionExpression,
    dKey,
    dTableName,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a request to perform a @DeleteItem@ operation.
--
-- /See:/ 'mkDelete' smart constructor.
data Delete = Delete'
  { expressionAttributeNames ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    expressionAttributeValues ::
      Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    returnValuesOnConditionCheckFailure ::
      Lude.Maybe ReturnValuesOnConditionCheckFailure,
    conditionExpression :: Lude.Maybe Lude.Text,
    key :: Lude.HashMap Lude.Text (AttributeValue),
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

-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- * 'conditionExpression' - A condition that must be satisfied in order for a conditional delete to succeed.
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- * 'expressionAttributeValues' - One or more values that can be substituted in an expression.
-- * 'key' - The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
-- * 'returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
-- * 'tableName' - Name of the table in which the item to be deleted resides.
mkDelete ::
  -- | 'tableName'
  Lude.Text ->
  Delete
mkDelete pTableName_ =
  Delete'
    { expressionAttributeNames = Lude.Nothing,
      expressionAttributeValues = Lude.Nothing,
      returnValuesOnConditionCheckFailure = Lude.Nothing,
      conditionExpression = Lude.Nothing,
      key = Lude.mempty,
      tableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpressionAttributeNames :: Lens.Lens' Delete (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dExpressionAttributeNames = Lens.lens (expressionAttributeNames :: Delete -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: Delete)
{-# DEPRECATED dExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpressionAttributeValues :: Lens.Lens' Delete (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
dExpressionAttributeValues = Lens.lens (expressionAttributeValues :: Delete -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {expressionAttributeValues = a} :: Delete)
{-# DEPRECATED dExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReturnValuesOnConditionCheckFailure :: Lens.Lens' Delete (Lude.Maybe ReturnValuesOnConditionCheckFailure)
dReturnValuesOnConditionCheckFailure = Lens.lens (returnValuesOnConditionCheckFailure :: Delete -> Lude.Maybe ReturnValuesOnConditionCheckFailure) (\s a -> s {returnValuesOnConditionCheckFailure = a} :: Delete)
{-# DEPRECATED dReturnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead." #-}

-- | A condition that must be satisfied in order for a conditional delete to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConditionExpression :: Lens.Lens' Delete (Lude.Maybe Lude.Text)
dConditionExpression = Lens.lens (conditionExpression :: Delete -> Lude.Maybe Lude.Text) (\s a -> s {conditionExpression = a} :: Delete)
{-# DEPRECATED dConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dKey :: Lens.Lens' Delete (Lude.HashMap Lude.Text (AttributeValue))
dKey = Lens.lens (key :: Delete -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {key = a} :: Delete)
{-# DEPRECATED dKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Name of the table in which the item to be deleted resides.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTableName :: Lens.Lens' Delete Lude.Text
dTableName = Lens.lens (tableName :: Delete -> Lude.Text) (\s a -> s {tableName = a} :: Delete)
{-# DEPRECATED dTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON Delete where
  toJSON Delete' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Lude..=)
              Lude.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Lude..=)
              Lude.<$> returnValuesOnConditionCheckFailure,
            ("ConditionExpression" Lude..=) Lude.<$> conditionExpression,
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )
