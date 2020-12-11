-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ConditionCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ConditionCheck
  ( ConditionCheck (..),

    -- * Smart constructor
    mkConditionCheck,

    -- * Lenses
    ccExpressionAttributeNames,
    ccExpressionAttributeValues,
    ccReturnValuesOnConditionCheckFailure,
    ccKey,
    ccTableName,
    ccConditionExpression,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a request to perform a check that an item exists or to check the condition of specific attributes of the item.
--
-- /See:/ 'mkConditionCheck' smart constructor.
data ConditionCheck = ConditionCheck'
  { expressionAttributeNames ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    expressionAttributeValues ::
      Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    returnValuesOnConditionCheckFailure ::
      Lude.Maybe ReturnValuesOnConditionCheckFailure,
    key :: Lude.HashMap Lude.Text (AttributeValue),
    tableName :: Lude.Text,
    conditionExpression :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConditionCheck' with the minimum fields required to make a request.
--
-- * 'conditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- * 'expressionAttributeValues' - One or more values that can be substituted in an expression.
-- * 'key' - The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
-- * 'returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
-- * 'tableName' - Name of the table for the check item request.
mkConditionCheck ::
  -- | 'tableName'
  Lude.Text ->
  -- | 'conditionExpression'
  Lude.Text ->
  ConditionCheck
mkConditionCheck pTableName_ pConditionExpression_ =
  ConditionCheck'
    { expressionAttributeNames = Lude.Nothing,
      expressionAttributeValues = Lude.Nothing,
      returnValuesOnConditionCheckFailure = Lude.Nothing,
      key = Lude.mempty,
      tableName = pTableName_,
      conditionExpression = pConditionExpression_
    }

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpressionAttributeNames :: Lens.Lens' ConditionCheck (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccExpressionAttributeNames = Lens.lens (expressionAttributeNames :: ConditionCheck -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: ConditionCheck)
{-# DEPRECATED ccExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpressionAttributeValues :: Lens.Lens' ConditionCheck (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
ccExpressionAttributeValues = Lens.lens (expressionAttributeValues :: ConditionCheck -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {expressionAttributeValues = a} :: ConditionCheck)
{-# DEPRECATED ccExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReturnValuesOnConditionCheckFailure :: Lens.Lens' ConditionCheck (Lude.Maybe ReturnValuesOnConditionCheckFailure)
ccReturnValuesOnConditionCheckFailure = Lens.lens (returnValuesOnConditionCheckFailure :: ConditionCheck -> Lude.Maybe ReturnValuesOnConditionCheckFailure) (\s a -> s {returnValuesOnConditionCheckFailure = a} :: ConditionCheck)
{-# DEPRECATED ccReturnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead." #-}

-- | The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKey :: Lens.Lens' ConditionCheck (Lude.HashMap Lude.Text (AttributeValue))
ccKey = Lens.lens (key :: ConditionCheck -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {key = a} :: ConditionCheck)
{-# DEPRECATED ccKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Name of the table for the check item request.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTableName :: Lens.Lens' ConditionCheck Lude.Text
ccTableName = Lens.lens (tableName :: ConditionCheck -> Lude.Text) (\s a -> s {tableName = a} :: ConditionCheck)
{-# DEPRECATED ccTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A condition that must be satisfied in order for a conditional update to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConditionExpression :: Lens.Lens' ConditionCheck Lude.Text
ccConditionExpression = Lens.lens (conditionExpression :: ConditionCheck -> Lude.Text) (\s a -> s {conditionExpression = a} :: ConditionCheck)
{-# DEPRECATED ccConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

instance Lude.ToJSON ConditionCheck where
  toJSON ConditionCheck' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Lude..=)
              Lude.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Lude..=)
              Lude.<$> returnValuesOnConditionCheckFailure,
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("ConditionExpression" Lude..= conditionExpression)
          ]
      )
