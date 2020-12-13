{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Update
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Update
  ( Update (..),

    -- * Smart constructor
    mkUpdate,

    -- * Lenses
    uExpressionAttributeNames,
    uUpdateExpression,
    uExpressionAttributeValues,
    uReturnValuesOnConditionCheckFailure,
    uConditionExpression,
    uKey,
    uTableName,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a request to perform an @UpdateItem@ operation.
--
-- /See:/ 'mkUpdate' smart constructor.
data Update = Update'
  { -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
    updateExpression :: Lude.Text,
    -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
    returnValuesOnConditionCheckFailure :: Lude.Maybe ReturnValuesOnConditionCheckFailure,
    -- | A condition that must be satisfied in order for a conditional update to succeed.
    conditionExpression :: Lude.Maybe Lude.Text,
    -- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
    key :: Lude.HashMap Lude.Text (AttributeValue),
    -- | Name of the table for the @UpdateItem@ request.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Update' with the minimum fields required to make a request.
--
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- * 'updateExpression' - An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
-- * 'expressionAttributeValues' - One or more values that can be substituted in an expression.
-- * 'returnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
-- * 'conditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
-- * 'key' - The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
-- * 'tableName' - Name of the table for the @UpdateItem@ request.
mkUpdate ::
  -- | 'updateExpression'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  Update
mkUpdate pUpdateExpression_ pTableName_ =
  Update'
    { expressionAttributeNames = Lude.Nothing,
      updateExpression = pUpdateExpression_,
      expressionAttributeValues = Lude.Nothing,
      returnValuesOnConditionCheckFailure = Lude.Nothing,
      conditionExpression = Lude.Nothing,
      key = Lude.mempty,
      tableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uExpressionAttributeNames :: Lens.Lens' Update (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uExpressionAttributeNames = Lens.lens (expressionAttributeNames :: Update -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: Update)
{-# DEPRECATED uExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
--
-- /Note:/ Consider using 'updateExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUpdateExpression :: Lens.Lens' Update Lude.Text
uUpdateExpression = Lens.lens (updateExpression :: Update -> Lude.Text) (\s a -> s {updateExpression = a} :: Update)
{-# DEPRECATED uUpdateExpression "Use generic-lens or generic-optics with 'updateExpression' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uExpressionAttributeValues :: Lens.Lens' Update (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
uExpressionAttributeValues = Lens.lens (expressionAttributeValues :: Update -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {expressionAttributeValues = a} :: Update)
{-# DEPRECATED uExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uReturnValuesOnConditionCheckFailure :: Lens.Lens' Update (Lude.Maybe ReturnValuesOnConditionCheckFailure)
uReturnValuesOnConditionCheckFailure = Lens.lens (returnValuesOnConditionCheckFailure :: Update -> Lude.Maybe ReturnValuesOnConditionCheckFailure) (\s a -> s {returnValuesOnConditionCheckFailure = a} :: Update)
{-# DEPRECATED uReturnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead." #-}

-- | A condition that must be satisfied in order for a conditional update to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConditionExpression :: Lens.Lens' Update (Lude.Maybe Lude.Text)
uConditionExpression = Lens.lens (conditionExpression :: Update -> Lude.Maybe Lude.Text) (\s a -> s {conditionExpression = a} :: Update)
{-# DEPRECATED uConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uKey :: Lens.Lens' Update (Lude.HashMap Lude.Text (AttributeValue))
uKey = Lens.lens (key :: Update -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {key = a} :: Update)
{-# DEPRECATED uKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Name of the table for the @UpdateItem@ request.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTableName :: Lens.Lens' Update Lude.Text
uTableName = Lens.lens (tableName :: Update -> Lude.Text) (\s a -> s {tableName = a} :: Update)
{-# DEPRECATED uTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON Update where
  toJSON Update' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            Lude.Just ("UpdateExpression" Lude..= updateExpression),
            ("ExpressionAttributeValues" Lude..=)
              Lude.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Lude..=)
              Lude.<$> returnValuesOnConditionCheckFailure,
            ("ConditionExpression" Lude..=) Lude.<$> conditionExpression,
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )
