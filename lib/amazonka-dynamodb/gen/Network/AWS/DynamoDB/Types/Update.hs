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
    uKey,
    uUpdateExpression,
    uTableName,
    uConditionExpression,
    uExpressionAttributeNames,
    uExpressionAttributeValues,
    uReturnValuesOnConditionCheckFailure,
  )
where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.ConditionExpression as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeNameVariable as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeValueVariable as Types
import qualified Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.DynamoDB.Types.UpdateExpression as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a request to perform an @UpdateItem@ operation.
--
-- /See:/ 'mkUpdate' smart constructor.
data Update = Update'
  { -- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
    key :: Core.HashMap Types.AttributeName Types.AttributeValue,
    -- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
    updateExpression :: Types.UpdateExpression,
    -- | Name of the table for the @UpdateItem@ request.
    tableName :: Types.TableName,
    -- | A condition that must be satisfied in order for a conditional update to succeed.
    conditionExpression :: Core.Maybe Types.ConditionExpression,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName),
    -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
    returnValuesOnConditionCheckFailure :: Core.Maybe Types.ReturnValuesOnConditionCheckFailure
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Update' value with any optional fields omitted.
mkUpdate ::
  -- | 'updateExpression'
  Types.UpdateExpression ->
  -- | 'tableName'
  Types.TableName ->
  Update
mkUpdate updateExpression tableName =
  Update'
    { key = Core.mempty,
      updateExpression,
      tableName,
      conditionExpression = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      expressionAttributeValues = Core.Nothing,
      returnValuesOnConditionCheckFailure = Core.Nothing
    }

-- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uKey :: Lens.Lens' Update (Core.HashMap Types.AttributeName Types.AttributeValue)
uKey = Lens.field @"key"
{-# DEPRECATED uKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
--
-- /Note:/ Consider using 'updateExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUpdateExpression :: Lens.Lens' Update Types.UpdateExpression
uUpdateExpression = Lens.field @"updateExpression"
{-# DEPRECATED uUpdateExpression "Use generic-lens or generic-optics with 'updateExpression' instead." #-}

-- | Name of the table for the @UpdateItem@ request.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTableName :: Lens.Lens' Update Types.TableName
uTableName = Lens.field @"tableName"
{-# DEPRECATED uTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A condition that must be satisfied in order for a conditional update to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConditionExpression :: Lens.Lens' Update (Core.Maybe Types.ConditionExpression)
uConditionExpression = Lens.field @"conditionExpression"
{-# DEPRECATED uConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uExpressionAttributeNames :: Lens.Lens' Update (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
uExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED uExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uExpressionAttributeValues :: Lens.Lens' Update (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
uExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# DEPRECATED uExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uReturnValuesOnConditionCheckFailure :: Lens.Lens' Update (Core.Maybe Types.ReturnValuesOnConditionCheckFailure)
uReturnValuesOnConditionCheckFailure = Lens.field @"returnValuesOnConditionCheckFailure"
{-# DEPRECATED uReturnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead." #-}

instance Core.FromJSON Update where
  toJSON Update {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("UpdateExpression" Core..= updateExpression),
            Core.Just ("TableName" Core..= tableName),
            ("ConditionExpression" Core..=) Core.<$> conditionExpression,
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Core..=)
              Core.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Core..=)
              Core.<$> returnValuesOnConditionCheckFailure
          ]
      )
