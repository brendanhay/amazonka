{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dKey,
    dTableName,
    dConditionExpression,
    dExpressionAttributeNames,
    dExpressionAttributeValues,
    dReturnValuesOnConditionCheckFailure,
  )
where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.ConditionExpression as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeNameVariable as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeValueVariable as Types
import qualified Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a request to perform a @DeleteItem@ operation.
--
-- /See:/ 'mkDelete' smart constructor.
data Delete = Delete'
  { -- | The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
    key :: Core.HashMap Types.AttributeName Types.AttributeValue,
    -- | Name of the table in which the item to be deleted resides.
    tableName :: Types.TableName,
    -- | A condition that must be satisfied in order for a conditional delete to succeed.
    conditionExpression :: Core.Maybe Types.ConditionExpression,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName),
    -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
    returnValuesOnConditionCheckFailure :: Core.Maybe Types.ReturnValuesOnConditionCheckFailure
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Delete' value with any optional fields omitted.
mkDelete ::
  -- | 'tableName'
  Types.TableName ->
  Delete
mkDelete tableName =
  Delete'
    { key = Core.mempty,
      tableName,
      conditionExpression = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      expressionAttributeValues = Core.Nothing,
      returnValuesOnConditionCheckFailure = Core.Nothing
    }

-- | The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dKey :: Lens.Lens' Delete (Core.HashMap Types.AttributeName Types.AttributeValue)
dKey = Lens.field @"key"
{-# DEPRECATED dKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Name of the table in which the item to be deleted resides.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTableName :: Lens.Lens' Delete Types.TableName
dTableName = Lens.field @"tableName"
{-# DEPRECATED dTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A condition that must be satisfied in order for a conditional delete to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConditionExpression :: Lens.Lens' Delete (Core.Maybe Types.ConditionExpression)
dConditionExpression = Lens.field @"conditionExpression"
{-# DEPRECATED dConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpressionAttributeNames :: Lens.Lens' Delete (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
dExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED dExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpressionAttributeValues :: Lens.Lens' Delete (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
dExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# DEPRECATED dExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReturnValuesOnConditionCheckFailure :: Lens.Lens' Delete (Core.Maybe Types.ReturnValuesOnConditionCheckFailure)
dReturnValuesOnConditionCheckFailure = Lens.field @"returnValuesOnConditionCheckFailure"
{-# DEPRECATED dReturnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead." #-}

instance Core.FromJSON Delete where
  toJSON Delete {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
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
