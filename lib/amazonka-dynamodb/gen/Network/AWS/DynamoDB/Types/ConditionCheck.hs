{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ccKey,
    ccTableName,
    ccConditionExpression,
    ccExpressionAttributeNames,
    ccExpressionAttributeValues,
    ccReturnValuesOnConditionCheckFailure,
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

-- | Represents a request to perform a check that an item exists or to check the condition of specific attributes of the item.
--
-- /See:/ 'mkConditionCheck' smart constructor.
data ConditionCheck = ConditionCheck'
  { -- | The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
    key :: Core.HashMap Types.AttributeName Types.AttributeValue,
    -- | Name of the table for the check item request.
    tableName :: Types.TableName,
    -- | A condition that must be satisfied in order for a conditional update to succeed.
    conditionExpression :: Types.ConditionExpression,
    -- | One or more substitution tokens for attribute names in an expression.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName),
    -- | One or more values that can be substituted in an expression.
    expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue),
    -- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
    returnValuesOnConditionCheckFailure :: Core.Maybe Types.ReturnValuesOnConditionCheckFailure
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConditionCheck' value with any optional fields omitted.
mkConditionCheck ::
  -- | 'tableName'
  Types.TableName ->
  -- | 'conditionExpression'
  Types.ConditionExpression ->
  ConditionCheck
mkConditionCheck tableName conditionExpression =
  ConditionCheck'
    { key = Core.mempty,
      tableName,
      conditionExpression,
      expressionAttributeNames = Core.Nothing,
      expressionAttributeValues = Core.Nothing,
      returnValuesOnConditionCheckFailure = Core.Nothing
    }

-- | The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKey :: Lens.Lens' ConditionCheck (Core.HashMap Types.AttributeName Types.AttributeValue)
ccKey = Lens.field @"key"
{-# DEPRECATED ccKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Name of the table for the check item request.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTableName :: Lens.Lens' ConditionCheck Types.TableName
ccTableName = Lens.field @"tableName"
{-# DEPRECATED ccTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A condition that must be satisfied in order for a conditional update to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConditionExpression :: Lens.Lens' ConditionCheck Types.ConditionExpression
ccConditionExpression = Lens.field @"conditionExpression"
{-# DEPRECATED ccConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpressionAttributeNames :: Lens.Lens' ConditionCheck (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
ccExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED ccExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpressionAttributeValues :: Lens.Lens' ConditionCheck (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
ccExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# DEPRECATED ccExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReturnValuesOnConditionCheckFailure :: Lens.Lens' ConditionCheck (Core.Maybe Types.ReturnValuesOnConditionCheckFailure)
ccReturnValuesOnConditionCheckFailure = Lens.field @"returnValuesOnConditionCheckFailure"
{-# DEPRECATED ccReturnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead." #-}

instance Core.FromJSON ConditionCheck where
  toJSON ConditionCheck {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("ConditionExpression" Core..= conditionExpression),
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Core..=)
              Core.<$> expressionAttributeValues,
            ("ReturnValuesOnConditionCheckFailure" Core..=)
              Core.<$> returnValuesOnConditionCheckFailure
          ]
      )
