{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Put
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.Put
  ( Put (..)
  -- * Smart constructor
  , mkPut
  -- * Lenses
  , pItem
  , pTableName
  , pConditionExpression
  , pExpressionAttributeNames
  , pExpressionAttributeValues
  , pReturnValuesOnConditionCheckFailure
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.ConditionExpression as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeNameVariable as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeValueVariable as Types
import qualified Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a request to perform a @PutItem@ operation.
--
-- /See:/ 'mkPut' smart constructor.
data Put = Put'
  { item :: Core.HashMap Types.AttributeName Types.AttributeValue
    -- ^ A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema. 
  , tableName :: Types.TableName
    -- ^ Name of the table in which to write the item.
  , conditionExpression :: Core.Maybe Types.ConditionExpression
    -- ^ A condition that must be satisfied in order for a conditional update to succeed.
  , expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName)
    -- ^ One or more substitution tokens for attribute names in an expression.
  , expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue)
    -- ^ One or more values that can be substituted in an expression.
  , returnValuesOnConditionCheckFailure :: Core.Maybe Types.ReturnValuesOnConditionCheckFailure
    -- ^ Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Put' value with any optional fields omitted.
mkPut
    :: Types.TableName -- ^ 'tableName'
    -> Put
mkPut tableName
  = Put'{item = Core.mempty, tableName,
         conditionExpression = Core.Nothing,
         expressionAttributeNames = Core.Nothing,
         expressionAttributeValues = Core.Nothing,
         returnValuesOnConditionCheckFailure = Core.Nothing}

-- | A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema. 
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pItem :: Lens.Lens' Put (Core.HashMap Types.AttributeName Types.AttributeValue)
pItem = Lens.field @"item"
{-# INLINEABLE pItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | Name of the table in which to write the item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTableName :: Lens.Lens' Put Types.TableName
pTableName = Lens.field @"tableName"
{-# INLINEABLE pTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A condition that must be satisfied in order for a conditional update to succeed.
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pConditionExpression :: Lens.Lens' Put (Core.Maybe Types.ConditionExpression)
pConditionExpression = Lens.field @"conditionExpression"
{-# INLINEABLE pConditionExpression #-}
{-# DEPRECATED conditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead"  #-}

-- | One or more substitution tokens for attribute names in an expression.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExpressionAttributeNames :: Lens.Lens' Put (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
pExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# INLINEABLE pExpressionAttributeNames #-}
{-# DEPRECATED expressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead"  #-}

-- | One or more values that can be substituted in an expression.
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExpressionAttributeValues :: Lens.Lens' Put (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
pExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# INLINEABLE pExpressionAttributeValues #-}
{-# DEPRECATED expressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead"  #-}

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- /Note:/ Consider using 'returnValuesOnConditionCheckFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pReturnValuesOnConditionCheckFailure :: Lens.Lens' Put (Core.Maybe Types.ReturnValuesOnConditionCheckFailure)
pReturnValuesOnConditionCheckFailure = Lens.field @"returnValuesOnConditionCheckFailure"
{-# INLINEABLE pReturnValuesOnConditionCheckFailure #-}
{-# DEPRECATED returnValuesOnConditionCheckFailure "Use generic-lens or generic-optics with 'returnValuesOnConditionCheckFailure' instead"  #-}

instance Core.FromJSON Put where
        toJSON Put{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Item" Core..= item),
                  Core.Just ("TableName" Core..= tableName),
                  ("ConditionExpression" Core..=) Core.<$> conditionExpression,
                  ("ExpressionAttributeNames" Core..=) Core.<$>
                    expressionAttributeNames,
                  ("ExpressionAttributeValues" Core..=) Core.<$>
                    expressionAttributeValues,
                  ("ReturnValuesOnConditionCheckFailure" Core..=) Core.<$>
                    returnValuesOnConditionCheckFailure])
