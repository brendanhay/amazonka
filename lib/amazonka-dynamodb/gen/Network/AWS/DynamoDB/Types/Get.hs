{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Get
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Get
  ( Get (..),

    -- * Smart constructor
    mkGet,

    -- * Lenses
    gKey,
    gTableName,
    gExpressionAttributeNames,
    gProjectionExpression,
  )
where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.ExpressionAttributeNameVariable as Types
import qualified Network.AWS.DynamoDB.Types.ProjectionExpression as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an item and related attribute values to retrieve in a @TransactGetItem@ object.
--
-- /See:/ 'mkGet' smart constructor.
data Get = Get'
  { -- | A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
    key :: Core.HashMap Types.AttributeName Types.AttributeValue,
    -- | The name of the table from which to retrieve the specified item.
    tableName :: Types.TableName,
    -- | One or more substitution tokens for attribute names in the ProjectionExpression parameter.
    expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName),
    -- | A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
    projectionExpression :: Core.Maybe Types.ProjectionExpression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Get' value with any optional fields omitted.
mkGet ::
  -- | 'tableName'
  Types.TableName ->
  Get
mkGet tableName =
  Get'
    { key = Core.mempty,
      tableName,
      expressionAttributeNames = Core.Nothing,
      projectionExpression = Core.Nothing
    }

-- | A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gKey :: Lens.Lens' Get (Core.HashMap Types.AttributeName Types.AttributeValue)
gKey = Lens.field @"key"
{-# DEPRECATED gKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The name of the table from which to retrieve the specified item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gTableName :: Lens.Lens' Get Types.TableName
gTableName = Lens.field @"tableName"
{-# DEPRECATED gTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | One or more substitution tokens for attribute names in the ProjectionExpression parameter.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gExpressionAttributeNames :: Lens.Lens' Get (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
gExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED gExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gProjectionExpression :: Lens.Lens' Get (Core.Maybe Types.ProjectionExpression)
gProjectionExpression = Lens.field @"projectionExpression"
{-# DEPRECATED gProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

instance Core.FromJSON Get where
  toJSON Get {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("TableName" Core..= tableName),
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ProjectionExpression" Core..=) Core.<$> projectionExpression
          ]
      )
