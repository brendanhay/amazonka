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
    gProjectionExpression,
    gExpressionAttributeNames,
    gKey,
    gTableName,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an item and related attribute values to retrieve in a @TransactGetItem@ object.
--
-- /See:/ 'mkGet' smart constructor.
data Get = Get'
  { projectionExpression :: Lude.Maybe Lude.Text,
    expressionAttributeNames ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'Get' with the minimum fields required to make a request.
--
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in the ProjectionExpression parameter.
-- * 'key' - A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
-- * 'projectionExpression' - A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
-- * 'tableName' - The name of the table from which to retrieve the specified item.
mkGet ::
  -- | 'tableName'
  Lude.Text ->
  Get
mkGet pTableName_ =
  Get'
    { projectionExpression = Lude.Nothing,
      expressionAttributeNames = Lude.Nothing,
      key = Lude.mempty,
      tableName = pTableName_
    }

-- | A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gProjectionExpression :: Lens.Lens' Get (Lude.Maybe Lude.Text)
gProjectionExpression = Lens.lens (projectionExpression :: Get -> Lude.Maybe Lude.Text) (\s a -> s {projectionExpression = a} :: Get)
{-# DEPRECATED gProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

-- | One or more substitution tokens for attribute names in the ProjectionExpression parameter.
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gExpressionAttributeNames :: Lens.Lens' Get (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gExpressionAttributeNames = Lens.lens (expressionAttributeNames :: Get -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: Get)
{-# DEPRECATED gExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gKey :: Lens.Lens' Get (Lude.HashMap Lude.Text (AttributeValue))
gKey = Lens.lens (key :: Get -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {key = a} :: Get)
{-# DEPRECATED gKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The name of the table from which to retrieve the specified item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gTableName :: Lens.Lens' Get Lude.Text
gTableName = Lens.lens (tableName :: Get -> Lude.Text) (\s a -> s {tableName = a} :: Get)
{-# DEPRECATED gTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON Get where
  toJSON Get' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProjectionExpression" Lude..=) Lude.<$> projectionExpression,
            ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )
