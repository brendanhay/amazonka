{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Expression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.Expression
  ( Expression (..),

    -- * Smart constructor
    mkExpression,

    -- * Lenses
    eExpressionName,
    eExpressionValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A named expression that can be evaluated at search time. Can be used to sort the search results, define other expressions, or return computed information in the search results.
--
-- /See:/ 'mkExpression' smart constructor.
data Expression = Expression'
  { expressionName :: Lude.Text,
    expressionValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Expression' with the minimum fields required to make a request.
--
-- * 'expressionName' - Undocumented field.
-- * 'expressionValue' - Undocumented field.
mkExpression ::
  -- | 'expressionName'
  Lude.Text ->
  -- | 'expressionValue'
  Lude.Text ->
  Expression
mkExpression pExpressionName_ pExpressionValue_ =
  Expression'
    { expressionName = pExpressionName_,
      expressionValue = pExpressionValue_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'expressionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExpressionName :: Lens.Lens' Expression Lude.Text
eExpressionName = Lens.lens (expressionName :: Expression -> Lude.Text) (\s a -> s {expressionName = a} :: Expression)
{-# DEPRECATED eExpressionName "Use generic-lens or generic-optics with 'expressionName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expressionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExpressionValue :: Lens.Lens' Expression Lude.Text
eExpressionValue = Lens.lens (expressionValue :: Expression -> Lude.Text) (\s a -> s {expressionValue = a} :: Expression)
{-# DEPRECATED eExpressionValue "Use generic-lens or generic-optics with 'expressionValue' instead." #-}

instance Lude.FromXML Expression where
  parseXML x =
    Expression'
      Lude.<$> (x Lude..@ "ExpressionName") Lude.<*> (x Lude..@ "ExpressionValue")

instance Lude.ToQuery Expression where
  toQuery Expression' {..} =
    Lude.mconcat
      [ "ExpressionName" Lude.=: expressionName,
        "ExpressionValue" Lude.=: expressionValue
      ]
