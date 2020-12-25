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

import qualified Network.AWS.CloudSearch.Types.ExpressionValue as Types
import qualified Network.AWS.CloudSearch.Types.StandardName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A named expression that can be evaluated at search time. Can be used to sort the search results, define other expressions, or return computed information in the search results.
--
-- /See:/ 'mkExpression' smart constructor.
data Expression = Expression'
  { expressionName :: Types.StandardName,
    expressionValue :: Types.ExpressionValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Expression' value with any optional fields omitted.
mkExpression ::
  -- | 'expressionName'
  Types.StandardName ->
  -- | 'expressionValue'
  Types.ExpressionValue ->
  Expression
mkExpression expressionName expressionValue =
  Expression' {expressionName, expressionValue}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expressionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExpressionName :: Lens.Lens' Expression Types.StandardName
eExpressionName = Lens.field @"expressionName"
{-# DEPRECATED eExpressionName "Use generic-lens or generic-optics with 'expressionName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expressionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExpressionValue :: Lens.Lens' Expression Types.ExpressionValue
eExpressionValue = Lens.field @"expressionValue"
{-# DEPRECATED eExpressionValue "Use generic-lens or generic-optics with 'expressionValue' instead." #-}

instance Core.FromXML Expression where
  parseXML x =
    Expression'
      Core.<$> (x Core..@ "ExpressionName") Core.<*> (x Core..@ "ExpressionValue")
