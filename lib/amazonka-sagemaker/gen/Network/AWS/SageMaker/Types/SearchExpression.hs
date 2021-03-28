{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SearchExpression
  ( SearchExpression (..)
  -- * Smart constructor
  , mkSearchExpression
  -- * Lenses
  , seFilters
  , seNestedFilters
  , seOperator
  , seSubExpressions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.BooleanOperator as Types
import qualified Network.AWS.SageMaker.Types.Filter as Types
import qualified Network.AWS.SageMaker.Types.NestedFilters as Types

-- | A multi-expression that searches for the specified resource or resources in a search. All resource objects that satisfy the expression's condition are included in the search results. You must specify at least one subexpression, filter, or nested filter. A @SearchExpression@ can contain up to twenty elements.
--
-- A @SearchExpression@ contains the following components:
--
--     * A list of @Filter@ objects. Each filter defines a simple Boolean expression comprised of a resource property name, Boolean operator, and value.
--
--
--     * A list of @NestedFilter@ objects. Each nested filter defines a list of Boolean expressions using a list of resource properties. A nested filter is satisfied if a single object in the list satisfies all Boolean expressions.
--
--
--     * A list of @SearchExpression@ objects. A search expression object can be nested in a list of search expression objects.
--
--
--     * A Boolean operator: @And@ or @Or@ .
--
--
--
-- /See:/ 'mkSearchExpression' smart constructor.
data SearchExpression = SearchExpression'
  { filters :: Core.Maybe (Core.NonEmpty Types.Filter)
    -- ^ A list of filter objects.
  , nestedFilters :: Core.Maybe (Core.NonEmpty Types.NestedFilters)
    -- ^ A list of nested filter objects.
  , operator :: Core.Maybe Types.BooleanOperator
    -- ^ A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
  , subExpressions :: Core.Maybe (Core.NonEmpty SearchExpression)
    -- ^ A list of search expression objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchExpression' value with any optional fields omitted.
mkSearchExpression
    :: SearchExpression
mkSearchExpression
  = SearchExpression'{filters = Core.Nothing,
                      nestedFilters = Core.Nothing, operator = Core.Nothing,
                      subExpressions = Core.Nothing}

-- | A list of filter objects.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFilters :: Lens.Lens' SearchExpression (Core.Maybe (Core.NonEmpty Types.Filter))
seFilters = Lens.field @"filters"
{-# INLINEABLE seFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | A list of nested filter objects.
--
-- /Note:/ Consider using 'nestedFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seNestedFilters :: Lens.Lens' SearchExpression (Core.Maybe (Core.NonEmpty Types.NestedFilters))
seNestedFilters = Lens.field @"nestedFilters"
{-# INLINEABLE seNestedFilters #-}
{-# DEPRECATED nestedFilters "Use generic-lens or generic-optics with 'nestedFilters' instead"  #-}

-- | A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOperator :: Lens.Lens' SearchExpression (Core.Maybe Types.BooleanOperator)
seOperator = Lens.field @"operator"
{-# INLINEABLE seOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

-- | A list of search expression objects.
--
-- /Note:/ Consider using 'subExpressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSubExpressions :: Lens.Lens' SearchExpression (Core.Maybe (Core.NonEmpty SearchExpression))
seSubExpressions = Lens.field @"subExpressions"
{-# INLINEABLE seSubExpressions #-}
{-# DEPRECATED subExpressions "Use generic-lens or generic-optics with 'subExpressions' instead"  #-}

instance Core.FromJSON SearchExpression where
        toJSON SearchExpression{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("NestedFilters" Core..=) Core.<$> nestedFilters,
                  ("Operator" Core..=) Core.<$> operator,
                  ("SubExpressions" Core..=) Core.<$> subExpressions])
