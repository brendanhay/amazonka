{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchExpression
  ( SearchExpression (..),

    -- * Smart constructor
    mkSearchExpression,

    -- * Lenses
    seSubExpressions,
    seOperator,
    seFilters,
    seNestedFilters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.BooleanOperator
import Network.AWS.SageMaker.Types.Filter
import Network.AWS.SageMaker.Types.NestedFilters

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
  { -- | A list of search expression objects.
    subExpressions :: Lude.Maybe (Lude.NonEmpty SearchExpression),
    -- | A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
    operator :: Lude.Maybe BooleanOperator,
    -- | A list of filter objects.
    filters :: Lude.Maybe (Lude.NonEmpty Filter),
    -- | A list of nested filter objects.
    nestedFilters :: Lude.Maybe (Lude.NonEmpty NestedFilters)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchExpression' with the minimum fields required to make a request.
--
-- * 'subExpressions' - A list of search expression objects.
-- * 'operator' - A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
-- * 'filters' - A list of filter objects.
-- * 'nestedFilters' - A list of nested filter objects.
mkSearchExpression ::
  SearchExpression
mkSearchExpression =
  SearchExpression'
    { subExpressions = Lude.Nothing,
      operator = Lude.Nothing,
      filters = Lude.Nothing,
      nestedFilters = Lude.Nothing
    }

-- | A list of search expression objects.
--
-- /Note:/ Consider using 'subExpressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSubExpressions :: Lens.Lens' SearchExpression (Lude.Maybe (Lude.NonEmpty SearchExpression))
seSubExpressions = Lens.lens (subExpressions :: SearchExpression -> Lude.Maybe (Lude.NonEmpty SearchExpression)) (\s a -> s {subExpressions = a} :: SearchExpression)
{-# DEPRECATED seSubExpressions "Use generic-lens or generic-optics with 'subExpressions' instead." #-}

-- | A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOperator :: Lens.Lens' SearchExpression (Lude.Maybe BooleanOperator)
seOperator = Lens.lens (operator :: SearchExpression -> Lude.Maybe BooleanOperator) (\s a -> s {operator = a} :: SearchExpression)
{-# DEPRECATED seOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | A list of filter objects.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFilters :: Lens.Lens' SearchExpression (Lude.Maybe (Lude.NonEmpty Filter))
seFilters = Lens.lens (filters :: SearchExpression -> Lude.Maybe (Lude.NonEmpty Filter)) (\s a -> s {filters = a} :: SearchExpression)
{-# DEPRECATED seFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A list of nested filter objects.
--
-- /Note:/ Consider using 'nestedFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seNestedFilters :: Lens.Lens' SearchExpression (Lude.Maybe (Lude.NonEmpty NestedFilters))
seNestedFilters = Lens.lens (nestedFilters :: SearchExpression -> Lude.Maybe (Lude.NonEmpty NestedFilters)) (\s a -> s {nestedFilters = a} :: SearchExpression)
{-# DEPRECATED seNestedFilters "Use generic-lens or generic-optics with 'nestedFilters' instead." #-}

instance Lude.ToJSON SearchExpression where
  toJSON SearchExpression' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubExpressions" Lude..=) Lude.<$> subExpressions,
            ("Operator" Lude..=) Lude.<$> operator,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NestedFilters" Lude..=) Lude.<$> nestedFilters
          ]
      )
