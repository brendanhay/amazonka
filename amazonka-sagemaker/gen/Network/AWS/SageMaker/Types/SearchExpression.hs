{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchExpression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchExpression where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.BooleanOperator
import Network.AWS.SageMaker.Types.Filter
import Network.AWS.SageMaker.Types.NestedFilters

-- | A multi-expression that searches for the specified resource or resources
-- in a search. All resource objects that satisfy the expression\'s
-- condition are included in the search results. You must specify at least
-- one subexpression, filter, or nested filter. A @SearchExpression@ can
-- contain up to twenty elements.
--
-- A @SearchExpression@ contains the following components:
--
-- -   A list of @Filter@ objects. Each filter defines a simple Boolean
--     expression comprised of a resource property name, Boolean operator,
--     and value.
--
-- -   A list of @NestedFilter@ objects. Each nested filter defines a list
--     of Boolean expressions using a list of resource properties. A nested
--     filter is satisfied if a single object in the list satisfies all
--     Boolean expressions.
--
-- -   A list of @SearchExpression@ objects. A search expression object can
--     be nested in a list of search expression objects.
--
-- -   A Boolean operator: @And@ or @Or@.
--
-- /See:/ 'newSearchExpression' smart constructor.
data SearchExpression = SearchExpression'
  { -- | A list of nested filter objects.
    nestedFilters :: Core.Maybe (Core.NonEmpty NestedFilters),
    -- | A Boolean operator used to evaluate the search expression. If you want
    -- every conditional statement in all lists to be satisfied for the entire
    -- search expression to be true, specify @And@. If only a single
    -- conditional statement needs to be true for the entire search expression
    -- to be true, specify @Or@. The default value is @And@.
    operator :: Core.Maybe BooleanOperator,
    -- | A list of filter objects.
    filters :: Core.Maybe (Core.NonEmpty Filter),
    -- | A list of search expression objects.
    subExpressions :: Core.Maybe (Core.NonEmpty SearchExpression)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nestedFilters', 'searchExpression_nestedFilters' - A list of nested filter objects.
--
-- 'operator', 'searchExpression_operator' - A Boolean operator used to evaluate the search expression. If you want
-- every conditional statement in all lists to be satisfied for the entire
-- search expression to be true, specify @And@. If only a single
-- conditional statement needs to be true for the entire search expression
-- to be true, specify @Or@. The default value is @And@.
--
-- 'filters', 'searchExpression_filters' - A list of filter objects.
--
-- 'subExpressions', 'searchExpression_subExpressions' - A list of search expression objects.
newSearchExpression ::
  SearchExpression
newSearchExpression =
  SearchExpression'
    { nestedFilters = Core.Nothing,
      operator = Core.Nothing,
      filters = Core.Nothing,
      subExpressions = Core.Nothing
    }

-- | A list of nested filter objects.
searchExpression_nestedFilters :: Lens.Lens' SearchExpression (Core.Maybe (Core.NonEmpty NestedFilters))
searchExpression_nestedFilters = Lens.lens (\SearchExpression' {nestedFilters} -> nestedFilters) (\s@SearchExpression' {} a -> s {nestedFilters = a} :: SearchExpression) Core.. Lens.mapping Lens._Coerce

-- | A Boolean operator used to evaluate the search expression. If you want
-- every conditional statement in all lists to be satisfied for the entire
-- search expression to be true, specify @And@. If only a single
-- conditional statement needs to be true for the entire search expression
-- to be true, specify @Or@. The default value is @And@.
searchExpression_operator :: Lens.Lens' SearchExpression (Core.Maybe BooleanOperator)
searchExpression_operator = Lens.lens (\SearchExpression' {operator} -> operator) (\s@SearchExpression' {} a -> s {operator = a} :: SearchExpression)

-- | A list of filter objects.
searchExpression_filters :: Lens.Lens' SearchExpression (Core.Maybe (Core.NonEmpty Filter))
searchExpression_filters = Lens.lens (\SearchExpression' {filters} -> filters) (\s@SearchExpression' {} a -> s {filters = a} :: SearchExpression) Core.. Lens.mapping Lens._Coerce

-- | A list of search expression objects.
searchExpression_subExpressions :: Lens.Lens' SearchExpression (Core.Maybe (Core.NonEmpty SearchExpression))
searchExpression_subExpressions = Lens.lens (\SearchExpression' {subExpressions} -> subExpressions) (\s@SearchExpression' {} a -> s {subExpressions = a} :: SearchExpression) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable SearchExpression

instance Core.NFData SearchExpression

instance Core.ToJSON SearchExpression where
  toJSON SearchExpression' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NestedFilters" Core..=) Core.<$> nestedFilters,
            ("Operator" Core..=) Core.<$> operator,
            ("Filters" Core..=) Core.<$> filters,
            ("SubExpressions" Core..=) Core.<$> subExpressions
          ]
      )
