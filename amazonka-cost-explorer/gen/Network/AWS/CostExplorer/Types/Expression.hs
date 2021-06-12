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
-- Module      : Network.AWS.CostExplorer.Types.Expression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Expression where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.TagValues
import qualified Network.AWS.Lens as Lens

-- | Use @Expression@ to filter by cost or by usage. There are two patterns:
--
-- -   Simple dimension values - You can set the dimension name and values
--     for the filters that you plan to use. For example, you can filter
--     for @REGION==us-east-1 OR REGION==us-west-1@. For
--     @GetRightsizingRecommendation@, the Region is a full name (for
--     example, @REGION==US East (N. Virginia)@. The @Expression@ example
--     looks like:
--
--     @{ \"Dimensions\": { \"Key\": \"REGION\", \"Values\": [ \"us-east-1\", “us-west-1” ] } }@
--
--     The list of dimension values are OR\'d together to retrieve cost or
--     usage data. You can create @Expression@ and @DimensionValues@
--     objects using either @with*@ methods or @set*@ methods in multiple
--     lines.
--
-- -   Compound dimension values with logical operations - You can use
--     multiple @Expression@ types and the logical operators @AND\/OR\/NOT@
--     to create a list of one or more @Expression@ objects. This allows
--     you to filter on more advanced options. For example, you can filter
--     on
--     @((REGION == us-east-1 OR REGION == us-west-1) OR (TAG.Type == Type1)) AND (USAGE_TYPE != DataTransfer)@.
--     The @Expression@ for that looks like this:
--
--     @{ \"And\": [ {\"Or\": [ {\"Dimensions\": { \"Key\": \"REGION\", \"Values\": [ \"us-east-1\", \"us-west-1\" ] }}, {\"Tags\": { \"Key\": \"TagName\", \"Values\": [\"Value1\"] } } ]}, {\"Not\": {\"Dimensions\": { \"Key\": \"USAGE_TYPE\", \"Values\": [\"DataTransfer\"] }}} ] } @
--
--     Because each @Expression@ can have only one operator, the service
--     returns an error if more than one is specified. The following
--     example shows an @Expression@ object that creates an error.
--
--     @ { \"And\": [ ... ], \"DimensionValues\": { \"Dimension\": \"USAGE_TYPE\", \"Values\": [ \"DataTransfer\" ] } } @
--
-- For the @GetRightsizingRecommendation@ action, a combination of OR and
-- NOT is not supported. OR is not supported between different dimensions,
-- or dimensions and tags. NOT operators aren\'t supported. Dimensions are
-- also limited to @LINKED_ACCOUNT@, @REGION@, or @RIGHTSIZING_TYPE@.
--
-- For the @GetReservationPurchaseRecommendation@ action, only NOT is
-- supported. AND and OR are not supported. Dimensions are limited to
-- @LINKED_ACCOUNT@.
--
-- /See:/ 'newExpression' smart constructor.
data Expression = Expression'
  { -- | Return results that don\'t match a @Dimension@ object.
    not :: Core.Maybe Expression,
    -- | Return results that match either @Dimension@ object.
    or :: Core.Maybe [Expression],
    -- | The filter based on @CostCategory@ values.
    costCategories :: Core.Maybe CostCategoryValues,
    -- | The specific @Tag@ to use for @Expression@.
    tags :: Core.Maybe TagValues,
    -- | Return results that match both @Dimension@ objects.
    and :: Core.Maybe [Expression],
    -- | The specific @Dimension@ to use for @Expression@.
    dimensions :: Core.Maybe DimensionValues
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Expression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'not', 'expression_not' - Return results that don\'t match a @Dimension@ object.
--
-- 'or', 'expression_or' - Return results that match either @Dimension@ object.
--
-- 'costCategories', 'expression_costCategories' - The filter based on @CostCategory@ values.
--
-- 'tags', 'expression_tags' - The specific @Tag@ to use for @Expression@.
--
-- 'and', 'expression_and' - Return results that match both @Dimension@ objects.
--
-- 'dimensions', 'expression_dimensions' - The specific @Dimension@ to use for @Expression@.
newExpression ::
  Expression
newExpression =
  Expression'
    { not = Core.Nothing,
      or = Core.Nothing,
      costCategories = Core.Nothing,
      tags = Core.Nothing,
      and = Core.Nothing,
      dimensions = Core.Nothing
    }

-- | Return results that don\'t match a @Dimension@ object.
expression_not :: Lens.Lens' Expression (Core.Maybe Expression)
expression_not = Lens.lens (\Expression' {not} -> not) (\s@Expression' {} a -> s {not = a} :: Expression)

-- | Return results that match either @Dimension@ object.
expression_or :: Lens.Lens' Expression (Core.Maybe [Expression])
expression_or = Lens.lens (\Expression' {or} -> or) (\s@Expression' {} a -> s {or = a} :: Expression) Core.. Lens.mapping Lens._Coerce

-- | The filter based on @CostCategory@ values.
expression_costCategories :: Lens.Lens' Expression (Core.Maybe CostCategoryValues)
expression_costCategories = Lens.lens (\Expression' {costCategories} -> costCategories) (\s@Expression' {} a -> s {costCategories = a} :: Expression)

-- | The specific @Tag@ to use for @Expression@.
expression_tags :: Lens.Lens' Expression (Core.Maybe TagValues)
expression_tags = Lens.lens (\Expression' {tags} -> tags) (\s@Expression' {} a -> s {tags = a} :: Expression)

-- | Return results that match both @Dimension@ objects.
expression_and :: Lens.Lens' Expression (Core.Maybe [Expression])
expression_and = Lens.lens (\Expression' {and} -> and) (\s@Expression' {} a -> s {and = a} :: Expression) Core.. Lens.mapping Lens._Coerce

-- | The specific @Dimension@ to use for @Expression@.
expression_dimensions :: Lens.Lens' Expression (Core.Maybe DimensionValues)
expression_dimensions = Lens.lens (\Expression' {dimensions} -> dimensions) (\s@Expression' {} a -> s {dimensions = a} :: Expression)

instance Core.FromJSON Expression where
  parseJSON =
    Core.withObject
      "Expression"
      ( \x ->
          Expression'
            Core.<$> (x Core..:? "Not")
            Core.<*> (x Core..:? "Or" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CostCategories")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (x Core..:? "And" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Dimensions")
      )

instance Core.Hashable Expression

instance Core.NFData Expression

instance Core.ToJSON Expression where
  toJSON Expression' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Not" Core..=) Core.<$> not,
            ("Or" Core..=) Core.<$> or,
            ("CostCategories" Core..=) Core.<$> costCategories,
            ("Tags" Core..=) Core.<$> tags,
            ("And" Core..=) Core.<$> and,
            ("Dimensions" Core..=) Core.<$> dimensions
          ]
      )
