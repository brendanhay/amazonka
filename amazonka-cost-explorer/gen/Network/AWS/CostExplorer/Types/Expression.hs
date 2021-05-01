{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.TagValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    not :: Prelude.Maybe Expression,
    -- | Return results that match either @Dimension@ object.
    or :: Prelude.Maybe [Expression],
    -- | The filter based on @CostCategory@ values.
    costCategories :: Prelude.Maybe CostCategoryValues,
    -- | The specific @Tag@ to use for @Expression@.
    tags :: Prelude.Maybe TagValues,
    -- | Return results that match both @Dimension@ objects.
    and :: Prelude.Maybe [Expression],
    -- | The specific @Dimension@ to use for @Expression@.
    dimensions :: Prelude.Maybe DimensionValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { not = Prelude.Nothing,
      or = Prelude.Nothing,
      costCategories = Prelude.Nothing,
      tags = Prelude.Nothing,
      and = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | Return results that don\'t match a @Dimension@ object.
expression_not :: Lens.Lens' Expression (Prelude.Maybe Expression)
expression_not = Lens.lens (\Expression' {not} -> not) (\s@Expression' {} a -> s {not = a} :: Expression)

-- | Return results that match either @Dimension@ object.
expression_or :: Lens.Lens' Expression (Prelude.Maybe [Expression])
expression_or = Lens.lens (\Expression' {or} -> or) (\s@Expression' {} a -> s {or = a} :: Expression) Prelude.. Lens.mapping Prelude._Coerce

-- | The filter based on @CostCategory@ values.
expression_costCategories :: Lens.Lens' Expression (Prelude.Maybe CostCategoryValues)
expression_costCategories = Lens.lens (\Expression' {costCategories} -> costCategories) (\s@Expression' {} a -> s {costCategories = a} :: Expression)

-- | The specific @Tag@ to use for @Expression@.
expression_tags :: Lens.Lens' Expression (Prelude.Maybe TagValues)
expression_tags = Lens.lens (\Expression' {tags} -> tags) (\s@Expression' {} a -> s {tags = a} :: Expression)

-- | Return results that match both @Dimension@ objects.
expression_and :: Lens.Lens' Expression (Prelude.Maybe [Expression])
expression_and = Lens.lens (\Expression' {and} -> and) (\s@Expression' {} a -> s {and = a} :: Expression) Prelude.. Lens.mapping Prelude._Coerce

-- | The specific @Dimension@ to use for @Expression@.
expression_dimensions :: Lens.Lens' Expression (Prelude.Maybe DimensionValues)
expression_dimensions = Lens.lens (\Expression' {dimensions} -> dimensions) (\s@Expression' {} a -> s {dimensions = a} :: Expression)

instance Prelude.FromJSON Expression where
  parseJSON =
    Prelude.withObject
      "Expression"
      ( \x ->
          Expression'
            Prelude.<$> (x Prelude..:? "Not")
            Prelude.<*> (x Prelude..:? "Or" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "CostCategories")
            Prelude.<*> (x Prelude..:? "Tags")
            Prelude.<*> (x Prelude..:? "And" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Dimensions")
      )

instance Prelude.Hashable Expression

instance Prelude.NFData Expression

instance Prelude.ToJSON Expression where
  toJSON Expression' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Not" Prelude..=) Prelude.<$> not,
            ("Or" Prelude..=) Prelude.<$> or,
            ("CostCategories" Prelude..=)
              Prelude.<$> costCategories,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("And" Prelude..=) Prelude.<$> and,
            ("Dimensions" Prelude..=) Prelude.<$> dimensions
          ]
      )
