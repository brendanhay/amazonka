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
-- Module      : Amazonka.CostExplorer.Types.Expression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Expression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategoryValues
import Amazonka.CostExplorer.Types.DimensionValues
import Amazonka.CostExplorer.Types.TagValues
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use @Expression@ to filter in various Cost Explorer APIs.
--
-- Not all @Expression@ types are supported in each API. Refer to the
-- documentation for each specific API to see what is supported.
--
-- There are two patterns:
--
-- -   Simple dimension values.
--
--     -   There are three types of simple dimension values:
--         @CostCategories@, @Tags@, and @Dimensions@.
--
--         -   Specify the @CostCategories@ field to define a filter that
--             acts on Cost Categories.
--
--         -   Specify the @Tags@ field to define a filter that acts on
--             Cost Allocation Tags.
--
--         -   Specify the @Dimensions@ field to define a filter that acts
--             on the
--             <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_DimensionValues.html DimensionValues>
--             .
--
--     -   For each filter type, you can set the dimension name and values
--         for the filters that you plan to use.
--
--         -   For example, you can filter for
--             @REGION==us-east-1 OR REGION==us-west-1@. For
--             @GetRightsizingRecommendation@, the Region is a full name
--             (for example, @REGION==US East (N. Virginia)@.
--
--         -   The corresponding @Expression@ for this example is as
--             follows:
--             @{ \"Dimensions\": { \"Key\": \"REGION\", \"Values\": [ \"us-east-1\", “us-west-1” ] } }@
--
--         -   As shown in the previous example, lists of dimension values
--             are combined with @OR@ when applying the filter.
--
--     -   You can also set different match options to further control how
--         the filter behaves. Not all APIs support match options. Refer to
--         the documentation for each specific API to see what is
--         supported.
--
--         -   For example, you can filter for linked account names that
--             start with “a”.
--
--         -   The corresponding @Expression@ for this example is as
--             follows:
--             @{ \"Dimensions\": { \"Key\": \"LINKED_ACCOUNT_NAME\", \"MatchOptions\": [ \"STARTS_WITH\" ], \"Values\": [ \"a\" ] } }@
--
-- -   Compound @Expression@ types with logical operations.
--
--     -   You can use multiple @Expression@ types and the logical
--         operators @AND\/OR\/NOT@ to create a list of one or more
--         @Expression@ objects. By doing this, you can filter by more
--         advanced options.
--
--     -   For example, you can filter by
--         @((REGION == us-east-1 OR REGION == us-west-1) OR (TAG.Type == Type1)) AND (USAGE_TYPE != DataTransfer)@.
--
--     -   The corresponding @Expression@ for this example is as follows:
--         @{ \"And\": [ {\"Or\": [ {\"Dimensions\": { \"Key\": \"REGION\", \"Values\": [ \"us-east-1\", \"us-west-1\" ] }}, {\"Tags\": { \"Key\": \"TagName\", \"Values\": [\"Value1\"] } } ]}, {\"Not\": {\"Dimensions\": { \"Key\": \"USAGE_TYPE\", \"Values\": [\"DataTransfer\"] }}} ] } @
--
--     Because each @Expression@ can have only one operator, the service
--     returns an error if more than one is specified. The following
--     example shows an @Expression@ object that creates an error:
--     @ { \"And\": [ ... ], \"Dimensions\": { \"Key\": \"USAGE_TYPE\", \"Values\": [ \"DataTransfer\" ] } } @
--
--     The following is an example of the corresponding error message:
--     @\"Expression has more than one roots. Only one root operator is allowed for each expression: And, Or, Not, Dimensions, Tags, CostCategories\"@
--
-- For the @GetRightsizingRecommendation@ action, a combination of OR and
-- NOT isn\'t supported. OR isn\'t supported between different dimensions,
-- or dimensions and tags. NOT operators aren\'t supported. Dimensions are
-- also limited to @LINKED_ACCOUNT@, @REGION@, or @RIGHTSIZING_TYPE@.
--
-- For the @GetReservationPurchaseRecommendation@ action, only NOT is
-- supported. AND and OR aren\'t supported. Dimensions are limited to
-- @LINKED_ACCOUNT@.
--
-- /See:/ 'newExpression' smart constructor.
data Expression = Expression'
  { -- | Return results that match both @Dimension@ objects.
    and :: Prelude.Maybe [Expression],
    -- | The filter that\'s based on @CostCategory@ values.
    costCategories :: Prelude.Maybe CostCategoryValues,
    -- | The specific @Dimension@ to use for @Expression@.
    dimensions :: Prelude.Maybe DimensionValues,
    -- | Return results that don\'t match a @Dimension@ object.
    not :: Prelude.Maybe Expression,
    -- | Return results that match either @Dimension@ object.
    or :: Prelude.Maybe [Expression],
    -- | The specific @Tag@ to use for @Expression@.
    tags :: Prelude.Maybe TagValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Expression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'and', 'expression_and' - Return results that match both @Dimension@ objects.
--
-- 'costCategories', 'expression_costCategories' - The filter that\'s based on @CostCategory@ values.
--
-- 'dimensions', 'expression_dimensions' - The specific @Dimension@ to use for @Expression@.
--
-- 'not', 'expression_not' - Return results that don\'t match a @Dimension@ object.
--
-- 'or', 'expression_or' - Return results that match either @Dimension@ object.
--
-- 'tags', 'expression_tags' - The specific @Tag@ to use for @Expression@.
newExpression ::
  Expression
newExpression =
  Expression'
    { and = Prelude.Nothing,
      costCategories = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      not = Prelude.Nothing,
      or = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Return results that match both @Dimension@ objects.
expression_and :: Lens.Lens' Expression (Prelude.Maybe [Expression])
expression_and = Lens.lens (\Expression' {and} -> and) (\s@Expression' {} a -> s {and = a} :: Expression) Prelude.. Lens.mapping Lens.coerced

-- | The filter that\'s based on @CostCategory@ values.
expression_costCategories :: Lens.Lens' Expression (Prelude.Maybe CostCategoryValues)
expression_costCategories = Lens.lens (\Expression' {costCategories} -> costCategories) (\s@Expression' {} a -> s {costCategories = a} :: Expression)

-- | The specific @Dimension@ to use for @Expression@.
expression_dimensions :: Lens.Lens' Expression (Prelude.Maybe DimensionValues)
expression_dimensions = Lens.lens (\Expression' {dimensions} -> dimensions) (\s@Expression' {} a -> s {dimensions = a} :: Expression)

-- | Return results that don\'t match a @Dimension@ object.
expression_not :: Lens.Lens' Expression (Prelude.Maybe Expression)
expression_not = Lens.lens (\Expression' {not} -> not) (\s@Expression' {} a -> s {not = a} :: Expression)

-- | Return results that match either @Dimension@ object.
expression_or :: Lens.Lens' Expression (Prelude.Maybe [Expression])
expression_or = Lens.lens (\Expression' {or} -> or) (\s@Expression' {} a -> s {or = a} :: Expression) Prelude.. Lens.mapping Lens.coerced

-- | The specific @Tag@ to use for @Expression@.
expression_tags :: Lens.Lens' Expression (Prelude.Maybe TagValues)
expression_tags = Lens.lens (\Expression' {tags} -> tags) (\s@Expression' {} a -> s {tags = a} :: Expression)

instance Data.FromJSON Expression where
  parseJSON =
    Data.withObject
      "Expression"
      ( \x ->
          Expression'
            Prelude.<$> (x Data..:? "And" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CostCategories")
            Prelude.<*> (x Data..:? "Dimensions")
            Prelude.<*> (x Data..:? "Not")
            Prelude.<*> (x Data..:? "Or" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tags")
      )

instance Prelude.Hashable Expression where
  hashWithSalt _salt Expression' {..} =
    _salt `Prelude.hashWithSalt` and
      `Prelude.hashWithSalt` costCategories
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` not
      `Prelude.hashWithSalt` or
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Expression where
  rnf Expression' {..} =
    Prelude.rnf and
      `Prelude.seq` Prelude.rnf costCategories
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf not
      `Prelude.seq` Prelude.rnf or
      `Prelude.seq` Prelude.rnf tags

instance Data.ToJSON Expression where
  toJSON Expression' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("And" Data..=) Prelude.<$> and,
            ("CostCategories" Data..=)
              Prelude.<$> costCategories,
            ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("Not" Data..=) Prelude.<$> not,
            ("Or" Data..=) Prelude.<$> or,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )
