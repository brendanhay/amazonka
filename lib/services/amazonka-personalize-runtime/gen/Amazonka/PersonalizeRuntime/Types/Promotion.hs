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
-- Module      : Amazonka.PersonalizeRuntime.Types.Promotion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeRuntime.Types.Promotion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information on a promotion. A promotion defines additional
-- business rules that apply to a configurable subset of recommended items.
--
-- /See:/ 'newPromotion' smart constructor.
data Promotion = Promotion'
  { -- | The Amazon Resource Name (ARN) of the filter used by the promotion. This
    -- filter defines the criteria for promoted items. For more information,
    -- see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/promoting-items.html#promotion-filters Promotion filters>.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The values to use when promoting items. For each placeholder parameter
    -- in your promotion\'s filter expression, provide the parameter name (in
    -- matching case) as a key and the filter value(s) as the corresponding
    -- value. Separate multiple values for one parameter with a comma.
    --
    -- For filter expressions that use an @INCLUDE@ element to include items,
    -- you must provide values for all parameters that are defined in the
    -- expression. For filters with expressions that use an @EXCLUDE@ element
    -- to exclude items, you can omit the @filter-values@. In this case, Amazon
    -- Personalize doesn\'t use that portion of the expression to filter
    -- recommendations.
    --
    -- For more information on creating filters, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering recommendations and user segments>.
    filterValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The name of the promotion.
    name :: Prelude.Maybe Prelude.Text,
    -- | The percentage of recommended items to apply the promotion to.
    percentPromotedItems :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Promotion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterArn', 'promotion_filterArn' - The Amazon Resource Name (ARN) of the filter used by the promotion. This
-- filter defines the criteria for promoted items. For more information,
-- see
-- <https://docs.aws.amazon.com/personalize/latest/dg/promoting-items.html#promotion-filters Promotion filters>.
--
-- 'filterValues', 'promotion_filterValues' - The values to use when promoting items. For each placeholder parameter
-- in your promotion\'s filter expression, provide the parameter name (in
-- matching case) as a key and the filter value(s) as the corresponding
-- value. Separate multiple values for one parameter with a comma.
--
-- For filter expressions that use an @INCLUDE@ element to include items,
-- you must provide values for all parameters that are defined in the
-- expression. For filters with expressions that use an @EXCLUDE@ element
-- to exclude items, you can omit the @filter-values@. In this case, Amazon
-- Personalize doesn\'t use that portion of the expression to filter
-- recommendations.
--
-- For more information on creating filters, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering recommendations and user segments>.
--
-- 'name', 'promotion_name' - The name of the promotion.
--
-- 'percentPromotedItems', 'promotion_percentPromotedItems' - The percentage of recommended items to apply the promotion to.
newPromotion ::
  Promotion
newPromotion =
  Promotion'
    { filterArn = Prelude.Nothing,
      filterValues = Prelude.Nothing,
      name = Prelude.Nothing,
      percentPromotedItems = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the filter used by the promotion. This
-- filter defines the criteria for promoted items. For more information,
-- see
-- <https://docs.aws.amazon.com/personalize/latest/dg/promoting-items.html#promotion-filters Promotion filters>.
promotion_filterArn :: Lens.Lens' Promotion (Prelude.Maybe Prelude.Text)
promotion_filterArn = Lens.lens (\Promotion' {filterArn} -> filterArn) (\s@Promotion' {} a -> s {filterArn = a} :: Promotion)

-- | The values to use when promoting items. For each placeholder parameter
-- in your promotion\'s filter expression, provide the parameter name (in
-- matching case) as a key and the filter value(s) as the corresponding
-- value. Separate multiple values for one parameter with a comma.
--
-- For filter expressions that use an @INCLUDE@ element to include items,
-- you must provide values for all parameters that are defined in the
-- expression. For filters with expressions that use an @EXCLUDE@ element
-- to exclude items, you can omit the @filter-values@. In this case, Amazon
-- Personalize doesn\'t use that portion of the expression to filter
-- recommendations.
--
-- For more information on creating filters, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering recommendations and user segments>.
promotion_filterValues :: Lens.Lens' Promotion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
promotion_filterValues = Lens.lens (\Promotion' {filterValues} -> filterValues) (\s@Promotion' {} a -> s {filterValues = a} :: Promotion) Prelude.. Lens.mapping Lens.coerced

-- | The name of the promotion.
promotion_name :: Lens.Lens' Promotion (Prelude.Maybe Prelude.Text)
promotion_name = Lens.lens (\Promotion' {name} -> name) (\s@Promotion' {} a -> s {name = a} :: Promotion)

-- | The percentage of recommended items to apply the promotion to.
promotion_percentPromotedItems :: Lens.Lens' Promotion (Prelude.Maybe Prelude.Natural)
promotion_percentPromotedItems = Lens.lens (\Promotion' {percentPromotedItems} -> percentPromotedItems) (\s@Promotion' {} a -> s {percentPromotedItems = a} :: Promotion)

instance Prelude.Hashable Promotion where
  hashWithSalt _salt Promotion' {..} =
    _salt
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` filterValues
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` percentPromotedItems

instance Prelude.NFData Promotion where
  rnf Promotion' {..} =
    Prelude.rnf filterArn `Prelude.seq`
      Prelude.rnf filterValues `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf percentPromotedItems

instance Data.ToJSON Promotion where
  toJSON Promotion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterArn" Data..=) Prelude.<$> filterArn,
            ("filterValues" Data..=) Prelude.<$> filterValues,
            ("name" Data..=) Prelude.<$> name,
            ("percentPromotedItems" Data..=)
              Prelude.<$> percentPromotedItems
          ]
      )
