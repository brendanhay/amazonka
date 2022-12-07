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
-- Module      : Amazonka.CostExplorer.Types.DimensionValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.DimensionValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.Dimension
import Amazonka.CostExplorer.Types.MatchOption
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata that you can use to filter and group your results. You can
-- use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'newDimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { -- | The names of the metadata types that you can use to filter and group
    -- your results. For example, @AZ@ returns a list of Availability Zones.
    -- @LINK_ACCOUNT_NAME@ and @SERVICE_CODE@ can only be used in
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/AAPI_CostCategoryRule.html CostCategoryRule>.
    key :: Prelude.Maybe Dimension,
    -- | The match options that you can use to filter your results.
    -- @MatchOptions@ is only applicable for actions related to Cost Category.
    -- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption],
    -- | The metadata values that you can use to filter and group your results.
    -- You can use @GetDimensionValues@ to find specific values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'dimensionValues_key' - The names of the metadata types that you can use to filter and group
-- your results. For example, @AZ@ returns a list of Availability Zones.
-- @LINK_ACCOUNT_NAME@ and @SERVICE_CODE@ can only be used in
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/AAPI_CostCategoryRule.html CostCategoryRule>.
--
-- 'matchOptions', 'dimensionValues_matchOptions' - The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
--
-- 'values', 'dimensionValues_values' - The metadata values that you can use to filter and group your results.
-- You can use @GetDimensionValues@ to find specific values.
newDimensionValues ::
  DimensionValues
newDimensionValues =
  DimensionValues'
    { key = Prelude.Nothing,
      matchOptions = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The names of the metadata types that you can use to filter and group
-- your results. For example, @AZ@ returns a list of Availability Zones.
-- @LINK_ACCOUNT_NAME@ and @SERVICE_CODE@ can only be used in
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/AAPI_CostCategoryRule.html CostCategoryRule>.
dimensionValues_key :: Lens.Lens' DimensionValues (Prelude.Maybe Dimension)
dimensionValues_key = Lens.lens (\DimensionValues' {key} -> key) (\s@DimensionValues' {} a -> s {key = a} :: DimensionValues)

-- | The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
dimensionValues_matchOptions :: Lens.Lens' DimensionValues (Prelude.Maybe [MatchOption])
dimensionValues_matchOptions = Lens.lens (\DimensionValues' {matchOptions} -> matchOptions) (\s@DimensionValues' {} a -> s {matchOptions = a} :: DimensionValues) Prelude.. Lens.mapping Lens.coerced

-- | The metadata values that you can use to filter and group your results.
-- You can use @GetDimensionValues@ to find specific values.
dimensionValues_values :: Lens.Lens' DimensionValues (Prelude.Maybe [Prelude.Text])
dimensionValues_values = Lens.lens (\DimensionValues' {values} -> values) (\s@DimensionValues' {} a -> s {values = a} :: DimensionValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DimensionValues where
  parseJSON =
    Data.withObject
      "DimensionValues"
      ( \x ->
          DimensionValues'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "MatchOptions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DimensionValues where
  hashWithSalt _salt DimensionValues' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` matchOptions
      `Prelude.hashWithSalt` values

instance Prelude.NFData DimensionValues where
  rnf DimensionValues' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf matchOptions
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON DimensionValues where
  toJSON DimensionValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("MatchOptions" Data..=) Prelude.<$> matchOptions,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
