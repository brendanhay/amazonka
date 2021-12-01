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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.DimensionValues where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.Dimension
import Amazonka.CostExplorer.Types.MatchOption
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata that you can use to filter and group your results. You can
-- use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'newDimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { -- | The metadata values that you can use to filter and group your results.
    -- You can use @GetDimensionValues@ to find specific values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The names of the metadata types that you can use to filter and group
    -- your results. For example, @AZ@ returns a list of Availability Zones.
    key :: Prelude.Maybe Dimension,
    -- | The match options that you can use to filter your results.
    -- @MatchOptions@ is only applicable for actions related to Cost Category.
    -- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption]
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
-- 'values', 'dimensionValues_values' - The metadata values that you can use to filter and group your results.
-- You can use @GetDimensionValues@ to find specific values.
--
-- 'key', 'dimensionValues_key' - The names of the metadata types that you can use to filter and group
-- your results. For example, @AZ@ returns a list of Availability Zones.
--
-- 'matchOptions', 'dimensionValues_matchOptions' - The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
newDimensionValues ::
  DimensionValues
newDimensionValues =
  DimensionValues'
    { values = Prelude.Nothing,
      key = Prelude.Nothing,
      matchOptions = Prelude.Nothing
    }

-- | The metadata values that you can use to filter and group your results.
-- You can use @GetDimensionValues@ to find specific values.
dimensionValues_values :: Lens.Lens' DimensionValues (Prelude.Maybe [Prelude.Text])
dimensionValues_values = Lens.lens (\DimensionValues' {values} -> values) (\s@DimensionValues' {} a -> s {values = a} :: DimensionValues) Prelude.. Lens.mapping Lens.coerced

-- | The names of the metadata types that you can use to filter and group
-- your results. For example, @AZ@ returns a list of Availability Zones.
dimensionValues_key :: Lens.Lens' DimensionValues (Prelude.Maybe Dimension)
dimensionValues_key = Lens.lens (\DimensionValues' {key} -> key) (\s@DimensionValues' {} a -> s {key = a} :: DimensionValues)

-- | The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
dimensionValues_matchOptions :: Lens.Lens' DimensionValues (Prelude.Maybe [MatchOption])
dimensionValues_matchOptions = Lens.lens (\DimensionValues' {matchOptions} -> matchOptions) (\s@DimensionValues' {} a -> s {matchOptions = a} :: DimensionValues) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DimensionValues where
  parseJSON =
    Core.withObject
      "DimensionValues"
      ( \x ->
          DimensionValues'
            Prelude.<$> (x Core..:? "Values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "MatchOptions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable DimensionValues where
  hashWithSalt salt' DimensionValues' {..} =
    salt' `Prelude.hashWithSalt` matchOptions
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData DimensionValues where
  rnf DimensionValues' {..} =
    Prelude.rnf values
      `Prelude.seq` Prelude.rnf matchOptions
      `Prelude.seq` Prelude.rnf key

instance Core.ToJSON DimensionValues where
  toJSON DimensionValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("Key" Core..=) Prelude.<$> key,
            ("MatchOptions" Core..=) Prelude.<$> matchOptions
          ]
      )
