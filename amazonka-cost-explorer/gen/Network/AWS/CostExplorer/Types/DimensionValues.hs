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
-- Module      : Network.AWS.CostExplorer.Types.DimensionValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValues where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.Dimension
import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens

-- | The metadata that you can use to filter and group your results. You can
-- use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'newDimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { -- | The names of the metadata types that you can use to filter and group
    -- your results. For example, @AZ@ returns a list of Availability Zones.
    key :: Core.Maybe Dimension,
    -- | The metadata values that you can use to filter and group your results.
    -- You can use @GetDimensionValues@ to find specific values.
    values :: Core.Maybe [Core.Text],
    -- | The match options that you can use to filter your results.
    -- @MatchOptions@ is only applicable for actions related to Cost Category.
    -- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Core.Maybe [MatchOption]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
--
-- 'values', 'dimensionValues_values' - The metadata values that you can use to filter and group your results.
-- You can use @GetDimensionValues@ to find specific values.
--
-- 'matchOptions', 'dimensionValues_matchOptions' - The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
newDimensionValues ::
  DimensionValues
newDimensionValues =
  DimensionValues'
    { key = Core.Nothing,
      values = Core.Nothing,
      matchOptions = Core.Nothing
    }

-- | The names of the metadata types that you can use to filter and group
-- your results. For example, @AZ@ returns a list of Availability Zones.
dimensionValues_key :: Lens.Lens' DimensionValues (Core.Maybe Dimension)
dimensionValues_key = Lens.lens (\DimensionValues' {key} -> key) (\s@DimensionValues' {} a -> s {key = a} :: DimensionValues)

-- | The metadata values that you can use to filter and group your results.
-- You can use @GetDimensionValues@ to find specific values.
dimensionValues_values :: Lens.Lens' DimensionValues (Core.Maybe [Core.Text])
dimensionValues_values = Lens.lens (\DimensionValues' {values} -> values) (\s@DimensionValues' {} a -> s {values = a} :: DimensionValues) Core.. Lens.mapping Lens._Coerce

-- | The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
dimensionValues_matchOptions :: Lens.Lens' DimensionValues (Core.Maybe [MatchOption])
dimensionValues_matchOptions = Lens.lens (\DimensionValues' {matchOptions} -> matchOptions) (\s@DimensionValues' {} a -> s {matchOptions = a} :: DimensionValues) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON DimensionValues where
  parseJSON =
    Core.withObject
      "DimensionValues"
      ( \x ->
          DimensionValues'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MatchOptions" Core..!= Core.mempty)
      )

instance Core.Hashable DimensionValues

instance Core.NFData DimensionValues

instance Core.ToJSON DimensionValues where
  toJSON DimensionValues' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values,
            ("MatchOptions" Core..=) Core.<$> matchOptions
          ]
      )
