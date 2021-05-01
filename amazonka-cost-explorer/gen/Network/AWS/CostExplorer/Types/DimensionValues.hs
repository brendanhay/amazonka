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
-- Module      : Network.AWS.CostExplorer.Types.DimensionValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValues where

import Network.AWS.CostExplorer.Types.Dimension
import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata that you can use to filter and group your results. You can
-- use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'newDimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { -- | The names of the metadata types that you can use to filter and group
    -- your results. For example, @AZ@ returns a list of Availability Zones.
    key :: Prelude.Maybe Dimension,
    -- | The metadata values that you can use to filter and group your results.
    -- You can use @GetDimensionValues@ to find specific values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The match options that you can use to filter your results.
    -- @MatchOptions@ is only applicable for actions related to Cost Category.
    -- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      values = Prelude.Nothing,
      matchOptions = Prelude.Nothing
    }

-- | The names of the metadata types that you can use to filter and group
-- your results. For example, @AZ@ returns a list of Availability Zones.
dimensionValues_key :: Lens.Lens' DimensionValues (Prelude.Maybe Dimension)
dimensionValues_key = Lens.lens (\DimensionValues' {key} -> key) (\s@DimensionValues' {} a -> s {key = a} :: DimensionValues)

-- | The metadata values that you can use to filter and group your results.
-- You can use @GetDimensionValues@ to find specific values.
dimensionValues_values :: Lens.Lens' DimensionValues (Prelude.Maybe [Prelude.Text])
dimensionValues_values = Lens.lens (\DimensionValues' {values} -> values) (\s@DimensionValues' {} a -> s {values = a} :: DimensionValues) Prelude.. Lens.mapping Prelude._Coerce

-- | The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
dimensionValues_matchOptions :: Lens.Lens' DimensionValues (Prelude.Maybe [MatchOption])
dimensionValues_matchOptions = Lens.lens (\DimensionValues' {matchOptions} -> matchOptions) (\s@DimensionValues' {} a -> s {matchOptions = a} :: DimensionValues) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON DimensionValues where
  parseJSON =
    Prelude.withObject
      "DimensionValues"
      ( \x ->
          DimensionValues'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "MatchOptions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DimensionValues

instance Prelude.NFData DimensionValues

instance Prelude.ToJSON DimensionValues where
  toJSON DimensionValues' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values,
            ("MatchOptions" Prelude..=)
              Prelude.<$> matchOptions
          ]
      )
