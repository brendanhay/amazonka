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
-- Module      : Amazonka.Pi.Types.DimensionKeyDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.DimensionKeyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An array of descriptions and aggregated values for each dimension within
-- a dimension group.
--
-- /See:/ 'newDimensionKeyDescription' smart constructor.
data DimensionKeyDescription = DimensionKeyDescription'
  { -- | If @PartitionBy@ was specified, @PartitionKeys@ contains the dimensions
    -- that were.
    partitions :: Prelude.Maybe [Prelude.Double],
    -- | The aggregated metric value for the dimension(s), over the requested
    -- time range.
    total :: Prelude.Maybe Prelude.Double,
    -- | A map of name-value pairs for the dimensions in the group.
    dimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionKeyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitions', 'dimensionKeyDescription_partitions' - If @PartitionBy@ was specified, @PartitionKeys@ contains the dimensions
-- that were.
--
-- 'total', 'dimensionKeyDescription_total' - The aggregated metric value for the dimension(s), over the requested
-- time range.
--
-- 'dimensions', 'dimensionKeyDescription_dimensions' - A map of name-value pairs for the dimensions in the group.
newDimensionKeyDescription ::
  DimensionKeyDescription
newDimensionKeyDescription =
  DimensionKeyDescription'
    { partitions =
        Prelude.Nothing,
      total = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | If @PartitionBy@ was specified, @PartitionKeys@ contains the dimensions
-- that were.
dimensionKeyDescription_partitions :: Lens.Lens' DimensionKeyDescription (Prelude.Maybe [Prelude.Double])
dimensionKeyDescription_partitions = Lens.lens (\DimensionKeyDescription' {partitions} -> partitions) (\s@DimensionKeyDescription' {} a -> s {partitions = a} :: DimensionKeyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The aggregated metric value for the dimension(s), over the requested
-- time range.
dimensionKeyDescription_total :: Lens.Lens' DimensionKeyDescription (Prelude.Maybe Prelude.Double)
dimensionKeyDescription_total = Lens.lens (\DimensionKeyDescription' {total} -> total) (\s@DimensionKeyDescription' {} a -> s {total = a} :: DimensionKeyDescription)

-- | A map of name-value pairs for the dimensions in the group.
dimensionKeyDescription_dimensions :: Lens.Lens' DimensionKeyDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dimensionKeyDescription_dimensions = Lens.lens (\DimensionKeyDescription' {dimensions} -> dimensions) (\s@DimensionKeyDescription' {} a -> s {dimensions = a} :: DimensionKeyDescription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DimensionKeyDescription where
  parseJSON =
    Core.withObject
      "DimensionKeyDescription"
      ( \x ->
          DimensionKeyDescription'
            Prelude.<$> (x Core..:? "Partitions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Total")
            Prelude.<*> (x Core..:? "Dimensions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable DimensionKeyDescription where
  hashWithSalt _salt DimensionKeyDescription' {..} =
    _salt `Prelude.hashWithSalt` partitions
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData DimensionKeyDescription where
  rnf DimensionKeyDescription' {..} =
    Prelude.rnf partitions
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf dimensions
