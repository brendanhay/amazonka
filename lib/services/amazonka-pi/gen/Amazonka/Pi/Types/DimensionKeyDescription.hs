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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.DimensionKeyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that includes the requested dimension key values and
-- aggregated metric values within a dimension group.
--
-- /See:/ 'newDimensionKeyDescription' smart constructor.
data DimensionKeyDescription = DimensionKeyDescription'
  { -- | The aggregated metric value for the dimensions, over the requested time
    -- range.
    total :: Prelude.Maybe Prelude.Double,
    -- | A map that contains the value for each additional metric.
    additionalMetrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | A map of name-value pairs for the dimensions in the group.
    dimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | If @PartitionBy@ was specified, @PartitionKeys@ contains the dimensions
    -- that were.
    partitions :: Prelude.Maybe [Prelude.Double]
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
-- 'total', 'dimensionKeyDescription_total' - The aggregated metric value for the dimensions, over the requested time
-- range.
--
-- 'additionalMetrics', 'dimensionKeyDescription_additionalMetrics' - A map that contains the value for each additional metric.
--
-- 'dimensions', 'dimensionKeyDescription_dimensions' - A map of name-value pairs for the dimensions in the group.
--
-- 'partitions', 'dimensionKeyDescription_partitions' - If @PartitionBy@ was specified, @PartitionKeys@ contains the dimensions
-- that were.
newDimensionKeyDescription ::
  DimensionKeyDescription
newDimensionKeyDescription =
  DimensionKeyDescription'
    { total = Prelude.Nothing,
      additionalMetrics = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      partitions = Prelude.Nothing
    }

-- | The aggregated metric value for the dimensions, over the requested time
-- range.
dimensionKeyDescription_total :: Lens.Lens' DimensionKeyDescription (Prelude.Maybe Prelude.Double)
dimensionKeyDescription_total = Lens.lens (\DimensionKeyDescription' {total} -> total) (\s@DimensionKeyDescription' {} a -> s {total = a} :: DimensionKeyDescription)

-- | A map that contains the value for each additional metric.
dimensionKeyDescription_additionalMetrics :: Lens.Lens' DimensionKeyDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
dimensionKeyDescription_additionalMetrics = Lens.lens (\DimensionKeyDescription' {additionalMetrics} -> additionalMetrics) (\s@DimensionKeyDescription' {} a -> s {additionalMetrics = a} :: DimensionKeyDescription) Prelude.. Lens.mapping Lens.coerced

-- | A map of name-value pairs for the dimensions in the group.
dimensionKeyDescription_dimensions :: Lens.Lens' DimensionKeyDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dimensionKeyDescription_dimensions = Lens.lens (\DimensionKeyDescription' {dimensions} -> dimensions) (\s@DimensionKeyDescription' {} a -> s {dimensions = a} :: DimensionKeyDescription) Prelude.. Lens.mapping Lens.coerced

-- | If @PartitionBy@ was specified, @PartitionKeys@ contains the dimensions
-- that were.
dimensionKeyDescription_partitions :: Lens.Lens' DimensionKeyDescription (Prelude.Maybe [Prelude.Double])
dimensionKeyDescription_partitions = Lens.lens (\DimensionKeyDescription' {partitions} -> partitions) (\s@DimensionKeyDescription' {} a -> s {partitions = a} :: DimensionKeyDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DimensionKeyDescription where
  parseJSON =
    Data.withObject
      "DimensionKeyDescription"
      ( \x ->
          DimensionKeyDescription'
            Prelude.<$> (x Data..:? "Total")
            Prelude.<*> ( x Data..:? "AdditionalMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Partitions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DimensionKeyDescription where
  hashWithSalt _salt DimensionKeyDescription' {..} =
    _salt `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` additionalMetrics
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` partitions

instance Prelude.NFData DimensionKeyDescription where
  rnf DimensionKeyDescription' {..} =
    Prelude.rnf total
      `Prelude.seq` Prelude.rnf additionalMetrics
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf partitions
