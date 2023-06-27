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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure representing the filters supported by a
-- RasterDataCollection.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The maximum value of the filter.
    maximum :: Prelude.Maybe Prelude.Double,
    -- | The minimum value of the filter.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | The name of the filter.
    name :: Prelude.Text,
    -- | The type of the filter being used.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximum', 'filter_maximum' - The maximum value of the filter.
--
-- 'minimum', 'filter_minimum' - The minimum value of the filter.
--
-- 'name', 'filter_name' - The name of the filter.
--
-- 'type'', 'filter_type' - The type of the filter being used.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  Filter
newFilter pName_ pType_ =
  Filter'
    { maximum = Prelude.Nothing,
      minimum = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The maximum value of the filter.
filter_maximum :: Lens.Lens' Filter (Prelude.Maybe Prelude.Double)
filter_maximum = Lens.lens (\Filter' {maximum} -> maximum) (\s@Filter' {} a -> s {maximum = a} :: Filter)

-- | The minimum value of the filter.
filter_minimum :: Lens.Lens' Filter (Prelude.Maybe Prelude.Double)
filter_minimum = Lens.lens (\Filter' {minimum} -> minimum) (\s@Filter' {} a -> s {minimum = a} :: Filter)

-- | The name of the filter.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The type of the filter being used.
filter_type :: Lens.Lens' Filter Prelude.Text
filter_type = Lens.lens (\Filter' {type'} -> type') (\s@Filter' {} a -> s {type' = a} :: Filter)

instance Data.FromJSON Filter where
  parseJSON =
    Data.withObject
      "Filter"
      ( \x ->
          Filter'
            Prelude.<$> (x Data..:? "Maximum")
            Prelude.<*> (x Data..:? "Minimum")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf maximum
      `Prelude.seq` Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
