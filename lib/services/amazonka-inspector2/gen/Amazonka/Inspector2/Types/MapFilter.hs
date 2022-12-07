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
-- Module      : Amazonka.Inspector2.Types.MapFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.MapFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.MapComparison
import qualified Amazonka.Prelude as Prelude

-- | An object that describes details of a map filter.
--
-- /See:/ 'newMapFilter' smart constructor.
data MapFilter = MapFilter'
  { -- | The tag value used in the filter.
    value :: Prelude.Maybe Prelude.Text,
    -- | The operator to use when comparing values in the filter.
    comparison :: MapComparison,
    -- | The tag key used in the filter.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'mapFilter_value' - The tag value used in the filter.
--
-- 'comparison', 'mapFilter_comparison' - The operator to use when comparing values in the filter.
--
-- 'key', 'mapFilter_key' - The tag key used in the filter.
newMapFilter ::
  -- | 'comparison'
  MapComparison ->
  -- | 'key'
  Prelude.Text ->
  MapFilter
newMapFilter pComparison_ pKey_ =
  MapFilter'
    { value = Prelude.Nothing,
      comparison = pComparison_,
      key = pKey_
    }

-- | The tag value used in the filter.
mapFilter_value :: Lens.Lens' MapFilter (Prelude.Maybe Prelude.Text)
mapFilter_value = Lens.lens (\MapFilter' {value} -> value) (\s@MapFilter' {} a -> s {value = a} :: MapFilter)

-- | The operator to use when comparing values in the filter.
mapFilter_comparison :: Lens.Lens' MapFilter MapComparison
mapFilter_comparison = Lens.lens (\MapFilter' {comparison} -> comparison) (\s@MapFilter' {} a -> s {comparison = a} :: MapFilter)

-- | The tag key used in the filter.
mapFilter_key :: Lens.Lens' MapFilter Prelude.Text
mapFilter_key = Lens.lens (\MapFilter' {key} -> key) (\s@MapFilter' {} a -> s {key = a} :: MapFilter)

instance Data.FromJSON MapFilter where
  parseJSON =
    Data.withObject
      "MapFilter"
      ( \x ->
          MapFilter'
            Prelude.<$> (x Data..:? "value")
            Prelude.<*> (x Data..: "comparison")
            Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable MapFilter where
  hashWithSalt _salt MapFilter' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` key

instance Prelude.NFData MapFilter where
  rnf MapFilter' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf key

instance Data.ToJSON MapFilter where
  toJSON MapFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("value" Data..=) Prelude.<$> value,
            Prelude.Just ("comparison" Data..= comparison),
            Prelude.Just ("key" Data..= key)
          ]
      )
