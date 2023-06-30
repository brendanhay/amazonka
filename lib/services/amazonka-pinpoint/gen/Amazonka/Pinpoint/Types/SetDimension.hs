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
-- Module      : Amazonka.Pinpoint.Types.SetDimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SetDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.DimensionType
import qualified Amazonka.Prelude as Prelude

-- | Specifies the dimension type and values for a segment dimension.
--
-- /See:/ 'newSetDimension' smart constructor.
data SetDimension = SetDimension'
  { -- | The type of segment dimension to use. Valid values are: INCLUSIVE,
    -- endpoints that match the criteria are included in the segment; and,
    -- EXCLUSIVE, endpoints that match the criteria are excluded from the
    -- segment.
    dimensionType :: Prelude.Maybe DimensionType,
    -- | The criteria values to use for the segment dimension. Depending on the
    -- value of the DimensionType property, endpoints are included or excluded
    -- from the segment if their values match the criteria values.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionType', 'setDimension_dimensionType' - The type of segment dimension to use. Valid values are: INCLUSIVE,
-- endpoints that match the criteria are included in the segment; and,
-- EXCLUSIVE, endpoints that match the criteria are excluded from the
-- segment.
--
-- 'values', 'setDimension_values' - The criteria values to use for the segment dimension. Depending on the
-- value of the DimensionType property, endpoints are included or excluded
-- from the segment if their values match the criteria values.
newSetDimension ::
  SetDimension
newSetDimension =
  SetDimension'
    { dimensionType = Prelude.Nothing,
      values = Prelude.mempty
    }

-- | The type of segment dimension to use. Valid values are: INCLUSIVE,
-- endpoints that match the criteria are included in the segment; and,
-- EXCLUSIVE, endpoints that match the criteria are excluded from the
-- segment.
setDimension_dimensionType :: Lens.Lens' SetDimension (Prelude.Maybe DimensionType)
setDimension_dimensionType = Lens.lens (\SetDimension' {dimensionType} -> dimensionType) (\s@SetDimension' {} a -> s {dimensionType = a} :: SetDimension)

-- | The criteria values to use for the segment dimension. Depending on the
-- value of the DimensionType property, endpoints are included or excluded
-- from the segment if their values match the criteria values.
setDimension_values :: Lens.Lens' SetDimension [Prelude.Text]
setDimension_values = Lens.lens (\SetDimension' {values} -> values) (\s@SetDimension' {} a -> s {values = a} :: SetDimension) Prelude.. Lens.coerced

instance Data.FromJSON SetDimension where
  parseJSON =
    Data.withObject
      "SetDimension"
      ( \x ->
          SetDimension'
            Prelude.<$> (x Data..:? "DimensionType")
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SetDimension where
  hashWithSalt _salt SetDimension' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionType
      `Prelude.hashWithSalt` values

instance Prelude.NFData SetDimension where
  rnf SetDimension' {..} =
    Prelude.rnf dimensionType
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SetDimension where
  toJSON SetDimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DimensionType" Data..=) Prelude.<$> dimensionType,
            Prelude.Just ("Values" Data..= values)
          ]
      )
