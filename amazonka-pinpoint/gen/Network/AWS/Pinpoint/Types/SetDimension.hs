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
-- Module      : Network.AWS.Pinpoint.Types.SetDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SetDimension where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DimensionType
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
setDimension_values = Lens.lens (\SetDimension' {values} -> values) (\s@SetDimension' {} a -> s {values = a} :: SetDimension) Prelude.. Prelude._Coerce

instance Prelude.FromJSON SetDimension where
  parseJSON =
    Prelude.withObject
      "SetDimension"
      ( \x ->
          SetDimension'
            Prelude.<$> (x Prelude..:? "DimensionType")
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable SetDimension

instance Prelude.NFData SetDimension

instance Prelude.ToJSON SetDimension where
  toJSON SetDimension' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DimensionType" Prelude..=)
              Prelude.<$> dimensionType,
            Prelude.Just ("Values" Prelude..= values)
          ]
      )
