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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DimensionType

-- | Specifies the dimension type and values for a segment dimension.
--
-- /See:/ 'newSetDimension' smart constructor.
data SetDimension = SetDimension'
  { -- | The type of segment dimension to use. Valid values are: INCLUSIVE,
    -- endpoints that match the criteria are included in the segment; and,
    -- EXCLUSIVE, endpoints that match the criteria are excluded from the
    -- segment.
    dimensionType :: Core.Maybe DimensionType,
    -- | The criteria values to use for the segment dimension. Depending on the
    -- value of the DimensionType property, endpoints are included or excluded
    -- from the segment if their values match the criteria values.
    values :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { dimensionType = Core.Nothing,
      values = Core.mempty
    }

-- | The type of segment dimension to use. Valid values are: INCLUSIVE,
-- endpoints that match the criteria are included in the segment; and,
-- EXCLUSIVE, endpoints that match the criteria are excluded from the
-- segment.
setDimension_dimensionType :: Lens.Lens' SetDimension (Core.Maybe DimensionType)
setDimension_dimensionType = Lens.lens (\SetDimension' {dimensionType} -> dimensionType) (\s@SetDimension' {} a -> s {dimensionType = a} :: SetDimension)

-- | The criteria values to use for the segment dimension. Depending on the
-- value of the DimensionType property, endpoints are included or excluded
-- from the segment if their values match the criteria values.
setDimension_values :: Lens.Lens' SetDimension [Core.Text]
setDimension_values = Lens.lens (\SetDimension' {values} -> values) (\s@SetDimension' {} a -> s {values = a} :: SetDimension) Core.. Lens._Coerce

instance Core.FromJSON SetDimension where
  parseJSON =
    Core.withObject
      "SetDimension"
      ( \x ->
          SetDimension'
            Core.<$> (x Core..:? "DimensionType")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
      )

instance Core.Hashable SetDimension

instance Core.NFData SetDimension

instance Core.ToJSON SetDimension where
  toJSON SetDimension' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DimensionType" Core..=) Core.<$> dimensionType,
            Core.Just ("Values" Core..= values)
          ]
      )
