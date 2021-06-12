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
-- Module      : Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The metadata of a specific type that you can use to filter and group
-- your results. You can use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'newDimensionValuesWithAttributes' smart constructor.
data DimensionValuesWithAttributes = DimensionValuesWithAttributes'
  { -- | The attribute that applies to a specific @Dimension@.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The value of a dimension with a specific attribute.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DimensionValuesWithAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'dimensionValuesWithAttributes_attributes' - The attribute that applies to a specific @Dimension@.
--
-- 'value', 'dimensionValuesWithAttributes_value' - The value of a dimension with a specific attribute.
newDimensionValuesWithAttributes ::
  DimensionValuesWithAttributes
newDimensionValuesWithAttributes =
  DimensionValuesWithAttributes'
    { attributes =
        Core.Nothing,
      value = Core.Nothing
    }

-- | The attribute that applies to a specific @Dimension@.
dimensionValuesWithAttributes_attributes :: Lens.Lens' DimensionValuesWithAttributes (Core.Maybe (Core.HashMap Core.Text Core.Text))
dimensionValuesWithAttributes_attributes = Lens.lens (\DimensionValuesWithAttributes' {attributes} -> attributes) (\s@DimensionValuesWithAttributes' {} a -> s {attributes = a} :: DimensionValuesWithAttributes) Core.. Lens.mapping Lens._Coerce

-- | The value of a dimension with a specific attribute.
dimensionValuesWithAttributes_value :: Lens.Lens' DimensionValuesWithAttributes (Core.Maybe Core.Text)
dimensionValuesWithAttributes_value = Lens.lens (\DimensionValuesWithAttributes' {value} -> value) (\s@DimensionValuesWithAttributes' {} a -> s {value = a} :: DimensionValuesWithAttributes)

instance Core.FromJSON DimensionValuesWithAttributes where
  parseJSON =
    Core.withObject
      "DimensionValuesWithAttributes"
      ( \x ->
          DimensionValuesWithAttributes'
            Core.<$> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable DimensionValuesWithAttributes

instance Core.NFData DimensionValuesWithAttributes
