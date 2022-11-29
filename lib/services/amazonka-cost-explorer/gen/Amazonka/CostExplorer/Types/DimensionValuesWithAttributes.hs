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
-- Module      : Amazonka.CostExplorer.Types.DimensionValuesWithAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.DimensionValuesWithAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a specific type that you can use to filter and group
-- your results. You can use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'newDimensionValuesWithAttributes' smart constructor.
data DimensionValuesWithAttributes = DimensionValuesWithAttributes'
  { -- | The attribute that applies to a specific @Dimension@.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The value of a dimension with a specific attribute.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The attribute that applies to a specific @Dimension@.
dimensionValuesWithAttributes_attributes :: Lens.Lens' DimensionValuesWithAttributes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dimensionValuesWithAttributes_attributes = Lens.lens (\DimensionValuesWithAttributes' {attributes} -> attributes) (\s@DimensionValuesWithAttributes' {} a -> s {attributes = a} :: DimensionValuesWithAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The value of a dimension with a specific attribute.
dimensionValuesWithAttributes_value :: Lens.Lens' DimensionValuesWithAttributes (Prelude.Maybe Prelude.Text)
dimensionValuesWithAttributes_value = Lens.lens (\DimensionValuesWithAttributes' {value} -> value) (\s@DimensionValuesWithAttributes' {} a -> s {value = a} :: DimensionValuesWithAttributes)

instance Core.FromJSON DimensionValuesWithAttributes where
  parseJSON =
    Core.withObject
      "DimensionValuesWithAttributes"
      ( \x ->
          DimensionValuesWithAttributes'
            Prelude.<$> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Value")
      )

instance
  Prelude.Hashable
    DimensionValuesWithAttributes
  where
  hashWithSalt _salt DimensionValuesWithAttributes' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` value

instance Prelude.NFData DimensionValuesWithAttributes where
  rnf DimensionValuesWithAttributes' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf value
