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
-- Module      : Amazonka.QuickSight.Types.DimensionField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DimensionField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoricalDimensionField
import Amazonka.QuickSight.Types.DateDimensionField
import Amazonka.QuickSight.Types.NumericalDimensionField

-- | The dimension type field.
--
-- /See:/ 'newDimensionField' smart constructor.
data DimensionField = DimensionField'
  { -- | The dimension type field with categorical type columns.
    categoricalDimensionField :: Prelude.Maybe CategoricalDimensionField,
    -- | The dimension type field with date type columns.
    dateDimensionField :: Prelude.Maybe DateDimensionField,
    -- | The dimension type field with numerical type columns.
    numericalDimensionField :: Prelude.Maybe NumericalDimensionField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalDimensionField', 'dimensionField_categoricalDimensionField' - The dimension type field with categorical type columns.
--
-- 'dateDimensionField', 'dimensionField_dateDimensionField' - The dimension type field with date type columns.
--
-- 'numericalDimensionField', 'dimensionField_numericalDimensionField' - The dimension type field with numerical type columns.
newDimensionField ::
  DimensionField
newDimensionField =
  DimensionField'
    { categoricalDimensionField =
        Prelude.Nothing,
      dateDimensionField = Prelude.Nothing,
      numericalDimensionField = Prelude.Nothing
    }

-- | The dimension type field with categorical type columns.
dimensionField_categoricalDimensionField :: Lens.Lens' DimensionField (Prelude.Maybe CategoricalDimensionField)
dimensionField_categoricalDimensionField = Lens.lens (\DimensionField' {categoricalDimensionField} -> categoricalDimensionField) (\s@DimensionField' {} a -> s {categoricalDimensionField = a} :: DimensionField)

-- | The dimension type field with date type columns.
dimensionField_dateDimensionField :: Lens.Lens' DimensionField (Prelude.Maybe DateDimensionField)
dimensionField_dateDimensionField = Lens.lens (\DimensionField' {dateDimensionField} -> dateDimensionField) (\s@DimensionField' {} a -> s {dateDimensionField = a} :: DimensionField)

-- | The dimension type field with numerical type columns.
dimensionField_numericalDimensionField :: Lens.Lens' DimensionField (Prelude.Maybe NumericalDimensionField)
dimensionField_numericalDimensionField = Lens.lens (\DimensionField' {numericalDimensionField} -> numericalDimensionField) (\s@DimensionField' {} a -> s {numericalDimensionField = a} :: DimensionField)

instance Data.FromJSON DimensionField where
  parseJSON =
    Data.withObject
      "DimensionField"
      ( \x ->
          DimensionField'
            Prelude.<$> (x Data..:? "CategoricalDimensionField")
            Prelude.<*> (x Data..:? "DateDimensionField")
            Prelude.<*> (x Data..:? "NumericalDimensionField")
      )

instance Prelude.Hashable DimensionField where
  hashWithSalt _salt DimensionField' {..} =
    _salt
      `Prelude.hashWithSalt` categoricalDimensionField
      `Prelude.hashWithSalt` dateDimensionField
      `Prelude.hashWithSalt` numericalDimensionField

instance Prelude.NFData DimensionField where
  rnf DimensionField' {..} =
    Prelude.rnf categoricalDimensionField `Prelude.seq`
      Prelude.rnf dateDimensionField `Prelude.seq`
        Prelude.rnf numericalDimensionField

instance Data.ToJSON DimensionField where
  toJSON DimensionField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoricalDimensionField" Data..=)
              Prelude.<$> categoricalDimensionField,
            ("DateDimensionField" Data..=)
              Prelude.<$> dateDimensionField,
            ("NumericalDimensionField" Data..=)
              Prelude.<$> numericalDimensionField
          ]
      )
