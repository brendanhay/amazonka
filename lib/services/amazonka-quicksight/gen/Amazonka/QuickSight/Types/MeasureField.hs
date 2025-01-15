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
-- Module      : Amazonka.QuickSight.Types.MeasureField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MeasureField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CalculatedMeasureField
import Amazonka.QuickSight.Types.CategoricalMeasureField
import Amazonka.QuickSight.Types.DateMeasureField
import Amazonka.QuickSight.Types.NumericalMeasureField

-- | The measure (metric) type field.
--
-- /See:/ 'newMeasureField' smart constructor.
data MeasureField = MeasureField'
  { -- | The calculated measure field only used in pivot tables.
    calculatedMeasureField :: Prelude.Maybe CalculatedMeasureField,
    -- | The measure type field with categorical type columns.
    categoricalMeasureField :: Prelude.Maybe CategoricalMeasureField,
    -- | The measure type field with date type columns.
    dateMeasureField :: Prelude.Maybe DateMeasureField,
    -- | The measure type field with numerical type columns.
    numericalMeasureField :: Prelude.Maybe NumericalMeasureField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeasureField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculatedMeasureField', 'measureField_calculatedMeasureField' - The calculated measure field only used in pivot tables.
--
-- 'categoricalMeasureField', 'measureField_categoricalMeasureField' - The measure type field with categorical type columns.
--
-- 'dateMeasureField', 'measureField_dateMeasureField' - The measure type field with date type columns.
--
-- 'numericalMeasureField', 'measureField_numericalMeasureField' - The measure type field with numerical type columns.
newMeasureField ::
  MeasureField
newMeasureField =
  MeasureField'
    { calculatedMeasureField =
        Prelude.Nothing,
      categoricalMeasureField = Prelude.Nothing,
      dateMeasureField = Prelude.Nothing,
      numericalMeasureField = Prelude.Nothing
    }

-- | The calculated measure field only used in pivot tables.
measureField_calculatedMeasureField :: Lens.Lens' MeasureField (Prelude.Maybe CalculatedMeasureField)
measureField_calculatedMeasureField = Lens.lens (\MeasureField' {calculatedMeasureField} -> calculatedMeasureField) (\s@MeasureField' {} a -> s {calculatedMeasureField = a} :: MeasureField)

-- | The measure type field with categorical type columns.
measureField_categoricalMeasureField :: Lens.Lens' MeasureField (Prelude.Maybe CategoricalMeasureField)
measureField_categoricalMeasureField = Lens.lens (\MeasureField' {categoricalMeasureField} -> categoricalMeasureField) (\s@MeasureField' {} a -> s {categoricalMeasureField = a} :: MeasureField)

-- | The measure type field with date type columns.
measureField_dateMeasureField :: Lens.Lens' MeasureField (Prelude.Maybe DateMeasureField)
measureField_dateMeasureField = Lens.lens (\MeasureField' {dateMeasureField} -> dateMeasureField) (\s@MeasureField' {} a -> s {dateMeasureField = a} :: MeasureField)

-- | The measure type field with numerical type columns.
measureField_numericalMeasureField :: Lens.Lens' MeasureField (Prelude.Maybe NumericalMeasureField)
measureField_numericalMeasureField = Lens.lens (\MeasureField' {numericalMeasureField} -> numericalMeasureField) (\s@MeasureField' {} a -> s {numericalMeasureField = a} :: MeasureField)

instance Data.FromJSON MeasureField where
  parseJSON =
    Data.withObject
      "MeasureField"
      ( \x ->
          MeasureField'
            Prelude.<$> (x Data..:? "CalculatedMeasureField")
            Prelude.<*> (x Data..:? "CategoricalMeasureField")
            Prelude.<*> (x Data..:? "DateMeasureField")
            Prelude.<*> (x Data..:? "NumericalMeasureField")
      )

instance Prelude.Hashable MeasureField where
  hashWithSalt _salt MeasureField' {..} =
    _salt
      `Prelude.hashWithSalt` calculatedMeasureField
      `Prelude.hashWithSalt` categoricalMeasureField
      `Prelude.hashWithSalt` dateMeasureField
      `Prelude.hashWithSalt` numericalMeasureField

instance Prelude.NFData MeasureField where
  rnf MeasureField' {..} =
    Prelude.rnf calculatedMeasureField `Prelude.seq`
      Prelude.rnf categoricalMeasureField `Prelude.seq`
        Prelude.rnf dateMeasureField `Prelude.seq`
          Prelude.rnf numericalMeasureField

instance Data.ToJSON MeasureField where
  toJSON MeasureField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CalculatedMeasureField" Data..=)
              Prelude.<$> calculatedMeasureField,
            ("CategoricalMeasureField" Data..=)
              Prelude.<$> categoricalMeasureField,
            ("DateMeasureField" Data..=)
              Prelude.<$> dateMeasureField,
            ("NumericalMeasureField" Data..=)
              Prelude.<$> numericalMeasureField
          ]
      )
