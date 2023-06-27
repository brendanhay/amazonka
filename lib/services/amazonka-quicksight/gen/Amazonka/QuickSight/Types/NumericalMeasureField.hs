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
-- Module      : Amazonka.QuickSight.Types.NumericalMeasureField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericalMeasureField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.NumberFormatConfiguration
import Amazonka.QuickSight.Types.NumericalAggregationFunction

-- | The measure type field with numerical type columns.
--
-- /See:/ 'newNumericalMeasureField' smart constructor.
data NumericalMeasureField = NumericalMeasureField'
  { -- | The aggregation function of the measure field.
    aggregationFunction :: Prelude.Maybe NumericalAggregationFunction,
    -- | The format configuration of the field.
    formatConfiguration :: Prelude.Maybe NumberFormatConfiguration,
    -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The column that is used in the @NumericalMeasureField@.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericalMeasureField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationFunction', 'numericalMeasureField_aggregationFunction' - The aggregation function of the measure field.
--
-- 'formatConfiguration', 'numericalMeasureField_formatConfiguration' - The format configuration of the field.
--
-- 'fieldId', 'numericalMeasureField_fieldId' - The custom field ID.
--
-- 'column', 'numericalMeasureField_column' - The column that is used in the @NumericalMeasureField@.
newNumericalMeasureField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  NumericalMeasureField
newNumericalMeasureField pFieldId_ pColumn_ =
  NumericalMeasureField'
    { aggregationFunction =
        Prelude.Nothing,
      formatConfiguration = Prelude.Nothing,
      fieldId = pFieldId_,
      column = pColumn_
    }

-- | The aggregation function of the measure field.
numericalMeasureField_aggregationFunction :: Lens.Lens' NumericalMeasureField (Prelude.Maybe NumericalAggregationFunction)
numericalMeasureField_aggregationFunction = Lens.lens (\NumericalMeasureField' {aggregationFunction} -> aggregationFunction) (\s@NumericalMeasureField' {} a -> s {aggregationFunction = a} :: NumericalMeasureField)

-- | The format configuration of the field.
numericalMeasureField_formatConfiguration :: Lens.Lens' NumericalMeasureField (Prelude.Maybe NumberFormatConfiguration)
numericalMeasureField_formatConfiguration = Lens.lens (\NumericalMeasureField' {formatConfiguration} -> formatConfiguration) (\s@NumericalMeasureField' {} a -> s {formatConfiguration = a} :: NumericalMeasureField)

-- | The custom field ID.
numericalMeasureField_fieldId :: Lens.Lens' NumericalMeasureField Prelude.Text
numericalMeasureField_fieldId = Lens.lens (\NumericalMeasureField' {fieldId} -> fieldId) (\s@NumericalMeasureField' {} a -> s {fieldId = a} :: NumericalMeasureField)

-- | The column that is used in the @NumericalMeasureField@.
numericalMeasureField_column :: Lens.Lens' NumericalMeasureField ColumnIdentifier
numericalMeasureField_column = Lens.lens (\NumericalMeasureField' {column} -> column) (\s@NumericalMeasureField' {} a -> s {column = a} :: NumericalMeasureField)

instance Data.FromJSON NumericalMeasureField where
  parseJSON =
    Data.withObject
      "NumericalMeasureField"
      ( \x ->
          NumericalMeasureField'
            Prelude.<$> (x Data..:? "AggregationFunction")
            Prelude.<*> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable NumericalMeasureField where
  hashWithSalt _salt NumericalMeasureField' {..} =
    _salt
      `Prelude.hashWithSalt` aggregationFunction
      `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData NumericalMeasureField where
  rnf NumericalMeasureField' {..} =
    Prelude.rnf aggregationFunction
      `Prelude.seq` Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON NumericalMeasureField where
  toJSON NumericalMeasureField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AggregationFunction" Data..=)
              Prelude.<$> aggregationFunction,
            ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration,
            Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Column" Data..= column)
          ]
      )
