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
-- Module      : Amazonka.QuickSight.Types.DateMeasureField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateMeasureField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.DateAggregationFunction
import Amazonka.QuickSight.Types.DateTimeFormatConfiguration

-- | The measure type field with date type columns.
--
-- /See:/ 'newDateMeasureField' smart constructor.
data DateMeasureField = DateMeasureField'
  { -- | The aggregation function of the measure field.
    aggregationFunction :: Prelude.Maybe DateAggregationFunction,
    -- | The format configuration of the field.
    formatConfiguration :: Prelude.Maybe DateTimeFormatConfiguration,
    -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The column that is used in the @DateMeasureField@.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateMeasureField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationFunction', 'dateMeasureField_aggregationFunction' - The aggregation function of the measure field.
--
-- 'formatConfiguration', 'dateMeasureField_formatConfiguration' - The format configuration of the field.
--
-- 'fieldId', 'dateMeasureField_fieldId' - The custom field ID.
--
-- 'column', 'dateMeasureField_column' - The column that is used in the @DateMeasureField@.
newDateMeasureField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  DateMeasureField
newDateMeasureField pFieldId_ pColumn_ =
  DateMeasureField'
    { aggregationFunction =
        Prelude.Nothing,
      formatConfiguration = Prelude.Nothing,
      fieldId = pFieldId_,
      column = pColumn_
    }

-- | The aggregation function of the measure field.
dateMeasureField_aggregationFunction :: Lens.Lens' DateMeasureField (Prelude.Maybe DateAggregationFunction)
dateMeasureField_aggregationFunction = Lens.lens (\DateMeasureField' {aggregationFunction} -> aggregationFunction) (\s@DateMeasureField' {} a -> s {aggregationFunction = a} :: DateMeasureField)

-- | The format configuration of the field.
dateMeasureField_formatConfiguration :: Lens.Lens' DateMeasureField (Prelude.Maybe DateTimeFormatConfiguration)
dateMeasureField_formatConfiguration = Lens.lens (\DateMeasureField' {formatConfiguration} -> formatConfiguration) (\s@DateMeasureField' {} a -> s {formatConfiguration = a} :: DateMeasureField)

-- | The custom field ID.
dateMeasureField_fieldId :: Lens.Lens' DateMeasureField Prelude.Text
dateMeasureField_fieldId = Lens.lens (\DateMeasureField' {fieldId} -> fieldId) (\s@DateMeasureField' {} a -> s {fieldId = a} :: DateMeasureField)

-- | The column that is used in the @DateMeasureField@.
dateMeasureField_column :: Lens.Lens' DateMeasureField ColumnIdentifier
dateMeasureField_column = Lens.lens (\DateMeasureField' {column} -> column) (\s@DateMeasureField' {} a -> s {column = a} :: DateMeasureField)

instance Data.FromJSON DateMeasureField where
  parseJSON =
    Data.withObject
      "DateMeasureField"
      ( \x ->
          DateMeasureField'
            Prelude.<$> (x Data..:? "AggregationFunction")
            Prelude.<*> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable DateMeasureField where
  hashWithSalt _salt DateMeasureField' {..} =
    _salt
      `Prelude.hashWithSalt` aggregationFunction
      `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData DateMeasureField where
  rnf DateMeasureField' {..} =
    Prelude.rnf aggregationFunction
      `Prelude.seq` Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON DateMeasureField where
  toJSON DateMeasureField' {..} =
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
