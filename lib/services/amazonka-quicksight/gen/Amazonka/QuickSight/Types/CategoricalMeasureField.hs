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
-- Module      : Amazonka.QuickSight.Types.CategoricalMeasureField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CategoricalMeasureField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoricalAggregationFunction
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.StringFormatConfiguration

-- | The measure type field with categorical type columns.
--
-- /See:/ 'newCategoricalMeasureField' smart constructor.
data CategoricalMeasureField = CategoricalMeasureField'
  { -- | The aggregation function of the measure field.
    aggregationFunction :: Prelude.Maybe CategoricalAggregationFunction,
    -- | The format configuration of the field.
    formatConfiguration :: Prelude.Maybe StringFormatConfiguration,
    -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The column that is used in the @CategoricalMeasureField@.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoricalMeasureField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationFunction', 'categoricalMeasureField_aggregationFunction' - The aggregation function of the measure field.
--
-- 'formatConfiguration', 'categoricalMeasureField_formatConfiguration' - The format configuration of the field.
--
-- 'fieldId', 'categoricalMeasureField_fieldId' - The custom field ID.
--
-- 'column', 'categoricalMeasureField_column' - The column that is used in the @CategoricalMeasureField@.
newCategoricalMeasureField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  CategoricalMeasureField
newCategoricalMeasureField pFieldId_ pColumn_ =
  CategoricalMeasureField'
    { aggregationFunction =
        Prelude.Nothing,
      formatConfiguration = Prelude.Nothing,
      fieldId = pFieldId_,
      column = pColumn_
    }

-- | The aggregation function of the measure field.
categoricalMeasureField_aggregationFunction :: Lens.Lens' CategoricalMeasureField (Prelude.Maybe CategoricalAggregationFunction)
categoricalMeasureField_aggregationFunction = Lens.lens (\CategoricalMeasureField' {aggregationFunction} -> aggregationFunction) (\s@CategoricalMeasureField' {} a -> s {aggregationFunction = a} :: CategoricalMeasureField)

-- | The format configuration of the field.
categoricalMeasureField_formatConfiguration :: Lens.Lens' CategoricalMeasureField (Prelude.Maybe StringFormatConfiguration)
categoricalMeasureField_formatConfiguration = Lens.lens (\CategoricalMeasureField' {formatConfiguration} -> formatConfiguration) (\s@CategoricalMeasureField' {} a -> s {formatConfiguration = a} :: CategoricalMeasureField)

-- | The custom field ID.
categoricalMeasureField_fieldId :: Lens.Lens' CategoricalMeasureField Prelude.Text
categoricalMeasureField_fieldId = Lens.lens (\CategoricalMeasureField' {fieldId} -> fieldId) (\s@CategoricalMeasureField' {} a -> s {fieldId = a} :: CategoricalMeasureField)

-- | The column that is used in the @CategoricalMeasureField@.
categoricalMeasureField_column :: Lens.Lens' CategoricalMeasureField ColumnIdentifier
categoricalMeasureField_column = Lens.lens (\CategoricalMeasureField' {column} -> column) (\s@CategoricalMeasureField' {} a -> s {column = a} :: CategoricalMeasureField)

instance Data.FromJSON CategoricalMeasureField where
  parseJSON =
    Data.withObject
      "CategoricalMeasureField"
      ( \x ->
          CategoricalMeasureField'
            Prelude.<$> (x Data..:? "AggregationFunction")
            Prelude.<*> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable CategoricalMeasureField where
  hashWithSalt _salt CategoricalMeasureField' {..} =
    _salt
      `Prelude.hashWithSalt` aggregationFunction
      `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData CategoricalMeasureField where
  rnf CategoricalMeasureField' {..} =
    Prelude.rnf aggregationFunction
      `Prelude.seq` Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON CategoricalMeasureField where
  toJSON CategoricalMeasureField' {..} =
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
