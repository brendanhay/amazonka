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
-- Module      : Amazonka.QuickSight.Types.NumericalDimensionField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericalDimensionField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.NumberFormatConfiguration

-- | The dimension type field with numerical type columns.
--
-- /See:/ 'newNumericalDimensionField' smart constructor.
data NumericalDimensionField = NumericalDimensionField'
  { -- | The format configuration of the field.
    formatConfiguration :: Prelude.Maybe NumberFormatConfiguration,
    -- | The custom hierarchy ID.
    hierarchyId :: Prelude.Maybe Prelude.Text,
    -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The column that is used in the @NumericalDimensionField@.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericalDimensionField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatConfiguration', 'numericalDimensionField_formatConfiguration' - The format configuration of the field.
--
-- 'hierarchyId', 'numericalDimensionField_hierarchyId' - The custom hierarchy ID.
--
-- 'fieldId', 'numericalDimensionField_fieldId' - The custom field ID.
--
-- 'column', 'numericalDimensionField_column' - The column that is used in the @NumericalDimensionField@.
newNumericalDimensionField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  NumericalDimensionField
newNumericalDimensionField pFieldId_ pColumn_ =
  NumericalDimensionField'
    { formatConfiguration =
        Prelude.Nothing,
      hierarchyId = Prelude.Nothing,
      fieldId = pFieldId_,
      column = pColumn_
    }

-- | The format configuration of the field.
numericalDimensionField_formatConfiguration :: Lens.Lens' NumericalDimensionField (Prelude.Maybe NumberFormatConfiguration)
numericalDimensionField_formatConfiguration = Lens.lens (\NumericalDimensionField' {formatConfiguration} -> formatConfiguration) (\s@NumericalDimensionField' {} a -> s {formatConfiguration = a} :: NumericalDimensionField)

-- | The custom hierarchy ID.
numericalDimensionField_hierarchyId :: Lens.Lens' NumericalDimensionField (Prelude.Maybe Prelude.Text)
numericalDimensionField_hierarchyId = Lens.lens (\NumericalDimensionField' {hierarchyId} -> hierarchyId) (\s@NumericalDimensionField' {} a -> s {hierarchyId = a} :: NumericalDimensionField)

-- | The custom field ID.
numericalDimensionField_fieldId :: Lens.Lens' NumericalDimensionField Prelude.Text
numericalDimensionField_fieldId = Lens.lens (\NumericalDimensionField' {fieldId} -> fieldId) (\s@NumericalDimensionField' {} a -> s {fieldId = a} :: NumericalDimensionField)

-- | The column that is used in the @NumericalDimensionField@.
numericalDimensionField_column :: Lens.Lens' NumericalDimensionField ColumnIdentifier
numericalDimensionField_column = Lens.lens (\NumericalDimensionField' {column} -> column) (\s@NumericalDimensionField' {} a -> s {column = a} :: NumericalDimensionField)

instance Data.FromJSON NumericalDimensionField where
  parseJSON =
    Data.withObject
      "NumericalDimensionField"
      ( \x ->
          NumericalDimensionField'
            Prelude.<$> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..:? "HierarchyId")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable NumericalDimensionField where
  hashWithSalt _salt NumericalDimensionField' {..} =
    _salt `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData NumericalDimensionField where
  rnf NumericalDimensionField' {..} =
    Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf hierarchyId
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON NumericalDimensionField where
  toJSON NumericalDimensionField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration,
            ("HierarchyId" Data..=) Prelude.<$> hierarchyId,
            Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Column" Data..= column)
          ]
      )
