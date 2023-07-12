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
-- Module      : Amazonka.QuickSight.Types.CategoricalDimensionField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CategoricalDimensionField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.StringFormatConfiguration

-- | The dimension type field with categorical type columns..
--
-- /See:/ 'newCategoricalDimensionField' smart constructor.
data CategoricalDimensionField = CategoricalDimensionField'
  { -- | The format configuration of the field.
    formatConfiguration :: Prelude.Maybe StringFormatConfiguration,
    -- | The custom hierarchy ID.
    hierarchyId :: Prelude.Maybe Prelude.Text,
    -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The column that is used in the @CategoricalDimensionField@.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoricalDimensionField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatConfiguration', 'categoricalDimensionField_formatConfiguration' - The format configuration of the field.
--
-- 'hierarchyId', 'categoricalDimensionField_hierarchyId' - The custom hierarchy ID.
--
-- 'fieldId', 'categoricalDimensionField_fieldId' - The custom field ID.
--
-- 'column', 'categoricalDimensionField_column' - The column that is used in the @CategoricalDimensionField@.
newCategoricalDimensionField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  CategoricalDimensionField
newCategoricalDimensionField pFieldId_ pColumn_ =
  CategoricalDimensionField'
    { formatConfiguration =
        Prelude.Nothing,
      hierarchyId = Prelude.Nothing,
      fieldId = pFieldId_,
      column = pColumn_
    }

-- | The format configuration of the field.
categoricalDimensionField_formatConfiguration :: Lens.Lens' CategoricalDimensionField (Prelude.Maybe StringFormatConfiguration)
categoricalDimensionField_formatConfiguration = Lens.lens (\CategoricalDimensionField' {formatConfiguration} -> formatConfiguration) (\s@CategoricalDimensionField' {} a -> s {formatConfiguration = a} :: CategoricalDimensionField)

-- | The custom hierarchy ID.
categoricalDimensionField_hierarchyId :: Lens.Lens' CategoricalDimensionField (Prelude.Maybe Prelude.Text)
categoricalDimensionField_hierarchyId = Lens.lens (\CategoricalDimensionField' {hierarchyId} -> hierarchyId) (\s@CategoricalDimensionField' {} a -> s {hierarchyId = a} :: CategoricalDimensionField)

-- | The custom field ID.
categoricalDimensionField_fieldId :: Lens.Lens' CategoricalDimensionField Prelude.Text
categoricalDimensionField_fieldId = Lens.lens (\CategoricalDimensionField' {fieldId} -> fieldId) (\s@CategoricalDimensionField' {} a -> s {fieldId = a} :: CategoricalDimensionField)

-- | The column that is used in the @CategoricalDimensionField@.
categoricalDimensionField_column :: Lens.Lens' CategoricalDimensionField ColumnIdentifier
categoricalDimensionField_column = Lens.lens (\CategoricalDimensionField' {column} -> column) (\s@CategoricalDimensionField' {} a -> s {column = a} :: CategoricalDimensionField)

instance Data.FromJSON CategoricalDimensionField where
  parseJSON =
    Data.withObject
      "CategoricalDimensionField"
      ( \x ->
          CategoricalDimensionField'
            Prelude.<$> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..:? "HierarchyId")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable CategoricalDimensionField where
  hashWithSalt _salt CategoricalDimensionField' {..} =
    _salt
      `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData CategoricalDimensionField where
  rnf CategoricalDimensionField' {..} =
    Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf hierarchyId
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON CategoricalDimensionField where
  toJSON CategoricalDimensionField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration,
            ("HierarchyId" Data..=) Prelude.<$> hierarchyId,
            Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Column" Data..= column)
          ]
      )
