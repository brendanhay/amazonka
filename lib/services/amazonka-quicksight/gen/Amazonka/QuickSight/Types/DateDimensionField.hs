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
-- Module      : Amazonka.QuickSight.Types.DateDimensionField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateDimensionField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.DateTimeFormatConfiguration
import Amazonka.QuickSight.Types.TimeGranularity

-- | The dimension type field with date type columns.
--
-- /See:/ 'newDateDimensionField' smart constructor.
data DateDimensionField = DateDimensionField'
  { -- | The date granularity of the @DateDimensionField@. Choose one of the
    -- following options:
    --
    -- -   @YEAR@
    --
    -- -   @QUARTER@
    --
    -- -   @MONTH@
    --
    -- -   @WEEK@
    --
    -- -   @DAY@
    --
    -- -   @HOUR@
    --
    -- -   @MINUTE@
    --
    -- -   @SECOND@
    --
    -- -   @MILLISECOND@
    dateGranularity :: Prelude.Maybe TimeGranularity,
    -- | The format configuration of the field.
    formatConfiguration :: Prelude.Maybe DateTimeFormatConfiguration,
    -- | The custom hierarchy ID.
    hierarchyId :: Prelude.Maybe Prelude.Text,
    -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The column that is used in the @DateDimensionField@.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateDimensionField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateGranularity', 'dateDimensionField_dateGranularity' - The date granularity of the @DateDimensionField@. Choose one of the
-- following options:
--
-- -   @YEAR@
--
-- -   @QUARTER@
--
-- -   @MONTH@
--
-- -   @WEEK@
--
-- -   @DAY@
--
-- -   @HOUR@
--
-- -   @MINUTE@
--
-- -   @SECOND@
--
-- -   @MILLISECOND@
--
-- 'formatConfiguration', 'dateDimensionField_formatConfiguration' - The format configuration of the field.
--
-- 'hierarchyId', 'dateDimensionField_hierarchyId' - The custom hierarchy ID.
--
-- 'fieldId', 'dateDimensionField_fieldId' - The custom field ID.
--
-- 'column', 'dateDimensionField_column' - The column that is used in the @DateDimensionField@.
newDateDimensionField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  DateDimensionField
newDateDimensionField pFieldId_ pColumn_ =
  DateDimensionField'
    { dateGranularity =
        Prelude.Nothing,
      formatConfiguration = Prelude.Nothing,
      hierarchyId = Prelude.Nothing,
      fieldId = pFieldId_,
      column = pColumn_
    }

-- | The date granularity of the @DateDimensionField@. Choose one of the
-- following options:
--
-- -   @YEAR@
--
-- -   @QUARTER@
--
-- -   @MONTH@
--
-- -   @WEEK@
--
-- -   @DAY@
--
-- -   @HOUR@
--
-- -   @MINUTE@
--
-- -   @SECOND@
--
-- -   @MILLISECOND@
dateDimensionField_dateGranularity :: Lens.Lens' DateDimensionField (Prelude.Maybe TimeGranularity)
dateDimensionField_dateGranularity = Lens.lens (\DateDimensionField' {dateGranularity} -> dateGranularity) (\s@DateDimensionField' {} a -> s {dateGranularity = a} :: DateDimensionField)

-- | The format configuration of the field.
dateDimensionField_formatConfiguration :: Lens.Lens' DateDimensionField (Prelude.Maybe DateTimeFormatConfiguration)
dateDimensionField_formatConfiguration = Lens.lens (\DateDimensionField' {formatConfiguration} -> formatConfiguration) (\s@DateDimensionField' {} a -> s {formatConfiguration = a} :: DateDimensionField)

-- | The custom hierarchy ID.
dateDimensionField_hierarchyId :: Lens.Lens' DateDimensionField (Prelude.Maybe Prelude.Text)
dateDimensionField_hierarchyId = Lens.lens (\DateDimensionField' {hierarchyId} -> hierarchyId) (\s@DateDimensionField' {} a -> s {hierarchyId = a} :: DateDimensionField)

-- | The custom field ID.
dateDimensionField_fieldId :: Lens.Lens' DateDimensionField Prelude.Text
dateDimensionField_fieldId = Lens.lens (\DateDimensionField' {fieldId} -> fieldId) (\s@DateDimensionField' {} a -> s {fieldId = a} :: DateDimensionField)

-- | The column that is used in the @DateDimensionField@.
dateDimensionField_column :: Lens.Lens' DateDimensionField ColumnIdentifier
dateDimensionField_column = Lens.lens (\DateDimensionField' {column} -> column) (\s@DateDimensionField' {} a -> s {column = a} :: DateDimensionField)

instance Data.FromJSON DateDimensionField where
  parseJSON =
    Data.withObject
      "DateDimensionField"
      ( \x ->
          DateDimensionField'
            Prelude.<$> (x Data..:? "DateGranularity")
            Prelude.<*> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..:? "HierarchyId")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable DateDimensionField where
  hashWithSalt _salt DateDimensionField' {..} =
    _salt
      `Prelude.hashWithSalt` dateGranularity
      `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData DateDimensionField where
  rnf DateDimensionField' {..} =
    Prelude.rnf dateGranularity
      `Prelude.seq` Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf hierarchyId
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON DateDimensionField where
  toJSON DateDimensionField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateGranularity" Data..=)
              Prelude.<$> dateGranularity,
            ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration,
            ("HierarchyId" Data..=) Prelude.<$> hierarchyId,
            Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Column" Data..= column)
          ]
      )
