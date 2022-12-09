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
-- Module      : Amazonka.Kendra.Types.ColumnConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ColumnConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Provides information about how Amazon Kendra should use the columns of a
-- database in an index.
--
-- /See:/ 'newColumnConfiguration' smart constructor.
data ColumnConfiguration = ColumnConfiguration'
  { -- | The column that contains the title of the document.
    documentTitleColumnName :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that map database column names to the corresponding
    -- fields in an index. You must first create the fields in the index using
    -- the @UpdateIndex@ API.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The column that provides the document\'s identifier.
    documentIdColumnName :: Prelude.Text,
    -- | The column that contains the contents of the document.
    documentDataColumnName :: Prelude.Text,
    -- | One to five columns that indicate when a document in the database has
    -- changed.
    changeDetectingColumns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentTitleColumnName', 'columnConfiguration_documentTitleColumnName' - The column that contains the title of the document.
--
-- 'fieldMappings', 'columnConfiguration_fieldMappings' - An array of objects that map database column names to the corresponding
-- fields in an index. You must first create the fields in the index using
-- the @UpdateIndex@ API.
--
-- 'documentIdColumnName', 'columnConfiguration_documentIdColumnName' - The column that provides the document\'s identifier.
--
-- 'documentDataColumnName', 'columnConfiguration_documentDataColumnName' - The column that contains the contents of the document.
--
-- 'changeDetectingColumns', 'columnConfiguration_changeDetectingColumns' - One to five columns that indicate when a document in the database has
-- changed.
newColumnConfiguration ::
  -- | 'documentIdColumnName'
  Prelude.Text ->
  -- | 'documentDataColumnName'
  Prelude.Text ->
  -- | 'changeDetectingColumns'
  Prelude.NonEmpty Prelude.Text ->
  ColumnConfiguration
newColumnConfiguration
  pDocumentIdColumnName_
  pDocumentDataColumnName_
  pChangeDetectingColumns_ =
    ColumnConfiguration'
      { documentTitleColumnName =
          Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        documentIdColumnName = pDocumentIdColumnName_,
        documentDataColumnName = pDocumentDataColumnName_,
        changeDetectingColumns =
          Lens.coerced Lens.# pChangeDetectingColumns_
      }

-- | The column that contains the title of the document.
columnConfiguration_documentTitleColumnName :: Lens.Lens' ColumnConfiguration (Prelude.Maybe Prelude.Text)
columnConfiguration_documentTitleColumnName = Lens.lens (\ColumnConfiguration' {documentTitleColumnName} -> documentTitleColumnName) (\s@ColumnConfiguration' {} a -> s {documentTitleColumnName = a} :: ColumnConfiguration)

-- | An array of objects that map database column names to the corresponding
-- fields in an index. You must first create the fields in the index using
-- the @UpdateIndex@ API.
columnConfiguration_fieldMappings :: Lens.Lens' ColumnConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
columnConfiguration_fieldMappings = Lens.lens (\ColumnConfiguration' {fieldMappings} -> fieldMappings) (\s@ColumnConfiguration' {} a -> s {fieldMappings = a} :: ColumnConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The column that provides the document\'s identifier.
columnConfiguration_documentIdColumnName :: Lens.Lens' ColumnConfiguration Prelude.Text
columnConfiguration_documentIdColumnName = Lens.lens (\ColumnConfiguration' {documentIdColumnName} -> documentIdColumnName) (\s@ColumnConfiguration' {} a -> s {documentIdColumnName = a} :: ColumnConfiguration)

-- | The column that contains the contents of the document.
columnConfiguration_documentDataColumnName :: Lens.Lens' ColumnConfiguration Prelude.Text
columnConfiguration_documentDataColumnName = Lens.lens (\ColumnConfiguration' {documentDataColumnName} -> documentDataColumnName) (\s@ColumnConfiguration' {} a -> s {documentDataColumnName = a} :: ColumnConfiguration)

-- | One to five columns that indicate when a document in the database has
-- changed.
columnConfiguration_changeDetectingColumns :: Lens.Lens' ColumnConfiguration (Prelude.NonEmpty Prelude.Text)
columnConfiguration_changeDetectingColumns = Lens.lens (\ColumnConfiguration' {changeDetectingColumns} -> changeDetectingColumns) (\s@ColumnConfiguration' {} a -> s {changeDetectingColumns = a} :: ColumnConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON ColumnConfiguration where
  parseJSON =
    Data.withObject
      "ColumnConfiguration"
      ( \x ->
          ColumnConfiguration'
            Prelude.<$> (x Data..:? "DocumentTitleColumnName")
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> (x Data..: "DocumentIdColumnName")
            Prelude.<*> (x Data..: "DocumentDataColumnName")
            Prelude.<*> (x Data..: "ChangeDetectingColumns")
      )

instance Prelude.Hashable ColumnConfiguration where
  hashWithSalt _salt ColumnConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` documentTitleColumnName
      `Prelude.hashWithSalt` fieldMappings
      `Prelude.hashWithSalt` documentIdColumnName
      `Prelude.hashWithSalt` documentDataColumnName
      `Prelude.hashWithSalt` changeDetectingColumns

instance Prelude.NFData ColumnConfiguration where
  rnf ColumnConfiguration' {..} =
    Prelude.rnf documentTitleColumnName
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf documentIdColumnName
      `Prelude.seq` Prelude.rnf documentDataColumnName
      `Prelude.seq` Prelude.rnf changeDetectingColumns

instance Data.ToJSON ColumnConfiguration where
  toJSON ColumnConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentTitleColumnName" Data..=)
              Prelude.<$> documentTitleColumnName,
            ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
            Prelude.Just
              ( "DocumentIdColumnName"
                  Data..= documentIdColumnName
              ),
            Prelude.Just
              ( "DocumentDataColumnName"
                  Data..= documentDataColumnName
              ),
            Prelude.Just
              ( "ChangeDetectingColumns"
                  Data..= changeDetectingColumns
              )
          ]
      )
