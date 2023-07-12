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
-- Module      : Amazonka.Kendra.Types.ConfluenceBlogToIndexFieldMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceBlogToIndexFieldMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluenceBlogFieldName
import qualified Amazonka.Prelude as Prelude

-- | Maps attributes or field names of Confluence blog to Amazon Kendra index
-- field names. To create custom fields, use the @UpdateIndex@ API before
-- you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- /See:/ 'newConfluenceBlogToIndexFieldMapping' smart constructor.
data ConfluenceBlogToIndexFieldMapping = ConfluenceBlogToIndexFieldMapping'
  { -- | The name of the field in the data source.
    dataSourceFieldName :: Prelude.Maybe ConfluenceBlogFieldName,
    -- | The format for date fields in the data source. If the field specified in
    -- @DataSourceFieldName@ is a date field you must specify the date format.
    -- If the field is not a date field, an exception is thrown.
    dateFieldFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the index field to map to the Confluence data source field.
    -- The index field type must match the Confluence field type.
    indexFieldName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfluenceBlogToIndexFieldMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceFieldName', 'confluenceBlogToIndexFieldMapping_dataSourceFieldName' - The name of the field in the data source.
--
-- 'dateFieldFormat', 'confluenceBlogToIndexFieldMapping_dateFieldFormat' - The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
--
-- 'indexFieldName', 'confluenceBlogToIndexFieldMapping_indexFieldName' - The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
newConfluenceBlogToIndexFieldMapping ::
  ConfluenceBlogToIndexFieldMapping
newConfluenceBlogToIndexFieldMapping =
  ConfluenceBlogToIndexFieldMapping'
    { dataSourceFieldName =
        Prelude.Nothing,
      dateFieldFormat = Prelude.Nothing,
      indexFieldName = Prelude.Nothing
    }

-- | The name of the field in the data source.
confluenceBlogToIndexFieldMapping_dataSourceFieldName :: Lens.Lens' ConfluenceBlogToIndexFieldMapping (Prelude.Maybe ConfluenceBlogFieldName)
confluenceBlogToIndexFieldMapping_dataSourceFieldName = Lens.lens (\ConfluenceBlogToIndexFieldMapping' {dataSourceFieldName} -> dataSourceFieldName) (\s@ConfluenceBlogToIndexFieldMapping' {} a -> s {dataSourceFieldName = a} :: ConfluenceBlogToIndexFieldMapping)

-- | The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
confluenceBlogToIndexFieldMapping_dateFieldFormat :: Lens.Lens' ConfluenceBlogToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceBlogToIndexFieldMapping_dateFieldFormat = Lens.lens (\ConfluenceBlogToIndexFieldMapping' {dateFieldFormat} -> dateFieldFormat) (\s@ConfluenceBlogToIndexFieldMapping' {} a -> s {dateFieldFormat = a} :: ConfluenceBlogToIndexFieldMapping)

-- | The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
confluenceBlogToIndexFieldMapping_indexFieldName :: Lens.Lens' ConfluenceBlogToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceBlogToIndexFieldMapping_indexFieldName = Lens.lens (\ConfluenceBlogToIndexFieldMapping' {indexFieldName} -> indexFieldName) (\s@ConfluenceBlogToIndexFieldMapping' {} a -> s {indexFieldName = a} :: ConfluenceBlogToIndexFieldMapping)

instance
  Data.FromJSON
    ConfluenceBlogToIndexFieldMapping
  where
  parseJSON =
    Data.withObject
      "ConfluenceBlogToIndexFieldMapping"
      ( \x ->
          ConfluenceBlogToIndexFieldMapping'
            Prelude.<$> (x Data..:? "DataSourceFieldName")
            Prelude.<*> (x Data..:? "DateFieldFormat")
            Prelude.<*> (x Data..:? "IndexFieldName")
      )

instance
  Prelude.Hashable
    ConfluenceBlogToIndexFieldMapping
  where
  hashWithSalt
    _salt
    ConfluenceBlogToIndexFieldMapping' {..} =
      _salt
        `Prelude.hashWithSalt` dataSourceFieldName
        `Prelude.hashWithSalt` dateFieldFormat
        `Prelude.hashWithSalt` indexFieldName

instance
  Prelude.NFData
    ConfluenceBlogToIndexFieldMapping
  where
  rnf ConfluenceBlogToIndexFieldMapping' {..} =
    Prelude.rnf dataSourceFieldName
      `Prelude.seq` Prelude.rnf dateFieldFormat
      `Prelude.seq` Prelude.rnf indexFieldName

instance
  Data.ToJSON
    ConfluenceBlogToIndexFieldMapping
  where
  toJSON ConfluenceBlogToIndexFieldMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceFieldName" Data..=)
              Prelude.<$> dataSourceFieldName,
            ("DateFieldFormat" Data..=)
              Prelude.<$> dateFieldFormat,
            ("IndexFieldName" Data..=)
              Prelude.<$> indexFieldName
          ]
      )
