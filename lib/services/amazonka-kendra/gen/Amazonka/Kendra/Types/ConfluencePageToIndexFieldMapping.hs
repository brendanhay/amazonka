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
-- Module      : Amazonka.Kendra.Types.ConfluencePageToIndexFieldMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluencePageToIndexFieldMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluencePageFieldName
import qualified Amazonka.Prelude as Prelude

-- | >Maps attributes or field names of Confluence pages to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- /See:/ 'newConfluencePageToIndexFieldMapping' smart constructor.
data ConfluencePageToIndexFieldMapping = ConfluencePageToIndexFieldMapping'
  { -- | The name of the field in the data source.
    dataSourceFieldName :: Prelude.Maybe ConfluencePageFieldName,
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
-- Create a value of 'ConfluencePageToIndexFieldMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceFieldName', 'confluencePageToIndexFieldMapping_dataSourceFieldName' - The name of the field in the data source.
--
-- 'dateFieldFormat', 'confluencePageToIndexFieldMapping_dateFieldFormat' - The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
--
-- 'indexFieldName', 'confluencePageToIndexFieldMapping_indexFieldName' - The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
newConfluencePageToIndexFieldMapping ::
  ConfluencePageToIndexFieldMapping
newConfluencePageToIndexFieldMapping =
  ConfluencePageToIndexFieldMapping'
    { dataSourceFieldName =
        Prelude.Nothing,
      dateFieldFormat = Prelude.Nothing,
      indexFieldName = Prelude.Nothing
    }

-- | The name of the field in the data source.
confluencePageToIndexFieldMapping_dataSourceFieldName :: Lens.Lens' ConfluencePageToIndexFieldMapping (Prelude.Maybe ConfluencePageFieldName)
confluencePageToIndexFieldMapping_dataSourceFieldName = Lens.lens (\ConfluencePageToIndexFieldMapping' {dataSourceFieldName} -> dataSourceFieldName) (\s@ConfluencePageToIndexFieldMapping' {} a -> s {dataSourceFieldName = a} :: ConfluencePageToIndexFieldMapping)

-- | The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
confluencePageToIndexFieldMapping_dateFieldFormat :: Lens.Lens' ConfluencePageToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluencePageToIndexFieldMapping_dateFieldFormat = Lens.lens (\ConfluencePageToIndexFieldMapping' {dateFieldFormat} -> dateFieldFormat) (\s@ConfluencePageToIndexFieldMapping' {} a -> s {dateFieldFormat = a} :: ConfluencePageToIndexFieldMapping)

-- | The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
confluencePageToIndexFieldMapping_indexFieldName :: Lens.Lens' ConfluencePageToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluencePageToIndexFieldMapping_indexFieldName = Lens.lens (\ConfluencePageToIndexFieldMapping' {indexFieldName} -> indexFieldName) (\s@ConfluencePageToIndexFieldMapping' {} a -> s {indexFieldName = a} :: ConfluencePageToIndexFieldMapping)

instance
  Data.FromJSON
    ConfluencePageToIndexFieldMapping
  where
  parseJSON =
    Data.withObject
      "ConfluencePageToIndexFieldMapping"
      ( \x ->
          ConfluencePageToIndexFieldMapping'
            Prelude.<$> (x Data..:? "DataSourceFieldName")
            Prelude.<*> (x Data..:? "DateFieldFormat")
            Prelude.<*> (x Data..:? "IndexFieldName")
      )

instance
  Prelude.Hashable
    ConfluencePageToIndexFieldMapping
  where
  hashWithSalt
    _salt
    ConfluencePageToIndexFieldMapping' {..} =
      _salt
        `Prelude.hashWithSalt` dataSourceFieldName
        `Prelude.hashWithSalt` dateFieldFormat
        `Prelude.hashWithSalt` indexFieldName

instance
  Prelude.NFData
    ConfluencePageToIndexFieldMapping
  where
  rnf ConfluencePageToIndexFieldMapping' {..} =
    Prelude.rnf dataSourceFieldName
      `Prelude.seq` Prelude.rnf dateFieldFormat
      `Prelude.seq` Prelude.rnf indexFieldName

instance
  Data.ToJSON
    ConfluencePageToIndexFieldMapping
  where
  toJSON ConfluencePageToIndexFieldMapping' {..} =
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
