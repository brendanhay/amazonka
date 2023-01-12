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
-- Module      : Amazonka.Kendra.Types.ConfluenceSpaceToIndexFieldMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceSpaceToIndexFieldMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluenceSpaceFieldName
import qualified Amazonka.Prelude as Prelude

-- | >Maps attributes or field names of Confluence spaces to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- /See:/ 'newConfluenceSpaceToIndexFieldMapping' smart constructor.
data ConfluenceSpaceToIndexFieldMapping = ConfluenceSpaceToIndexFieldMapping'
  { -- | The name of the field in the data source.
    dataSourceFieldName :: Prelude.Maybe ConfluenceSpaceFieldName,
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
-- Create a value of 'ConfluenceSpaceToIndexFieldMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceFieldName', 'confluenceSpaceToIndexFieldMapping_dataSourceFieldName' - The name of the field in the data source.
--
-- 'dateFieldFormat', 'confluenceSpaceToIndexFieldMapping_dateFieldFormat' - The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
--
-- 'indexFieldName', 'confluenceSpaceToIndexFieldMapping_indexFieldName' - The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
newConfluenceSpaceToIndexFieldMapping ::
  ConfluenceSpaceToIndexFieldMapping
newConfluenceSpaceToIndexFieldMapping =
  ConfluenceSpaceToIndexFieldMapping'
    { dataSourceFieldName =
        Prelude.Nothing,
      dateFieldFormat = Prelude.Nothing,
      indexFieldName = Prelude.Nothing
    }

-- | The name of the field in the data source.
confluenceSpaceToIndexFieldMapping_dataSourceFieldName :: Lens.Lens' ConfluenceSpaceToIndexFieldMapping (Prelude.Maybe ConfluenceSpaceFieldName)
confluenceSpaceToIndexFieldMapping_dataSourceFieldName = Lens.lens (\ConfluenceSpaceToIndexFieldMapping' {dataSourceFieldName} -> dataSourceFieldName) (\s@ConfluenceSpaceToIndexFieldMapping' {} a -> s {dataSourceFieldName = a} :: ConfluenceSpaceToIndexFieldMapping)

-- | The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
confluenceSpaceToIndexFieldMapping_dateFieldFormat :: Lens.Lens' ConfluenceSpaceToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceSpaceToIndexFieldMapping_dateFieldFormat = Lens.lens (\ConfluenceSpaceToIndexFieldMapping' {dateFieldFormat} -> dateFieldFormat) (\s@ConfluenceSpaceToIndexFieldMapping' {} a -> s {dateFieldFormat = a} :: ConfluenceSpaceToIndexFieldMapping)

-- | The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
confluenceSpaceToIndexFieldMapping_indexFieldName :: Lens.Lens' ConfluenceSpaceToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceSpaceToIndexFieldMapping_indexFieldName = Lens.lens (\ConfluenceSpaceToIndexFieldMapping' {indexFieldName} -> indexFieldName) (\s@ConfluenceSpaceToIndexFieldMapping' {} a -> s {indexFieldName = a} :: ConfluenceSpaceToIndexFieldMapping)

instance
  Data.FromJSON
    ConfluenceSpaceToIndexFieldMapping
  where
  parseJSON =
    Data.withObject
      "ConfluenceSpaceToIndexFieldMapping"
      ( \x ->
          ConfluenceSpaceToIndexFieldMapping'
            Prelude.<$> (x Data..:? "DataSourceFieldName")
            Prelude.<*> (x Data..:? "DateFieldFormat")
            Prelude.<*> (x Data..:? "IndexFieldName")
      )

instance
  Prelude.Hashable
    ConfluenceSpaceToIndexFieldMapping
  where
  hashWithSalt
    _salt
    ConfluenceSpaceToIndexFieldMapping' {..} =
      _salt `Prelude.hashWithSalt` dataSourceFieldName
        `Prelude.hashWithSalt` dateFieldFormat
        `Prelude.hashWithSalt` indexFieldName

instance
  Prelude.NFData
    ConfluenceSpaceToIndexFieldMapping
  where
  rnf ConfluenceSpaceToIndexFieldMapping' {..} =
    Prelude.rnf dataSourceFieldName
      `Prelude.seq` Prelude.rnf dateFieldFormat
      `Prelude.seq` Prelude.rnf indexFieldName

instance
  Data.ToJSON
    ConfluenceSpaceToIndexFieldMapping
  where
  toJSON ConfluenceSpaceToIndexFieldMapping' {..} =
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
