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
-- Module      : Amazonka.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluenceAttachmentFieldName
import qualified Amazonka.Prelude as Prelude

-- | Maps attributes or field names of Confluence attachments to Amazon
-- Kendra index field names. To create custom fields, use the @UpdateIndex@
-- API before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confuence data source field names must exist in your Confluence
-- custom metadata.
--
-- /See:/ 'newConfluenceAttachmentToIndexFieldMapping' smart constructor.
data ConfluenceAttachmentToIndexFieldMapping = ConfluenceAttachmentToIndexFieldMapping'
  { -- | The name of the field in the data source.
    --
    -- You must first create the index field using the @UpdateIndex@ API.
    dataSourceFieldName :: Prelude.Maybe ConfluenceAttachmentFieldName,
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
-- Create a value of 'ConfluenceAttachmentToIndexFieldMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceFieldName', 'confluenceAttachmentToIndexFieldMapping_dataSourceFieldName' - The name of the field in the data source.
--
-- You must first create the index field using the @UpdateIndex@ API.
--
-- 'dateFieldFormat', 'confluenceAttachmentToIndexFieldMapping_dateFieldFormat' - The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
--
-- 'indexFieldName', 'confluenceAttachmentToIndexFieldMapping_indexFieldName' - The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
newConfluenceAttachmentToIndexFieldMapping ::
  ConfluenceAttachmentToIndexFieldMapping
newConfluenceAttachmentToIndexFieldMapping =
  ConfluenceAttachmentToIndexFieldMapping'
    { dataSourceFieldName =
        Prelude.Nothing,
      dateFieldFormat = Prelude.Nothing,
      indexFieldName = Prelude.Nothing
    }

-- | The name of the field in the data source.
--
-- You must first create the index field using the @UpdateIndex@ API.
confluenceAttachmentToIndexFieldMapping_dataSourceFieldName :: Lens.Lens' ConfluenceAttachmentToIndexFieldMapping (Prelude.Maybe ConfluenceAttachmentFieldName)
confluenceAttachmentToIndexFieldMapping_dataSourceFieldName = Lens.lens (\ConfluenceAttachmentToIndexFieldMapping' {dataSourceFieldName} -> dataSourceFieldName) (\s@ConfluenceAttachmentToIndexFieldMapping' {} a -> s {dataSourceFieldName = a} :: ConfluenceAttachmentToIndexFieldMapping)

-- | The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
confluenceAttachmentToIndexFieldMapping_dateFieldFormat :: Lens.Lens' ConfluenceAttachmentToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceAttachmentToIndexFieldMapping_dateFieldFormat = Lens.lens (\ConfluenceAttachmentToIndexFieldMapping' {dateFieldFormat} -> dateFieldFormat) (\s@ConfluenceAttachmentToIndexFieldMapping' {} a -> s {dateFieldFormat = a} :: ConfluenceAttachmentToIndexFieldMapping)

-- | The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
confluenceAttachmentToIndexFieldMapping_indexFieldName :: Lens.Lens' ConfluenceAttachmentToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceAttachmentToIndexFieldMapping_indexFieldName = Lens.lens (\ConfluenceAttachmentToIndexFieldMapping' {indexFieldName} -> indexFieldName) (\s@ConfluenceAttachmentToIndexFieldMapping' {} a -> s {indexFieldName = a} :: ConfluenceAttachmentToIndexFieldMapping)

instance
  Data.FromJSON
    ConfluenceAttachmentToIndexFieldMapping
  where
  parseJSON =
    Data.withObject
      "ConfluenceAttachmentToIndexFieldMapping"
      ( \x ->
          ConfluenceAttachmentToIndexFieldMapping'
            Prelude.<$> (x Data..:? "DataSourceFieldName")
            Prelude.<*> (x Data..:? "DateFieldFormat")
            Prelude.<*> (x Data..:? "IndexFieldName")
      )

instance
  Prelude.Hashable
    ConfluenceAttachmentToIndexFieldMapping
  where
  hashWithSalt
    _salt
    ConfluenceAttachmentToIndexFieldMapping' {..} =
      _salt `Prelude.hashWithSalt` dataSourceFieldName
        `Prelude.hashWithSalt` dateFieldFormat
        `Prelude.hashWithSalt` indexFieldName

instance
  Prelude.NFData
    ConfluenceAttachmentToIndexFieldMapping
  where
  rnf ConfluenceAttachmentToIndexFieldMapping' {..} =
    Prelude.rnf dataSourceFieldName
      `Prelude.seq` Prelude.rnf dateFieldFormat
      `Prelude.seq` Prelude.rnf indexFieldName

instance
  Data.ToJSON
    ConfluenceAttachmentToIndexFieldMapping
  where
  toJSON ConfluenceAttachmentToIndexFieldMapping' {..} =
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
