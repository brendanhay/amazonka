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
-- Module      : Network.AWS.Kendra.Types.ConfluenceBlogToIndexFieldMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluenceBlogToIndexFieldMapping where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluenceBlogFieldName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the mapping between a blog field in the Confluence data source
-- to a Amazon Kendra index field.
--
-- You must first create the index field using the @UpdateIndex@ operation.
--
-- /See:/ 'newConfluenceBlogToIndexFieldMapping' smart constructor.
data ConfluenceBlogToIndexFieldMapping = ConfluenceBlogToIndexFieldMapping'
  { -- | The format for date fields in the data source. If the field specified in
    -- @DataSourceFieldName@ is a date field you must specify the date format.
    -- If the field is not a date field, an exception is thrown.
    dateFieldFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the field in the data source.
    dataSourceFieldName :: Prelude.Maybe ConfluenceBlogFieldName,
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
-- 'dateFieldFormat', 'confluenceBlogToIndexFieldMapping_dateFieldFormat' - The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
--
-- 'dataSourceFieldName', 'confluenceBlogToIndexFieldMapping_dataSourceFieldName' - The name of the field in the data source.
--
-- 'indexFieldName', 'confluenceBlogToIndexFieldMapping_indexFieldName' - The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
newConfluenceBlogToIndexFieldMapping ::
  ConfluenceBlogToIndexFieldMapping
newConfluenceBlogToIndexFieldMapping =
  ConfluenceBlogToIndexFieldMapping'
    { dateFieldFormat =
        Prelude.Nothing,
      dataSourceFieldName = Prelude.Nothing,
      indexFieldName = Prelude.Nothing
    }

-- | The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
confluenceBlogToIndexFieldMapping_dateFieldFormat :: Lens.Lens' ConfluenceBlogToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceBlogToIndexFieldMapping_dateFieldFormat = Lens.lens (\ConfluenceBlogToIndexFieldMapping' {dateFieldFormat} -> dateFieldFormat) (\s@ConfluenceBlogToIndexFieldMapping' {} a -> s {dateFieldFormat = a} :: ConfluenceBlogToIndexFieldMapping)

-- | The name of the field in the data source.
confluenceBlogToIndexFieldMapping_dataSourceFieldName :: Lens.Lens' ConfluenceBlogToIndexFieldMapping (Prelude.Maybe ConfluenceBlogFieldName)
confluenceBlogToIndexFieldMapping_dataSourceFieldName = Lens.lens (\ConfluenceBlogToIndexFieldMapping' {dataSourceFieldName} -> dataSourceFieldName) (\s@ConfluenceBlogToIndexFieldMapping' {} a -> s {dataSourceFieldName = a} :: ConfluenceBlogToIndexFieldMapping)

-- | The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
confluenceBlogToIndexFieldMapping_indexFieldName :: Lens.Lens' ConfluenceBlogToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluenceBlogToIndexFieldMapping_indexFieldName = Lens.lens (\ConfluenceBlogToIndexFieldMapping' {indexFieldName} -> indexFieldName) (\s@ConfluenceBlogToIndexFieldMapping' {} a -> s {indexFieldName = a} :: ConfluenceBlogToIndexFieldMapping)

instance
  Core.FromJSON
    ConfluenceBlogToIndexFieldMapping
  where
  parseJSON =
    Core.withObject
      "ConfluenceBlogToIndexFieldMapping"
      ( \x ->
          ConfluenceBlogToIndexFieldMapping'
            Prelude.<$> (x Core..:? "DateFieldFormat")
            Prelude.<*> (x Core..:? "DataSourceFieldName")
            Prelude.<*> (x Core..:? "IndexFieldName")
      )

instance
  Prelude.Hashable
    ConfluenceBlogToIndexFieldMapping

instance
  Prelude.NFData
    ConfluenceBlogToIndexFieldMapping

instance
  Core.ToJSON
    ConfluenceBlogToIndexFieldMapping
  where
  toJSON ConfluenceBlogToIndexFieldMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DateFieldFormat" Core..=)
              Prelude.<$> dateFieldFormat,
            ("DataSourceFieldName" Core..=)
              Prelude.<$> dataSourceFieldName,
            ("IndexFieldName" Core..=)
              Prelude.<$> indexFieldName
          ]
      )
