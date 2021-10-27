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
-- Module      : Network.AWS.Kendra.Types.ConfluencePageToIndexFieldMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluencePageToIndexFieldMapping where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluencePageFieldName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the mapping between a field in the Confluence data source to a
-- Amazon Kendra index field.
--
-- You must first create the index field using the @UpdateIndex@ operation.
--
-- /See:/ 'newConfluencePageToIndexFieldMapping' smart constructor.
data ConfluencePageToIndexFieldMapping = ConfluencePageToIndexFieldMapping'
  { -- | The format for date fields in the data source. If the field specified in
    -- @DataSourceFieldName@ is a date field you must specify the date format.
    -- If the field is not a date field, an exception is thrown.
    dateFieldFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the field in the data source.
    dataSourceFieldName :: Prelude.Maybe ConfluencePageFieldName,
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
-- 'dateFieldFormat', 'confluencePageToIndexFieldMapping_dateFieldFormat' - The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
--
-- 'dataSourceFieldName', 'confluencePageToIndexFieldMapping_dataSourceFieldName' - The name of the field in the data source.
--
-- 'indexFieldName', 'confluencePageToIndexFieldMapping_indexFieldName' - The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
newConfluencePageToIndexFieldMapping ::
  ConfluencePageToIndexFieldMapping
newConfluencePageToIndexFieldMapping =
  ConfluencePageToIndexFieldMapping'
    { dateFieldFormat =
        Prelude.Nothing,
      dataSourceFieldName = Prelude.Nothing,
      indexFieldName = Prelude.Nothing
    }

-- | The format for date fields in the data source. If the field specified in
-- @DataSourceFieldName@ is a date field you must specify the date format.
-- If the field is not a date field, an exception is thrown.
confluencePageToIndexFieldMapping_dateFieldFormat :: Lens.Lens' ConfluencePageToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluencePageToIndexFieldMapping_dateFieldFormat = Lens.lens (\ConfluencePageToIndexFieldMapping' {dateFieldFormat} -> dateFieldFormat) (\s@ConfluencePageToIndexFieldMapping' {} a -> s {dateFieldFormat = a} :: ConfluencePageToIndexFieldMapping)

-- | The name of the field in the data source.
confluencePageToIndexFieldMapping_dataSourceFieldName :: Lens.Lens' ConfluencePageToIndexFieldMapping (Prelude.Maybe ConfluencePageFieldName)
confluencePageToIndexFieldMapping_dataSourceFieldName = Lens.lens (\ConfluencePageToIndexFieldMapping' {dataSourceFieldName} -> dataSourceFieldName) (\s@ConfluencePageToIndexFieldMapping' {} a -> s {dataSourceFieldName = a} :: ConfluencePageToIndexFieldMapping)

-- | The name of the index field to map to the Confluence data source field.
-- The index field type must match the Confluence field type.
confluencePageToIndexFieldMapping_indexFieldName :: Lens.Lens' ConfluencePageToIndexFieldMapping (Prelude.Maybe Prelude.Text)
confluencePageToIndexFieldMapping_indexFieldName = Lens.lens (\ConfluencePageToIndexFieldMapping' {indexFieldName} -> indexFieldName) (\s@ConfluencePageToIndexFieldMapping' {} a -> s {indexFieldName = a} :: ConfluencePageToIndexFieldMapping)

instance
  Core.FromJSON
    ConfluencePageToIndexFieldMapping
  where
  parseJSON =
    Core.withObject
      "ConfluencePageToIndexFieldMapping"
      ( \x ->
          ConfluencePageToIndexFieldMapping'
            Prelude.<$> (x Core..:? "DateFieldFormat")
            Prelude.<*> (x Core..:? "DataSourceFieldName")
            Prelude.<*> (x Core..:? "IndexFieldName")
      )

instance
  Prelude.Hashable
    ConfluencePageToIndexFieldMapping

instance
  Prelude.NFData
    ConfluencePageToIndexFieldMapping

instance
  Core.ToJSON
    ConfluencePageToIndexFieldMapping
  where
  toJSON ConfluencePageToIndexFieldMapping' {..} =
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
