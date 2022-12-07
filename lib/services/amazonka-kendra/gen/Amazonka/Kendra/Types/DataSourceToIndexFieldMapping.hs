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
-- Module      : Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceToIndexFieldMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Maps a column or attribute in the data source to an index field. You
-- must first create the fields in the index using the @UpdateIndex@ API.
--
-- /See:/ 'newDataSourceToIndexFieldMapping' smart constructor.
data DataSourceToIndexFieldMapping = DataSourceToIndexFieldMapping'
  { -- | The type of data stored in the column or attribute.
    dateFieldFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the column or attribute in the data source.
    dataSourceFieldName :: Prelude.Text,
    -- | The name of the field in the index.
    indexFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceToIndexFieldMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateFieldFormat', 'dataSourceToIndexFieldMapping_dateFieldFormat' - The type of data stored in the column or attribute.
--
-- 'dataSourceFieldName', 'dataSourceToIndexFieldMapping_dataSourceFieldName' - The name of the column or attribute in the data source.
--
-- 'indexFieldName', 'dataSourceToIndexFieldMapping_indexFieldName' - The name of the field in the index.
newDataSourceToIndexFieldMapping ::
  -- | 'dataSourceFieldName'
  Prelude.Text ->
  -- | 'indexFieldName'
  Prelude.Text ->
  DataSourceToIndexFieldMapping
newDataSourceToIndexFieldMapping
  pDataSourceFieldName_
  pIndexFieldName_ =
    DataSourceToIndexFieldMapping'
      { dateFieldFormat =
          Prelude.Nothing,
        dataSourceFieldName = pDataSourceFieldName_,
        indexFieldName = pIndexFieldName_
      }

-- | The type of data stored in the column or attribute.
dataSourceToIndexFieldMapping_dateFieldFormat :: Lens.Lens' DataSourceToIndexFieldMapping (Prelude.Maybe Prelude.Text)
dataSourceToIndexFieldMapping_dateFieldFormat = Lens.lens (\DataSourceToIndexFieldMapping' {dateFieldFormat} -> dateFieldFormat) (\s@DataSourceToIndexFieldMapping' {} a -> s {dateFieldFormat = a} :: DataSourceToIndexFieldMapping)

-- | The name of the column or attribute in the data source.
dataSourceToIndexFieldMapping_dataSourceFieldName :: Lens.Lens' DataSourceToIndexFieldMapping Prelude.Text
dataSourceToIndexFieldMapping_dataSourceFieldName = Lens.lens (\DataSourceToIndexFieldMapping' {dataSourceFieldName} -> dataSourceFieldName) (\s@DataSourceToIndexFieldMapping' {} a -> s {dataSourceFieldName = a} :: DataSourceToIndexFieldMapping)

-- | The name of the field in the index.
dataSourceToIndexFieldMapping_indexFieldName :: Lens.Lens' DataSourceToIndexFieldMapping Prelude.Text
dataSourceToIndexFieldMapping_indexFieldName = Lens.lens (\DataSourceToIndexFieldMapping' {indexFieldName} -> indexFieldName) (\s@DataSourceToIndexFieldMapping' {} a -> s {indexFieldName = a} :: DataSourceToIndexFieldMapping)

instance Data.FromJSON DataSourceToIndexFieldMapping where
  parseJSON =
    Data.withObject
      "DataSourceToIndexFieldMapping"
      ( \x ->
          DataSourceToIndexFieldMapping'
            Prelude.<$> (x Data..:? "DateFieldFormat")
            Prelude.<*> (x Data..: "DataSourceFieldName")
            Prelude.<*> (x Data..: "IndexFieldName")
      )

instance
  Prelude.Hashable
    DataSourceToIndexFieldMapping
  where
  hashWithSalt _salt DataSourceToIndexFieldMapping' {..} =
    _salt `Prelude.hashWithSalt` dateFieldFormat
      `Prelude.hashWithSalt` dataSourceFieldName
      `Prelude.hashWithSalt` indexFieldName

instance Prelude.NFData DataSourceToIndexFieldMapping where
  rnf DataSourceToIndexFieldMapping' {..} =
    Prelude.rnf dateFieldFormat
      `Prelude.seq` Prelude.rnf dataSourceFieldName
      `Prelude.seq` Prelude.rnf indexFieldName

instance Data.ToJSON DataSourceToIndexFieldMapping where
  toJSON DataSourceToIndexFieldMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateFieldFormat" Data..=)
              Prelude.<$> dateFieldFormat,
            Prelude.Just
              ("DataSourceFieldName" Data..= dataSourceFieldName),
            Prelude.Just
              ("IndexFieldName" Data..= indexFieldName)
          ]
      )
