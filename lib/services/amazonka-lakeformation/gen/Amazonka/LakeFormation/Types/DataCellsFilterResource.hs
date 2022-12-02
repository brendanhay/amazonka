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
-- Module      : Amazonka.LakeFormation.Types.DataCellsFilterResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DataCellsFilterResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure for a data cells filter resource.
--
-- /See:/ 'newDataCellsFilterResource' smart constructor.
data DataCellsFilterResource = DataCellsFilterResource'
  { -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The name of the data cells filter.
    name :: Prelude.Maybe Prelude.Text,
    -- | A database in the Glue Data Catalog.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the catalog to which the table belongs.
    tableCatalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCellsFilterResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'dataCellsFilterResource_tableName' - The name of the table.
--
-- 'name', 'dataCellsFilterResource_name' - The name of the data cells filter.
--
-- 'databaseName', 'dataCellsFilterResource_databaseName' - A database in the Glue Data Catalog.
--
-- 'tableCatalogId', 'dataCellsFilterResource_tableCatalogId' - The ID of the catalog to which the table belongs.
newDataCellsFilterResource ::
  DataCellsFilterResource
newDataCellsFilterResource =
  DataCellsFilterResource'
    { tableName =
        Prelude.Nothing,
      name = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      tableCatalogId = Prelude.Nothing
    }

-- | The name of the table.
dataCellsFilterResource_tableName :: Lens.Lens' DataCellsFilterResource (Prelude.Maybe Prelude.Text)
dataCellsFilterResource_tableName = Lens.lens (\DataCellsFilterResource' {tableName} -> tableName) (\s@DataCellsFilterResource' {} a -> s {tableName = a} :: DataCellsFilterResource)

-- | The name of the data cells filter.
dataCellsFilterResource_name :: Lens.Lens' DataCellsFilterResource (Prelude.Maybe Prelude.Text)
dataCellsFilterResource_name = Lens.lens (\DataCellsFilterResource' {name} -> name) (\s@DataCellsFilterResource' {} a -> s {name = a} :: DataCellsFilterResource)

-- | A database in the Glue Data Catalog.
dataCellsFilterResource_databaseName :: Lens.Lens' DataCellsFilterResource (Prelude.Maybe Prelude.Text)
dataCellsFilterResource_databaseName = Lens.lens (\DataCellsFilterResource' {databaseName} -> databaseName) (\s@DataCellsFilterResource' {} a -> s {databaseName = a} :: DataCellsFilterResource)

-- | The ID of the catalog to which the table belongs.
dataCellsFilterResource_tableCatalogId :: Lens.Lens' DataCellsFilterResource (Prelude.Maybe Prelude.Text)
dataCellsFilterResource_tableCatalogId = Lens.lens (\DataCellsFilterResource' {tableCatalogId} -> tableCatalogId) (\s@DataCellsFilterResource' {} a -> s {tableCatalogId = a} :: DataCellsFilterResource)

instance Data.FromJSON DataCellsFilterResource where
  parseJSON =
    Data.withObject
      "DataCellsFilterResource"
      ( \x ->
          DataCellsFilterResource'
            Prelude.<$> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "TableCatalogId")
      )

instance Prelude.Hashable DataCellsFilterResource where
  hashWithSalt _salt DataCellsFilterResource' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableCatalogId

instance Prelude.NFData DataCellsFilterResource where
  rnf DataCellsFilterResource' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableCatalogId

instance Data.ToJSON DataCellsFilterResource where
  toJSON DataCellsFilterResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TableName" Data..=) Prelude.<$> tableName,
            ("Name" Data..=) Prelude.<$> name,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("TableCatalogId" Data..=)
              Prelude.<$> tableCatalogId
          ]
      )
