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
-- Module      : Amazonka.SageMaker.Types.DataCatalogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataCatalogConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The meta data of the Glue table which serves as data catalog for the
-- @OfflineStore@.
--
-- /See:/ 'newDataCatalogConfig' smart constructor.
data DataCatalogConfig = DataCatalogConfig'
  { -- | The name of the Glue table.
    tableName :: Prelude.Text,
    -- | The name of the Glue table catalog.
    catalog :: Prelude.Text,
    -- | The name of the Glue table database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCatalogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'dataCatalogConfig_tableName' - The name of the Glue table.
--
-- 'catalog', 'dataCatalogConfig_catalog' - The name of the Glue table catalog.
--
-- 'database', 'dataCatalogConfig_database' - The name of the Glue table database.
newDataCatalogConfig ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'catalog'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  DataCatalogConfig
newDataCatalogConfig pTableName_ pCatalog_ pDatabase_ =
  DataCatalogConfig'
    { tableName = pTableName_,
      catalog = pCatalog_,
      database = pDatabase_
    }

-- | The name of the Glue table.
dataCatalogConfig_tableName :: Lens.Lens' DataCatalogConfig Prelude.Text
dataCatalogConfig_tableName = Lens.lens (\DataCatalogConfig' {tableName} -> tableName) (\s@DataCatalogConfig' {} a -> s {tableName = a} :: DataCatalogConfig)

-- | The name of the Glue table catalog.
dataCatalogConfig_catalog :: Lens.Lens' DataCatalogConfig Prelude.Text
dataCatalogConfig_catalog = Lens.lens (\DataCatalogConfig' {catalog} -> catalog) (\s@DataCatalogConfig' {} a -> s {catalog = a} :: DataCatalogConfig)

-- | The name of the Glue table database.
dataCatalogConfig_database :: Lens.Lens' DataCatalogConfig Prelude.Text
dataCatalogConfig_database = Lens.lens (\DataCatalogConfig' {database} -> database) (\s@DataCatalogConfig' {} a -> s {database = a} :: DataCatalogConfig)

instance Core.FromJSON DataCatalogConfig where
  parseJSON =
    Core.withObject
      "DataCatalogConfig"
      ( \x ->
          DataCatalogConfig'
            Prelude.<$> (x Core..: "TableName")
            Prelude.<*> (x Core..: "Catalog")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable DataCatalogConfig where
  hashWithSalt _salt DataCatalogConfig' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` database

instance Prelude.NFData DataCatalogConfig where
  rnf DataCatalogConfig' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf database

instance Core.ToJSON DataCatalogConfig where
  toJSON DataCatalogConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("Catalog" Core..= catalog),
            Prelude.Just ("Database" Core..= database)
          ]
      )
