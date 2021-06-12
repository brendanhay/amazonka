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
-- Module      : Network.AWS.SageMaker.Types.DataCatalogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataCatalogConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The meta data of the Glue table which serves as data catalog for the
-- @OfflineStore@.
--
-- /See:/ 'newDataCatalogConfig' smart constructor.
data DataCatalogConfig = DataCatalogConfig'
  { -- | The name of the Glue table.
    tableName :: Core.Text,
    -- | The name of the Glue table catalog.
    catalog :: Core.Text,
    -- | The name of the Glue table database.
    database :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'catalog'
  Core.Text ->
  -- | 'database'
  Core.Text ->
  DataCatalogConfig
newDataCatalogConfig pTableName_ pCatalog_ pDatabase_ =
  DataCatalogConfig'
    { tableName = pTableName_,
      catalog = pCatalog_,
      database = pDatabase_
    }

-- | The name of the Glue table.
dataCatalogConfig_tableName :: Lens.Lens' DataCatalogConfig Core.Text
dataCatalogConfig_tableName = Lens.lens (\DataCatalogConfig' {tableName} -> tableName) (\s@DataCatalogConfig' {} a -> s {tableName = a} :: DataCatalogConfig)

-- | The name of the Glue table catalog.
dataCatalogConfig_catalog :: Lens.Lens' DataCatalogConfig Core.Text
dataCatalogConfig_catalog = Lens.lens (\DataCatalogConfig' {catalog} -> catalog) (\s@DataCatalogConfig' {} a -> s {catalog = a} :: DataCatalogConfig)

-- | The name of the Glue table database.
dataCatalogConfig_database :: Lens.Lens' DataCatalogConfig Core.Text
dataCatalogConfig_database = Lens.lens (\DataCatalogConfig' {database} -> database) (\s@DataCatalogConfig' {} a -> s {database = a} :: DataCatalogConfig)

instance Core.FromJSON DataCatalogConfig where
  parseJSON =
    Core.withObject
      "DataCatalogConfig"
      ( \x ->
          DataCatalogConfig'
            Core.<$> (x Core..: "TableName")
            Core.<*> (x Core..: "Catalog")
            Core.<*> (x Core..: "Database")
      )

instance Core.Hashable DataCatalogConfig

instance Core.NFData DataCatalogConfig

instance Core.ToJSON DataCatalogConfig where
  toJSON DataCatalogConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            Core.Just ("Catalog" Core..= catalog),
            Core.Just ("Database" Core..= database)
          ]
      )
