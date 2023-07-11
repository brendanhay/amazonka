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
-- Module      : Amazonka.DataBrew.Types.DataCatalogInputDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.DataCatalogInputDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | Represents how metadata stored in the Glue Data Catalog is defined in a
-- DataBrew dataset.
--
-- /See:/ 'newDataCatalogInputDefinition' smart constructor.
data DataCatalogInputDefinition = DataCatalogInputDefinition'
  { -- | The unique identifier of the Amazon Web Services account that holds the
    -- Data Catalog that stores the data.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Represents an Amazon location where DataBrew can store intermediate
    -- results.
    tempDirectory :: Prelude.Maybe S3Location,
    -- | The name of a database in the Data Catalog.
    databaseName :: Prelude.Text,
    -- | The name of a database table in the Data Catalog. This table corresponds
    -- to a DataBrew dataset.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCatalogInputDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'dataCatalogInputDefinition_catalogId' - The unique identifier of the Amazon Web Services account that holds the
-- Data Catalog that stores the data.
--
-- 'tempDirectory', 'dataCatalogInputDefinition_tempDirectory' - Represents an Amazon location where DataBrew can store intermediate
-- results.
--
-- 'databaseName', 'dataCatalogInputDefinition_databaseName' - The name of a database in the Data Catalog.
--
-- 'tableName', 'dataCatalogInputDefinition_tableName' - The name of a database table in the Data Catalog. This table corresponds
-- to a DataBrew dataset.
newDataCatalogInputDefinition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  DataCatalogInputDefinition
newDataCatalogInputDefinition
  pDatabaseName_
  pTableName_ =
    DataCatalogInputDefinition'
      { catalogId =
          Prelude.Nothing,
        tempDirectory = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_
      }

-- | The unique identifier of the Amazon Web Services account that holds the
-- Data Catalog that stores the data.
dataCatalogInputDefinition_catalogId :: Lens.Lens' DataCatalogInputDefinition (Prelude.Maybe Prelude.Text)
dataCatalogInputDefinition_catalogId = Lens.lens (\DataCatalogInputDefinition' {catalogId} -> catalogId) (\s@DataCatalogInputDefinition' {} a -> s {catalogId = a} :: DataCatalogInputDefinition)

-- | Represents an Amazon location where DataBrew can store intermediate
-- results.
dataCatalogInputDefinition_tempDirectory :: Lens.Lens' DataCatalogInputDefinition (Prelude.Maybe S3Location)
dataCatalogInputDefinition_tempDirectory = Lens.lens (\DataCatalogInputDefinition' {tempDirectory} -> tempDirectory) (\s@DataCatalogInputDefinition' {} a -> s {tempDirectory = a} :: DataCatalogInputDefinition)

-- | The name of a database in the Data Catalog.
dataCatalogInputDefinition_databaseName :: Lens.Lens' DataCatalogInputDefinition Prelude.Text
dataCatalogInputDefinition_databaseName = Lens.lens (\DataCatalogInputDefinition' {databaseName} -> databaseName) (\s@DataCatalogInputDefinition' {} a -> s {databaseName = a} :: DataCatalogInputDefinition)

-- | The name of a database table in the Data Catalog. This table corresponds
-- to a DataBrew dataset.
dataCatalogInputDefinition_tableName :: Lens.Lens' DataCatalogInputDefinition Prelude.Text
dataCatalogInputDefinition_tableName = Lens.lens (\DataCatalogInputDefinition' {tableName} -> tableName) (\s@DataCatalogInputDefinition' {} a -> s {tableName = a} :: DataCatalogInputDefinition)

instance Data.FromJSON DataCatalogInputDefinition where
  parseJSON =
    Data.withObject
      "DataCatalogInputDefinition"
      ( \x ->
          DataCatalogInputDefinition'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..:? "TempDirectory")
            Prelude.<*> (x Data..: "DatabaseName")
            Prelude.<*> (x Data..: "TableName")
      )

instance Prelude.Hashable DataCatalogInputDefinition where
  hashWithSalt _salt DataCatalogInputDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tempDirectory
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData DataCatalogInputDefinition where
  rnf DataCatalogInputDefinition' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tempDirectory
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToJSON DataCatalogInputDefinition where
  toJSON DataCatalogInputDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("TempDirectory" Data..=) Prelude.<$> tempDirectory,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )
