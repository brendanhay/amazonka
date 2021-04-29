{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.GlueTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GlueTable where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The database and table in the AWS Glue Data Catalog that is used for
-- input or output data.
--
-- /See:/ 'newGlueTable' smart constructor.
data GlueTable = GlueTable'
  { -- | The name of the connection to the AWS Glue Data Catalog.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the AWS Glue Data Catalog.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A database name in the AWS Glue Data Catalog.
    databaseName :: Prelude.Text,
    -- | A table name in the AWS Glue Data Catalog.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GlueTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 'glueTable_connectionName' - The name of the connection to the AWS Glue Data Catalog.
--
-- 'catalogId', 'glueTable_catalogId' - A unique identifier for the AWS Glue Data Catalog.
--
-- 'databaseName', 'glueTable_databaseName' - A database name in the AWS Glue Data Catalog.
--
-- 'tableName', 'glueTable_tableName' - A table name in the AWS Glue Data Catalog.
newGlueTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GlueTable
newGlueTable pDatabaseName_ pTableName_ =
  GlueTable'
    { connectionName = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The name of the connection to the AWS Glue Data Catalog.
glueTable_connectionName :: Lens.Lens' GlueTable (Prelude.Maybe Prelude.Text)
glueTable_connectionName = Lens.lens (\GlueTable' {connectionName} -> connectionName) (\s@GlueTable' {} a -> s {connectionName = a} :: GlueTable)

-- | A unique identifier for the AWS Glue Data Catalog.
glueTable_catalogId :: Lens.Lens' GlueTable (Prelude.Maybe Prelude.Text)
glueTable_catalogId = Lens.lens (\GlueTable' {catalogId} -> catalogId) (\s@GlueTable' {} a -> s {catalogId = a} :: GlueTable)

-- | A database name in the AWS Glue Data Catalog.
glueTable_databaseName :: Lens.Lens' GlueTable Prelude.Text
glueTable_databaseName = Lens.lens (\GlueTable' {databaseName} -> databaseName) (\s@GlueTable' {} a -> s {databaseName = a} :: GlueTable)

-- | A table name in the AWS Glue Data Catalog.
glueTable_tableName :: Lens.Lens' GlueTable Prelude.Text
glueTable_tableName = Lens.lens (\GlueTable' {tableName} -> tableName) (\s@GlueTable' {} a -> s {tableName = a} :: GlueTable)

instance Prelude.FromJSON GlueTable where
  parseJSON =
    Prelude.withObject
      "GlueTable"
      ( \x ->
          GlueTable'
            Prelude.<$> (x Prelude..:? "ConnectionName")
            Prelude.<*> (x Prelude..:? "CatalogId")
            Prelude.<*> (x Prelude..: "DatabaseName")
            Prelude.<*> (x Prelude..: "TableName")
      )

instance Prelude.Hashable GlueTable

instance Prelude.NFData GlueTable

instance Prelude.ToJSON GlueTable where
  toJSON GlueTable' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ConnectionName" Prelude..=)
              Prelude.<$> connectionName,
            ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableName" Prelude..= tableName)
          ]
      )
