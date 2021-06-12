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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The database and table in the AWS Glue Data Catalog that is used for
-- input or output data.
--
-- /See:/ 'newGlueTable' smart constructor.
data GlueTable = GlueTable'
  { -- | The name of the connection to the AWS Glue Data Catalog.
    connectionName :: Core.Maybe Core.Text,
    -- | A unique identifier for the AWS Glue Data Catalog.
    catalogId :: Core.Maybe Core.Text,
    -- | A database name in the AWS Glue Data Catalog.
    databaseName :: Core.Text,
    -- | A table name in the AWS Glue Data Catalog.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  GlueTable
newGlueTable pDatabaseName_ pTableName_ =
  GlueTable'
    { connectionName = Core.Nothing,
      catalogId = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The name of the connection to the AWS Glue Data Catalog.
glueTable_connectionName :: Lens.Lens' GlueTable (Core.Maybe Core.Text)
glueTable_connectionName = Lens.lens (\GlueTable' {connectionName} -> connectionName) (\s@GlueTable' {} a -> s {connectionName = a} :: GlueTable)

-- | A unique identifier for the AWS Glue Data Catalog.
glueTable_catalogId :: Lens.Lens' GlueTable (Core.Maybe Core.Text)
glueTable_catalogId = Lens.lens (\GlueTable' {catalogId} -> catalogId) (\s@GlueTable' {} a -> s {catalogId = a} :: GlueTable)

-- | A database name in the AWS Glue Data Catalog.
glueTable_databaseName :: Lens.Lens' GlueTable Core.Text
glueTable_databaseName = Lens.lens (\GlueTable' {databaseName} -> databaseName) (\s@GlueTable' {} a -> s {databaseName = a} :: GlueTable)

-- | A table name in the AWS Glue Data Catalog.
glueTable_tableName :: Lens.Lens' GlueTable Core.Text
glueTable_tableName = Lens.lens (\GlueTable' {tableName} -> tableName) (\s@GlueTable' {} a -> s {tableName = a} :: GlueTable)

instance Core.FromJSON GlueTable where
  parseJSON =
    Core.withObject
      "GlueTable"
      ( \x ->
          GlueTable'
            Core.<$> (x Core..:? "ConnectionName")
            Core.<*> (x Core..:? "CatalogId")
            Core.<*> (x Core..: "DatabaseName")
            Core.<*> (x Core..: "TableName")
      )

instance Core.Hashable GlueTable

instance Core.NFData GlueTable

instance Core.ToJSON GlueTable where
  toJSON GlueTable' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectionName" Core..=) Core.<$> connectionName,
            ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )
