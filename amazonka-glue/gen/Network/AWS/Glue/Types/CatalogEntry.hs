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
-- Module      : Network.AWS.Glue.Types.CatalogEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies a table definition in the AWS Glue Data Catalog.
--
-- /See:/ 'newCatalogEntry' smart constructor.
data CatalogEntry = CatalogEntry'
  { -- | The database in which the table metadata resides.
    databaseName :: Core.Text,
    -- | The name of the table in question.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CatalogEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'catalogEntry_databaseName' - The database in which the table metadata resides.
--
-- 'tableName', 'catalogEntry_tableName' - The name of the table in question.
newCatalogEntry ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  CatalogEntry
newCatalogEntry pDatabaseName_ pTableName_ =
  CatalogEntry'
    { databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The database in which the table metadata resides.
catalogEntry_databaseName :: Lens.Lens' CatalogEntry Core.Text
catalogEntry_databaseName = Lens.lens (\CatalogEntry' {databaseName} -> databaseName) (\s@CatalogEntry' {} a -> s {databaseName = a} :: CatalogEntry)

-- | The name of the table in question.
catalogEntry_tableName :: Lens.Lens' CatalogEntry Core.Text
catalogEntry_tableName = Lens.lens (\CatalogEntry' {tableName} -> tableName) (\s@CatalogEntry' {} a -> s {tableName = a} :: CatalogEntry)

instance Core.Hashable CatalogEntry

instance Core.NFData CatalogEntry

instance Core.ToJSON CatalogEntry where
  toJSON CatalogEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )
