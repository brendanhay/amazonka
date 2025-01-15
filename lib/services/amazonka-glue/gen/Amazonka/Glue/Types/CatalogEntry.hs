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
-- Module      : Amazonka.Glue.Types.CatalogEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a table definition in the Glue Data Catalog.
--
-- /See:/ 'newCatalogEntry' smart constructor.
data CatalogEntry = CatalogEntry'
  { -- | The database in which the table metadata resides.
    databaseName :: Prelude.Text,
    -- | The name of the table in question.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  CatalogEntry
newCatalogEntry pDatabaseName_ pTableName_ =
  CatalogEntry'
    { databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The database in which the table metadata resides.
catalogEntry_databaseName :: Lens.Lens' CatalogEntry Prelude.Text
catalogEntry_databaseName = Lens.lens (\CatalogEntry' {databaseName} -> databaseName) (\s@CatalogEntry' {} a -> s {databaseName = a} :: CatalogEntry)

-- | The name of the table in question.
catalogEntry_tableName :: Lens.Lens' CatalogEntry Prelude.Text
catalogEntry_tableName = Lens.lens (\CatalogEntry' {tableName} -> tableName) (\s@CatalogEntry' {} a -> s {tableName = a} :: CatalogEntry)

instance Prelude.Hashable CatalogEntry where
  hashWithSalt _salt CatalogEntry' {..} =
    _salt
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData CatalogEntry where
  rnf CatalogEntry' {..} =
    Prelude.rnf databaseName `Prelude.seq`
      Prelude.rnf tableName

instance Data.ToJSON CatalogEntry where
  toJSON CatalogEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )
