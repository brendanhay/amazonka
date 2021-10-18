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
-- Module      : Network.AWS.Glue.Types.CatalogTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies an Glue Data Catalog target.
--
-- /See:/ 'newCatalogTarget' smart constructor.
data CatalogTarget = CatalogTarget'
  { -- | The name of the database to be synchronized.
    databaseName :: Prelude.Text,
    -- | A list of the tables to be synchronized.
    tables :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'catalogTarget_databaseName' - The name of the database to be synchronized.
--
-- 'tables', 'catalogTarget_tables' - A list of the tables to be synchronized.
newCatalogTarget ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tables'
  Prelude.NonEmpty Prelude.Text ->
  CatalogTarget
newCatalogTarget pDatabaseName_ pTables_ =
  CatalogTarget'
    { databaseName = pDatabaseName_,
      tables = Lens._Coerce Lens.# pTables_
    }

-- | The name of the database to be synchronized.
catalogTarget_databaseName :: Lens.Lens' CatalogTarget Prelude.Text
catalogTarget_databaseName = Lens.lens (\CatalogTarget' {databaseName} -> databaseName) (\s@CatalogTarget' {} a -> s {databaseName = a} :: CatalogTarget)

-- | A list of the tables to be synchronized.
catalogTarget_tables :: Lens.Lens' CatalogTarget (Prelude.NonEmpty Prelude.Text)
catalogTarget_tables = Lens.lens (\CatalogTarget' {tables} -> tables) (\s@CatalogTarget' {} a -> s {tables = a} :: CatalogTarget) Prelude.. Lens._Coerce

instance Core.FromJSON CatalogTarget where
  parseJSON =
    Core.withObject
      "CatalogTarget"
      ( \x ->
          CatalogTarget'
            Prelude.<$> (x Core..: "DatabaseName")
            Prelude.<*> (x Core..: "Tables")
      )

instance Prelude.Hashable CatalogTarget

instance Prelude.NFData CatalogTarget

instance Core.ToJSON CatalogTarget where
  toJSON CatalogTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("Tables" Core..= tables)
          ]
      )
