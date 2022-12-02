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
-- Module      : Amazonka.Glue.Types.DatabaseIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DatabaseIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that describes a target database for resource linking.
--
-- /See:/ 'newDatabaseIdentifier' smart constructor.
data DatabaseIdentifier = DatabaseIdentifier'
  { -- | The name of the catalog database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'databaseIdentifier_databaseName' - The name of the catalog database.
--
-- 'catalogId', 'databaseIdentifier_catalogId' - The ID of the Data Catalog in which the database resides.
newDatabaseIdentifier ::
  DatabaseIdentifier
newDatabaseIdentifier =
  DatabaseIdentifier'
    { databaseName = Prelude.Nothing,
      catalogId = Prelude.Nothing
    }

-- | The name of the catalog database.
databaseIdentifier_databaseName :: Lens.Lens' DatabaseIdentifier (Prelude.Maybe Prelude.Text)
databaseIdentifier_databaseName = Lens.lens (\DatabaseIdentifier' {databaseName} -> databaseName) (\s@DatabaseIdentifier' {} a -> s {databaseName = a} :: DatabaseIdentifier)

-- | The ID of the Data Catalog in which the database resides.
databaseIdentifier_catalogId :: Lens.Lens' DatabaseIdentifier (Prelude.Maybe Prelude.Text)
databaseIdentifier_catalogId = Lens.lens (\DatabaseIdentifier' {catalogId} -> catalogId) (\s@DatabaseIdentifier' {} a -> s {catalogId = a} :: DatabaseIdentifier)

instance Data.FromJSON DatabaseIdentifier where
  parseJSON =
    Data.withObject
      "DatabaseIdentifier"
      ( \x ->
          DatabaseIdentifier'
            Prelude.<$> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "CatalogId")
      )

instance Prelude.Hashable DatabaseIdentifier where
  hashWithSalt _salt DatabaseIdentifier' {..} =
    _salt `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` catalogId

instance Prelude.NFData DatabaseIdentifier where
  rnf DatabaseIdentifier' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf catalogId

instance Data.ToJSON DatabaseIdentifier where
  toJSON DatabaseIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("CatalogId" Data..=) Prelude.<$> catalogId
          ]
      )
