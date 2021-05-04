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
-- Module      : Network.AWS.Glue.Types.DatabaseIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseIdentifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that describes a target database for resource linking.
--
-- /See:/ 'newDatabaseIdentifier' smart constructor.
data DatabaseIdentifier = DatabaseIdentifier'
  { -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DatabaseIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'databaseIdentifier_catalogId' - The ID of the Data Catalog in which the database resides.
--
-- 'databaseName', 'databaseIdentifier_databaseName' - The name of the catalog database.
newDatabaseIdentifier ::
  DatabaseIdentifier
newDatabaseIdentifier =
  DatabaseIdentifier'
    { catalogId = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The ID of the Data Catalog in which the database resides.
databaseIdentifier_catalogId :: Lens.Lens' DatabaseIdentifier (Prelude.Maybe Prelude.Text)
databaseIdentifier_catalogId = Lens.lens (\DatabaseIdentifier' {catalogId} -> catalogId) (\s@DatabaseIdentifier' {} a -> s {catalogId = a} :: DatabaseIdentifier)

-- | The name of the catalog database.
databaseIdentifier_databaseName :: Lens.Lens' DatabaseIdentifier (Prelude.Maybe Prelude.Text)
databaseIdentifier_databaseName = Lens.lens (\DatabaseIdentifier' {databaseName} -> databaseName) (\s@DatabaseIdentifier' {} a -> s {databaseName = a} :: DatabaseIdentifier)

instance Prelude.FromJSON DatabaseIdentifier where
  parseJSON =
    Prelude.withObject
      "DatabaseIdentifier"
      ( \x ->
          DatabaseIdentifier'
            Prelude.<$> (x Prelude..:? "CatalogId")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable DatabaseIdentifier

instance Prelude.NFData DatabaseIdentifier

instance Prelude.ToJSON DatabaseIdentifier where
  toJSON DatabaseIdentifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
