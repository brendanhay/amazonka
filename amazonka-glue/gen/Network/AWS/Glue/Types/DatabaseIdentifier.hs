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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure that describes a target database for resource linking.
--
-- /See:/ 'newDatabaseIdentifier' smart constructor.
data DatabaseIdentifier = DatabaseIdentifier'
  { -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { catalogId = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The ID of the Data Catalog in which the database resides.
databaseIdentifier_catalogId :: Lens.Lens' DatabaseIdentifier (Core.Maybe Core.Text)
databaseIdentifier_catalogId = Lens.lens (\DatabaseIdentifier' {catalogId} -> catalogId) (\s@DatabaseIdentifier' {} a -> s {catalogId = a} :: DatabaseIdentifier)

-- | The name of the catalog database.
databaseIdentifier_databaseName :: Lens.Lens' DatabaseIdentifier (Core.Maybe Core.Text)
databaseIdentifier_databaseName = Lens.lens (\DatabaseIdentifier' {databaseName} -> databaseName) (\s@DatabaseIdentifier' {} a -> s {databaseName = a} :: DatabaseIdentifier)

instance Core.FromJSON DatabaseIdentifier where
  parseJSON =
    Core.withObject
      "DatabaseIdentifier"
      ( \x ->
          DatabaseIdentifier'
            Core.<$> (x Core..:? "CatalogId")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable DatabaseIdentifier

instance Core.NFData DatabaseIdentifier

instance Core.ToJSON DatabaseIdentifier where
  toJSON DatabaseIdentifier' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
