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
-- Module      : Amazonka.Glue.Types.DirectSchemaChangePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DirectSchemaChangePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.UpdateCatalogBehavior
import qualified Amazonka.Prelude as Prelude

-- | A policy that specifies update behavior for the crawler.
--
-- /See:/ 'newDirectSchemaChangePolicy' smart constructor.
data DirectSchemaChangePolicy = DirectSchemaChangePolicy'
  { -- | Specifies the database that the schema change policy applies to.
    database :: Prelude.Maybe Prelude.Text,
    -- | Whether to use the specified update behavior when the crawler finds a
    -- changed schema.
    enableUpdateCatalog :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the table in the database that the schema change policy
    -- applies to.
    table :: Prelude.Maybe Prelude.Text,
    -- | The update behavior when the crawler finds a changed schema.
    updateBehavior :: Prelude.Maybe UpdateCatalogBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectSchemaChangePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'directSchemaChangePolicy_database' - Specifies the database that the schema change policy applies to.
--
-- 'enableUpdateCatalog', 'directSchemaChangePolicy_enableUpdateCatalog' - Whether to use the specified update behavior when the crawler finds a
-- changed schema.
--
-- 'table', 'directSchemaChangePolicy_table' - Specifies the table in the database that the schema change policy
-- applies to.
--
-- 'updateBehavior', 'directSchemaChangePolicy_updateBehavior' - The update behavior when the crawler finds a changed schema.
newDirectSchemaChangePolicy ::
  DirectSchemaChangePolicy
newDirectSchemaChangePolicy =
  DirectSchemaChangePolicy'
    { database =
        Prelude.Nothing,
      enableUpdateCatalog = Prelude.Nothing,
      table = Prelude.Nothing,
      updateBehavior = Prelude.Nothing
    }

-- | Specifies the database that the schema change policy applies to.
directSchemaChangePolicy_database :: Lens.Lens' DirectSchemaChangePolicy (Prelude.Maybe Prelude.Text)
directSchemaChangePolicy_database = Lens.lens (\DirectSchemaChangePolicy' {database} -> database) (\s@DirectSchemaChangePolicy' {} a -> s {database = a} :: DirectSchemaChangePolicy)

-- | Whether to use the specified update behavior when the crawler finds a
-- changed schema.
directSchemaChangePolicy_enableUpdateCatalog :: Lens.Lens' DirectSchemaChangePolicy (Prelude.Maybe Prelude.Bool)
directSchemaChangePolicy_enableUpdateCatalog = Lens.lens (\DirectSchemaChangePolicy' {enableUpdateCatalog} -> enableUpdateCatalog) (\s@DirectSchemaChangePolicy' {} a -> s {enableUpdateCatalog = a} :: DirectSchemaChangePolicy)

-- | Specifies the table in the database that the schema change policy
-- applies to.
directSchemaChangePolicy_table :: Lens.Lens' DirectSchemaChangePolicy (Prelude.Maybe Prelude.Text)
directSchemaChangePolicy_table = Lens.lens (\DirectSchemaChangePolicy' {table} -> table) (\s@DirectSchemaChangePolicy' {} a -> s {table = a} :: DirectSchemaChangePolicy)

-- | The update behavior when the crawler finds a changed schema.
directSchemaChangePolicy_updateBehavior :: Lens.Lens' DirectSchemaChangePolicy (Prelude.Maybe UpdateCatalogBehavior)
directSchemaChangePolicy_updateBehavior = Lens.lens (\DirectSchemaChangePolicy' {updateBehavior} -> updateBehavior) (\s@DirectSchemaChangePolicy' {} a -> s {updateBehavior = a} :: DirectSchemaChangePolicy)

instance Data.FromJSON DirectSchemaChangePolicy where
  parseJSON =
    Data.withObject
      "DirectSchemaChangePolicy"
      ( \x ->
          DirectSchemaChangePolicy'
            Prelude.<$> (x Data..:? "Database")
            Prelude.<*> (x Data..:? "EnableUpdateCatalog")
            Prelude.<*> (x Data..:? "Table")
            Prelude.<*> (x Data..:? "UpdateBehavior")
      )

instance Prelude.Hashable DirectSchemaChangePolicy where
  hashWithSalt _salt DirectSchemaChangePolicy' {..} =
    _salt `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` enableUpdateCatalog
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` updateBehavior

instance Prelude.NFData DirectSchemaChangePolicy where
  rnf DirectSchemaChangePolicy' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf enableUpdateCatalog
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf updateBehavior

instance Data.ToJSON DirectSchemaChangePolicy where
  toJSON DirectSchemaChangePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Database" Data..=) Prelude.<$> database,
            ("EnableUpdateCatalog" Data..=)
              Prelude.<$> enableUpdateCatalog,
            ("Table" Data..=) Prelude.<$> table,
            ("UpdateBehavior" Data..=)
              Prelude.<$> updateBehavior
          ]
      )
