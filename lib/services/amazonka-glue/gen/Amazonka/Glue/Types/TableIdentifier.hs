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
-- Module      : Amazonka.Glue.Types.TableIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TableIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that describes a target table for resource linking.
--
-- /See:/ 'newTableIdentifier' smart constructor.
data TableIdentifier = TableIdentifier'
  { -- | The name of the target table.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database that contains the target table.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog in which the table resides.
    catalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'tableIdentifier_name' - The name of the target table.
--
-- 'databaseName', 'tableIdentifier_databaseName' - The name of the catalog database that contains the target table.
--
-- 'catalogId', 'tableIdentifier_catalogId' - The ID of the Data Catalog in which the table resides.
newTableIdentifier ::
  TableIdentifier
newTableIdentifier =
  TableIdentifier'
    { name = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      catalogId = Prelude.Nothing
    }

-- | The name of the target table.
tableIdentifier_name :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_name = Lens.lens (\TableIdentifier' {name} -> name) (\s@TableIdentifier' {} a -> s {name = a} :: TableIdentifier)

-- | The name of the catalog database that contains the target table.
tableIdentifier_databaseName :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_databaseName = Lens.lens (\TableIdentifier' {databaseName} -> databaseName) (\s@TableIdentifier' {} a -> s {databaseName = a} :: TableIdentifier)

-- | The ID of the Data Catalog in which the table resides.
tableIdentifier_catalogId :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_catalogId = Lens.lens (\TableIdentifier' {catalogId} -> catalogId) (\s@TableIdentifier' {} a -> s {catalogId = a} :: TableIdentifier)

instance Core.FromJSON TableIdentifier where
  parseJSON =
    Core.withObject
      "TableIdentifier"
      ( \x ->
          TableIdentifier'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "CatalogId")
      )

instance Prelude.Hashable TableIdentifier where
  hashWithSalt _salt TableIdentifier' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` catalogId

instance Prelude.NFData TableIdentifier where
  rnf TableIdentifier' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf catalogId

instance Core.ToJSON TableIdentifier where
  toJSON TableIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("CatalogId" Core..=) Prelude.<$> catalogId
          ]
      )
