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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TableIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that describes a target table for resource linking.
--
-- /See:/ 'newTableIdentifier' smart constructor.
data TableIdentifier = TableIdentifier'
  { -- | The ID of the Data Catalog in which the table resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database that contains the target table.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the target table.
    name :: Prelude.Maybe Prelude.Text
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
-- 'catalogId', 'tableIdentifier_catalogId' - The ID of the Data Catalog in which the table resides.
--
-- 'databaseName', 'tableIdentifier_databaseName' - The name of the catalog database that contains the target table.
--
-- 'name', 'tableIdentifier_name' - The name of the target table.
newTableIdentifier ::
  TableIdentifier
newTableIdentifier =
  TableIdentifier'
    { catalogId = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ID of the Data Catalog in which the table resides.
tableIdentifier_catalogId :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_catalogId = Lens.lens (\TableIdentifier' {catalogId} -> catalogId) (\s@TableIdentifier' {} a -> s {catalogId = a} :: TableIdentifier)

-- | The name of the catalog database that contains the target table.
tableIdentifier_databaseName :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_databaseName = Lens.lens (\TableIdentifier' {databaseName} -> databaseName) (\s@TableIdentifier' {} a -> s {databaseName = a} :: TableIdentifier)

-- | The name of the target table.
tableIdentifier_name :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_name = Lens.lens (\TableIdentifier' {name} -> name) (\s@TableIdentifier' {} a -> s {name = a} :: TableIdentifier)

instance Data.FromJSON TableIdentifier where
  parseJSON =
    Data.withObject
      "TableIdentifier"
      ( \x ->
          TableIdentifier'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable TableIdentifier where
  hashWithSalt _salt TableIdentifier' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` name

instance Prelude.NFData TableIdentifier where
  rnf TableIdentifier' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON TableIdentifier where
  toJSON TableIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
