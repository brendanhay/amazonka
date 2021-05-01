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
-- Module      : Network.AWS.Glue.Types.TableIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableIdentifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that describes a target table for resource linking.
--
-- /See:/ 'newTableIdentifier' smart constructor.
data TableIdentifier = TableIdentifier'
  { -- | The ID of the Data Catalog in which the table resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the target table.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database that contains the target table.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'name', 'tableIdentifier_name' - The name of the target table.
--
-- 'databaseName', 'tableIdentifier_databaseName' - The name of the catalog database that contains the target table.
newTableIdentifier ::
  TableIdentifier
newTableIdentifier =
  TableIdentifier'
    { catalogId = Prelude.Nothing,
      name = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The ID of the Data Catalog in which the table resides.
tableIdentifier_catalogId :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_catalogId = Lens.lens (\TableIdentifier' {catalogId} -> catalogId) (\s@TableIdentifier' {} a -> s {catalogId = a} :: TableIdentifier)

-- | The name of the target table.
tableIdentifier_name :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_name = Lens.lens (\TableIdentifier' {name} -> name) (\s@TableIdentifier' {} a -> s {name = a} :: TableIdentifier)

-- | The name of the catalog database that contains the target table.
tableIdentifier_databaseName :: Lens.Lens' TableIdentifier (Prelude.Maybe Prelude.Text)
tableIdentifier_databaseName = Lens.lens (\TableIdentifier' {databaseName} -> databaseName) (\s@TableIdentifier' {} a -> s {databaseName = a} :: TableIdentifier)

instance Prelude.FromJSON TableIdentifier where
  parseJSON =
    Prelude.withObject
      "TableIdentifier"
      ( \x ->
          TableIdentifier'
            Prelude.<$> (x Prelude..:? "CatalogId")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable TableIdentifier

instance Prelude.NFData TableIdentifier

instance Prelude.ToJSON TableIdentifier where
  toJSON TableIdentifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            ("Name" Prelude..=) Prelude.<$> name,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
