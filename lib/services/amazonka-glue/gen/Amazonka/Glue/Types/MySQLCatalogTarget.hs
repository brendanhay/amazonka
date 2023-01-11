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
-- Module      : Amazonka.Glue.Types.MySQLCatalogTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MySQLCatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that uses MySQL.
--
-- /See:/ 'newMySQLCatalogTarget' smart constructor.
data MySQLCatalogTarget = MySQLCatalogTarget'
  { -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the database to write to.
    database :: Prelude.Text,
    -- | The name of the table in the database to write to.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MySQLCatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'mySQLCatalogTarget_name' - The name of the data target.
--
-- 'inputs', 'mySQLCatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'database', 'mySQLCatalogTarget_database' - The name of the database to write to.
--
-- 'table', 'mySQLCatalogTarget_table' - The name of the table in the database to write to.
newMySQLCatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  MySQLCatalogTarget
newMySQLCatalogTarget
  pName_
  pInputs_
  pDatabase_
  pTable_ =
    MySQLCatalogTarget'
      { name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        database = pDatabase_,
        table = pTable_
      }

-- | The name of the data target.
mySQLCatalogTarget_name :: Lens.Lens' MySQLCatalogTarget Prelude.Text
mySQLCatalogTarget_name = Lens.lens (\MySQLCatalogTarget' {name} -> name) (\s@MySQLCatalogTarget' {} a -> s {name = a} :: MySQLCatalogTarget)

-- | The nodes that are inputs to the data target.
mySQLCatalogTarget_inputs :: Lens.Lens' MySQLCatalogTarget (Prelude.NonEmpty Prelude.Text)
mySQLCatalogTarget_inputs = Lens.lens (\MySQLCatalogTarget' {inputs} -> inputs) (\s@MySQLCatalogTarget' {} a -> s {inputs = a} :: MySQLCatalogTarget) Prelude.. Lens.coerced

-- | The name of the database to write to.
mySQLCatalogTarget_database :: Lens.Lens' MySQLCatalogTarget Prelude.Text
mySQLCatalogTarget_database = Lens.lens (\MySQLCatalogTarget' {database} -> database) (\s@MySQLCatalogTarget' {} a -> s {database = a} :: MySQLCatalogTarget)

-- | The name of the table in the database to write to.
mySQLCatalogTarget_table :: Lens.Lens' MySQLCatalogTarget Prelude.Text
mySQLCatalogTarget_table = Lens.lens (\MySQLCatalogTarget' {table} -> table) (\s@MySQLCatalogTarget' {} a -> s {table = a} :: MySQLCatalogTarget)

instance Data.FromJSON MySQLCatalogTarget where
  parseJSON =
    Data.withObject
      "MySQLCatalogTarget"
      ( \x ->
          MySQLCatalogTarget'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable MySQLCatalogTarget where
  hashWithSalt _salt MySQLCatalogTarget' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData MySQLCatalogTarget where
  rnf MySQLCatalogTarget' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON MySQLCatalogTarget where
  toJSON MySQLCatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
