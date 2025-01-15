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
-- Module      : Amazonka.Glue.Types.PostgreSQLCatalogTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PostgreSQLCatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that uses Postgres SQL.
--
-- /See:/ 'newPostgreSQLCatalogTarget' smart constructor.
data PostgreSQLCatalogTarget = PostgreSQLCatalogTarget'
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
-- Create a value of 'PostgreSQLCatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'postgreSQLCatalogTarget_name' - The name of the data target.
--
-- 'inputs', 'postgreSQLCatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'database', 'postgreSQLCatalogTarget_database' - The name of the database to write to.
--
-- 'table', 'postgreSQLCatalogTarget_table' - The name of the table in the database to write to.
newPostgreSQLCatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  PostgreSQLCatalogTarget
newPostgreSQLCatalogTarget
  pName_
  pInputs_
  pDatabase_
  pTable_ =
    PostgreSQLCatalogTarget'
      { name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        database = pDatabase_,
        table = pTable_
      }

-- | The name of the data target.
postgreSQLCatalogTarget_name :: Lens.Lens' PostgreSQLCatalogTarget Prelude.Text
postgreSQLCatalogTarget_name = Lens.lens (\PostgreSQLCatalogTarget' {name} -> name) (\s@PostgreSQLCatalogTarget' {} a -> s {name = a} :: PostgreSQLCatalogTarget)

-- | The nodes that are inputs to the data target.
postgreSQLCatalogTarget_inputs :: Lens.Lens' PostgreSQLCatalogTarget (Prelude.NonEmpty Prelude.Text)
postgreSQLCatalogTarget_inputs = Lens.lens (\PostgreSQLCatalogTarget' {inputs} -> inputs) (\s@PostgreSQLCatalogTarget' {} a -> s {inputs = a} :: PostgreSQLCatalogTarget) Prelude.. Lens.coerced

-- | The name of the database to write to.
postgreSQLCatalogTarget_database :: Lens.Lens' PostgreSQLCatalogTarget Prelude.Text
postgreSQLCatalogTarget_database = Lens.lens (\PostgreSQLCatalogTarget' {database} -> database) (\s@PostgreSQLCatalogTarget' {} a -> s {database = a} :: PostgreSQLCatalogTarget)

-- | The name of the table in the database to write to.
postgreSQLCatalogTarget_table :: Lens.Lens' PostgreSQLCatalogTarget Prelude.Text
postgreSQLCatalogTarget_table = Lens.lens (\PostgreSQLCatalogTarget' {table} -> table) (\s@PostgreSQLCatalogTarget' {} a -> s {table = a} :: PostgreSQLCatalogTarget)

instance Data.FromJSON PostgreSQLCatalogTarget where
  parseJSON =
    Data.withObject
      "PostgreSQLCatalogTarget"
      ( \x ->
          PostgreSQLCatalogTarget'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable PostgreSQLCatalogTarget where
  hashWithSalt _salt PostgreSQLCatalogTarget' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData PostgreSQLCatalogTarget where
  rnf PostgreSQLCatalogTarget' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf inputs `Prelude.seq`
        Prelude.rnf database `Prelude.seq`
          Prelude.rnf table

instance Data.ToJSON PostgreSQLCatalogTarget where
  toJSON PostgreSQLCatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
