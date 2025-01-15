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
-- Module      : Amazonka.Glue.Types.OracleSQLCatalogTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.OracleSQLCatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that uses Oracle SQL.
--
-- /See:/ 'newOracleSQLCatalogTarget' smart constructor.
data OracleSQLCatalogTarget = OracleSQLCatalogTarget'
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
-- Create a value of 'OracleSQLCatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'oracleSQLCatalogTarget_name' - The name of the data target.
--
-- 'inputs', 'oracleSQLCatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'database', 'oracleSQLCatalogTarget_database' - The name of the database to write to.
--
-- 'table', 'oracleSQLCatalogTarget_table' - The name of the table in the database to write to.
newOracleSQLCatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  OracleSQLCatalogTarget
newOracleSQLCatalogTarget
  pName_
  pInputs_
  pDatabase_
  pTable_ =
    OracleSQLCatalogTarget'
      { name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        database = pDatabase_,
        table = pTable_
      }

-- | The name of the data target.
oracleSQLCatalogTarget_name :: Lens.Lens' OracleSQLCatalogTarget Prelude.Text
oracleSQLCatalogTarget_name = Lens.lens (\OracleSQLCatalogTarget' {name} -> name) (\s@OracleSQLCatalogTarget' {} a -> s {name = a} :: OracleSQLCatalogTarget)

-- | The nodes that are inputs to the data target.
oracleSQLCatalogTarget_inputs :: Lens.Lens' OracleSQLCatalogTarget (Prelude.NonEmpty Prelude.Text)
oracleSQLCatalogTarget_inputs = Lens.lens (\OracleSQLCatalogTarget' {inputs} -> inputs) (\s@OracleSQLCatalogTarget' {} a -> s {inputs = a} :: OracleSQLCatalogTarget) Prelude.. Lens.coerced

-- | The name of the database to write to.
oracleSQLCatalogTarget_database :: Lens.Lens' OracleSQLCatalogTarget Prelude.Text
oracleSQLCatalogTarget_database = Lens.lens (\OracleSQLCatalogTarget' {database} -> database) (\s@OracleSQLCatalogTarget' {} a -> s {database = a} :: OracleSQLCatalogTarget)

-- | The name of the table in the database to write to.
oracleSQLCatalogTarget_table :: Lens.Lens' OracleSQLCatalogTarget Prelude.Text
oracleSQLCatalogTarget_table = Lens.lens (\OracleSQLCatalogTarget' {table} -> table) (\s@OracleSQLCatalogTarget' {} a -> s {table = a} :: OracleSQLCatalogTarget)

instance Data.FromJSON OracleSQLCatalogTarget where
  parseJSON =
    Data.withObject
      "OracleSQLCatalogTarget"
      ( \x ->
          OracleSQLCatalogTarget'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable OracleSQLCatalogTarget where
  hashWithSalt _salt OracleSQLCatalogTarget' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData OracleSQLCatalogTarget where
  rnf OracleSQLCatalogTarget' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf inputs `Prelude.seq`
        Prelude.rnf database `Prelude.seq`
          Prelude.rnf table

instance Data.ToJSON OracleSQLCatalogTarget where
  toJSON OracleSQLCatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
