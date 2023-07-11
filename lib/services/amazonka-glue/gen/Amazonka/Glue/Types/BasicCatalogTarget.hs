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
-- Module      : Amazonka.Glue.Types.BasicCatalogTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BasicCatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that uses a Glue Data Catalog table.
--
-- /See:/ 'newBasicCatalogTarget' smart constructor.
data BasicCatalogTarget = BasicCatalogTarget'
  { -- | The name of your data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The database that contains the table you want to use as the target. This
    -- database must already exist in the Data Catalog.
    database :: Prelude.Text,
    -- | The table that defines the schema of your output data. This table must
    -- already exist in the Data Catalog.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BasicCatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'basicCatalogTarget_name' - The name of your data target.
--
-- 'inputs', 'basicCatalogTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'database', 'basicCatalogTarget_database' - The database that contains the table you want to use as the target. This
-- database must already exist in the Data Catalog.
--
-- 'table', 'basicCatalogTarget_table' - The table that defines the schema of your output data. This table must
-- already exist in the Data Catalog.
newBasicCatalogTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  BasicCatalogTarget
newBasicCatalogTarget
  pName_
  pInputs_
  pDatabase_
  pTable_ =
    BasicCatalogTarget'
      { name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        database = pDatabase_,
        table = pTable_
      }

-- | The name of your data target.
basicCatalogTarget_name :: Lens.Lens' BasicCatalogTarget Prelude.Text
basicCatalogTarget_name = Lens.lens (\BasicCatalogTarget' {name} -> name) (\s@BasicCatalogTarget' {} a -> s {name = a} :: BasicCatalogTarget)

-- | The nodes that are inputs to the data target.
basicCatalogTarget_inputs :: Lens.Lens' BasicCatalogTarget (Prelude.NonEmpty Prelude.Text)
basicCatalogTarget_inputs = Lens.lens (\BasicCatalogTarget' {inputs} -> inputs) (\s@BasicCatalogTarget' {} a -> s {inputs = a} :: BasicCatalogTarget) Prelude.. Lens.coerced

-- | The database that contains the table you want to use as the target. This
-- database must already exist in the Data Catalog.
basicCatalogTarget_database :: Lens.Lens' BasicCatalogTarget Prelude.Text
basicCatalogTarget_database = Lens.lens (\BasicCatalogTarget' {database} -> database) (\s@BasicCatalogTarget' {} a -> s {database = a} :: BasicCatalogTarget)

-- | The table that defines the schema of your output data. This table must
-- already exist in the Data Catalog.
basicCatalogTarget_table :: Lens.Lens' BasicCatalogTarget Prelude.Text
basicCatalogTarget_table = Lens.lens (\BasicCatalogTarget' {table} -> table) (\s@BasicCatalogTarget' {} a -> s {table = a} :: BasicCatalogTarget)

instance Data.FromJSON BasicCatalogTarget where
  parseJSON =
    Data.withObject
      "BasicCatalogTarget"
      ( \x ->
          BasicCatalogTarget'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable BasicCatalogTarget where
  hashWithSalt _salt BasicCatalogTarget' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData BasicCatalogTarget where
  rnf BasicCatalogTarget' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON BasicCatalogTarget where
  toJSON BasicCatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
