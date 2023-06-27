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
-- Module      : Amazonka.Glue.Types.DatabaseInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DatabaseInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DatabaseIdentifier
import Amazonka.Glue.Types.FederatedDatabase
import Amazonka.Glue.Types.PrincipalPermissions
import qualified Amazonka.Prelude as Prelude

-- | The structure used to create or update a database.
--
-- /See:/ 'newDatabaseInput' smart constructor.
data DatabaseInput = DatabaseInput'
  { -- | Creates a set of default permissions on the table for principals. Used
    -- by Lake Formation. Not used in the normal course of Glue operations.
    createTableDefaultPermissions :: Prelude.Maybe [PrincipalPermissions],
    -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | A @FederatedDatabase@ structure that references an entity outside the
    -- Glue Data Catalog.
    federatedDatabase :: Prelude.Maybe FederatedDatabase,
    -- | The location of the database (for example, an HDFS path).
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define parameters and properties of the database.
    --
    -- These key-value pairs define parameters and properties of the database.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A @DatabaseIdentifier@ structure that describes a target database for
    -- resource linking.
    targetDatabase :: Prelude.Maybe DatabaseIdentifier,
    -- | The name of the database. For Hive compatibility, this is folded to
    -- lowercase when it is stored.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTableDefaultPermissions', 'databaseInput_createTableDefaultPermissions' - Creates a set of default permissions on the table for principals. Used
-- by Lake Formation. Not used in the normal course of Glue operations.
--
-- 'description', 'databaseInput_description' - A description of the database.
--
-- 'federatedDatabase', 'databaseInput_federatedDatabase' - A @FederatedDatabase@ structure that references an entity outside the
-- Glue Data Catalog.
--
-- 'locationUri', 'databaseInput_locationUri' - The location of the database (for example, an HDFS path).
--
-- 'parameters', 'databaseInput_parameters' - These key-value pairs define parameters and properties of the database.
--
-- These key-value pairs define parameters and properties of the database.
--
-- 'targetDatabase', 'databaseInput_targetDatabase' - A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
--
-- 'name', 'databaseInput_name' - The name of the database. For Hive compatibility, this is folded to
-- lowercase when it is stored.
newDatabaseInput ::
  -- | 'name'
  Prelude.Text ->
  DatabaseInput
newDatabaseInput pName_ =
  DatabaseInput'
    { createTableDefaultPermissions =
        Prelude.Nothing,
      description = Prelude.Nothing,
      federatedDatabase = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      parameters = Prelude.Nothing,
      targetDatabase = Prelude.Nothing,
      name = pName_
    }

-- | Creates a set of default permissions on the table for principals. Used
-- by Lake Formation. Not used in the normal course of Glue operations.
databaseInput_createTableDefaultPermissions :: Lens.Lens' DatabaseInput (Prelude.Maybe [PrincipalPermissions])
databaseInput_createTableDefaultPermissions = Lens.lens (\DatabaseInput' {createTableDefaultPermissions} -> createTableDefaultPermissions) (\s@DatabaseInput' {} a -> s {createTableDefaultPermissions = a} :: DatabaseInput) Prelude.. Lens.mapping Lens.coerced

-- | A description of the database.
databaseInput_description :: Lens.Lens' DatabaseInput (Prelude.Maybe Prelude.Text)
databaseInput_description = Lens.lens (\DatabaseInput' {description} -> description) (\s@DatabaseInput' {} a -> s {description = a} :: DatabaseInput)

-- | A @FederatedDatabase@ structure that references an entity outside the
-- Glue Data Catalog.
databaseInput_federatedDatabase :: Lens.Lens' DatabaseInput (Prelude.Maybe FederatedDatabase)
databaseInput_federatedDatabase = Lens.lens (\DatabaseInput' {federatedDatabase} -> federatedDatabase) (\s@DatabaseInput' {} a -> s {federatedDatabase = a} :: DatabaseInput)

-- | The location of the database (for example, an HDFS path).
databaseInput_locationUri :: Lens.Lens' DatabaseInput (Prelude.Maybe Prelude.Text)
databaseInput_locationUri = Lens.lens (\DatabaseInput' {locationUri} -> locationUri) (\s@DatabaseInput' {} a -> s {locationUri = a} :: DatabaseInput)

-- | These key-value pairs define parameters and properties of the database.
--
-- These key-value pairs define parameters and properties of the database.
databaseInput_parameters :: Lens.Lens' DatabaseInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
databaseInput_parameters = Lens.lens (\DatabaseInput' {parameters} -> parameters) (\s@DatabaseInput' {} a -> s {parameters = a} :: DatabaseInput) Prelude.. Lens.mapping Lens.coerced

-- | A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
databaseInput_targetDatabase :: Lens.Lens' DatabaseInput (Prelude.Maybe DatabaseIdentifier)
databaseInput_targetDatabase = Lens.lens (\DatabaseInput' {targetDatabase} -> targetDatabase) (\s@DatabaseInput' {} a -> s {targetDatabase = a} :: DatabaseInput)

-- | The name of the database. For Hive compatibility, this is folded to
-- lowercase when it is stored.
databaseInput_name :: Lens.Lens' DatabaseInput Prelude.Text
databaseInput_name = Lens.lens (\DatabaseInput' {name} -> name) (\s@DatabaseInput' {} a -> s {name = a} :: DatabaseInput)

instance Prelude.Hashable DatabaseInput where
  hashWithSalt _salt DatabaseInput' {..} =
    _salt
      `Prelude.hashWithSalt` createTableDefaultPermissions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` federatedDatabase
      `Prelude.hashWithSalt` locationUri
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` targetDatabase
      `Prelude.hashWithSalt` name

instance Prelude.NFData DatabaseInput where
  rnf DatabaseInput' {..} =
    Prelude.rnf createTableDefaultPermissions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf federatedDatabase
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf targetDatabase
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON DatabaseInput where
  toJSON DatabaseInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreateTableDefaultPermissions" Data..=)
              Prelude.<$> createTableDefaultPermissions,
            ("Description" Data..=) Prelude.<$> description,
            ("FederatedDatabase" Data..=)
              Prelude.<$> federatedDatabase,
            ("LocationUri" Data..=) Prelude.<$> locationUri,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("TargetDatabase" Data..=)
              Prelude.<$> targetDatabase,
            Prelude.Just ("Name" Data..= name)
          ]
      )
