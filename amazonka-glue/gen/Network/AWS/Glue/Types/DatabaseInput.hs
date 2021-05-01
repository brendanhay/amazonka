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
-- Module      : Network.AWS.Glue.Types.DatabaseInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseInput where

import Network.AWS.Glue.Types.DatabaseIdentifier
import Network.AWS.Glue.Types.PrincipalPermissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The structure used to create or update a database.
--
-- /See:/ 'newDatabaseInput' smart constructor.
data DatabaseInput = DatabaseInput'
  { -- | Creates a set of default permissions on the table for principals.
    createTableDefaultPermissions :: Prelude.Maybe [PrincipalPermissions],
    -- | A @DatabaseIdentifier@ structure that describes a target database for
    -- resource linking.
    targetDatabase :: Prelude.Maybe DatabaseIdentifier,
    -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | The location of the database (for example, an HDFS path).
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define parameters and properties of the database.
    --
    -- These key-value pairs define parameters and properties of the database.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the database. For Hive compatibility, this is folded to
    -- lowercase when it is stored.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DatabaseInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTableDefaultPermissions', 'databaseInput_createTableDefaultPermissions' - Creates a set of default permissions on the table for principals.
--
-- 'targetDatabase', 'databaseInput_targetDatabase' - A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
--
-- 'description', 'databaseInput_description' - A description of the database.
--
-- 'locationUri', 'databaseInput_locationUri' - The location of the database (for example, an HDFS path).
--
-- 'parameters', 'databaseInput_parameters' - These key-value pairs define parameters and properties of the database.
--
-- These key-value pairs define parameters and properties of the database.
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
      targetDatabase = Prelude.Nothing,
      description = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_
    }

-- | Creates a set of default permissions on the table for principals.
databaseInput_createTableDefaultPermissions :: Lens.Lens' DatabaseInput (Prelude.Maybe [PrincipalPermissions])
databaseInput_createTableDefaultPermissions = Lens.lens (\DatabaseInput' {createTableDefaultPermissions} -> createTableDefaultPermissions) (\s@DatabaseInput' {} a -> s {createTableDefaultPermissions = a} :: DatabaseInput) Prelude.. Lens.mapping Prelude._Coerce

-- | A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
databaseInput_targetDatabase :: Lens.Lens' DatabaseInput (Prelude.Maybe DatabaseIdentifier)
databaseInput_targetDatabase = Lens.lens (\DatabaseInput' {targetDatabase} -> targetDatabase) (\s@DatabaseInput' {} a -> s {targetDatabase = a} :: DatabaseInput)

-- | A description of the database.
databaseInput_description :: Lens.Lens' DatabaseInput (Prelude.Maybe Prelude.Text)
databaseInput_description = Lens.lens (\DatabaseInput' {description} -> description) (\s@DatabaseInput' {} a -> s {description = a} :: DatabaseInput)

-- | The location of the database (for example, an HDFS path).
databaseInput_locationUri :: Lens.Lens' DatabaseInput (Prelude.Maybe Prelude.Text)
databaseInput_locationUri = Lens.lens (\DatabaseInput' {locationUri} -> locationUri) (\s@DatabaseInput' {} a -> s {locationUri = a} :: DatabaseInput)

-- | These key-value pairs define parameters and properties of the database.
--
-- These key-value pairs define parameters and properties of the database.
databaseInput_parameters :: Lens.Lens' DatabaseInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
databaseInput_parameters = Lens.lens (\DatabaseInput' {parameters} -> parameters) (\s@DatabaseInput' {} a -> s {parameters = a} :: DatabaseInput) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the database. For Hive compatibility, this is folded to
-- lowercase when it is stored.
databaseInput_name :: Lens.Lens' DatabaseInput Prelude.Text
databaseInput_name = Lens.lens (\DatabaseInput' {name} -> name) (\s@DatabaseInput' {} a -> s {name = a} :: DatabaseInput)

instance Prelude.Hashable DatabaseInput

instance Prelude.NFData DatabaseInput

instance Prelude.ToJSON DatabaseInput where
  toJSON DatabaseInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CreateTableDefaultPermissions" Prelude..=)
              Prelude.<$> createTableDefaultPermissions,
            ("TargetDatabase" Prelude..=)
              Prelude.<$> targetDatabase,
            ("Description" Prelude..=) Prelude.<$> description,
            ("LocationUri" Prelude..=) Prelude.<$> locationUri,
            ("Parameters" Prelude..=) Prelude.<$> parameters,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )
