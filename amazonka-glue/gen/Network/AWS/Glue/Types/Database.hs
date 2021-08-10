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
-- Module      : Network.AWS.Glue.Types.Database
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Database where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.DatabaseIdentifier
import Network.AWS.Glue.Types.PrincipalPermissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @Database@ object represents a logical grouping of tables that might
-- reside in a Hive metastore or an RDBMS.
--
-- /See:/ 'newDatabase' smart constructor.
data Database = Database'
  { -- | Creates a set of default permissions on the table for principals.
    createTableDefaultPermissions :: Prelude.Maybe [PrincipalPermissions],
    -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A @DatabaseIdentifier@ structure that describes a target database for
    -- resource linking.
    targetDatabase :: Prelude.Maybe DatabaseIdentifier,
    -- | The time at which the metadata database was created in the catalog.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | The location of the database (for example, an HDFS path).
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define parameters and properties of the database.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the database. For Hive compatibility, this is folded to
    -- lowercase when it is stored.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Database' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTableDefaultPermissions', 'database_createTableDefaultPermissions' - Creates a set of default permissions on the table for principals.
--
-- 'catalogId', 'database_catalogId' - The ID of the Data Catalog in which the database resides.
--
-- 'targetDatabase', 'database_targetDatabase' - A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
--
-- 'createTime', 'database_createTime' - The time at which the metadata database was created in the catalog.
--
-- 'description', 'database_description' - A description of the database.
--
-- 'locationUri', 'database_locationUri' - The location of the database (for example, an HDFS path).
--
-- 'parameters', 'database_parameters' - These key-value pairs define parameters and properties of the database.
--
-- 'name', 'database_name' - The name of the database. For Hive compatibility, this is folded to
-- lowercase when it is stored.
newDatabase ::
  -- | 'name'
  Prelude.Text ->
  Database
newDatabase pName_ =
  Database'
    { createTableDefaultPermissions =
        Prelude.Nothing,
      catalogId = Prelude.Nothing,
      targetDatabase = Prelude.Nothing,
      createTime = Prelude.Nothing,
      description = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_
    }

-- | Creates a set of default permissions on the table for principals.
database_createTableDefaultPermissions :: Lens.Lens' Database (Prelude.Maybe [PrincipalPermissions])
database_createTableDefaultPermissions = Lens.lens (\Database' {createTableDefaultPermissions} -> createTableDefaultPermissions) (\s@Database' {} a -> s {createTableDefaultPermissions = a} :: Database) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the Data Catalog in which the database resides.
database_catalogId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_catalogId = Lens.lens (\Database' {catalogId} -> catalogId) (\s@Database' {} a -> s {catalogId = a} :: Database)

-- | A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
database_targetDatabase :: Lens.Lens' Database (Prelude.Maybe DatabaseIdentifier)
database_targetDatabase = Lens.lens (\Database' {targetDatabase} -> targetDatabase) (\s@Database' {} a -> s {targetDatabase = a} :: Database)

-- | The time at which the metadata database was created in the catalog.
database_createTime :: Lens.Lens' Database (Prelude.Maybe Prelude.UTCTime)
database_createTime = Lens.lens (\Database' {createTime} -> createTime) (\s@Database' {} a -> s {createTime = a} :: Database) Prelude.. Lens.mapping Core._Time

-- | A description of the database.
database_description :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_description = Lens.lens (\Database' {description} -> description) (\s@Database' {} a -> s {description = a} :: Database)

-- | The location of the database (for example, an HDFS path).
database_locationUri :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_locationUri = Lens.lens (\Database' {locationUri} -> locationUri) (\s@Database' {} a -> s {locationUri = a} :: Database)

-- | These key-value pairs define parameters and properties of the database.
database_parameters :: Lens.Lens' Database (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
database_parameters = Lens.lens (\Database' {parameters} -> parameters) (\s@Database' {} a -> s {parameters = a} :: Database) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the database. For Hive compatibility, this is folded to
-- lowercase when it is stored.
database_name :: Lens.Lens' Database Prelude.Text
database_name = Lens.lens (\Database' {name} -> name) (\s@Database' {} a -> s {name = a} :: Database)

instance Core.FromJSON Database where
  parseJSON =
    Core.withObject
      "Database"
      ( \x ->
          Database'
            Prelude.<$> ( x Core..:? "CreateTableDefaultPermissions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..:? "TargetDatabase")
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LocationUri")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable Database

instance Prelude.NFData Database
