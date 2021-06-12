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

-- | The @Database@ object represents a logical grouping of tables that might
-- reside in a Hive metastore or an RDBMS.
--
-- /See:/ 'newDatabase' smart constructor.
data Database = Database'
  { -- | Creates a set of default permissions on the table for principals.
    createTableDefaultPermissions :: Core.Maybe [PrincipalPermissions],
    -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Core.Maybe Core.Text,
    -- | A @DatabaseIdentifier@ structure that describes a target database for
    -- resource linking.
    targetDatabase :: Core.Maybe DatabaseIdentifier,
    -- | The time at which the metadata database was created in the catalog.
    createTime :: Core.Maybe Core.POSIX,
    -- | A description of the database.
    description :: Core.Maybe Core.Text,
    -- | The location of the database (for example, an HDFS path).
    locationUri :: Core.Maybe Core.Text,
    -- | These key-value pairs define parameters and properties of the database.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the database. For Hive compatibility, this is folded to
    -- lowercase when it is stored.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  Database
newDatabase pName_ =
  Database'
    { createTableDefaultPermissions =
        Core.Nothing,
      catalogId = Core.Nothing,
      targetDatabase = Core.Nothing,
      createTime = Core.Nothing,
      description = Core.Nothing,
      locationUri = Core.Nothing,
      parameters = Core.Nothing,
      name = pName_
    }

-- | Creates a set of default permissions on the table for principals.
database_createTableDefaultPermissions :: Lens.Lens' Database (Core.Maybe [PrincipalPermissions])
database_createTableDefaultPermissions = Lens.lens (\Database' {createTableDefaultPermissions} -> createTableDefaultPermissions) (\s@Database' {} a -> s {createTableDefaultPermissions = a} :: Database) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Data Catalog in which the database resides.
database_catalogId :: Lens.Lens' Database (Core.Maybe Core.Text)
database_catalogId = Lens.lens (\Database' {catalogId} -> catalogId) (\s@Database' {} a -> s {catalogId = a} :: Database)

-- | A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
database_targetDatabase :: Lens.Lens' Database (Core.Maybe DatabaseIdentifier)
database_targetDatabase = Lens.lens (\Database' {targetDatabase} -> targetDatabase) (\s@Database' {} a -> s {targetDatabase = a} :: Database)

-- | The time at which the metadata database was created in the catalog.
database_createTime :: Lens.Lens' Database (Core.Maybe Core.UTCTime)
database_createTime = Lens.lens (\Database' {createTime} -> createTime) (\s@Database' {} a -> s {createTime = a} :: Database) Core.. Lens.mapping Core._Time

-- | A description of the database.
database_description :: Lens.Lens' Database (Core.Maybe Core.Text)
database_description = Lens.lens (\Database' {description} -> description) (\s@Database' {} a -> s {description = a} :: Database)

-- | The location of the database (for example, an HDFS path).
database_locationUri :: Lens.Lens' Database (Core.Maybe Core.Text)
database_locationUri = Lens.lens (\Database' {locationUri} -> locationUri) (\s@Database' {} a -> s {locationUri = a} :: Database)

-- | These key-value pairs define parameters and properties of the database.
database_parameters :: Lens.Lens' Database (Core.Maybe (Core.HashMap Core.Text Core.Text))
database_parameters = Lens.lens (\Database' {parameters} -> parameters) (\s@Database' {} a -> s {parameters = a} :: Database) Core.. Lens.mapping Lens._Coerce

-- | The name of the database. For Hive compatibility, this is folded to
-- lowercase when it is stored.
database_name :: Lens.Lens' Database Core.Text
database_name = Lens.lens (\Database' {name} -> name) (\s@Database' {} a -> s {name = a} :: Database)

instance Core.FromJSON Database where
  parseJSON =
    Core.withObject
      "Database"
      ( \x ->
          Database'
            Core.<$> ( x Core..:? "CreateTableDefaultPermissions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "CatalogId")
            Core.<*> (x Core..:? "TargetDatabase")
            Core.<*> (x Core..:? "CreateTime")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "LocationUri")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable Database

instance Core.NFData Database
