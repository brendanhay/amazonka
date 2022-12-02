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
-- Module      : Amazonka.Glue.Types.Database
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Database where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DatabaseIdentifier
import Amazonka.Glue.Types.PrincipalPermissions
import qualified Amazonka.Prelude as Prelude

-- | The @Database@ object represents a logical grouping of tables that might
-- reside in a Hive metastore or an RDBMS.
--
-- /See:/ 'newDatabase' smart constructor.
data Database = Database'
  { -- | A @DatabaseIdentifier@ structure that describes a target database for
    -- resource linking.
    targetDatabase :: Prelude.Maybe DatabaseIdentifier,
    -- | A description of the database.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The location of the database (for example, an HDFS path).
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The time at which the metadata database was created in the catalog.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | Creates a set of default permissions on the table for principals.
    createTableDefaultPermissions :: Prelude.Maybe [PrincipalPermissions],
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
-- 'targetDatabase', 'database_targetDatabase' - A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
--
-- 'description', 'database_description' - A description of the database.
--
-- 'catalogId', 'database_catalogId' - The ID of the Data Catalog in which the database resides.
--
-- 'locationUri', 'database_locationUri' - The location of the database (for example, an HDFS path).
--
-- 'createTime', 'database_createTime' - The time at which the metadata database was created in the catalog.
--
-- 'createTableDefaultPermissions', 'database_createTableDefaultPermissions' - Creates a set of default permissions on the table for principals.
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
    { targetDatabase = Prelude.Nothing,
      description = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      createTime = Prelude.Nothing,
      createTableDefaultPermissions = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_
    }

-- | A @DatabaseIdentifier@ structure that describes a target database for
-- resource linking.
database_targetDatabase :: Lens.Lens' Database (Prelude.Maybe DatabaseIdentifier)
database_targetDatabase = Lens.lens (\Database' {targetDatabase} -> targetDatabase) (\s@Database' {} a -> s {targetDatabase = a} :: Database)

-- | A description of the database.
database_description :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_description = Lens.lens (\Database' {description} -> description) (\s@Database' {} a -> s {description = a} :: Database)

-- | The ID of the Data Catalog in which the database resides.
database_catalogId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_catalogId = Lens.lens (\Database' {catalogId} -> catalogId) (\s@Database' {} a -> s {catalogId = a} :: Database)

-- | The location of the database (for example, an HDFS path).
database_locationUri :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_locationUri = Lens.lens (\Database' {locationUri} -> locationUri) (\s@Database' {} a -> s {locationUri = a} :: Database)

-- | The time at which the metadata database was created in the catalog.
database_createTime :: Lens.Lens' Database (Prelude.Maybe Prelude.UTCTime)
database_createTime = Lens.lens (\Database' {createTime} -> createTime) (\s@Database' {} a -> s {createTime = a} :: Database) Prelude.. Lens.mapping Data._Time

-- | Creates a set of default permissions on the table for principals.
database_createTableDefaultPermissions :: Lens.Lens' Database (Prelude.Maybe [PrincipalPermissions])
database_createTableDefaultPermissions = Lens.lens (\Database' {createTableDefaultPermissions} -> createTableDefaultPermissions) (\s@Database' {} a -> s {createTableDefaultPermissions = a} :: Database) Prelude.. Lens.mapping Lens.coerced

-- | These key-value pairs define parameters and properties of the database.
database_parameters :: Lens.Lens' Database (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
database_parameters = Lens.lens (\Database' {parameters} -> parameters) (\s@Database' {} a -> s {parameters = a} :: Database) Prelude.. Lens.mapping Lens.coerced

-- | The name of the database. For Hive compatibility, this is folded to
-- lowercase when it is stored.
database_name :: Lens.Lens' Database Prelude.Text
database_name = Lens.lens (\Database' {name} -> name) (\s@Database' {} a -> s {name = a} :: Database)

instance Data.FromJSON Database where
  parseJSON =
    Data.withObject
      "Database"
      ( \x ->
          Database'
            Prelude.<$> (x Data..:? "TargetDatabase")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..:? "LocationUri")
            Prelude.<*> (x Data..:? "CreateTime")
            Prelude.<*> ( x Data..:? "CreateTableDefaultPermissions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Database where
  hashWithSalt _salt Database' {..} =
    _salt `Prelude.hashWithSalt` targetDatabase
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` locationUri
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` createTableDefaultPermissions
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` name

instance Prelude.NFData Database where
  rnf Database' {..} =
    Prelude.rnf targetDatabase
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf createTableDefaultPermissions
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf name
