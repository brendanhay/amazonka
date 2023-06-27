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
-- Module      : Amazonka.Glue.Types.DirectJDBCSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DirectJDBCSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.JDBCConnectionType
import qualified Amazonka.Prelude as Prelude

-- | Specifies the direct JDBC source connection.
--
-- /See:/ 'newDirectJDBCSource' smart constructor.
data DirectJDBCSource = DirectJDBCSource'
  { -- | The temp directory of the JDBC Redshift source.
    redshiftTmpDir :: Prelude.Maybe Prelude.Text,
    -- | The name of the JDBC source connection.
    name :: Prelude.Text,
    -- | The database of the JDBC source connection.
    database :: Prelude.Text,
    -- | The table of the JDBC source connection.
    table :: Prelude.Text,
    -- | The connection name of the JDBC source.
    connectionName :: Prelude.Text,
    -- | The connection type of the JDBC source.
    connectionType :: JDBCConnectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectJDBCSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redshiftTmpDir', 'directJDBCSource_redshiftTmpDir' - The temp directory of the JDBC Redshift source.
--
-- 'name', 'directJDBCSource_name' - The name of the JDBC source connection.
--
-- 'database', 'directJDBCSource_database' - The database of the JDBC source connection.
--
-- 'table', 'directJDBCSource_table' - The table of the JDBC source connection.
--
-- 'connectionName', 'directJDBCSource_connectionName' - The connection name of the JDBC source.
--
-- 'connectionType', 'directJDBCSource_connectionType' - The connection type of the JDBC source.
newDirectJDBCSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  -- | 'connectionName'
  Prelude.Text ->
  -- | 'connectionType'
  JDBCConnectionType ->
  DirectJDBCSource
newDirectJDBCSource
  pName_
  pDatabase_
  pTable_
  pConnectionName_
  pConnectionType_ =
    DirectJDBCSource'
      { redshiftTmpDir = Prelude.Nothing,
        name = pName_,
        database = pDatabase_,
        table = pTable_,
        connectionName = pConnectionName_,
        connectionType = pConnectionType_
      }

-- | The temp directory of the JDBC Redshift source.
directJDBCSource_redshiftTmpDir :: Lens.Lens' DirectJDBCSource (Prelude.Maybe Prelude.Text)
directJDBCSource_redshiftTmpDir = Lens.lens (\DirectJDBCSource' {redshiftTmpDir} -> redshiftTmpDir) (\s@DirectJDBCSource' {} a -> s {redshiftTmpDir = a} :: DirectJDBCSource)

-- | The name of the JDBC source connection.
directJDBCSource_name :: Lens.Lens' DirectJDBCSource Prelude.Text
directJDBCSource_name = Lens.lens (\DirectJDBCSource' {name} -> name) (\s@DirectJDBCSource' {} a -> s {name = a} :: DirectJDBCSource)

-- | The database of the JDBC source connection.
directJDBCSource_database :: Lens.Lens' DirectJDBCSource Prelude.Text
directJDBCSource_database = Lens.lens (\DirectJDBCSource' {database} -> database) (\s@DirectJDBCSource' {} a -> s {database = a} :: DirectJDBCSource)

-- | The table of the JDBC source connection.
directJDBCSource_table :: Lens.Lens' DirectJDBCSource Prelude.Text
directJDBCSource_table = Lens.lens (\DirectJDBCSource' {table} -> table) (\s@DirectJDBCSource' {} a -> s {table = a} :: DirectJDBCSource)

-- | The connection name of the JDBC source.
directJDBCSource_connectionName :: Lens.Lens' DirectJDBCSource Prelude.Text
directJDBCSource_connectionName = Lens.lens (\DirectJDBCSource' {connectionName} -> connectionName) (\s@DirectJDBCSource' {} a -> s {connectionName = a} :: DirectJDBCSource)

-- | The connection type of the JDBC source.
directJDBCSource_connectionType :: Lens.Lens' DirectJDBCSource JDBCConnectionType
directJDBCSource_connectionType = Lens.lens (\DirectJDBCSource' {connectionType} -> connectionType) (\s@DirectJDBCSource' {} a -> s {connectionType = a} :: DirectJDBCSource)

instance Data.FromJSON DirectJDBCSource where
  parseJSON =
    Data.withObject
      "DirectJDBCSource"
      ( \x ->
          DirectJDBCSource'
            Prelude.<$> (x Data..:? "RedshiftTmpDir")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
            Prelude.<*> (x Data..: "ConnectionName")
            Prelude.<*> (x Data..: "ConnectionType")
      )

instance Prelude.Hashable DirectJDBCSource where
  hashWithSalt _salt DirectJDBCSource' {..} =
    _salt
      `Prelude.hashWithSalt` redshiftTmpDir
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` connectionType

instance Prelude.NFData DirectJDBCSource where
  rnf DirectJDBCSource' {..} =
    Prelude.rnf redshiftTmpDir
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf connectionType

instance Data.ToJSON DirectJDBCSource where
  toJSON DirectJDBCSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RedshiftTmpDir" Data..=)
              Prelude.<$> redshiftTmpDir,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table),
            Prelude.Just
              ("ConnectionName" Data..= connectionName),
            Prelude.Just
              ("ConnectionType" Data..= connectionType)
          ]
      )
