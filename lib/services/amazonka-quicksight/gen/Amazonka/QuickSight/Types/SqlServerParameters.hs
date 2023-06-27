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
-- Module      : Amazonka.QuickSight.Types.SqlServerParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SqlServerParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for SQL Server.
--
-- /See:/ 'newSqlServerParameters' smart constructor.
data SqlServerParameters = SqlServerParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlServerParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'sqlServerParameters_host' - Host.
--
-- 'port', 'sqlServerParameters_port' - Port.
--
-- 'database', 'sqlServerParameters_database' - Database.
newSqlServerParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'database'
  Prelude.Text ->
  SqlServerParameters
newSqlServerParameters pHost_ pPort_ pDatabase_ =
  SqlServerParameters'
    { host = pHost_,
      port = pPort_,
      database = pDatabase_
    }

-- | Host.
sqlServerParameters_host :: Lens.Lens' SqlServerParameters Prelude.Text
sqlServerParameters_host = Lens.lens (\SqlServerParameters' {host} -> host) (\s@SqlServerParameters' {} a -> s {host = a} :: SqlServerParameters)

-- | Port.
sqlServerParameters_port :: Lens.Lens' SqlServerParameters Prelude.Natural
sqlServerParameters_port = Lens.lens (\SqlServerParameters' {port} -> port) (\s@SqlServerParameters' {} a -> s {port = a} :: SqlServerParameters)

-- | Database.
sqlServerParameters_database :: Lens.Lens' SqlServerParameters Prelude.Text
sqlServerParameters_database = Lens.lens (\SqlServerParameters' {database} -> database) (\s@SqlServerParameters' {} a -> s {database = a} :: SqlServerParameters)

instance Data.FromJSON SqlServerParameters where
  parseJSON =
    Data.withObject
      "SqlServerParameters"
      ( \x ->
          SqlServerParameters'
            Prelude.<$> (x Data..: "Host")
            Prelude.<*> (x Data..: "Port")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable SqlServerParameters where
  hashWithSalt _salt SqlServerParameters' {..} =
    _salt
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` database

instance Prelude.NFData SqlServerParameters where
  rnf SqlServerParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON SqlServerParameters where
  toJSON SqlServerParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port),
            Prelude.Just ("Database" Data..= database)
          ]
      )
