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
-- Module      : Amazonka.QuickSight.Types.OracleParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.OracleParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for Oracle.
--
-- /See:/ 'newOracleParameters' smart constructor.
data OracleParameters = OracleParameters'
  { -- | An Oracle host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OracleParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'oracleParameters_host' - An Oracle host.
--
-- 'port', 'oracleParameters_port' - Port.
--
-- 'database', 'oracleParameters_database' - Database.
newOracleParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'database'
  Prelude.Text ->
  OracleParameters
newOracleParameters pHost_ pPort_ pDatabase_ =
  OracleParameters'
    { host = pHost_,
      port = pPort_,
      database = pDatabase_
    }

-- | An Oracle host.
oracleParameters_host :: Lens.Lens' OracleParameters Prelude.Text
oracleParameters_host = Lens.lens (\OracleParameters' {host} -> host) (\s@OracleParameters' {} a -> s {host = a} :: OracleParameters)

-- | Port.
oracleParameters_port :: Lens.Lens' OracleParameters Prelude.Natural
oracleParameters_port = Lens.lens (\OracleParameters' {port} -> port) (\s@OracleParameters' {} a -> s {port = a} :: OracleParameters)

-- | Database.
oracleParameters_database :: Lens.Lens' OracleParameters Prelude.Text
oracleParameters_database = Lens.lens (\OracleParameters' {database} -> database) (\s@OracleParameters' {} a -> s {database = a} :: OracleParameters)

instance Data.FromJSON OracleParameters where
  parseJSON =
    Data.withObject
      "OracleParameters"
      ( \x ->
          OracleParameters'
            Prelude.<$> (x Data..: "Host")
            Prelude.<*> (x Data..: "Port")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable OracleParameters where
  hashWithSalt _salt OracleParameters' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` database

instance Prelude.NFData OracleParameters where
  rnf OracleParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON OracleParameters where
  toJSON OracleParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port),
            Prelude.Just ("Database" Data..= database)
          ]
      )
