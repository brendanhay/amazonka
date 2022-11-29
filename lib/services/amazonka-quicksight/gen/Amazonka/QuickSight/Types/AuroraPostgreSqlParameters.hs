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
-- Module      : Amazonka.QuickSight.Types.AuroraPostgreSqlParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AuroraPostgreSqlParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Parameters for Amazon Aurora PostgreSQL-Compatible Edition.
--
-- /See:/ 'newAuroraPostgreSqlParameters' smart constructor.
data AuroraPostgreSqlParameters = AuroraPostgreSqlParameters'
  { -- | The Amazon Aurora PostgreSQL-Compatible host to connect to.
    host :: Prelude.Text,
    -- | The port that Amazon Aurora PostgreSQL is listening on.
    port :: Prelude.Natural,
    -- | The Amazon Aurora PostgreSQL database to connect to.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuroraPostgreSqlParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'auroraPostgreSqlParameters_host' - The Amazon Aurora PostgreSQL-Compatible host to connect to.
--
-- 'port', 'auroraPostgreSqlParameters_port' - The port that Amazon Aurora PostgreSQL is listening on.
--
-- 'database', 'auroraPostgreSqlParameters_database' - The Amazon Aurora PostgreSQL database to connect to.
newAuroraPostgreSqlParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'database'
  Prelude.Text ->
  AuroraPostgreSqlParameters
newAuroraPostgreSqlParameters
  pHost_
  pPort_
  pDatabase_ =
    AuroraPostgreSqlParameters'
      { host = pHost_,
        port = pPort_,
        database = pDatabase_
      }

-- | The Amazon Aurora PostgreSQL-Compatible host to connect to.
auroraPostgreSqlParameters_host :: Lens.Lens' AuroraPostgreSqlParameters Prelude.Text
auroraPostgreSqlParameters_host = Lens.lens (\AuroraPostgreSqlParameters' {host} -> host) (\s@AuroraPostgreSqlParameters' {} a -> s {host = a} :: AuroraPostgreSqlParameters)

-- | The port that Amazon Aurora PostgreSQL is listening on.
auroraPostgreSqlParameters_port :: Lens.Lens' AuroraPostgreSqlParameters Prelude.Natural
auroraPostgreSqlParameters_port = Lens.lens (\AuroraPostgreSqlParameters' {port} -> port) (\s@AuroraPostgreSqlParameters' {} a -> s {port = a} :: AuroraPostgreSqlParameters)

-- | The Amazon Aurora PostgreSQL database to connect to.
auroraPostgreSqlParameters_database :: Lens.Lens' AuroraPostgreSqlParameters Prelude.Text
auroraPostgreSqlParameters_database = Lens.lens (\AuroraPostgreSqlParameters' {database} -> database) (\s@AuroraPostgreSqlParameters' {} a -> s {database = a} :: AuroraPostgreSqlParameters)

instance Core.FromJSON AuroraPostgreSqlParameters where
  parseJSON =
    Core.withObject
      "AuroraPostgreSqlParameters"
      ( \x ->
          AuroraPostgreSqlParameters'
            Prelude.<$> (x Core..: "Host")
            Prelude.<*> (x Core..: "Port")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable AuroraPostgreSqlParameters where
  hashWithSalt _salt AuroraPostgreSqlParameters' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` database

instance Prelude.NFData AuroraPostgreSqlParameters where
  rnf AuroraPostgreSqlParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf database

instance Core.ToJSON AuroraPostgreSqlParameters where
  toJSON AuroraPostgreSqlParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Core..= host),
            Prelude.Just ("Port" Core..= port),
            Prelude.Just ("Database" Core..= database)
          ]
      )
