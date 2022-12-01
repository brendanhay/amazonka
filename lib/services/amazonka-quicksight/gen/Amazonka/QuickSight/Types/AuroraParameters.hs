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
-- Module      : Amazonka.QuickSight.Types.AuroraParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AuroraParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Parameters for Amazon Aurora.
--
-- /See:/ 'newAuroraParameters' smart constructor.
data AuroraParameters = AuroraParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuroraParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'auroraParameters_host' - Host.
--
-- 'port', 'auroraParameters_port' - Port.
--
-- 'database', 'auroraParameters_database' - Database.
newAuroraParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'database'
  Prelude.Text ->
  AuroraParameters
newAuroraParameters pHost_ pPort_ pDatabase_ =
  AuroraParameters'
    { host = pHost_,
      port = pPort_,
      database = pDatabase_
    }

-- | Host.
auroraParameters_host :: Lens.Lens' AuroraParameters Prelude.Text
auroraParameters_host = Lens.lens (\AuroraParameters' {host} -> host) (\s@AuroraParameters' {} a -> s {host = a} :: AuroraParameters)

-- | Port.
auroraParameters_port :: Lens.Lens' AuroraParameters Prelude.Natural
auroraParameters_port = Lens.lens (\AuroraParameters' {port} -> port) (\s@AuroraParameters' {} a -> s {port = a} :: AuroraParameters)

-- | Database.
auroraParameters_database :: Lens.Lens' AuroraParameters Prelude.Text
auroraParameters_database = Lens.lens (\AuroraParameters' {database} -> database) (\s@AuroraParameters' {} a -> s {database = a} :: AuroraParameters)

instance Core.FromJSON AuroraParameters where
  parseJSON =
    Core.withObject
      "AuroraParameters"
      ( \x ->
          AuroraParameters'
            Prelude.<$> (x Core..: "Host")
            Prelude.<*> (x Core..: "Port")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable AuroraParameters where
  hashWithSalt _salt AuroraParameters' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` database

instance Prelude.NFData AuroraParameters where
  rnf AuroraParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf database

instance Core.ToJSON AuroraParameters where
  toJSON AuroraParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Core..= host),
            Prelude.Just ("Port" Core..= port),
            Prelude.Just ("Database" Core..= database)
          ]
      )
