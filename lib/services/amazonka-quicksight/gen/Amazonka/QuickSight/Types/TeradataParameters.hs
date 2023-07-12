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
-- Module      : Amazonka.QuickSight.Types.TeradataParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TeradataParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for Teradata.
--
-- /See:/ 'newTeradataParameters' smart constructor.
data TeradataParameters = TeradataParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TeradataParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'teradataParameters_host' - Host.
--
-- 'port', 'teradataParameters_port' - Port.
--
-- 'database', 'teradataParameters_database' - Database.
newTeradataParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'database'
  Prelude.Text ->
  TeradataParameters
newTeradataParameters pHost_ pPort_ pDatabase_ =
  TeradataParameters'
    { host = pHost_,
      port = pPort_,
      database = pDatabase_
    }

-- | Host.
teradataParameters_host :: Lens.Lens' TeradataParameters Prelude.Text
teradataParameters_host = Lens.lens (\TeradataParameters' {host} -> host) (\s@TeradataParameters' {} a -> s {host = a} :: TeradataParameters)

-- | Port.
teradataParameters_port :: Lens.Lens' TeradataParameters Prelude.Natural
teradataParameters_port = Lens.lens (\TeradataParameters' {port} -> port) (\s@TeradataParameters' {} a -> s {port = a} :: TeradataParameters)

-- | Database.
teradataParameters_database :: Lens.Lens' TeradataParameters Prelude.Text
teradataParameters_database = Lens.lens (\TeradataParameters' {database} -> database) (\s@TeradataParameters' {} a -> s {database = a} :: TeradataParameters)

instance Data.FromJSON TeradataParameters where
  parseJSON =
    Data.withObject
      "TeradataParameters"
      ( \x ->
          TeradataParameters'
            Prelude.<$> (x Data..: "Host")
            Prelude.<*> (x Data..: "Port")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable TeradataParameters where
  hashWithSalt _salt TeradataParameters' {..} =
    _salt
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` database

instance Prelude.NFData TeradataParameters where
  rnf TeradataParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON TeradataParameters where
  toJSON TeradataParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port),
            Prelude.Just ("Database" Data..= database)
          ]
      )
