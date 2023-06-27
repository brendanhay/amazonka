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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an endpoint for a database.
--
-- /See:/ 'newRelationalDatabaseEndpoint' smart constructor.
data RelationalDatabaseEndpoint = RelationalDatabaseEndpoint'
  { -- | Specifies the DNS address of the database.
    address :: Prelude.Maybe Prelude.Text,
    -- | Specifies the port that the database is listening on.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'relationalDatabaseEndpoint_address' - Specifies the DNS address of the database.
--
-- 'port', 'relationalDatabaseEndpoint_port' - Specifies the port that the database is listening on.
newRelationalDatabaseEndpoint ::
  RelationalDatabaseEndpoint
newRelationalDatabaseEndpoint =
  RelationalDatabaseEndpoint'
    { address =
        Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | Specifies the DNS address of the database.
relationalDatabaseEndpoint_address :: Lens.Lens' RelationalDatabaseEndpoint (Prelude.Maybe Prelude.Text)
relationalDatabaseEndpoint_address = Lens.lens (\RelationalDatabaseEndpoint' {address} -> address) (\s@RelationalDatabaseEndpoint' {} a -> s {address = a} :: RelationalDatabaseEndpoint)

-- | Specifies the port that the database is listening on.
relationalDatabaseEndpoint_port :: Lens.Lens' RelationalDatabaseEndpoint (Prelude.Maybe Prelude.Int)
relationalDatabaseEndpoint_port = Lens.lens (\RelationalDatabaseEndpoint' {port} -> port) (\s@RelationalDatabaseEndpoint' {} a -> s {port = a} :: RelationalDatabaseEndpoint)

instance Data.FromJSON RelationalDatabaseEndpoint where
  parseJSON =
    Data.withObject
      "RelationalDatabaseEndpoint"
      ( \x ->
          RelationalDatabaseEndpoint'
            Prelude.<$> (x Data..:? "address")
            Prelude.<*> (x Data..:? "port")
      )

instance Prelude.Hashable RelationalDatabaseEndpoint where
  hashWithSalt _salt RelationalDatabaseEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` port

instance Prelude.NFData RelationalDatabaseEndpoint where
  rnf RelationalDatabaseEndpoint' {..} =
    Prelude.rnf address `Prelude.seq` Prelude.rnf port
