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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an endpoint for a database.
--
-- /See:/ 'newRelationalDatabaseEndpoint' smart constructor.
data RelationalDatabaseEndpoint = RelationalDatabaseEndpoint'
  { -- | Specifies the DNS address of the database.
    address :: Core.Maybe Core.Text,
    -- | Specifies the port that the database is listening on.
    port :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { address = Core.Nothing,
      port = Core.Nothing
    }

-- | Specifies the DNS address of the database.
relationalDatabaseEndpoint_address :: Lens.Lens' RelationalDatabaseEndpoint (Core.Maybe Core.Text)
relationalDatabaseEndpoint_address = Lens.lens (\RelationalDatabaseEndpoint' {address} -> address) (\s@RelationalDatabaseEndpoint' {} a -> s {address = a} :: RelationalDatabaseEndpoint)

-- | Specifies the port that the database is listening on.
relationalDatabaseEndpoint_port :: Lens.Lens' RelationalDatabaseEndpoint (Core.Maybe Core.Int)
relationalDatabaseEndpoint_port = Lens.lens (\RelationalDatabaseEndpoint' {port} -> port) (\s@RelationalDatabaseEndpoint' {} a -> s {port = a} :: RelationalDatabaseEndpoint)

instance Core.FromJSON RelationalDatabaseEndpoint where
  parseJSON =
    Core.withObject
      "RelationalDatabaseEndpoint"
      ( \x ->
          RelationalDatabaseEndpoint'
            Core.<$> (x Core..:? "address") Core.<*> (x Core..:? "port")
      )

instance Core.Hashable RelationalDatabaseEndpoint

instance Core.NFData RelationalDatabaseEndpoint
