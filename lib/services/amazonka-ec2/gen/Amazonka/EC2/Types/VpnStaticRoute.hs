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
-- Module      : Amazonka.EC2.Types.VpnStaticRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnStaticRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VpnState
import Amazonka.EC2.Types.VpnStaticRouteSource
import qualified Amazonka.Prelude as Prelude

-- | Describes a static route for a VPN connection.
--
-- /See:/ 'newVpnStaticRoute' smart constructor.
data VpnStaticRoute = VpnStaticRoute'
  { -- | The CIDR block associated with the local subnet of the customer data
    -- center.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Indicates how the routes were provided.
    source :: Prelude.Maybe VpnStaticRouteSource,
    -- | The current state of the static route.
    state :: Prelude.Maybe VpnState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnStaticRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCidrBlock', 'vpnStaticRoute_destinationCidrBlock' - The CIDR block associated with the local subnet of the customer data
-- center.
--
-- 'source', 'vpnStaticRoute_source' - Indicates how the routes were provided.
--
-- 'state', 'vpnStaticRoute_state' - The current state of the static route.
newVpnStaticRoute ::
  VpnStaticRoute
newVpnStaticRoute =
  VpnStaticRoute'
    { destinationCidrBlock =
        Prelude.Nothing,
      source = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The CIDR block associated with the local subnet of the customer data
-- center.
vpnStaticRoute_destinationCidrBlock :: Lens.Lens' VpnStaticRoute (Prelude.Maybe Prelude.Text)
vpnStaticRoute_destinationCidrBlock = Lens.lens (\VpnStaticRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@VpnStaticRoute' {} a -> s {destinationCidrBlock = a} :: VpnStaticRoute)

-- | Indicates how the routes were provided.
vpnStaticRoute_source :: Lens.Lens' VpnStaticRoute (Prelude.Maybe VpnStaticRouteSource)
vpnStaticRoute_source = Lens.lens (\VpnStaticRoute' {source} -> source) (\s@VpnStaticRoute' {} a -> s {source = a} :: VpnStaticRoute)

-- | The current state of the static route.
vpnStaticRoute_state :: Lens.Lens' VpnStaticRoute (Prelude.Maybe VpnState)
vpnStaticRoute_state = Lens.lens (\VpnStaticRoute' {state} -> state) (\s@VpnStaticRoute' {} a -> s {state = a} :: VpnStaticRoute)

instance Data.FromXML VpnStaticRoute where
  parseXML x =
    VpnStaticRoute'
      Prelude.<$> (x Data..@? "destinationCidrBlock")
      Prelude.<*> (x Data..@? "source")
      Prelude.<*> (x Data..@? "state")

instance Prelude.Hashable VpnStaticRoute where
  hashWithSalt _salt VpnStaticRoute' {..} =
    _salt
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` state

instance Prelude.NFData VpnStaticRoute where
  rnf VpnStaticRoute' {..} =
    Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf state
