{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.VpnStaticRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpnStaticRoute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VpnState
import Network.AWS.EC2.Types.VpnStaticRouteSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a static route for a VPN connection.
--
-- /See:/ 'newVpnStaticRoute' smart constructor.
data VpnStaticRoute = VpnStaticRoute'
  { -- | Indicates how the routes were provided.
    source :: Prelude.Maybe VpnStaticRouteSource,
    -- | The current state of the static route.
    state :: Prelude.Maybe VpnState,
    -- | The CIDR block associated with the local subnet of the customer data
    -- center.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpnStaticRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'vpnStaticRoute_source' - Indicates how the routes were provided.
--
-- 'state', 'vpnStaticRoute_state' - The current state of the static route.
--
-- 'destinationCidrBlock', 'vpnStaticRoute_destinationCidrBlock' - The CIDR block associated with the local subnet of the customer data
-- center.
newVpnStaticRoute ::
  VpnStaticRoute
newVpnStaticRoute =
  VpnStaticRoute'
    { source = Prelude.Nothing,
      state = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing
    }

-- | Indicates how the routes were provided.
vpnStaticRoute_source :: Lens.Lens' VpnStaticRoute (Prelude.Maybe VpnStaticRouteSource)
vpnStaticRoute_source = Lens.lens (\VpnStaticRoute' {source} -> source) (\s@VpnStaticRoute' {} a -> s {source = a} :: VpnStaticRoute)

-- | The current state of the static route.
vpnStaticRoute_state :: Lens.Lens' VpnStaticRoute (Prelude.Maybe VpnState)
vpnStaticRoute_state = Lens.lens (\VpnStaticRoute' {state} -> state) (\s@VpnStaticRoute' {} a -> s {state = a} :: VpnStaticRoute)

-- | The CIDR block associated with the local subnet of the customer data
-- center.
vpnStaticRoute_destinationCidrBlock :: Lens.Lens' VpnStaticRoute (Prelude.Maybe Prelude.Text)
vpnStaticRoute_destinationCidrBlock = Lens.lens (\VpnStaticRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@VpnStaticRoute' {} a -> s {destinationCidrBlock = a} :: VpnStaticRoute)

instance Prelude.FromXML VpnStaticRoute where
  parseXML x =
    VpnStaticRoute'
      Prelude.<$> (x Prelude..@? "source")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "destinationCidrBlock")

instance Prelude.Hashable VpnStaticRoute

instance Prelude.NFData VpnStaticRoute
