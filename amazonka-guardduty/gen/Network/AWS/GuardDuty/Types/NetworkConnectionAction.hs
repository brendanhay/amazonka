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
-- Module      : Network.AWS.GuardDuty.Types.NetworkConnectionAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.NetworkConnectionAction where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.LocalIpDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import qualified Network.AWS.Lens as Lens

-- | Contains information about the NETWORK_CONNECTION action described in
-- the finding.
--
-- /See:/ 'newNetworkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { -- | The remote port information of the connection.
    remotePortDetails :: Core.Maybe RemotePortDetails,
    -- | The local port information of the connection.
    localPortDetails :: Core.Maybe LocalPortDetails,
    -- | The network connection direction.
    connectionDirection :: Core.Maybe Core.Text,
    -- | Indicates whether EC2 blocked the network connection to your instance.
    blocked :: Core.Maybe Core.Bool,
    -- | The remote IP information of the connection.
    remoteIpDetails :: Core.Maybe RemoteIpDetails,
    -- | The local IP information of the connection.
    localIpDetails :: Core.Maybe LocalIpDetails,
    -- | The network connection protocol.
    protocol :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkConnectionAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remotePortDetails', 'networkConnectionAction_remotePortDetails' - The remote port information of the connection.
--
-- 'localPortDetails', 'networkConnectionAction_localPortDetails' - The local port information of the connection.
--
-- 'connectionDirection', 'networkConnectionAction_connectionDirection' - The network connection direction.
--
-- 'blocked', 'networkConnectionAction_blocked' - Indicates whether EC2 blocked the network connection to your instance.
--
-- 'remoteIpDetails', 'networkConnectionAction_remoteIpDetails' - The remote IP information of the connection.
--
-- 'localIpDetails', 'networkConnectionAction_localIpDetails' - The local IP information of the connection.
--
-- 'protocol', 'networkConnectionAction_protocol' - The network connection protocol.
newNetworkConnectionAction ::
  NetworkConnectionAction
newNetworkConnectionAction =
  NetworkConnectionAction'
    { remotePortDetails =
        Core.Nothing,
      localPortDetails = Core.Nothing,
      connectionDirection = Core.Nothing,
      blocked = Core.Nothing,
      remoteIpDetails = Core.Nothing,
      localIpDetails = Core.Nothing,
      protocol = Core.Nothing
    }

-- | The remote port information of the connection.
networkConnectionAction_remotePortDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe RemotePortDetails)
networkConnectionAction_remotePortDetails = Lens.lens (\NetworkConnectionAction' {remotePortDetails} -> remotePortDetails) (\s@NetworkConnectionAction' {} a -> s {remotePortDetails = a} :: NetworkConnectionAction)

-- | The local port information of the connection.
networkConnectionAction_localPortDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe LocalPortDetails)
networkConnectionAction_localPortDetails = Lens.lens (\NetworkConnectionAction' {localPortDetails} -> localPortDetails) (\s@NetworkConnectionAction' {} a -> s {localPortDetails = a} :: NetworkConnectionAction)

-- | The network connection direction.
networkConnectionAction_connectionDirection :: Lens.Lens' NetworkConnectionAction (Core.Maybe Core.Text)
networkConnectionAction_connectionDirection = Lens.lens (\NetworkConnectionAction' {connectionDirection} -> connectionDirection) (\s@NetworkConnectionAction' {} a -> s {connectionDirection = a} :: NetworkConnectionAction)

-- | Indicates whether EC2 blocked the network connection to your instance.
networkConnectionAction_blocked :: Lens.Lens' NetworkConnectionAction (Core.Maybe Core.Bool)
networkConnectionAction_blocked = Lens.lens (\NetworkConnectionAction' {blocked} -> blocked) (\s@NetworkConnectionAction' {} a -> s {blocked = a} :: NetworkConnectionAction)

-- | The remote IP information of the connection.
networkConnectionAction_remoteIpDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe RemoteIpDetails)
networkConnectionAction_remoteIpDetails = Lens.lens (\NetworkConnectionAction' {remoteIpDetails} -> remoteIpDetails) (\s@NetworkConnectionAction' {} a -> s {remoteIpDetails = a} :: NetworkConnectionAction)

-- | The local IP information of the connection.
networkConnectionAction_localIpDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe LocalIpDetails)
networkConnectionAction_localIpDetails = Lens.lens (\NetworkConnectionAction' {localIpDetails} -> localIpDetails) (\s@NetworkConnectionAction' {} a -> s {localIpDetails = a} :: NetworkConnectionAction)

-- | The network connection protocol.
networkConnectionAction_protocol :: Lens.Lens' NetworkConnectionAction (Core.Maybe Core.Text)
networkConnectionAction_protocol = Lens.lens (\NetworkConnectionAction' {protocol} -> protocol) (\s@NetworkConnectionAction' {} a -> s {protocol = a} :: NetworkConnectionAction)

instance Core.FromJSON NetworkConnectionAction where
  parseJSON =
    Core.withObject
      "NetworkConnectionAction"
      ( \x ->
          NetworkConnectionAction'
            Core.<$> (x Core..:? "remotePortDetails")
            Core.<*> (x Core..:? "localPortDetails")
            Core.<*> (x Core..:? "connectionDirection")
            Core.<*> (x Core..:? "blocked")
            Core.<*> (x Core..:? "remoteIpDetails")
            Core.<*> (x Core..:? "localIpDetails")
            Core.<*> (x Core..:? "protocol")
      )

instance Core.Hashable NetworkConnectionAction

instance Core.NFData NetworkConnectionAction
