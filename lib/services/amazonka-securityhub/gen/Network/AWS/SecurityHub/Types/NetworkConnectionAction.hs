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
-- Module      : Network.AWS.SecurityHub.Types.NetworkConnectionAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.NetworkConnectionAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.ActionLocalPortDetails
import Network.AWS.SecurityHub.Types.ActionRemoteIpDetails
import Network.AWS.SecurityHub.Types.ActionRemotePortDetails

-- | Provided if @ActionType@ is @NETWORK_CONNECTION@. It provides details
-- about the attempted network connection that was detected.
--
-- /See:/ 'newNetworkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { -- | Information about the remote IP address that issued the network
    -- connection request.
    remoteIpDetails :: Prelude.Maybe ActionRemoteIpDetails,
    -- | The protocol used to make the network connection request.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | Information about the port on the remote IP address.
    remotePortDetails :: Prelude.Maybe ActionRemotePortDetails,
    -- | Indicates whether the network connection attempt was blocked.
    blocked :: Prelude.Maybe Prelude.Bool,
    -- | The direction of the network connection request (@IN@ or @OUT@).
    connectionDirection :: Prelude.Maybe Prelude.Text,
    -- | Information about the port on the EC2 instance.
    localPortDetails :: Prelude.Maybe ActionLocalPortDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkConnectionAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteIpDetails', 'networkConnectionAction_remoteIpDetails' - Information about the remote IP address that issued the network
-- connection request.
--
-- 'protocol', 'networkConnectionAction_protocol' - The protocol used to make the network connection request.
--
-- 'remotePortDetails', 'networkConnectionAction_remotePortDetails' - Information about the port on the remote IP address.
--
-- 'blocked', 'networkConnectionAction_blocked' - Indicates whether the network connection attempt was blocked.
--
-- 'connectionDirection', 'networkConnectionAction_connectionDirection' - The direction of the network connection request (@IN@ or @OUT@).
--
-- 'localPortDetails', 'networkConnectionAction_localPortDetails' - Information about the port on the EC2 instance.
newNetworkConnectionAction ::
  NetworkConnectionAction
newNetworkConnectionAction =
  NetworkConnectionAction'
    { remoteIpDetails =
        Prelude.Nothing,
      protocol = Prelude.Nothing,
      remotePortDetails = Prelude.Nothing,
      blocked = Prelude.Nothing,
      connectionDirection = Prelude.Nothing,
      localPortDetails = Prelude.Nothing
    }

-- | Information about the remote IP address that issued the network
-- connection request.
networkConnectionAction_remoteIpDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe ActionRemoteIpDetails)
networkConnectionAction_remoteIpDetails = Lens.lens (\NetworkConnectionAction' {remoteIpDetails} -> remoteIpDetails) (\s@NetworkConnectionAction' {} a -> s {remoteIpDetails = a} :: NetworkConnectionAction)

-- | The protocol used to make the network connection request.
networkConnectionAction_protocol :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_protocol = Lens.lens (\NetworkConnectionAction' {protocol} -> protocol) (\s@NetworkConnectionAction' {} a -> s {protocol = a} :: NetworkConnectionAction)

-- | Information about the port on the remote IP address.
networkConnectionAction_remotePortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe ActionRemotePortDetails)
networkConnectionAction_remotePortDetails = Lens.lens (\NetworkConnectionAction' {remotePortDetails} -> remotePortDetails) (\s@NetworkConnectionAction' {} a -> s {remotePortDetails = a} :: NetworkConnectionAction)

-- | Indicates whether the network connection attempt was blocked.
networkConnectionAction_blocked :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Bool)
networkConnectionAction_blocked = Lens.lens (\NetworkConnectionAction' {blocked} -> blocked) (\s@NetworkConnectionAction' {} a -> s {blocked = a} :: NetworkConnectionAction)

-- | The direction of the network connection request (@IN@ or @OUT@).
networkConnectionAction_connectionDirection :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_connectionDirection = Lens.lens (\NetworkConnectionAction' {connectionDirection} -> connectionDirection) (\s@NetworkConnectionAction' {} a -> s {connectionDirection = a} :: NetworkConnectionAction)

-- | Information about the port on the EC2 instance.
networkConnectionAction_localPortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe ActionLocalPortDetails)
networkConnectionAction_localPortDetails = Lens.lens (\NetworkConnectionAction' {localPortDetails} -> localPortDetails) (\s@NetworkConnectionAction' {} a -> s {localPortDetails = a} :: NetworkConnectionAction)

instance Core.FromJSON NetworkConnectionAction where
  parseJSON =
    Core.withObject
      "NetworkConnectionAction"
      ( \x ->
          NetworkConnectionAction'
            Prelude.<$> (x Core..:? "RemoteIpDetails")
            Prelude.<*> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "RemotePortDetails")
            Prelude.<*> (x Core..:? "Blocked")
            Prelude.<*> (x Core..:? "ConnectionDirection")
            Prelude.<*> (x Core..:? "LocalPortDetails")
      )

instance Prelude.Hashable NetworkConnectionAction

instance Prelude.NFData NetworkConnectionAction

instance Core.ToJSON NetworkConnectionAction where
  toJSON NetworkConnectionAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RemoteIpDetails" Core..=)
              Prelude.<$> remoteIpDetails,
            ("Protocol" Core..=) Prelude.<$> protocol,
            ("RemotePortDetails" Core..=)
              Prelude.<$> remotePortDetails,
            ("Blocked" Core..=) Prelude.<$> blocked,
            ("ConnectionDirection" Core..=)
              Prelude.<$> connectionDirection,
            ("LocalPortDetails" Core..=)
              Prelude.<$> localPortDetails
          ]
      )
