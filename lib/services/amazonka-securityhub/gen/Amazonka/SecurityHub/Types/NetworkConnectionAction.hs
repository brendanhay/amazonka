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
-- Module      : Amazonka.SecurityHub.Types.NetworkConnectionAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.NetworkConnectionAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ActionLocalPortDetails
import Amazonka.SecurityHub.Types.ActionRemoteIpDetails
import Amazonka.SecurityHub.Types.ActionRemotePortDetails

-- | Provided if @ActionType@ is @NETWORK_CONNECTION@. It provides details
-- about the attempted network connection that was detected.
--
-- /See:/ 'newNetworkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { -- | Indicates whether the network connection attempt was blocked.
    blocked :: Prelude.Maybe Prelude.Bool,
    -- | The direction of the network connection request (@IN@ or @OUT@).
    connectionDirection :: Prelude.Maybe Prelude.Text,
    -- | Information about the port on the EC2 instance.
    localPortDetails :: Prelude.Maybe ActionLocalPortDetails,
    -- | The protocol used to make the network connection request.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | Information about the remote IP address that issued the network
    -- connection request.
    remoteIpDetails :: Prelude.Maybe ActionRemoteIpDetails,
    -- | Information about the port on the remote IP address.
    remotePortDetails :: Prelude.Maybe ActionRemotePortDetails
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
-- 'blocked', 'networkConnectionAction_blocked' - Indicates whether the network connection attempt was blocked.
--
-- 'connectionDirection', 'networkConnectionAction_connectionDirection' - The direction of the network connection request (@IN@ or @OUT@).
--
-- 'localPortDetails', 'networkConnectionAction_localPortDetails' - Information about the port on the EC2 instance.
--
-- 'protocol', 'networkConnectionAction_protocol' - The protocol used to make the network connection request.
--
-- 'remoteIpDetails', 'networkConnectionAction_remoteIpDetails' - Information about the remote IP address that issued the network
-- connection request.
--
-- 'remotePortDetails', 'networkConnectionAction_remotePortDetails' - Information about the port on the remote IP address.
newNetworkConnectionAction ::
  NetworkConnectionAction
newNetworkConnectionAction =
  NetworkConnectionAction'
    { blocked = Prelude.Nothing,
      connectionDirection = Prelude.Nothing,
      localPortDetails = Prelude.Nothing,
      protocol = Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing,
      remotePortDetails = Prelude.Nothing
    }

-- | Indicates whether the network connection attempt was blocked.
networkConnectionAction_blocked :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Bool)
networkConnectionAction_blocked = Lens.lens (\NetworkConnectionAction' {blocked} -> blocked) (\s@NetworkConnectionAction' {} a -> s {blocked = a} :: NetworkConnectionAction)

-- | The direction of the network connection request (@IN@ or @OUT@).
networkConnectionAction_connectionDirection :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_connectionDirection = Lens.lens (\NetworkConnectionAction' {connectionDirection} -> connectionDirection) (\s@NetworkConnectionAction' {} a -> s {connectionDirection = a} :: NetworkConnectionAction)

-- | Information about the port on the EC2 instance.
networkConnectionAction_localPortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe ActionLocalPortDetails)
networkConnectionAction_localPortDetails = Lens.lens (\NetworkConnectionAction' {localPortDetails} -> localPortDetails) (\s@NetworkConnectionAction' {} a -> s {localPortDetails = a} :: NetworkConnectionAction)

-- | The protocol used to make the network connection request.
networkConnectionAction_protocol :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_protocol = Lens.lens (\NetworkConnectionAction' {protocol} -> protocol) (\s@NetworkConnectionAction' {} a -> s {protocol = a} :: NetworkConnectionAction)

-- | Information about the remote IP address that issued the network
-- connection request.
networkConnectionAction_remoteIpDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe ActionRemoteIpDetails)
networkConnectionAction_remoteIpDetails = Lens.lens (\NetworkConnectionAction' {remoteIpDetails} -> remoteIpDetails) (\s@NetworkConnectionAction' {} a -> s {remoteIpDetails = a} :: NetworkConnectionAction)

-- | Information about the port on the remote IP address.
networkConnectionAction_remotePortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe ActionRemotePortDetails)
networkConnectionAction_remotePortDetails = Lens.lens (\NetworkConnectionAction' {remotePortDetails} -> remotePortDetails) (\s@NetworkConnectionAction' {} a -> s {remotePortDetails = a} :: NetworkConnectionAction)

instance Data.FromJSON NetworkConnectionAction where
  parseJSON =
    Data.withObject
      "NetworkConnectionAction"
      ( \x ->
          NetworkConnectionAction'
            Prelude.<$> (x Data..:? "Blocked")
            Prelude.<*> (x Data..:? "ConnectionDirection")
            Prelude.<*> (x Data..:? "LocalPortDetails")
            Prelude.<*> (x Data..:? "Protocol")
            Prelude.<*> (x Data..:? "RemoteIpDetails")
            Prelude.<*> (x Data..:? "RemotePortDetails")
      )

instance Prelude.Hashable NetworkConnectionAction where
  hashWithSalt _salt NetworkConnectionAction' {..} =
    _salt `Prelude.hashWithSalt` blocked
      `Prelude.hashWithSalt` connectionDirection
      `Prelude.hashWithSalt` localPortDetails
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` remoteIpDetails
      `Prelude.hashWithSalt` remotePortDetails

instance Prelude.NFData NetworkConnectionAction where
  rnf NetworkConnectionAction' {..} =
    Prelude.rnf blocked
      `Prelude.seq` Prelude.rnf connectionDirection
      `Prelude.seq` Prelude.rnf localPortDetails
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf remotePortDetails

instance Data.ToJSON NetworkConnectionAction where
  toJSON NetworkConnectionAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Blocked" Data..=) Prelude.<$> blocked,
            ("ConnectionDirection" Data..=)
              Prelude.<$> connectionDirection,
            ("LocalPortDetails" Data..=)
              Prelude.<$> localPortDetails,
            ("Protocol" Data..=) Prelude.<$> protocol,
            ("RemoteIpDetails" Data..=)
              Prelude.<$> remoteIpDetails,
            ("RemotePortDetails" Data..=)
              Prelude.<$> remotePortDetails
          ]
      )
