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
-- Module      : Amazonka.GuardDuty.Types.NetworkConnectionAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.NetworkConnectionAction where

import qualified Amazonka.Core as Core
import Amazonka.GuardDuty.Types.LocalIpDetails
import Amazonka.GuardDuty.Types.LocalPortDetails
import Amazonka.GuardDuty.Types.RemoteIpDetails
import Amazonka.GuardDuty.Types.RemotePortDetails
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the NETWORK_CONNECTION action described in
-- the finding.
--
-- /See:/ 'newNetworkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { -- | The remote IP information of the connection.
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The network connection protocol.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The local IP information of the connection.
    localIpDetails :: Prelude.Maybe LocalIpDetails,
    -- | The remote port information of the connection.
    remotePortDetails :: Prelude.Maybe RemotePortDetails,
    -- | Indicates whether EC2 blocked the network connection to your instance.
    blocked :: Prelude.Maybe Prelude.Bool,
    -- | The network connection direction.
    connectionDirection :: Prelude.Maybe Prelude.Text,
    -- | The local port information of the connection.
    localPortDetails :: Prelude.Maybe LocalPortDetails
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
-- 'remoteIpDetails', 'networkConnectionAction_remoteIpDetails' - The remote IP information of the connection.
--
-- 'protocol', 'networkConnectionAction_protocol' - The network connection protocol.
--
-- 'localIpDetails', 'networkConnectionAction_localIpDetails' - The local IP information of the connection.
--
-- 'remotePortDetails', 'networkConnectionAction_remotePortDetails' - The remote port information of the connection.
--
-- 'blocked', 'networkConnectionAction_blocked' - Indicates whether EC2 blocked the network connection to your instance.
--
-- 'connectionDirection', 'networkConnectionAction_connectionDirection' - The network connection direction.
--
-- 'localPortDetails', 'networkConnectionAction_localPortDetails' - The local port information of the connection.
newNetworkConnectionAction ::
  NetworkConnectionAction
newNetworkConnectionAction =
  NetworkConnectionAction'
    { remoteIpDetails =
        Prelude.Nothing,
      protocol = Prelude.Nothing,
      localIpDetails = Prelude.Nothing,
      remotePortDetails = Prelude.Nothing,
      blocked = Prelude.Nothing,
      connectionDirection = Prelude.Nothing,
      localPortDetails = Prelude.Nothing
    }

-- | The remote IP information of the connection.
networkConnectionAction_remoteIpDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe RemoteIpDetails)
networkConnectionAction_remoteIpDetails = Lens.lens (\NetworkConnectionAction' {remoteIpDetails} -> remoteIpDetails) (\s@NetworkConnectionAction' {} a -> s {remoteIpDetails = a} :: NetworkConnectionAction)

-- | The network connection protocol.
networkConnectionAction_protocol :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_protocol = Lens.lens (\NetworkConnectionAction' {protocol} -> protocol) (\s@NetworkConnectionAction' {} a -> s {protocol = a} :: NetworkConnectionAction)

-- | The local IP information of the connection.
networkConnectionAction_localIpDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe LocalIpDetails)
networkConnectionAction_localIpDetails = Lens.lens (\NetworkConnectionAction' {localIpDetails} -> localIpDetails) (\s@NetworkConnectionAction' {} a -> s {localIpDetails = a} :: NetworkConnectionAction)

-- | The remote port information of the connection.
networkConnectionAction_remotePortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe RemotePortDetails)
networkConnectionAction_remotePortDetails = Lens.lens (\NetworkConnectionAction' {remotePortDetails} -> remotePortDetails) (\s@NetworkConnectionAction' {} a -> s {remotePortDetails = a} :: NetworkConnectionAction)

-- | Indicates whether EC2 blocked the network connection to your instance.
networkConnectionAction_blocked :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Bool)
networkConnectionAction_blocked = Lens.lens (\NetworkConnectionAction' {blocked} -> blocked) (\s@NetworkConnectionAction' {} a -> s {blocked = a} :: NetworkConnectionAction)

-- | The network connection direction.
networkConnectionAction_connectionDirection :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_connectionDirection = Lens.lens (\NetworkConnectionAction' {connectionDirection} -> connectionDirection) (\s@NetworkConnectionAction' {} a -> s {connectionDirection = a} :: NetworkConnectionAction)

-- | The local port information of the connection.
networkConnectionAction_localPortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe LocalPortDetails)
networkConnectionAction_localPortDetails = Lens.lens (\NetworkConnectionAction' {localPortDetails} -> localPortDetails) (\s@NetworkConnectionAction' {} a -> s {localPortDetails = a} :: NetworkConnectionAction)

instance Core.FromJSON NetworkConnectionAction where
  parseJSON =
    Core.withObject
      "NetworkConnectionAction"
      ( \x ->
          NetworkConnectionAction'
            Prelude.<$> (x Core..:? "remoteIpDetails")
            Prelude.<*> (x Core..:? "protocol")
            Prelude.<*> (x Core..:? "localIpDetails")
            Prelude.<*> (x Core..:? "remotePortDetails")
            Prelude.<*> (x Core..:? "blocked")
            Prelude.<*> (x Core..:? "connectionDirection")
            Prelude.<*> (x Core..:? "localPortDetails")
      )

instance Prelude.Hashable NetworkConnectionAction where
  hashWithSalt salt' NetworkConnectionAction' {..} =
    salt' `Prelude.hashWithSalt` localPortDetails
      `Prelude.hashWithSalt` connectionDirection
      `Prelude.hashWithSalt` blocked
      `Prelude.hashWithSalt` remotePortDetails
      `Prelude.hashWithSalt` localIpDetails
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` remoteIpDetails

instance Prelude.NFData NetworkConnectionAction where
  rnf NetworkConnectionAction' {..} =
    Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf localPortDetails
      `Prelude.seq` Prelude.rnf connectionDirection
      `Prelude.seq` Prelude.rnf blocked
      `Prelude.seq` Prelude.rnf remotePortDetails
      `Prelude.seq` Prelude.rnf localIpDetails
      `Prelude.seq` Prelude.rnf protocol
