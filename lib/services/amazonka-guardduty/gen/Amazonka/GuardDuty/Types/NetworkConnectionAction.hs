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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.NetworkConnectionAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.LocalIpDetails
import Amazonka.GuardDuty.Types.LocalPortDetails
import Amazonka.GuardDuty.Types.RemoteIpDetails
import Amazonka.GuardDuty.Types.RemotePortDetails
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the NETWORK_CONNECTION action described in
-- the finding.
--
-- /See:/ 'newNetworkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { -- | Indicates whether EC2 blocked the network connection to your instance.
    blocked :: Prelude.Maybe Prelude.Bool,
    -- | The network connection direction.
    connectionDirection :: Prelude.Maybe Prelude.Text,
    -- | The local IP information of the connection.
    localIpDetails :: Prelude.Maybe LocalIpDetails,
    -- | The local port information of the connection.
    localPortDetails :: Prelude.Maybe LocalPortDetails,
    -- | The network connection protocol.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The remote IP information of the connection.
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The remote port information of the connection.
    remotePortDetails :: Prelude.Maybe RemotePortDetails
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
-- 'blocked', 'networkConnectionAction_blocked' - Indicates whether EC2 blocked the network connection to your instance.
--
-- 'connectionDirection', 'networkConnectionAction_connectionDirection' - The network connection direction.
--
-- 'localIpDetails', 'networkConnectionAction_localIpDetails' - The local IP information of the connection.
--
-- 'localPortDetails', 'networkConnectionAction_localPortDetails' - The local port information of the connection.
--
-- 'protocol', 'networkConnectionAction_protocol' - The network connection protocol.
--
-- 'remoteIpDetails', 'networkConnectionAction_remoteIpDetails' - The remote IP information of the connection.
--
-- 'remotePortDetails', 'networkConnectionAction_remotePortDetails' - The remote port information of the connection.
newNetworkConnectionAction ::
  NetworkConnectionAction
newNetworkConnectionAction =
  NetworkConnectionAction'
    { blocked = Prelude.Nothing,
      connectionDirection = Prelude.Nothing,
      localIpDetails = Prelude.Nothing,
      localPortDetails = Prelude.Nothing,
      protocol = Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing,
      remotePortDetails = Prelude.Nothing
    }

-- | Indicates whether EC2 blocked the network connection to your instance.
networkConnectionAction_blocked :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Bool)
networkConnectionAction_blocked = Lens.lens (\NetworkConnectionAction' {blocked} -> blocked) (\s@NetworkConnectionAction' {} a -> s {blocked = a} :: NetworkConnectionAction)

-- | The network connection direction.
networkConnectionAction_connectionDirection :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_connectionDirection = Lens.lens (\NetworkConnectionAction' {connectionDirection} -> connectionDirection) (\s@NetworkConnectionAction' {} a -> s {connectionDirection = a} :: NetworkConnectionAction)

-- | The local IP information of the connection.
networkConnectionAction_localIpDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe LocalIpDetails)
networkConnectionAction_localIpDetails = Lens.lens (\NetworkConnectionAction' {localIpDetails} -> localIpDetails) (\s@NetworkConnectionAction' {} a -> s {localIpDetails = a} :: NetworkConnectionAction)

-- | The local port information of the connection.
networkConnectionAction_localPortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe LocalPortDetails)
networkConnectionAction_localPortDetails = Lens.lens (\NetworkConnectionAction' {localPortDetails} -> localPortDetails) (\s@NetworkConnectionAction' {} a -> s {localPortDetails = a} :: NetworkConnectionAction)

-- | The network connection protocol.
networkConnectionAction_protocol :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe Prelude.Text)
networkConnectionAction_protocol = Lens.lens (\NetworkConnectionAction' {protocol} -> protocol) (\s@NetworkConnectionAction' {} a -> s {protocol = a} :: NetworkConnectionAction)

-- | The remote IP information of the connection.
networkConnectionAction_remoteIpDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe RemoteIpDetails)
networkConnectionAction_remoteIpDetails = Lens.lens (\NetworkConnectionAction' {remoteIpDetails} -> remoteIpDetails) (\s@NetworkConnectionAction' {} a -> s {remoteIpDetails = a} :: NetworkConnectionAction)

-- | The remote port information of the connection.
networkConnectionAction_remotePortDetails :: Lens.Lens' NetworkConnectionAction (Prelude.Maybe RemotePortDetails)
networkConnectionAction_remotePortDetails = Lens.lens (\NetworkConnectionAction' {remotePortDetails} -> remotePortDetails) (\s@NetworkConnectionAction' {} a -> s {remotePortDetails = a} :: NetworkConnectionAction)

instance Data.FromJSON NetworkConnectionAction where
  parseJSON =
    Data.withObject
      "NetworkConnectionAction"
      ( \x ->
          NetworkConnectionAction'
            Prelude.<$> (x Data..:? "blocked")
            Prelude.<*> (x Data..:? "connectionDirection")
            Prelude.<*> (x Data..:? "localIpDetails")
            Prelude.<*> (x Data..:? "localPortDetails")
            Prelude.<*> (x Data..:? "protocol")
            Prelude.<*> (x Data..:? "remoteIpDetails")
            Prelude.<*> (x Data..:? "remotePortDetails")
      )

instance Prelude.Hashable NetworkConnectionAction where
  hashWithSalt _salt NetworkConnectionAction' {..} =
    _salt
      `Prelude.hashWithSalt` blocked
      `Prelude.hashWithSalt` connectionDirection
      `Prelude.hashWithSalt` localIpDetails
      `Prelude.hashWithSalt` localPortDetails
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` remoteIpDetails
      `Prelude.hashWithSalt` remotePortDetails

instance Prelude.NFData NetworkConnectionAction where
  rnf NetworkConnectionAction' {..} =
    Prelude.rnf blocked
      `Prelude.seq` Prelude.rnf connectionDirection
      `Prelude.seq` Prelude.rnf localIpDetails
      `Prelude.seq` Prelude.rnf localPortDetails
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf remotePortDetails
