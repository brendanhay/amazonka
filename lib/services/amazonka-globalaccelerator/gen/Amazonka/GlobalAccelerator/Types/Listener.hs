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
-- Module      : Amazonka.GlobalAccelerator.Types.Listener
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.Listener where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.ClientAffinity
import Amazonka.GlobalAccelerator.Types.PortRange
import Amazonka.GlobalAccelerator.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | A complex type for a listener.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Maybe Prelude.Text,
    -- | Client affinity lets you direct all requests from a user to the same
    -- endpoint, if you have stateful applications, regardless of the port and
    -- protocol of the client request. Client affinity gives you control over
    -- whether to always route each client to the same specific endpoint.
    --
    -- Global Accelerator uses a consistent-flow hashing algorithm to choose
    -- the optimal endpoint for a connection. If client affinity is @NONE@,
    -- Global Accelerator uses the \"five-tuple\" (5-tuple) properties—source
    -- IP address, source port, destination IP address, destination port, and
    -- protocol—to select the hash value, and then chooses the best endpoint.
    -- However, with this setting, if someone uses different ports to connect
    -- to Global Accelerator, their connections might not be always routed to
    -- the same endpoint because the hash value changes.
    --
    -- If you want a given client to always be routed to the same endpoint, set
    -- client affinity to @SOURCE_IP@ instead. When you use the @SOURCE_IP@
    -- setting, Global Accelerator uses the \"two-tuple\" (2-tuple) properties—
    -- source (client) IP address and destination IP address—to select the hash
    -- value.
    --
    -- The default value is @NONE@.
    clientAffinity :: Prelude.Maybe ClientAffinity,
    -- | The protocol for the connections from clients to the accelerator.
    protocol :: Prelude.Maybe Protocol,
    -- | The list of port ranges for the connections from clients to the
    -- accelerator.
    portRanges :: Prelude.Maybe (Prelude.NonEmpty PortRange)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Listener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'listener_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'clientAffinity', 'listener_clientAffinity' - Client affinity lets you direct all requests from a user to the same
-- endpoint, if you have stateful applications, regardless of the port and
-- protocol of the client request. Client affinity gives you control over
-- whether to always route each client to the same specific endpoint.
--
-- Global Accelerator uses a consistent-flow hashing algorithm to choose
-- the optimal endpoint for a connection. If client affinity is @NONE@,
-- Global Accelerator uses the \"five-tuple\" (5-tuple) properties—source
-- IP address, source port, destination IP address, destination port, and
-- protocol—to select the hash value, and then chooses the best endpoint.
-- However, with this setting, if someone uses different ports to connect
-- to Global Accelerator, their connections might not be always routed to
-- the same endpoint because the hash value changes.
--
-- If you want a given client to always be routed to the same endpoint, set
-- client affinity to @SOURCE_IP@ instead. When you use the @SOURCE_IP@
-- setting, Global Accelerator uses the \"two-tuple\" (2-tuple) properties—
-- source (client) IP address and destination IP address—to select the hash
-- value.
--
-- The default value is @NONE@.
--
-- 'protocol', 'listener_protocol' - The protocol for the connections from clients to the accelerator.
--
-- 'portRanges', 'listener_portRanges' - The list of port ranges for the connections from clients to the
-- accelerator.
newListener ::
  Listener
newListener =
  Listener'
    { listenerArn = Prelude.Nothing,
      clientAffinity = Prelude.Nothing,
      protocol = Prelude.Nothing,
      portRanges = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
listener_listenerArn :: Lens.Lens' Listener (Prelude.Maybe Prelude.Text)
listener_listenerArn = Lens.lens (\Listener' {listenerArn} -> listenerArn) (\s@Listener' {} a -> s {listenerArn = a} :: Listener)

-- | Client affinity lets you direct all requests from a user to the same
-- endpoint, if you have stateful applications, regardless of the port and
-- protocol of the client request. Client affinity gives you control over
-- whether to always route each client to the same specific endpoint.
--
-- Global Accelerator uses a consistent-flow hashing algorithm to choose
-- the optimal endpoint for a connection. If client affinity is @NONE@,
-- Global Accelerator uses the \"five-tuple\" (5-tuple) properties—source
-- IP address, source port, destination IP address, destination port, and
-- protocol—to select the hash value, and then chooses the best endpoint.
-- However, with this setting, if someone uses different ports to connect
-- to Global Accelerator, their connections might not be always routed to
-- the same endpoint because the hash value changes.
--
-- If you want a given client to always be routed to the same endpoint, set
-- client affinity to @SOURCE_IP@ instead. When you use the @SOURCE_IP@
-- setting, Global Accelerator uses the \"two-tuple\" (2-tuple) properties—
-- source (client) IP address and destination IP address—to select the hash
-- value.
--
-- The default value is @NONE@.
listener_clientAffinity :: Lens.Lens' Listener (Prelude.Maybe ClientAffinity)
listener_clientAffinity = Lens.lens (\Listener' {clientAffinity} -> clientAffinity) (\s@Listener' {} a -> s {clientAffinity = a} :: Listener)

-- | The protocol for the connections from clients to the accelerator.
listener_protocol :: Lens.Lens' Listener (Prelude.Maybe Protocol)
listener_protocol = Lens.lens (\Listener' {protocol} -> protocol) (\s@Listener' {} a -> s {protocol = a} :: Listener)

-- | The list of port ranges for the connections from clients to the
-- accelerator.
listener_portRanges :: Lens.Lens' Listener (Prelude.Maybe (Prelude.NonEmpty PortRange))
listener_portRanges = Lens.lens (\Listener' {portRanges} -> portRanges) (\s@Listener' {} a -> s {portRanges = a} :: Listener) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Listener where
  parseJSON =
    Data.withObject
      "Listener"
      ( \x ->
          Listener'
            Prelude.<$> (x Data..:? "ListenerArn")
            Prelude.<*> (x Data..:? "ClientAffinity")
            Prelude.<*> (x Data..:? "Protocol")
            Prelude.<*> (x Data..:? "PortRanges")
      )

instance Prelude.Hashable Listener where
  hashWithSalt _salt Listener' {..} =
    _salt `Prelude.hashWithSalt` listenerArn
      `Prelude.hashWithSalt` clientAffinity
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` portRanges

instance Prelude.NFData Listener where
  rnf Listener' {..} =
    Prelude.rnf listenerArn
      `Prelude.seq` Prelude.rnf clientAffinity
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf portRanges
