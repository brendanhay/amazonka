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
-- Module      : Network.AWS.GlobalAccelerator.Types.SocketAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GlobalAccelerator.Types.SocketAddress where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An IP address\/port combination.
--
-- /See:/ 'newSocketAddress' smart constructor.
data SocketAddress = SocketAddress'
  { -- | The IP address for the socket address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The port for the socket address.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SocketAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'socketAddress_ipAddress' - The IP address for the socket address.
--
-- 'port', 'socketAddress_port' - The port for the socket address.
newSocketAddress ::
  SocketAddress
newSocketAddress =
  SocketAddress'
    { ipAddress = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The IP address for the socket address.
socketAddress_ipAddress :: Lens.Lens' SocketAddress (Prelude.Maybe Prelude.Text)
socketAddress_ipAddress = Lens.lens (\SocketAddress' {ipAddress} -> ipAddress) (\s@SocketAddress' {} a -> s {ipAddress = a} :: SocketAddress)

-- | The port for the socket address.
socketAddress_port :: Lens.Lens' SocketAddress (Prelude.Maybe Prelude.Natural)
socketAddress_port = Lens.lens (\SocketAddress' {port} -> port) (\s@SocketAddress' {} a -> s {port = a} :: SocketAddress)

instance Core.FromJSON SocketAddress where
  parseJSON =
    Core.withObject
      "SocketAddress"
      ( \x ->
          SocketAddress'
            Prelude.<$> (x Core..:? "IpAddress")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable SocketAddress

instance Prelude.NFData SocketAddress
