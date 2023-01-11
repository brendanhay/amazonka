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
-- Module      : Amazonka.GlobalAccelerator.Types.SocketAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.SocketAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON SocketAddress where
  parseJSON =
    Data.withObject
      "SocketAddress"
      ( \x ->
          SocketAddress'
            Prelude.<$> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "Port")
      )

instance Prelude.Hashable SocketAddress where
  hashWithSalt _salt SocketAddress' {..} =
    _salt `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` port

instance Prelude.NFData SocketAddress where
  rnf SocketAddress' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf port
