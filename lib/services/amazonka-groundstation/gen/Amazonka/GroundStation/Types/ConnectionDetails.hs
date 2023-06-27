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
-- Module      : Amazonka.GroundStation.Types.ConnectionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ConnectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.SocketAddress
import qualified Amazonka.Prelude as Prelude

-- | Egress address of AgentEndpoint with an optional mtu.
--
-- /See:/ 'newConnectionDetails' smart constructor.
data ConnectionDetails = ConnectionDetails'
  { -- | Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
    mtu :: Prelude.Maybe Prelude.Int,
    -- | A socket address.
    socketAddress :: SocketAddress
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mtu', 'connectionDetails_mtu' - Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
--
-- 'socketAddress', 'connectionDetails_socketAddress' - A socket address.
newConnectionDetails ::
  -- | 'socketAddress'
  SocketAddress ->
  ConnectionDetails
newConnectionDetails pSocketAddress_ =
  ConnectionDetails'
    { mtu = Prelude.Nothing,
      socketAddress = pSocketAddress_
    }

-- | Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
connectionDetails_mtu :: Lens.Lens' ConnectionDetails (Prelude.Maybe Prelude.Int)
connectionDetails_mtu = Lens.lens (\ConnectionDetails' {mtu} -> mtu) (\s@ConnectionDetails' {} a -> s {mtu = a} :: ConnectionDetails)

-- | A socket address.
connectionDetails_socketAddress :: Lens.Lens' ConnectionDetails SocketAddress
connectionDetails_socketAddress = Lens.lens (\ConnectionDetails' {socketAddress} -> socketAddress) (\s@ConnectionDetails' {} a -> s {socketAddress = a} :: ConnectionDetails)

instance Data.FromJSON ConnectionDetails where
  parseJSON =
    Data.withObject
      "ConnectionDetails"
      ( \x ->
          ConnectionDetails'
            Prelude.<$> (x Data..:? "mtu")
            Prelude.<*> (x Data..: "socketAddress")
      )

instance Prelude.Hashable ConnectionDetails where
  hashWithSalt _salt ConnectionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` mtu
      `Prelude.hashWithSalt` socketAddress

instance Prelude.NFData ConnectionDetails where
  rnf ConnectionDetails' {..} =
    Prelude.rnf mtu
      `Prelude.seq` Prelude.rnf socketAddress

instance Data.ToJSON ConnectionDetails where
  toJSON ConnectionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mtu" Data..=) Prelude.<$> mtu,
            Prelude.Just
              ("socketAddress" Data..= socketAddress)
          ]
      )
