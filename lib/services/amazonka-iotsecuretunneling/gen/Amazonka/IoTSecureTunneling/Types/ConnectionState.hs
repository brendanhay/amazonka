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
-- Module      : Amazonka.IoTSecureTunneling.Types.ConnectionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Types.ConnectionState where

import qualified Amazonka.Core as Core
import Amazonka.IoTSecureTunneling.Types.ConnectionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The state of a connection.
--
-- /See:/ 'newConnectionState' smart constructor.
data ConnectionState = ConnectionState'
  { -- | The connection status of the tunnel. Valid values are @CONNECTED@ and
    -- @DISCONNECTED@.
    status :: Prelude.Maybe ConnectionStatus,
    -- | The last time the connection status was updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'connectionState_status' - The connection status of the tunnel. Valid values are @CONNECTED@ and
-- @DISCONNECTED@.
--
-- 'lastUpdatedAt', 'connectionState_lastUpdatedAt' - The last time the connection status was updated.
newConnectionState ::
  ConnectionState
newConnectionState =
  ConnectionState'
    { status = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing
    }

-- | The connection status of the tunnel. Valid values are @CONNECTED@ and
-- @DISCONNECTED@.
connectionState_status :: Lens.Lens' ConnectionState (Prelude.Maybe ConnectionStatus)
connectionState_status = Lens.lens (\ConnectionState' {status} -> status) (\s@ConnectionState' {} a -> s {status = a} :: ConnectionState)

-- | The last time the connection status was updated.
connectionState_lastUpdatedAt :: Lens.Lens' ConnectionState (Prelude.Maybe Prelude.UTCTime)
connectionState_lastUpdatedAt = Lens.lens (\ConnectionState' {lastUpdatedAt} -> lastUpdatedAt) (\s@ConnectionState' {} a -> s {lastUpdatedAt = a} :: ConnectionState) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ConnectionState where
  parseJSON =
    Core.withObject
      "ConnectionState"
      ( \x ->
          ConnectionState'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
      )

instance Prelude.Hashable ConnectionState where
  hashWithSalt salt' ConnectionState' {..} =
    salt' `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData ConnectionState where
  rnf ConnectionState' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedAt
