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
-- Module      : Amazonka.EC2.Types.TerminateConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TerminateConnectionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a terminated Client VPN endpoint client connection.
--
-- /See:/ 'newTerminateConnectionStatus' smart constructor.
data TerminateConnectionStatus = TerminateConnectionStatus'
  { -- | The ID of the client connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | A message about the status of the client connection, if applicable.
    currentStatus :: Prelude.Maybe ClientVpnConnectionStatus,
    -- | The state of the client connection.
    previousStatus :: Prelude.Maybe ClientVpnConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'terminateConnectionStatus_connectionId' - The ID of the client connection.
--
-- 'currentStatus', 'terminateConnectionStatus_currentStatus' - A message about the status of the client connection, if applicable.
--
-- 'previousStatus', 'terminateConnectionStatus_previousStatus' - The state of the client connection.
newTerminateConnectionStatus ::
  TerminateConnectionStatus
newTerminateConnectionStatus =
  TerminateConnectionStatus'
    { connectionId =
        Prelude.Nothing,
      currentStatus = Prelude.Nothing,
      previousStatus = Prelude.Nothing
    }

-- | The ID of the client connection.
terminateConnectionStatus_connectionId :: Lens.Lens' TerminateConnectionStatus (Prelude.Maybe Prelude.Text)
terminateConnectionStatus_connectionId = Lens.lens (\TerminateConnectionStatus' {connectionId} -> connectionId) (\s@TerminateConnectionStatus' {} a -> s {connectionId = a} :: TerminateConnectionStatus)

-- | A message about the status of the client connection, if applicable.
terminateConnectionStatus_currentStatus :: Lens.Lens' TerminateConnectionStatus (Prelude.Maybe ClientVpnConnectionStatus)
terminateConnectionStatus_currentStatus = Lens.lens (\TerminateConnectionStatus' {currentStatus} -> currentStatus) (\s@TerminateConnectionStatus' {} a -> s {currentStatus = a} :: TerminateConnectionStatus)

-- | The state of the client connection.
terminateConnectionStatus_previousStatus :: Lens.Lens' TerminateConnectionStatus (Prelude.Maybe ClientVpnConnectionStatus)
terminateConnectionStatus_previousStatus = Lens.lens (\TerminateConnectionStatus' {previousStatus} -> previousStatus) (\s@TerminateConnectionStatus' {} a -> s {previousStatus = a} :: TerminateConnectionStatus)

instance Data.FromXML TerminateConnectionStatus where
  parseXML x =
    TerminateConnectionStatus'
      Prelude.<$> (x Data..@? "connectionId")
      Prelude.<*> (x Data..@? "currentStatus")
      Prelude.<*> (x Data..@? "previousStatus")

instance Prelude.Hashable TerminateConnectionStatus where
  hashWithSalt _salt TerminateConnectionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` currentStatus
      `Prelude.hashWithSalt` previousStatus

instance Prelude.NFData TerminateConnectionStatus where
  rnf TerminateConnectionStatus' {..} =
    Prelude.rnf connectionId `Prelude.seq`
      Prelude.rnf currentStatus `Prelude.seq`
        Prelude.rnf previousStatus
