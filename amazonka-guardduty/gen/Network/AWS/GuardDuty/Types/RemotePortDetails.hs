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
-- Module      : Network.AWS.GuardDuty.Types.RemotePortDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.RemotePortDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the remote port.
--
-- /See:/ 'newRemotePortDetails' smart constructor.
data RemotePortDetails = RemotePortDetails'
  { -- | The port name of the remote connection.
    portName :: Core.Maybe Core.Text,
    -- | The port number of the remote connection.
    port :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemotePortDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portName', 'remotePortDetails_portName' - The port name of the remote connection.
--
-- 'port', 'remotePortDetails_port' - The port number of the remote connection.
newRemotePortDetails ::
  RemotePortDetails
newRemotePortDetails =
  RemotePortDetails'
    { portName = Core.Nothing,
      port = Core.Nothing
    }

-- | The port name of the remote connection.
remotePortDetails_portName :: Lens.Lens' RemotePortDetails (Core.Maybe Core.Text)
remotePortDetails_portName = Lens.lens (\RemotePortDetails' {portName} -> portName) (\s@RemotePortDetails' {} a -> s {portName = a} :: RemotePortDetails)

-- | The port number of the remote connection.
remotePortDetails_port :: Lens.Lens' RemotePortDetails (Core.Maybe Core.Int)
remotePortDetails_port = Lens.lens (\RemotePortDetails' {port} -> port) (\s@RemotePortDetails' {} a -> s {port = a} :: RemotePortDetails)

instance Core.FromJSON RemotePortDetails where
  parseJSON =
    Core.withObject
      "RemotePortDetails"
      ( \x ->
          RemotePortDetails'
            Core.<$> (x Core..:? "portName") Core.<*> (x Core..:? "port")
      )

instance Core.Hashable RemotePortDetails

instance Core.NFData RemotePortDetails
