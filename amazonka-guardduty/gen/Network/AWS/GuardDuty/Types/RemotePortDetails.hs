{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the remote port.
--
-- /See:/ 'newRemotePortDetails' smart constructor.
data RemotePortDetails = RemotePortDetails'
  { -- | The port name of the remote connection.
    portName :: Prelude.Maybe Prelude.Text,
    -- | The port number of the remote connection.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { portName = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The port name of the remote connection.
remotePortDetails_portName :: Lens.Lens' RemotePortDetails (Prelude.Maybe Prelude.Text)
remotePortDetails_portName = Lens.lens (\RemotePortDetails' {portName} -> portName) (\s@RemotePortDetails' {} a -> s {portName = a} :: RemotePortDetails)

-- | The port number of the remote connection.
remotePortDetails_port :: Lens.Lens' RemotePortDetails (Prelude.Maybe Prelude.Int)
remotePortDetails_port = Lens.lens (\RemotePortDetails' {port} -> port) (\s@RemotePortDetails' {} a -> s {port = a} :: RemotePortDetails)

instance Prelude.FromJSON RemotePortDetails where
  parseJSON =
    Prelude.withObject
      "RemotePortDetails"
      ( \x ->
          RemotePortDetails'
            Prelude.<$> (x Prelude..:? "portName")
            Prelude.<*> (x Prelude..:? "port")
      )

instance Prelude.Hashable RemotePortDetails

instance Prelude.NFData RemotePortDetails
