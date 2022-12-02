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
-- Module      : Amazonka.GuardDuty.Types.RemotePortDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RemotePortDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the remote port.
--
-- /See:/ 'newRemotePortDetails' smart constructor.
data RemotePortDetails = RemotePortDetails'
  { -- | The port number of the remote connection.
    port :: Prelude.Maybe Prelude.Int,
    -- | The port name of the remote connection.
    portName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemotePortDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'remotePortDetails_port' - The port number of the remote connection.
--
-- 'portName', 'remotePortDetails_portName' - The port name of the remote connection.
newRemotePortDetails ::
  RemotePortDetails
newRemotePortDetails =
  RemotePortDetails'
    { port = Prelude.Nothing,
      portName = Prelude.Nothing
    }

-- | The port number of the remote connection.
remotePortDetails_port :: Lens.Lens' RemotePortDetails (Prelude.Maybe Prelude.Int)
remotePortDetails_port = Lens.lens (\RemotePortDetails' {port} -> port) (\s@RemotePortDetails' {} a -> s {port = a} :: RemotePortDetails)

-- | The port name of the remote connection.
remotePortDetails_portName :: Lens.Lens' RemotePortDetails (Prelude.Maybe Prelude.Text)
remotePortDetails_portName = Lens.lens (\RemotePortDetails' {portName} -> portName) (\s@RemotePortDetails' {} a -> s {portName = a} :: RemotePortDetails)

instance Data.FromJSON RemotePortDetails where
  parseJSON =
    Data.withObject
      "RemotePortDetails"
      ( \x ->
          RemotePortDetails'
            Prelude.<$> (x Data..:? "port")
            Prelude.<*> (x Data..:? "portName")
      )

instance Prelude.Hashable RemotePortDetails where
  hashWithSalt _salt RemotePortDetails' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` portName

instance Prelude.NFData RemotePortDetails where
  rnf RemotePortDetails' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf portName
