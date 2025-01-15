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
-- Module      : Amazonka.DMS.Types.ServerShortInfoResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ServerShortInfoResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a server in a Fleet Advisor collector inventory.
--
-- /See:/ 'newServerShortInfoResponse' smart constructor.
data ServerShortInfoResponse = ServerShortInfoResponse'
  { -- | The IP address of a server in a Fleet Advisor collector inventory.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of a server in a Fleet Advisor collector inventory.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | The name address of a server in a Fleet Advisor collector inventory.
    serverName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerShortInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'serverShortInfoResponse_ipAddress' - The IP address of a server in a Fleet Advisor collector inventory.
--
-- 'serverId', 'serverShortInfoResponse_serverId' - The ID of a server in a Fleet Advisor collector inventory.
--
-- 'serverName', 'serverShortInfoResponse_serverName' - The name address of a server in a Fleet Advisor collector inventory.
newServerShortInfoResponse ::
  ServerShortInfoResponse
newServerShortInfoResponse =
  ServerShortInfoResponse'
    { ipAddress =
        Prelude.Nothing,
      serverId = Prelude.Nothing,
      serverName = Prelude.Nothing
    }

-- | The IP address of a server in a Fleet Advisor collector inventory.
serverShortInfoResponse_ipAddress :: Lens.Lens' ServerShortInfoResponse (Prelude.Maybe Prelude.Text)
serverShortInfoResponse_ipAddress = Lens.lens (\ServerShortInfoResponse' {ipAddress} -> ipAddress) (\s@ServerShortInfoResponse' {} a -> s {ipAddress = a} :: ServerShortInfoResponse)

-- | The ID of a server in a Fleet Advisor collector inventory.
serverShortInfoResponse_serverId :: Lens.Lens' ServerShortInfoResponse (Prelude.Maybe Prelude.Text)
serverShortInfoResponse_serverId = Lens.lens (\ServerShortInfoResponse' {serverId} -> serverId) (\s@ServerShortInfoResponse' {} a -> s {serverId = a} :: ServerShortInfoResponse)

-- | The name address of a server in a Fleet Advisor collector inventory.
serverShortInfoResponse_serverName :: Lens.Lens' ServerShortInfoResponse (Prelude.Maybe Prelude.Text)
serverShortInfoResponse_serverName = Lens.lens (\ServerShortInfoResponse' {serverName} -> serverName) (\s@ServerShortInfoResponse' {} a -> s {serverName = a} :: ServerShortInfoResponse)

instance Data.FromJSON ServerShortInfoResponse where
  parseJSON =
    Data.withObject
      "ServerShortInfoResponse"
      ( \x ->
          ServerShortInfoResponse'
            Prelude.<$> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "ServerId")
            Prelude.<*> (x Data..:? "ServerName")
      )

instance Prelude.Hashable ServerShortInfoResponse where
  hashWithSalt _salt ServerShortInfoResponse' {..} =
    _salt
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` serverName

instance Prelude.NFData ServerShortInfoResponse where
  rnf ServerShortInfoResponse' {..} =
    Prelude.rnf ipAddress `Prelude.seq`
      Prelude.rnf serverId `Prelude.seq`
        Prelude.rnf serverName
