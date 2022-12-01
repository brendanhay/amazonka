{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transfer.StopServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the state of a file transfer protocol-enabled server from
-- @ONLINE@ to @OFFLINE@. An @OFFLINE@ server cannot accept and process
-- file transfer jobs. Information tied to your server, such as server and
-- user properties, are not affected by stopping your server.
--
-- Stopping the server does not reduce or impact your file transfer
-- protocol endpoint billing; you must delete the server to stop being
-- billed.
--
-- The state of @STOPPING@ indicates that the server is in an intermediate
-- state, either not fully able to respond, or not fully offline. The
-- values of @STOP_FAILED@ can indicate an error condition.
--
-- No response is returned from this call.
module Amazonka.Transfer.StopServer
  ( -- * Creating a Request
    StopServer (..),
    newStopServer,

    -- * Request Lenses
    stopServer_serverId,

    -- * Destructuring the Response
    StopServerResponse (..),
    newStopServerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newStopServer' smart constructor.
data StopServer = StopServer'
  { -- | A system-assigned unique identifier for a server that you stopped.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'stopServer_serverId' - A system-assigned unique identifier for a server that you stopped.
newStopServer ::
  -- | 'serverId'
  Prelude.Text ->
  StopServer
newStopServer pServerId_ =
  StopServer' {serverId = pServerId_}

-- | A system-assigned unique identifier for a server that you stopped.
stopServer_serverId :: Lens.Lens' StopServer Prelude.Text
stopServer_serverId = Lens.lens (\StopServer' {serverId} -> serverId) (\s@StopServer' {} a -> s {serverId = a} :: StopServer)

instance Core.AWSRequest StopServer where
  type AWSResponse StopServer = StopServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull StopServerResponse'

instance Prelude.Hashable StopServer where
  hashWithSalt _salt StopServer' {..} =
    _salt `Prelude.hashWithSalt` serverId

instance Prelude.NFData StopServer where
  rnf StopServer' {..} = Prelude.rnf serverId

instance Core.ToHeaders StopServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TransferService.StopServer" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopServer where
  toJSON StopServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ServerId" Core..= serverId)]
      )

instance Core.ToPath StopServer where
  toPath = Prelude.const "/"

instance Core.ToQuery StopServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopServerResponse' smart constructor.
data StopServerResponse = StopServerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopServerResponse ::
  StopServerResponse
newStopServerResponse = StopServerResponse'

instance Prelude.NFData StopServerResponse where
  rnf _ = ()
