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
-- Module      : Amazonka.Transfer.DeleteServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the file transfer protocol-enabled server that you specify.
--
-- No response returns from this operation.
module Amazonka.Transfer.DeleteServer
  ( -- * Creating a Request
    DeleteServer (..),
    newDeleteServer,

    -- * Request Lenses
    deleteServer_serverId,

    -- * Destructuring the Response
    DeleteServerResponse (..),
    newDeleteServerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDeleteServer' smart constructor.
data DeleteServer = DeleteServer'
  { -- | A unique system-assigned identifier for a server instance.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'deleteServer_serverId' - A unique system-assigned identifier for a server instance.
newDeleteServer ::
  -- | 'serverId'
  Prelude.Text ->
  DeleteServer
newDeleteServer pServerId_ =
  DeleteServer' {serverId = pServerId_}

-- | A unique system-assigned identifier for a server instance.
deleteServer_serverId :: Lens.Lens' DeleteServer Prelude.Text
deleteServer_serverId = Lens.lens (\DeleteServer' {serverId} -> serverId) (\s@DeleteServer' {} a -> s {serverId = a} :: DeleteServer)

instance Core.AWSRequest DeleteServer where
  type AWSResponse DeleteServer = DeleteServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteServerResponse'

instance Prelude.Hashable DeleteServer where
  hashWithSalt _salt DeleteServer' {..} =
    _salt `Prelude.hashWithSalt` serverId

instance Prelude.NFData DeleteServer where
  rnf DeleteServer' {..} = Prelude.rnf serverId

instance Data.ToHeaders DeleteServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.DeleteServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteServer where
  toJSON DeleteServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ServerId" Data..= serverId)]
      )

instance Data.ToPath DeleteServer where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServerResponse' smart constructor.
data DeleteServerResponse = DeleteServerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteServerResponse ::
  DeleteServerResponse
newDeleteServerResponse = DeleteServerResponse'

instance Prelude.NFData DeleteServerResponse where
  rnf _ = ()
