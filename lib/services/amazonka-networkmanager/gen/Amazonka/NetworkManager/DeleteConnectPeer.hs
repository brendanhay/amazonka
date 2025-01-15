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
-- Module      : Amazonka.NetworkManager.DeleteConnectPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Connect peer.
module Amazonka.NetworkManager.DeleteConnectPeer
  ( -- * Creating a Request
    DeleteConnectPeer (..),
    newDeleteConnectPeer,

    -- * Request Lenses
    deleteConnectPeer_connectPeerId,

    -- * Destructuring the Response
    DeleteConnectPeerResponse (..),
    newDeleteConnectPeerResponse,

    -- * Response Lenses
    deleteConnectPeerResponse_connectPeer,
    deleteConnectPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConnectPeer' smart constructor.
data DeleteConnectPeer = DeleteConnectPeer'
  { -- | The ID of the deleted Connect peer.
    connectPeerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerId', 'deleteConnectPeer_connectPeerId' - The ID of the deleted Connect peer.
newDeleteConnectPeer ::
  -- | 'connectPeerId'
  Prelude.Text ->
  DeleteConnectPeer
newDeleteConnectPeer pConnectPeerId_ =
  DeleteConnectPeer' {connectPeerId = pConnectPeerId_}

-- | The ID of the deleted Connect peer.
deleteConnectPeer_connectPeerId :: Lens.Lens' DeleteConnectPeer Prelude.Text
deleteConnectPeer_connectPeerId = Lens.lens (\DeleteConnectPeer' {connectPeerId} -> connectPeerId) (\s@DeleteConnectPeer' {} a -> s {connectPeerId = a} :: DeleteConnectPeer)

instance Core.AWSRequest DeleteConnectPeer where
  type
    AWSResponse DeleteConnectPeer =
      DeleteConnectPeerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConnectPeerResponse'
            Prelude.<$> (x Data..?> "ConnectPeer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnectPeer where
  hashWithSalt _salt DeleteConnectPeer' {..} =
    _salt `Prelude.hashWithSalt` connectPeerId

instance Prelude.NFData DeleteConnectPeer where
  rnf DeleteConnectPeer' {..} =
    Prelude.rnf connectPeerId

instance Data.ToHeaders DeleteConnectPeer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConnectPeer where
  toPath DeleteConnectPeer' {..} =
    Prelude.mconcat
      ["/connect-peers/", Data.toBS connectPeerId]

instance Data.ToQuery DeleteConnectPeer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectPeerResponse' smart constructor.
data DeleteConnectPeerResponse = DeleteConnectPeerResponse'
  { -- | Information about the deleted Connect peer.
    connectPeer :: Prelude.Maybe ConnectPeer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeer', 'deleteConnectPeerResponse_connectPeer' - Information about the deleted Connect peer.
--
-- 'httpStatus', 'deleteConnectPeerResponse_httpStatus' - The response's http status code.
newDeleteConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConnectPeerResponse
newDeleteConnectPeerResponse pHttpStatus_ =
  DeleteConnectPeerResponse'
    { connectPeer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted Connect peer.
deleteConnectPeerResponse_connectPeer :: Lens.Lens' DeleteConnectPeerResponse (Prelude.Maybe ConnectPeer)
deleteConnectPeerResponse_connectPeer = Lens.lens (\DeleteConnectPeerResponse' {connectPeer} -> connectPeer) (\s@DeleteConnectPeerResponse' {} a -> s {connectPeer = a} :: DeleteConnectPeerResponse)

-- | The response's http status code.
deleteConnectPeerResponse_httpStatus :: Lens.Lens' DeleteConnectPeerResponse Prelude.Int
deleteConnectPeerResponse_httpStatus = Lens.lens (\DeleteConnectPeerResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectPeerResponse' {} a -> s {httpStatus = a} :: DeleteConnectPeerResponse)

instance Prelude.NFData DeleteConnectPeerResponse where
  rnf DeleteConnectPeerResponse' {..} =
    Prelude.rnf connectPeer `Prelude.seq`
      Prelude.rnf httpStatus
