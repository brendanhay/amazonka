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
-- Module      : Amazonka.OpenSearch.DeleteInboundConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination Amazon OpenSearch Service domain owner to delete
-- an existing inbound cross-cluster search connection. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cross-cluster-search.html Cross-cluster search for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DeleteInboundConnection
  ( -- * Creating a Request
    DeleteInboundConnection (..),
    newDeleteInboundConnection,

    -- * Request Lenses
    deleteInboundConnection_connectionId,

    -- * Destructuring the Response
    DeleteInboundConnectionResponse (..),
    newDeleteInboundConnectionResponse,

    -- * Response Lenses
    deleteInboundConnectionResponse_connection,
    deleteInboundConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DeleteInboundConnection@ operation.
--
-- /See:/ 'newDeleteInboundConnection' smart constructor.
data DeleteInboundConnection = DeleteInboundConnection'
  { -- | The ID of the inbound connection to permanently delete.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInboundConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'deleteInboundConnection_connectionId' - The ID of the inbound connection to permanently delete.
newDeleteInboundConnection ::
  -- | 'connectionId'
  Prelude.Text ->
  DeleteInboundConnection
newDeleteInboundConnection pConnectionId_ =
  DeleteInboundConnection'
    { connectionId =
        pConnectionId_
    }

-- | The ID of the inbound connection to permanently delete.
deleteInboundConnection_connectionId :: Lens.Lens' DeleteInboundConnection Prelude.Text
deleteInboundConnection_connectionId = Lens.lens (\DeleteInboundConnection' {connectionId} -> connectionId) (\s@DeleteInboundConnection' {} a -> s {connectionId = a} :: DeleteInboundConnection)

instance Core.AWSRequest DeleteInboundConnection where
  type
    AWSResponse DeleteInboundConnection =
      DeleteInboundConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInboundConnectionResponse'
            Prelude.<$> (x Core..?> "Connection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInboundConnection where
  hashWithSalt _salt DeleteInboundConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData DeleteInboundConnection where
  rnf DeleteInboundConnection' {..} =
    Prelude.rnf connectionId

instance Core.ToHeaders DeleteInboundConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteInboundConnection where
  toPath DeleteInboundConnection' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/cc/inboundConnection/",
        Core.toBS connectionId
      ]

instance Core.ToQuery DeleteInboundConnection where
  toQuery = Prelude.const Prelude.mempty

-- | The results of a @DeleteInboundConnection@ operation. Contains details
-- about the deleted inbound connection.
--
-- /See:/ 'newDeleteInboundConnectionResponse' smart constructor.
data DeleteInboundConnectionResponse = DeleteInboundConnectionResponse'
  { -- | The deleted inbound connection.
    connection :: Prelude.Maybe InboundConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInboundConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connection', 'deleteInboundConnectionResponse_connection' - The deleted inbound connection.
--
-- 'httpStatus', 'deleteInboundConnectionResponse_httpStatus' - The response's http status code.
newDeleteInboundConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInboundConnectionResponse
newDeleteInboundConnectionResponse pHttpStatus_ =
  DeleteInboundConnectionResponse'
    { connection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deleted inbound connection.
deleteInboundConnectionResponse_connection :: Lens.Lens' DeleteInboundConnectionResponse (Prelude.Maybe InboundConnection)
deleteInboundConnectionResponse_connection = Lens.lens (\DeleteInboundConnectionResponse' {connection} -> connection) (\s@DeleteInboundConnectionResponse' {} a -> s {connection = a} :: DeleteInboundConnectionResponse)

-- | The response's http status code.
deleteInboundConnectionResponse_httpStatus :: Lens.Lens' DeleteInboundConnectionResponse Prelude.Int
deleteInboundConnectionResponse_httpStatus = Lens.lens (\DeleteInboundConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteInboundConnectionResponse' {} a -> s {httpStatus = a} :: DeleteInboundConnectionResponse)

instance
  Prelude.NFData
    DeleteInboundConnectionResponse
  where
  rnf DeleteInboundConnectionResponse' {..} =
    Prelude.rnf connection
      `Prelude.seq` Prelude.rnf httpStatus
