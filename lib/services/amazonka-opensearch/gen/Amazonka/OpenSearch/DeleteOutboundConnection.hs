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
-- Module      : Amazonka.OpenSearch.DeleteOutboundConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the source Amazon OpenSearch Service domain owner to delete an
-- existing outbound cross-cluster search connection. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cross-cluster-search.html Cross-cluster search for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DeleteOutboundConnection
  ( -- * Creating a Request
    DeleteOutboundConnection (..),
    newDeleteOutboundConnection,

    -- * Request Lenses
    deleteOutboundConnection_connectionId,

    -- * Destructuring the Response
    DeleteOutboundConnectionResponse (..),
    newDeleteOutboundConnectionResponse,

    -- * Response Lenses
    deleteOutboundConnectionResponse_connection,
    deleteOutboundConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DeleteOutboundConnection@
-- operation.
--
-- /See:/ 'newDeleteOutboundConnection' smart constructor.
data DeleteOutboundConnection = DeleteOutboundConnection'
  { -- | The ID of the outbound connection you want to permanently delete.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOutboundConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'deleteOutboundConnection_connectionId' - The ID of the outbound connection you want to permanently delete.
newDeleteOutboundConnection ::
  -- | 'connectionId'
  Prelude.Text ->
  DeleteOutboundConnection
newDeleteOutboundConnection pConnectionId_ =
  DeleteOutboundConnection'
    { connectionId =
        pConnectionId_
    }

-- | The ID of the outbound connection you want to permanently delete.
deleteOutboundConnection_connectionId :: Lens.Lens' DeleteOutboundConnection Prelude.Text
deleteOutboundConnection_connectionId = Lens.lens (\DeleteOutboundConnection' {connectionId} -> connectionId) (\s@DeleteOutboundConnection' {} a -> s {connectionId = a} :: DeleteOutboundConnection)

instance Core.AWSRequest DeleteOutboundConnection where
  type
    AWSResponse DeleteOutboundConnection =
      DeleteOutboundConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteOutboundConnectionResponse'
            Prelude.<$> (x Core..?> "Connection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOutboundConnection where
  hashWithSalt _salt DeleteOutboundConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData DeleteOutboundConnection where
  rnf DeleteOutboundConnection' {..} =
    Prelude.rnf connectionId

instance Core.ToHeaders DeleteOutboundConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteOutboundConnection where
  toPath DeleteOutboundConnection' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/cc/outboundConnection/",
        Core.toBS connectionId
      ]

instance Core.ToQuery DeleteOutboundConnection where
  toQuery = Prelude.const Prelude.mempty

-- | Details about the deleted outbound connection.
--
-- /See:/ 'newDeleteOutboundConnectionResponse' smart constructor.
data DeleteOutboundConnectionResponse = DeleteOutboundConnectionResponse'
  { -- | The deleted inbound connection.
    connection :: Prelude.Maybe OutboundConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOutboundConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connection', 'deleteOutboundConnectionResponse_connection' - The deleted inbound connection.
--
-- 'httpStatus', 'deleteOutboundConnectionResponse_httpStatus' - The response's http status code.
newDeleteOutboundConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOutboundConnectionResponse
newDeleteOutboundConnectionResponse pHttpStatus_ =
  DeleteOutboundConnectionResponse'
    { connection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deleted inbound connection.
deleteOutboundConnectionResponse_connection :: Lens.Lens' DeleteOutboundConnectionResponse (Prelude.Maybe OutboundConnection)
deleteOutboundConnectionResponse_connection = Lens.lens (\DeleteOutboundConnectionResponse' {connection} -> connection) (\s@DeleteOutboundConnectionResponse' {} a -> s {connection = a} :: DeleteOutboundConnectionResponse)

-- | The response's http status code.
deleteOutboundConnectionResponse_httpStatus :: Lens.Lens' DeleteOutboundConnectionResponse Prelude.Int
deleteOutboundConnectionResponse_httpStatus = Lens.lens (\DeleteOutboundConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteOutboundConnectionResponse' {} a -> s {httpStatus = a} :: DeleteOutboundConnectionResponse)

instance
  Prelude.NFData
    DeleteOutboundConnectionResponse
  where
  rnf DeleteOutboundConnectionResponse' {..} =
    Prelude.rnf connection
      `Prelude.seq` Prelude.rnf httpStatus
