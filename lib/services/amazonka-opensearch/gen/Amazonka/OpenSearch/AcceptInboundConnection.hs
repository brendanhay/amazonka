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
-- Module      : Amazonka.OpenSearch.AcceptInboundConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination Amazon OpenSearch Service domain owner to accept
-- an inbound cross-cluster search connection request. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cross-cluster-search.html Cross-cluster search for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.AcceptInboundConnection
  ( -- * Creating a Request
    AcceptInboundConnection (..),
    newAcceptInboundConnection,

    -- * Request Lenses
    acceptInboundConnection_connectionId,

    -- * Destructuring the Response
    AcceptInboundConnectionResponse (..),
    newAcceptInboundConnectionResponse,

    -- * Response Lenses
    acceptInboundConnectionResponse_connection,
    acceptInboundConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @AcceptInboundConnection@ operation.
--
-- /See:/ 'newAcceptInboundConnection' smart constructor.
data AcceptInboundConnection = AcceptInboundConnection'
  { -- | The ID of the inbound connection to accept.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInboundConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'acceptInboundConnection_connectionId' - The ID of the inbound connection to accept.
newAcceptInboundConnection ::
  -- | 'connectionId'
  Prelude.Text ->
  AcceptInboundConnection
newAcceptInboundConnection pConnectionId_ =
  AcceptInboundConnection'
    { connectionId =
        pConnectionId_
    }

-- | The ID of the inbound connection to accept.
acceptInboundConnection_connectionId :: Lens.Lens' AcceptInboundConnection Prelude.Text
acceptInboundConnection_connectionId = Lens.lens (\AcceptInboundConnection' {connectionId} -> connectionId) (\s@AcceptInboundConnection' {} a -> s {connectionId = a} :: AcceptInboundConnection)

instance Core.AWSRequest AcceptInboundConnection where
  type
    AWSResponse AcceptInboundConnection =
      AcceptInboundConnectionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptInboundConnectionResponse'
            Prelude.<$> (x Core..?> "Connection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptInboundConnection where
  hashWithSalt _salt AcceptInboundConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData AcceptInboundConnection where
  rnf AcceptInboundConnection' {..} =
    Prelude.rnf connectionId

instance Core.ToHeaders AcceptInboundConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON AcceptInboundConnection where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath AcceptInboundConnection where
  toPath AcceptInboundConnection' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/cc/inboundConnection/",
        Core.toBS connectionId,
        "/accept"
      ]

instance Core.ToQuery AcceptInboundConnection where
  toQuery = Prelude.const Prelude.mempty

-- | Contains details about the accepted inbound connection.
--
-- /See:/ 'newAcceptInboundConnectionResponse' smart constructor.
data AcceptInboundConnectionResponse = AcceptInboundConnectionResponse'
  { -- | Information about the accepted inbound connection.
    connection :: Prelude.Maybe InboundConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInboundConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connection', 'acceptInboundConnectionResponse_connection' - Information about the accepted inbound connection.
--
-- 'httpStatus', 'acceptInboundConnectionResponse_httpStatus' - The response's http status code.
newAcceptInboundConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptInboundConnectionResponse
newAcceptInboundConnectionResponse pHttpStatus_ =
  AcceptInboundConnectionResponse'
    { connection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the accepted inbound connection.
acceptInboundConnectionResponse_connection :: Lens.Lens' AcceptInboundConnectionResponse (Prelude.Maybe InboundConnection)
acceptInboundConnectionResponse_connection = Lens.lens (\AcceptInboundConnectionResponse' {connection} -> connection) (\s@AcceptInboundConnectionResponse' {} a -> s {connection = a} :: AcceptInboundConnectionResponse)

-- | The response's http status code.
acceptInboundConnectionResponse_httpStatus :: Lens.Lens' AcceptInboundConnectionResponse Prelude.Int
acceptInboundConnectionResponse_httpStatus = Lens.lens (\AcceptInboundConnectionResponse' {httpStatus} -> httpStatus) (\s@AcceptInboundConnectionResponse' {} a -> s {httpStatus = a} :: AcceptInboundConnectionResponse)

instance
  Prelude.NFData
    AcceptInboundConnectionResponse
  where
  rnf AcceptInboundConnectionResponse' {..} =
    Prelude.rnf connection
      `Prelude.seq` Prelude.rnf httpStatus
