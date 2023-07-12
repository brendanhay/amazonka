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
-- Module      : Amazonka.ElasticSearch.AcceptInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to accept an inbound cross-cluster
-- search connection request.
module Amazonka.ElasticSearch.AcceptInboundCrossClusterSearchConnection
  ( -- * Creating a Request
    AcceptInboundCrossClusterSearchConnection (..),
    newAcceptInboundCrossClusterSearchConnection,

    -- * Request Lenses
    acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,

    -- * Destructuring the Response
    AcceptInboundCrossClusterSearchConnectionResponse (..),
    newAcceptInboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    acceptInboundCrossClusterSearchConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the
-- @AcceptInboundCrossClusterSearchConnection@ operation.
--
-- /See:/ 'newAcceptInboundCrossClusterSearchConnection' smart constructor.
data AcceptInboundCrossClusterSearchConnection = AcceptInboundCrossClusterSearchConnection'
  { -- | The id of the inbound connection that you want to accept.
    crossClusterSearchConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnectionId', 'acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - The id of the inbound connection that you want to accept.
newAcceptInboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Prelude.Text ->
  AcceptInboundCrossClusterSearchConnection
newAcceptInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    AcceptInboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to accept.
acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' AcceptInboundCrossClusterSearchConnection Prelude.Text
acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\AcceptInboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@AcceptInboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: AcceptInboundCrossClusterSearchConnection)

instance
  Core.AWSRequest
    AcceptInboundCrossClusterSearchConnection
  where
  type
    AWSResponse
      AcceptInboundCrossClusterSearchConnection =
      AcceptInboundCrossClusterSearchConnectionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptInboundCrossClusterSearchConnectionResponse'
            Prelude.<$> (x Data..?> "CrossClusterSearchConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptInboundCrossClusterSearchConnection
  where
  hashWithSalt
    _salt
    AcceptInboundCrossClusterSearchConnection' {..} =
      _salt
        `Prelude.hashWithSalt` crossClusterSearchConnectionId

instance
  Prelude.NFData
    AcceptInboundCrossClusterSearchConnection
  where
  rnf AcceptInboundCrossClusterSearchConnection' {..} =
    Prelude.rnf crossClusterSearchConnectionId

instance
  Data.ToHeaders
    AcceptInboundCrossClusterSearchConnection
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AcceptInboundCrossClusterSearchConnection
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    AcceptInboundCrossClusterSearchConnection
  where
  toPath AcceptInboundCrossClusterSearchConnection' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        Data.toBS crossClusterSearchConnectionId,
        "/accept"
      ]

instance
  Data.ToQuery
    AcceptInboundCrossClusterSearchConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @AcceptInboundCrossClusterSearchConnection@ operation.
-- Contains details of accepted inbound connection.
--
-- /See:/ 'newAcceptInboundCrossClusterSearchConnectionResponse' smart constructor.
data AcceptInboundCrossClusterSearchConnectionResponse = AcceptInboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @InboundCrossClusterSearchConnection@ of accepted inbound
    -- connection.
    crossClusterSearchConnection :: Prelude.Maybe InboundCrossClusterSearchConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInboundCrossClusterSearchConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnection', 'acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection' - Specifies the @InboundCrossClusterSearchConnection@ of accepted inbound
-- connection.
--
-- 'httpStatus', 'acceptInboundCrossClusterSearchConnectionResponse_httpStatus' - The response's http status code.
newAcceptInboundCrossClusterSearchConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptInboundCrossClusterSearchConnectionResponse
newAcceptInboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    AcceptInboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the @InboundCrossClusterSearchConnection@ of accepted inbound
-- connection.
acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection :: Lens.Lens' AcceptInboundCrossClusterSearchConnectionResponse (Prelude.Maybe InboundCrossClusterSearchConnection)
acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection = Lens.lens (\AcceptInboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnection} -> crossClusterSearchConnection) (\s@AcceptInboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnection = a} :: AcceptInboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
acceptInboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' AcceptInboundCrossClusterSearchConnectionResponse Prelude.Int
acceptInboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\AcceptInboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@AcceptInboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: AcceptInboundCrossClusterSearchConnectionResponse)

instance
  Prelude.NFData
    AcceptInboundCrossClusterSearchConnectionResponse
  where
  rnf
    AcceptInboundCrossClusterSearchConnectionResponse' {..} =
      Prelude.rnf crossClusterSearchConnection
        `Prelude.seq` Prelude.rnf httpStatus
