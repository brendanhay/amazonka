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
-- Module      : Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to accept an inbound cross-cluster
-- search connection request.
module Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the
-- @AcceptInboundCrossClusterSearchConnection@ operation.
--
-- /See:/ 'newAcceptInboundCrossClusterSearchConnection' smart constructor.
data AcceptInboundCrossClusterSearchConnection = AcceptInboundCrossClusterSearchConnection'
  { -- | The id of the inbound connection that you want to accept.
    crossClusterSearchConnectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  AcceptInboundCrossClusterSearchConnection
newAcceptInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    AcceptInboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to accept.
acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' AcceptInboundCrossClusterSearchConnection Core.Text
acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\AcceptInboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@AcceptInboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: AcceptInboundCrossClusterSearchConnection)

instance
  Core.AWSRequest
    AcceptInboundCrossClusterSearchConnection
  where
  type
    AWSResponse
      AcceptInboundCrossClusterSearchConnection =
      AcceptInboundCrossClusterSearchConnectionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptInboundCrossClusterSearchConnectionResponse'
            Core.<$> (x Core..?> "CrossClusterSearchConnection")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AcceptInboundCrossClusterSearchConnection

instance
  Core.NFData
    AcceptInboundCrossClusterSearchConnection

instance
  Core.ToHeaders
    AcceptInboundCrossClusterSearchConnection
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToJSON
    AcceptInboundCrossClusterSearchConnection
  where
  toJSON = Core.const (Core.Object Core.mempty)

instance
  Core.ToPath
    AcceptInboundCrossClusterSearchConnection
  where
  toPath AcceptInboundCrossClusterSearchConnection' {..} =
    Core.mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        Core.toBS crossClusterSearchConnectionId,
        "/accept"
      ]

instance
  Core.ToQuery
    AcceptInboundCrossClusterSearchConnection
  where
  toQuery = Core.const Core.mempty

-- | The result of a @AcceptInboundCrossClusterSearchConnection@ operation.
-- Contains details of accepted inbound connection.
--
-- /See:/ 'newAcceptInboundCrossClusterSearchConnectionResponse' smart constructor.
data AcceptInboundCrossClusterSearchConnectionResponse = AcceptInboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @InboundCrossClusterSearchConnection@ of accepted inbound
    -- connection.
    crossClusterSearchConnection :: Core.Maybe InboundCrossClusterSearchConnection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AcceptInboundCrossClusterSearchConnectionResponse
newAcceptInboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    AcceptInboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the @InboundCrossClusterSearchConnection@ of accepted inbound
-- connection.
acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection :: Lens.Lens' AcceptInboundCrossClusterSearchConnectionResponse (Core.Maybe InboundCrossClusterSearchConnection)
acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection = Lens.lens (\AcceptInboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnection} -> crossClusterSearchConnection) (\s@AcceptInboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnection = a} :: AcceptInboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
acceptInboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' AcceptInboundCrossClusterSearchConnectionResponse Core.Int
acceptInboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\AcceptInboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@AcceptInboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: AcceptInboundCrossClusterSearchConnectionResponse)

instance
  Core.NFData
    AcceptInboundCrossClusterSearchConnectionResponse
