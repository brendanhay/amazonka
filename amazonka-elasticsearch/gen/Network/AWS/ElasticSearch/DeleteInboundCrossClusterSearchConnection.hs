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
-- Module      : Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to delete an existing inbound
-- cross-cluster search connection.
module Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
  ( -- * Creating a Request
    DeleteInboundCrossClusterSearchConnection (..),
    newDeleteInboundCrossClusterSearchConnection,

    -- * Request Lenses
    deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,

    -- * Destructuring the Response
    DeleteInboundCrossClusterSearchConnectionResponse (..),
    newDeleteInboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteInboundCrossClusterSearchConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the
-- @DeleteInboundCrossClusterSearchConnection@ operation.
--
-- /See:/ 'newDeleteInboundCrossClusterSearchConnection' smart constructor.
data DeleteInboundCrossClusterSearchConnection = DeleteInboundCrossClusterSearchConnection'
  { -- | The id of the inbound connection that you want to permanently delete.
    crossClusterSearchConnectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnectionId', 'deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - The id of the inbound connection that you want to permanently delete.
newDeleteInboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Core.Text ->
  DeleteInboundCrossClusterSearchConnection
newDeleteInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    DeleteInboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to permanently delete.
deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' DeleteInboundCrossClusterSearchConnection Core.Text
deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\DeleteInboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@DeleteInboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: DeleteInboundCrossClusterSearchConnection)

instance
  Core.AWSRequest
    DeleteInboundCrossClusterSearchConnection
  where
  type
    AWSResponse
      DeleteInboundCrossClusterSearchConnection =
      DeleteInboundCrossClusterSearchConnectionResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInboundCrossClusterSearchConnectionResponse'
            Core.<$> (x Core..?> "CrossClusterSearchConnection")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteInboundCrossClusterSearchConnection

instance
  Core.NFData
    DeleteInboundCrossClusterSearchConnection

instance
  Core.ToHeaders
    DeleteInboundCrossClusterSearchConnection
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteInboundCrossClusterSearchConnection
  where
  toPath DeleteInboundCrossClusterSearchConnection' {..} =
    Core.mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        Core.toBS crossClusterSearchConnectionId
      ]

instance
  Core.ToQuery
    DeleteInboundCrossClusterSearchConnection
  where
  toQuery = Core.const Core.mempty

-- | The result of a @DeleteInboundCrossClusterSearchConnection@ operation.
-- Contains details of deleted inbound connection.
--
-- /See:/ 'newDeleteInboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteInboundCrossClusterSearchConnectionResponse = DeleteInboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @InboundCrossClusterSearchConnection@ of deleted inbound
    -- connection.
    crossClusterSearchConnection :: Core.Maybe InboundCrossClusterSearchConnection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInboundCrossClusterSearchConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnection', 'deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection' - Specifies the @InboundCrossClusterSearchConnection@ of deleted inbound
-- connection.
--
-- 'httpStatus', 'deleteInboundCrossClusterSearchConnectionResponse_httpStatus' - The response's http status code.
newDeleteInboundCrossClusterSearchConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteInboundCrossClusterSearchConnectionResponse
newDeleteInboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    DeleteInboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the @InboundCrossClusterSearchConnection@ of deleted inbound
-- connection.
deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse (Core.Maybe InboundCrossClusterSearchConnection)
deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection = Lens.lens (\DeleteInboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnection} -> crossClusterSearchConnection) (\s@DeleteInboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnection = a} :: DeleteInboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
deleteInboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse Core.Int
deleteInboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\DeleteInboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteInboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: DeleteInboundCrossClusterSearchConnectionResponse)

instance
  Core.NFData
    DeleteInboundCrossClusterSearchConnectionResponse
