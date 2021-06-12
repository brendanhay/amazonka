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
-- Module      : Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the source domain owner to delete an existing outbound
-- cross-cluster search connection.
module Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
  ( -- * Creating a Request
    DeleteOutboundCrossClusterSearchConnection (..),
    newDeleteOutboundCrossClusterSearchConnection,

    -- * Request Lenses
    deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId,

    -- * Destructuring the Response
    DeleteOutboundCrossClusterSearchConnectionResponse (..),
    newDeleteOutboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteOutboundCrossClusterSearchConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the
-- @DeleteOutboundCrossClusterSearchConnection@ operation.
--
-- /See:/ 'newDeleteOutboundCrossClusterSearchConnection' smart constructor.
data DeleteOutboundCrossClusterSearchConnection = DeleteOutboundCrossClusterSearchConnection'
  { -- | The id of the outbound connection that you want to permanently delete.
    crossClusterSearchConnectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOutboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnectionId', 'deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - The id of the outbound connection that you want to permanently delete.
newDeleteOutboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Core.Text ->
  DeleteOutboundCrossClusterSearchConnection
newDeleteOutboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    DeleteOutboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the outbound connection that you want to permanently delete.
deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' DeleteOutboundCrossClusterSearchConnection Core.Text
deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\DeleteOutboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@DeleteOutboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: DeleteOutboundCrossClusterSearchConnection)

instance
  Core.AWSRequest
    DeleteOutboundCrossClusterSearchConnection
  where
  type
    AWSResponse
      DeleteOutboundCrossClusterSearchConnection =
      DeleteOutboundCrossClusterSearchConnectionResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteOutboundCrossClusterSearchConnectionResponse'
            Core.<$> (x Core..?> "CrossClusterSearchConnection")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteOutboundCrossClusterSearchConnection

instance
  Core.NFData
    DeleteOutboundCrossClusterSearchConnection

instance
  Core.ToHeaders
    DeleteOutboundCrossClusterSearchConnection
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteOutboundCrossClusterSearchConnection
  where
  toPath
    DeleteOutboundCrossClusterSearchConnection' {..} =
      Core.mconcat
        [ "/2015-01-01/es/ccs/outboundConnection/",
          Core.toBS crossClusterSearchConnectionId
        ]

instance
  Core.ToQuery
    DeleteOutboundCrossClusterSearchConnection
  where
  toQuery = Core.const Core.mempty

-- | The result of a @DeleteOutboundCrossClusterSearchConnection@ operation.
-- Contains details of deleted outbound connection.
--
-- /See:/ 'newDeleteOutboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteOutboundCrossClusterSearchConnectionResponse = DeleteOutboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @OutboundCrossClusterSearchConnection@ of deleted outbound
    -- connection.
    crossClusterSearchConnection :: Core.Maybe OutboundCrossClusterSearchConnection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOutboundCrossClusterSearchConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnection', 'deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection' - Specifies the @OutboundCrossClusterSearchConnection@ of deleted outbound
-- connection.
--
-- 'httpStatus', 'deleteOutboundCrossClusterSearchConnectionResponse_httpStatus' - The response's http status code.
newDeleteOutboundCrossClusterSearchConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteOutboundCrossClusterSearchConnectionResponse
newDeleteOutboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    DeleteOutboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the @OutboundCrossClusterSearchConnection@ of deleted outbound
-- connection.
deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse (Core.Maybe OutboundCrossClusterSearchConnection)
deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection = Lens.lens (\DeleteOutboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnection} -> crossClusterSearchConnection) (\s@DeleteOutboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnection = a} :: DeleteOutboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
deleteOutboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse Core.Int
deleteOutboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\DeleteOutboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteOutboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: DeleteOutboundCrossClusterSearchConnectionResponse)

instance
  Core.NFData
    DeleteOutboundCrossClusterSearchConnectionResponse
