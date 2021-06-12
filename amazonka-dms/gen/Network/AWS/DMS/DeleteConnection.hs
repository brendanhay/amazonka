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
-- Module      : Network.AWS.DMS.DeleteConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the connection between a replication instance and an endpoint.
module Network.AWS.DMS.DeleteConnection
  ( -- * Creating a Request
    DeleteConnection (..),
    newDeleteConnection,

    -- * Request Lenses
    deleteConnection_endpointArn,
    deleteConnection_replicationInstanceArn,

    -- * Destructuring the Response
    DeleteConnectionResponse (..),
    newDeleteConnectionResponse,

    -- * Response Lenses
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'deleteConnection_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
--
-- 'replicationInstanceArn', 'deleteConnection_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
newDeleteConnection ::
  -- | 'endpointArn'
  Core.Text ->
  -- | 'replicationInstanceArn'
  Core.Text ->
  DeleteConnection
newDeleteConnection
  pEndpointArn_
  pReplicationInstanceArn_ =
    DeleteConnection'
      { endpointArn = pEndpointArn_,
        replicationInstanceArn = pReplicationInstanceArn_
      }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
deleteConnection_endpointArn :: Lens.Lens' DeleteConnection Core.Text
deleteConnection_endpointArn = Lens.lens (\DeleteConnection' {endpointArn} -> endpointArn) (\s@DeleteConnection' {} a -> s {endpointArn = a} :: DeleteConnection)

-- | The Amazon Resource Name (ARN) of the replication instance.
deleteConnection_replicationInstanceArn :: Lens.Lens' DeleteConnection Core.Text
deleteConnection_replicationInstanceArn = Lens.lens (\DeleteConnection' {replicationInstanceArn} -> replicationInstanceArn) (\s@DeleteConnection' {} a -> s {replicationInstanceArn = a} :: DeleteConnection)

instance Core.AWSRequest DeleteConnection where
  type
    AWSResponse DeleteConnection =
      DeleteConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConnectionResponse'
            Core.<$> (x Core..?> "Connection")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteConnection

instance Core.NFData DeleteConnection

instance Core.ToHeaders DeleteConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteConnection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointArn" Core..= endpointArn),
            Core.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              )
          ]
      )

instance Core.ToPath DeleteConnection where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConnection where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | The connection that is being deleted.
    connection :: Core.Maybe Connection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connection', 'deleteConnectionResponse_connection' - The connection that is being deleted.
--
-- 'httpStatus', 'deleteConnectionResponse_httpStatus' - The response's http status code.
newDeleteConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteConnectionResponse
newDeleteConnectionResponse pHttpStatus_ =
  DeleteConnectionResponse'
    { connection =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The connection that is being deleted.
deleteConnectionResponse_connection :: Lens.Lens' DeleteConnectionResponse (Core.Maybe Connection)
deleteConnectionResponse_connection = Lens.lens (\DeleteConnectionResponse' {connection} -> connection) (\s@DeleteConnectionResponse' {} a -> s {connection = a} :: DeleteConnectionResponse)

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Core.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Core.NFData DeleteConnectionResponse
