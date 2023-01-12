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
-- Module      : Amazonka.DMS.DeleteConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the connection between a replication instance and an endpoint.
module Amazonka.DMS.DeleteConnection
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'replicationInstanceArn'
  Prelude.Text ->
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
deleteConnection_endpointArn :: Lens.Lens' DeleteConnection Prelude.Text
deleteConnection_endpointArn = Lens.lens (\DeleteConnection' {endpointArn} -> endpointArn) (\s@DeleteConnection' {} a -> s {endpointArn = a} :: DeleteConnection)

-- | The Amazon Resource Name (ARN) of the replication instance.
deleteConnection_replicationInstanceArn :: Lens.Lens' DeleteConnection Prelude.Text
deleteConnection_replicationInstanceArn = Lens.lens (\DeleteConnection' {replicationInstanceArn} -> replicationInstanceArn) (\s@DeleteConnection' {} a -> s {replicationInstanceArn = a} :: DeleteConnection)

instance Core.AWSRequest DeleteConnection where
  type
    AWSResponse DeleteConnection =
      DeleteConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConnectionResponse'
            Prelude.<$> (x Data..?> "Connection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnection where
  hashWithSalt _salt DeleteConnection' {..} =
    _salt `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` replicationInstanceArn

instance Prelude.NFData DeleteConnection where
  rnf DeleteConnection' {..} =
    Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf replicationInstanceArn

instance Data.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DeleteConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EndpointArn" Data..= endpointArn),
            Prelude.Just
              ( "ReplicationInstanceArn"
                  Data..= replicationInstanceArn
              )
          ]
      )

instance Data.ToPath DeleteConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | The connection that is being deleted.
    connection :: Prelude.Maybe Connection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteConnectionResponse
newDeleteConnectionResponse pHttpStatus_ =
  DeleteConnectionResponse'
    { connection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The connection that is being deleted.
deleteConnectionResponse_connection :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Connection)
deleteConnectionResponse_connection = Lens.lens (\DeleteConnectionResponse' {connection} -> connection) (\s@DeleteConnectionResponse' {} a -> s {connection = a} :: DeleteConnectionResponse)

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Prelude.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Prelude.NFData DeleteConnectionResponse where
  rnf DeleteConnectionResponse' {..} =
    Prelude.rnf connection
      `Prelude.seq` Prelude.rnf httpStatus
