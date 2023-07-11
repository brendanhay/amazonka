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
-- Module      : Amazonka.AppRunner.DeleteConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an App Runner connection. You must first ensure that there are no
-- running App Runner services that use this connection. If there are any,
-- the @DeleteConnection@ action fails.
module Amazonka.AppRunner.DeleteConnection
  ( -- * Creating a Request
    DeleteConnection (..),
    newDeleteConnection,

    -- * Request Lenses
    deleteConnection_connectionArn,

    -- * Destructuring the Response
    DeleteConnectionResponse (..),
    newDeleteConnectionResponse,

    -- * Response Lenses
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The Amazon Resource Name (ARN) of the App Runner connection that you
    -- want to delete.
    connectionArn :: Prelude.Text
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
-- 'connectionArn', 'deleteConnection_connectionArn' - The Amazon Resource Name (ARN) of the App Runner connection that you
-- want to delete.
newDeleteConnection ::
  -- | 'connectionArn'
  Prelude.Text ->
  DeleteConnection
newDeleteConnection pConnectionArn_ =
  DeleteConnection' {connectionArn = pConnectionArn_}

-- | The Amazon Resource Name (ARN) of the App Runner connection that you
-- want to delete.
deleteConnection_connectionArn :: Lens.Lens' DeleteConnection Prelude.Text
deleteConnection_connectionArn = Lens.lens (\DeleteConnection' {connectionArn} -> connectionArn) (\s@DeleteConnection' {} a -> s {connectionArn = a} :: DeleteConnection)

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
    _salt `Prelude.hashWithSalt` connectionArn

instance Prelude.NFData DeleteConnection where
  rnf DeleteConnection' {..} = Prelude.rnf connectionArn

instance Data.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AppRunner.DeleteConnection" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConnectionArn" Data..= connectionArn)
          ]
      )

instance Data.ToPath DeleteConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | A description of the App Runner connection that this request just
    -- deleted.
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
-- 'connection', 'deleteConnectionResponse_connection' - A description of the App Runner connection that this request just
-- deleted.
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

-- | A description of the App Runner connection that this request just
-- deleted.
deleteConnectionResponse_connection :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Connection)
deleteConnectionResponse_connection = Lens.lens (\DeleteConnectionResponse' {connection} -> connection) (\s@DeleteConnectionResponse' {} a -> s {connection = a} :: DeleteConnectionResponse)

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Prelude.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Prelude.NFData DeleteConnectionResponse where
  rnf DeleteConnectionResponse' {..} =
    Prelude.rnf connection
      `Prelude.seq` Prelude.rnf httpStatus
