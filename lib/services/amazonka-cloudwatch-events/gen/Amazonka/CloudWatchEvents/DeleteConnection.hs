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
-- Module      : Amazonka.CloudWatchEvents.DeleteConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connection.
module Amazonka.CloudWatchEvents.DeleteConnection
  ( -- * Creating a Request
    DeleteConnection (..),
    newDeleteConnection,

    -- * Request Lenses
    deleteConnection_name,

    -- * Destructuring the Response
    DeleteConnectionResponse (..),
    newDeleteConnectionResponse,

    -- * Response Lenses
    deleteConnectionResponse_connectionState,
    deleteConnectionResponse_connectionArn,
    deleteConnectionResponse_lastModifiedTime,
    deleteConnectionResponse_lastAuthorizedTime,
    deleteConnectionResponse_creationTime,
    deleteConnectionResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The name of the connection to delete.
    name :: Prelude.Text
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
-- 'name', 'deleteConnection_name' - The name of the connection to delete.
newDeleteConnection ::
  -- | 'name'
  Prelude.Text ->
  DeleteConnection
newDeleteConnection pName_ =
  DeleteConnection' {name = pName_}

-- | The name of the connection to delete.
deleteConnection_name :: Lens.Lens' DeleteConnection Prelude.Text
deleteConnection_name = Lens.lens (\DeleteConnection' {name} -> name) (\s@DeleteConnection' {} a -> s {name = a} :: DeleteConnection)

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
            Prelude.<$> (x Data..?> "ConnectionState")
            Prelude.<*> (x Data..?> "ConnectionArn")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LastAuthorizedTime")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnection where
  hashWithSalt _salt DeleteConnection' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteConnection where
  rnf DeleteConnection' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DeleteConnection" :: Prelude.ByteString),
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
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | The state of the connection before it was deleted.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The ARN of the connection that was deleted.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the connection was last modified before
    -- it was deleted.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A time stamp for the time that the connection was last authorized before
    -- it wa deleted.
    lastAuthorizedTime :: Prelude.Maybe Data.POSIX,
    -- | A time stamp for the time that the connection was created.
    creationTime :: Prelude.Maybe Data.POSIX,
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
-- 'connectionState', 'deleteConnectionResponse_connectionState' - The state of the connection before it was deleted.
--
-- 'connectionArn', 'deleteConnectionResponse_connectionArn' - The ARN of the connection that was deleted.
--
-- 'lastModifiedTime', 'deleteConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last modified before
-- it was deleted.
--
-- 'lastAuthorizedTime', 'deleteConnectionResponse_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized before
-- it wa deleted.
--
-- 'creationTime', 'deleteConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'httpStatus', 'deleteConnectionResponse_httpStatus' - The response's http status code.
newDeleteConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConnectionResponse
newDeleteConnectionResponse pHttpStatus_ =
  DeleteConnectionResponse'
    { connectionState =
        Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastAuthorizedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the connection before it was deleted.
deleteConnectionResponse_connectionState :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe ConnectionState)
deleteConnectionResponse_connectionState = Lens.lens (\DeleteConnectionResponse' {connectionState} -> connectionState) (\s@DeleteConnectionResponse' {} a -> s {connectionState = a} :: DeleteConnectionResponse)

-- | The ARN of the connection that was deleted.
deleteConnectionResponse_connectionArn :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.Text)
deleteConnectionResponse_connectionArn = Lens.lens (\DeleteConnectionResponse' {connectionArn} -> connectionArn) (\s@DeleteConnectionResponse' {} a -> s {connectionArn = a} :: DeleteConnectionResponse)

-- | A time stamp for the time that the connection was last modified before
-- it was deleted.
deleteConnectionResponse_lastModifiedTime :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.UTCTime)
deleteConnectionResponse_lastModifiedTime = Lens.lens (\DeleteConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DeleteConnectionResponse' {} a -> s {lastModifiedTime = a} :: DeleteConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | A time stamp for the time that the connection was last authorized before
-- it wa deleted.
deleteConnectionResponse_lastAuthorizedTime :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.UTCTime)
deleteConnectionResponse_lastAuthorizedTime = Lens.lens (\DeleteConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@DeleteConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: DeleteConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | A time stamp for the time that the connection was created.
deleteConnectionResponse_creationTime :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.UTCTime)
deleteConnectionResponse_creationTime = Lens.lens (\DeleteConnectionResponse' {creationTime} -> creationTime) (\s@DeleteConnectionResponse' {} a -> s {creationTime = a} :: DeleteConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Prelude.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Prelude.NFData DeleteConnectionResponse where
  rnf DeleteConnectionResponse' {..} =
    Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastAuthorizedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
