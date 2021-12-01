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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    deleteConnectionResponse_creationTime,
    deleteConnectionResponse_lastModifiedTime,
    deleteConnectionResponse_lastAuthorizedTime,
    deleteConnectionResponse_connectionArn,
    deleteConnectionResponse_connectionState,
    deleteConnectionResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConnectionResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "LastAuthorizedTime")
            Prelude.<*> (x Core..?> "ConnectionArn")
            Prelude.<*> (x Core..?> "ConnectionState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnection where
  hashWithSalt salt' DeleteConnection' {..} =
    salt' `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteConnection where
  rnf DeleteConnection' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DeleteConnection" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath DeleteConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A time stamp for the time that the connection was last modified before
    -- it was deleted.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | A time stamp for the time that the connection was last authorized before
    -- it wa deleted.
    lastAuthorizedTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the connection that was deleted.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the connection before it was deleted.
    connectionState :: Prelude.Maybe ConnectionState,
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
-- 'creationTime', 'deleteConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'lastModifiedTime', 'deleteConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last modified before
-- it was deleted.
--
-- 'lastAuthorizedTime', 'deleteConnectionResponse_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized before
-- it wa deleted.
--
-- 'connectionArn', 'deleteConnectionResponse_connectionArn' - The ARN of the connection that was deleted.
--
-- 'connectionState', 'deleteConnectionResponse_connectionState' - The state of the connection before it was deleted.
--
-- 'httpStatus', 'deleteConnectionResponse_httpStatus' - The response's http status code.
newDeleteConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConnectionResponse
newDeleteConnectionResponse pHttpStatus_ =
  DeleteConnectionResponse'
    { creationTime =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastAuthorizedTime = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the connection was created.
deleteConnectionResponse_creationTime :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.UTCTime)
deleteConnectionResponse_creationTime = Lens.lens (\DeleteConnectionResponse' {creationTime} -> creationTime) (\s@DeleteConnectionResponse' {} a -> s {creationTime = a} :: DeleteConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | A time stamp for the time that the connection was last modified before
-- it was deleted.
deleteConnectionResponse_lastModifiedTime :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.UTCTime)
deleteConnectionResponse_lastModifiedTime = Lens.lens (\DeleteConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DeleteConnectionResponse' {} a -> s {lastModifiedTime = a} :: DeleteConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | A time stamp for the time that the connection was last authorized before
-- it wa deleted.
deleteConnectionResponse_lastAuthorizedTime :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.UTCTime)
deleteConnectionResponse_lastAuthorizedTime = Lens.lens (\DeleteConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@DeleteConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: DeleteConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of the connection that was deleted.
deleteConnectionResponse_connectionArn :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe Prelude.Text)
deleteConnectionResponse_connectionArn = Lens.lens (\DeleteConnectionResponse' {connectionArn} -> connectionArn) (\s@DeleteConnectionResponse' {} a -> s {connectionArn = a} :: DeleteConnectionResponse)

-- | The state of the connection before it was deleted.
deleteConnectionResponse_connectionState :: Lens.Lens' DeleteConnectionResponse (Prelude.Maybe ConnectionState)
deleteConnectionResponse_connectionState = Lens.lens (\DeleteConnectionResponse' {connectionState} -> connectionState) (\s@DeleteConnectionResponse' {} a -> s {connectionState = a} :: DeleteConnectionResponse)

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Prelude.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Prelude.NFData DeleteConnectionResponse where
  rnf DeleteConnectionResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf lastAuthorizedTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
