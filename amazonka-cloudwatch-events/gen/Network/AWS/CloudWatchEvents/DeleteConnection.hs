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
-- Module      : Network.AWS.CloudWatchEvents.DeleteConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connection.
module Network.AWS.CloudWatchEvents.DeleteConnection
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
    deleteConnectionResponse_connectionState,
    deleteConnectionResponse_connectionArn,
    deleteConnectionResponse_lastModifiedTime,
    deleteConnectionResponse_lastAuthorizedTime,
    deleteConnectionResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The name of the connection to delete.
    name :: Core.Text
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
-- 'name', 'deleteConnection_name' - The name of the connection to delete.
newDeleteConnection ::
  -- | 'name'
  Core.Text ->
  DeleteConnection
newDeleteConnection pName_ =
  DeleteConnection' {name = pName_}

-- | The name of the connection to delete.
deleteConnection_name :: Lens.Lens' DeleteConnection Core.Text
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
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ConnectionState")
            Core.<*> (x Core..?> "ConnectionArn")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "LastAuthorizedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteConnection

instance Core.NFData DeleteConnection

instance Core.ToHeaders DeleteConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DeleteConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteConnection where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The state of the connection before it was deleted.
    connectionState :: Core.Maybe ConnectionState,
    -- | The ARN of the connection that was deleted.
    connectionArn :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the connection was last modified before
    -- it was deleted.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the connection was last authorized before
    -- it wa deleted.
    lastAuthorizedTime :: Core.Maybe Core.POSIX,
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
-- 'creationTime', 'deleteConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
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
-- 'httpStatus', 'deleteConnectionResponse_httpStatus' - The response's http status code.
newDeleteConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteConnectionResponse
newDeleteConnectionResponse pHttpStatus_ =
  DeleteConnectionResponse'
    { creationTime =
        Core.Nothing,
      connectionState = Core.Nothing,
      connectionArn = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      lastAuthorizedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the connection was created.
deleteConnectionResponse_creationTime :: Lens.Lens' DeleteConnectionResponse (Core.Maybe Core.UTCTime)
deleteConnectionResponse_creationTime = Lens.lens (\DeleteConnectionResponse' {creationTime} -> creationTime) (\s@DeleteConnectionResponse' {} a -> s {creationTime = a} :: DeleteConnectionResponse) Core.. Lens.mapping Core._Time

-- | The state of the connection before it was deleted.
deleteConnectionResponse_connectionState :: Lens.Lens' DeleteConnectionResponse (Core.Maybe ConnectionState)
deleteConnectionResponse_connectionState = Lens.lens (\DeleteConnectionResponse' {connectionState} -> connectionState) (\s@DeleteConnectionResponse' {} a -> s {connectionState = a} :: DeleteConnectionResponse)

-- | The ARN of the connection that was deleted.
deleteConnectionResponse_connectionArn :: Lens.Lens' DeleteConnectionResponse (Core.Maybe Core.Text)
deleteConnectionResponse_connectionArn = Lens.lens (\DeleteConnectionResponse' {connectionArn} -> connectionArn) (\s@DeleteConnectionResponse' {} a -> s {connectionArn = a} :: DeleteConnectionResponse)

-- | A time stamp for the time that the connection was last modified before
-- it was deleted.
deleteConnectionResponse_lastModifiedTime :: Lens.Lens' DeleteConnectionResponse (Core.Maybe Core.UTCTime)
deleteConnectionResponse_lastModifiedTime = Lens.lens (\DeleteConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DeleteConnectionResponse' {} a -> s {lastModifiedTime = a} :: DeleteConnectionResponse) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the connection was last authorized before
-- it wa deleted.
deleteConnectionResponse_lastAuthorizedTime :: Lens.Lens' DeleteConnectionResponse (Core.Maybe Core.UTCTime)
deleteConnectionResponse_lastAuthorizedTime = Lens.lens (\DeleteConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@DeleteConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: DeleteConnectionResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
deleteConnectionResponse_httpStatus :: Lens.Lens' DeleteConnectionResponse Core.Int
deleteConnectionResponse_httpStatus = Lens.lens (\DeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionResponse' {} a -> s {httpStatus = a} :: DeleteConnectionResponse)

instance Core.NFData DeleteConnectionResponse
