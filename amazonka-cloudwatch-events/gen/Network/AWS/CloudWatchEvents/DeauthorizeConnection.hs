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
-- Module      : Network.AWS.CloudWatchEvents.DeauthorizeConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes all authorization parameters from the connection. This lets you
-- remove the secret from the connection so you can reuse it without having
-- to create a new connection.
module Network.AWS.CloudWatchEvents.DeauthorizeConnection
  ( -- * Creating a Request
    DeauthorizeConnection (..),
    newDeauthorizeConnection,

    -- * Request Lenses
    deauthorizeConnection_name,

    -- * Destructuring the Response
    DeauthorizeConnectionResponse (..),
    newDeauthorizeConnectionResponse,

    -- * Response Lenses
    deauthorizeConnectionResponse_creationTime,
    deauthorizeConnectionResponse_connectionState,
    deauthorizeConnectionResponse_connectionArn,
    deauthorizeConnectionResponse_lastModifiedTime,
    deauthorizeConnectionResponse_lastAuthorizedTime,
    deauthorizeConnectionResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeauthorizeConnection' smart constructor.
data DeauthorizeConnection = DeauthorizeConnection'
  { -- | The name of the connection to remove authorization from.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeauthorizeConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deauthorizeConnection_name' - The name of the connection to remove authorization from.
newDeauthorizeConnection ::
  -- | 'name'
  Core.Text ->
  DeauthorizeConnection
newDeauthorizeConnection pName_ =
  DeauthorizeConnection' {name = pName_}

-- | The name of the connection to remove authorization from.
deauthorizeConnection_name :: Lens.Lens' DeauthorizeConnection Core.Text
deauthorizeConnection_name = Lens.lens (\DeauthorizeConnection' {name} -> name) (\s@DeauthorizeConnection' {} a -> s {name = a} :: DeauthorizeConnection)

instance Core.AWSRequest DeauthorizeConnection where
  type
    AWSResponse DeauthorizeConnection =
      DeauthorizeConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeauthorizeConnectionResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ConnectionState")
            Core.<*> (x Core..?> "ConnectionArn")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "LastAuthorizedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeauthorizeConnection

instance Core.NFData DeauthorizeConnection

instance Core.ToHeaders DeauthorizeConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.DeauthorizeConnection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeauthorizeConnection where
  toJSON DeauthorizeConnection' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeauthorizeConnection where
  toPath = Core.const "/"

instance Core.ToQuery DeauthorizeConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeauthorizeConnectionResponse' smart constructor.
data DeauthorizeConnectionResponse = DeauthorizeConnectionResponse'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The state of the connection.
    connectionState :: Core.Maybe ConnectionState,
    -- | The ARN of the connection that authorization was removed from.
    connectionArn :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the connection was last updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeauthorizeConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'deauthorizeConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'connectionState', 'deauthorizeConnectionResponse_connectionState' - The state of the connection.
--
-- 'connectionArn', 'deauthorizeConnectionResponse_connectionArn' - The ARN of the connection that authorization was removed from.
--
-- 'lastModifiedTime', 'deauthorizeConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last updated.
--
-- 'lastAuthorizedTime', 'deauthorizeConnectionResponse_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized.
--
-- 'httpStatus', 'deauthorizeConnectionResponse_httpStatus' - The response's http status code.
newDeauthorizeConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeauthorizeConnectionResponse
newDeauthorizeConnectionResponse pHttpStatus_ =
  DeauthorizeConnectionResponse'
    { creationTime =
        Core.Nothing,
      connectionState = Core.Nothing,
      connectionArn = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      lastAuthorizedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the connection was created.
deauthorizeConnectionResponse_creationTime :: Lens.Lens' DeauthorizeConnectionResponse (Core.Maybe Core.UTCTime)
deauthorizeConnectionResponse_creationTime = Lens.lens (\DeauthorizeConnectionResponse' {creationTime} -> creationTime) (\s@DeauthorizeConnectionResponse' {} a -> s {creationTime = a} :: DeauthorizeConnectionResponse) Core.. Lens.mapping Core._Time

-- | The state of the connection.
deauthorizeConnectionResponse_connectionState :: Lens.Lens' DeauthorizeConnectionResponse (Core.Maybe ConnectionState)
deauthorizeConnectionResponse_connectionState = Lens.lens (\DeauthorizeConnectionResponse' {connectionState} -> connectionState) (\s@DeauthorizeConnectionResponse' {} a -> s {connectionState = a} :: DeauthorizeConnectionResponse)

-- | The ARN of the connection that authorization was removed from.
deauthorizeConnectionResponse_connectionArn :: Lens.Lens' DeauthorizeConnectionResponse (Core.Maybe Core.Text)
deauthorizeConnectionResponse_connectionArn = Lens.lens (\DeauthorizeConnectionResponse' {connectionArn} -> connectionArn) (\s@DeauthorizeConnectionResponse' {} a -> s {connectionArn = a} :: DeauthorizeConnectionResponse)

-- | A time stamp for the time that the connection was last updated.
deauthorizeConnectionResponse_lastModifiedTime :: Lens.Lens' DeauthorizeConnectionResponse (Core.Maybe Core.UTCTime)
deauthorizeConnectionResponse_lastModifiedTime = Lens.lens (\DeauthorizeConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DeauthorizeConnectionResponse' {} a -> s {lastModifiedTime = a} :: DeauthorizeConnectionResponse) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the connection was last authorized.
deauthorizeConnectionResponse_lastAuthorizedTime :: Lens.Lens' DeauthorizeConnectionResponse (Core.Maybe Core.UTCTime)
deauthorizeConnectionResponse_lastAuthorizedTime = Lens.lens (\DeauthorizeConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@DeauthorizeConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: DeauthorizeConnectionResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
deauthorizeConnectionResponse_httpStatus :: Lens.Lens' DeauthorizeConnectionResponse Core.Int
deauthorizeConnectionResponse_httpStatus = Lens.lens (\DeauthorizeConnectionResponse' {httpStatus} -> httpStatus) (\s@DeauthorizeConnectionResponse' {} a -> s {httpStatus = a} :: DeauthorizeConnectionResponse)

instance Core.NFData DeauthorizeConnectionResponse
