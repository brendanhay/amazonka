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
-- Module      : Network.AWS.CloudWatchEvents.UpdateConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a connection.
module Network.AWS.CloudWatchEvents.UpdateConnection
  ( -- * Creating a Request
    UpdateConnection (..),
    newUpdateConnection,

    -- * Request Lenses
    updateConnection_authorizationType,
    updateConnection_description,
    updateConnection_authParameters,
    updateConnection_name,

    -- * Destructuring the Response
    UpdateConnectionResponse (..),
    newUpdateConnectionResponse,

    -- * Response Lenses
    updateConnectionResponse_creationTime,
    updateConnectionResponse_connectionState,
    updateConnectionResponse_connectionArn,
    updateConnectionResponse_lastModifiedTime,
    updateConnectionResponse_lastAuthorizedTime,
    updateConnectionResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The type of authorization to use for the connection.
    authorizationType :: Core.Maybe ConnectionAuthorizationType,
    -- | A description for the connection.
    description :: Core.Maybe Core.Text,
    -- | The authorization parameters to use for the connection.
    authParameters :: Core.Maybe UpdateConnectionAuthRequestParameters,
    -- | The name of the connection to update.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationType', 'updateConnection_authorizationType' - The type of authorization to use for the connection.
--
-- 'description', 'updateConnection_description' - A description for the connection.
--
-- 'authParameters', 'updateConnection_authParameters' - The authorization parameters to use for the connection.
--
-- 'name', 'updateConnection_name' - The name of the connection to update.
newUpdateConnection ::
  -- | 'name'
  Core.Text ->
  UpdateConnection
newUpdateConnection pName_ =
  UpdateConnection'
    { authorizationType = Core.Nothing,
      description = Core.Nothing,
      authParameters = Core.Nothing,
      name = pName_
    }

-- | The type of authorization to use for the connection.
updateConnection_authorizationType :: Lens.Lens' UpdateConnection (Core.Maybe ConnectionAuthorizationType)
updateConnection_authorizationType = Lens.lens (\UpdateConnection' {authorizationType} -> authorizationType) (\s@UpdateConnection' {} a -> s {authorizationType = a} :: UpdateConnection)

-- | A description for the connection.
updateConnection_description :: Lens.Lens' UpdateConnection (Core.Maybe Core.Text)
updateConnection_description = Lens.lens (\UpdateConnection' {description} -> description) (\s@UpdateConnection' {} a -> s {description = a} :: UpdateConnection)

-- | The authorization parameters to use for the connection.
updateConnection_authParameters :: Lens.Lens' UpdateConnection (Core.Maybe UpdateConnectionAuthRequestParameters)
updateConnection_authParameters = Lens.lens (\UpdateConnection' {authParameters} -> authParameters) (\s@UpdateConnection' {} a -> s {authParameters = a} :: UpdateConnection)

-- | The name of the connection to update.
updateConnection_name :: Lens.Lens' UpdateConnection Core.Text
updateConnection_name = Lens.lens (\UpdateConnection' {name} -> name) (\s@UpdateConnection' {} a -> s {name = a} :: UpdateConnection)

instance Core.AWSRequest UpdateConnection where
  type
    AWSResponse UpdateConnection =
      UpdateConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectionResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ConnectionState")
            Core.<*> (x Core..?> "ConnectionArn")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "LastAuthorizedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateConnection

instance Core.NFData UpdateConnection

instance Core.ToHeaders UpdateConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.UpdateConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateConnection where
  toJSON UpdateConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AuthorizationType" Core..=)
              Core.<$> authorizationType,
            ("Description" Core..=) Core.<$> description,
            ("AuthParameters" Core..=) Core.<$> authParameters,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateConnection where
  toPath = Core.const "/"

instance Core.ToQuery UpdateConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateConnectionResponse' smart constructor.
data UpdateConnectionResponse = UpdateConnectionResponse'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The state of the connection that was updated.
    connectionState :: Core.Maybe ConnectionState,
    -- | The ARN of the connection that was updated.
    connectionArn :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the connection was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'updateConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'connectionState', 'updateConnectionResponse_connectionState' - The state of the connection that was updated.
--
-- 'connectionArn', 'updateConnectionResponse_connectionArn' - The ARN of the connection that was updated.
--
-- 'lastModifiedTime', 'updateConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last modified.
--
-- 'lastAuthorizedTime', 'updateConnectionResponse_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized.
--
-- 'httpStatus', 'updateConnectionResponse_httpStatus' - The response's http status code.
newUpdateConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateConnectionResponse
newUpdateConnectionResponse pHttpStatus_ =
  UpdateConnectionResponse'
    { creationTime =
        Core.Nothing,
      connectionState = Core.Nothing,
      connectionArn = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      lastAuthorizedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the connection was created.
updateConnectionResponse_creationTime :: Lens.Lens' UpdateConnectionResponse (Core.Maybe Core.UTCTime)
updateConnectionResponse_creationTime = Lens.lens (\UpdateConnectionResponse' {creationTime} -> creationTime) (\s@UpdateConnectionResponse' {} a -> s {creationTime = a} :: UpdateConnectionResponse) Core.. Lens.mapping Core._Time

-- | The state of the connection that was updated.
updateConnectionResponse_connectionState :: Lens.Lens' UpdateConnectionResponse (Core.Maybe ConnectionState)
updateConnectionResponse_connectionState = Lens.lens (\UpdateConnectionResponse' {connectionState} -> connectionState) (\s@UpdateConnectionResponse' {} a -> s {connectionState = a} :: UpdateConnectionResponse)

-- | The ARN of the connection that was updated.
updateConnectionResponse_connectionArn :: Lens.Lens' UpdateConnectionResponse (Core.Maybe Core.Text)
updateConnectionResponse_connectionArn = Lens.lens (\UpdateConnectionResponse' {connectionArn} -> connectionArn) (\s@UpdateConnectionResponse' {} a -> s {connectionArn = a} :: UpdateConnectionResponse)

-- | A time stamp for the time that the connection was last modified.
updateConnectionResponse_lastModifiedTime :: Lens.Lens' UpdateConnectionResponse (Core.Maybe Core.UTCTime)
updateConnectionResponse_lastModifiedTime = Lens.lens (\UpdateConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateConnectionResponse' {} a -> s {lastModifiedTime = a} :: UpdateConnectionResponse) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the connection was last authorized.
updateConnectionResponse_lastAuthorizedTime :: Lens.Lens' UpdateConnectionResponse (Core.Maybe Core.UTCTime)
updateConnectionResponse_lastAuthorizedTime = Lens.lens (\UpdateConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@UpdateConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: UpdateConnectionResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
updateConnectionResponse_httpStatus :: Lens.Lens' UpdateConnectionResponse Core.Int
updateConnectionResponse_httpStatus = Lens.lens (\UpdateConnectionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectionResponse' {} a -> s {httpStatus = a} :: UpdateConnectionResponse)

instance Core.NFData UpdateConnectionResponse
