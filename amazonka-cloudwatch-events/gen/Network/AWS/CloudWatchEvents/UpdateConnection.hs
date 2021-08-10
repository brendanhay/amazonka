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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The type of authorization to use for the connection.
    authorizationType :: Prelude.Maybe ConnectionAuthorizationType,
    -- | A description for the connection.
    description :: Prelude.Maybe Prelude.Text,
    -- | The authorization parameters to use for the connection.
    authParameters :: Prelude.Maybe UpdateConnectionAuthRequestParameters,
    -- | The name of the connection to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateConnection
newUpdateConnection pName_ =
  UpdateConnection'
    { authorizationType =
        Prelude.Nothing,
      description = Prelude.Nothing,
      authParameters = Prelude.Nothing,
      name = pName_
    }

-- | The type of authorization to use for the connection.
updateConnection_authorizationType :: Lens.Lens' UpdateConnection (Prelude.Maybe ConnectionAuthorizationType)
updateConnection_authorizationType = Lens.lens (\UpdateConnection' {authorizationType} -> authorizationType) (\s@UpdateConnection' {} a -> s {authorizationType = a} :: UpdateConnection)

-- | A description for the connection.
updateConnection_description :: Lens.Lens' UpdateConnection (Prelude.Maybe Prelude.Text)
updateConnection_description = Lens.lens (\UpdateConnection' {description} -> description) (\s@UpdateConnection' {} a -> s {description = a} :: UpdateConnection)

-- | The authorization parameters to use for the connection.
updateConnection_authParameters :: Lens.Lens' UpdateConnection (Prelude.Maybe UpdateConnectionAuthRequestParameters)
updateConnection_authParameters = Lens.lens (\UpdateConnection' {authParameters} -> authParameters) (\s@UpdateConnection' {} a -> s {authParameters = a} :: UpdateConnection)

-- | The name of the connection to update.
updateConnection_name :: Lens.Lens' UpdateConnection Prelude.Text
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
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "ConnectionState")
            Prelude.<*> (x Core..?> "ConnectionArn")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "LastAuthorizedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnection

instance Prelude.NFData UpdateConnection

instance Core.ToHeaders UpdateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.UpdateConnection" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateConnection where
  toJSON UpdateConnection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AuthorizationType" Core..=)
              Prelude.<$> authorizationType,
            ("Description" Core..=) Prelude.<$> description,
            ("AuthParameters" Core..=)
              Prelude.<$> authParameters,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectionResponse' smart constructor.
data UpdateConnectionResponse = UpdateConnectionResponse'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The state of the connection that was updated.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The ARN of the connection that was updated.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the connection was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateConnectionResponse
newUpdateConnectionResponse pHttpStatus_ =
  UpdateConnectionResponse'
    { creationTime =
        Prelude.Nothing,
      connectionState = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastAuthorizedTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the connection was created.
updateConnectionResponse_creationTime :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
updateConnectionResponse_creationTime = Lens.lens (\UpdateConnectionResponse' {creationTime} -> creationTime) (\s@UpdateConnectionResponse' {} a -> s {creationTime = a} :: UpdateConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | The state of the connection that was updated.
updateConnectionResponse_connectionState :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe ConnectionState)
updateConnectionResponse_connectionState = Lens.lens (\UpdateConnectionResponse' {connectionState} -> connectionState) (\s@UpdateConnectionResponse' {} a -> s {connectionState = a} :: UpdateConnectionResponse)

-- | The ARN of the connection that was updated.
updateConnectionResponse_connectionArn :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.Text)
updateConnectionResponse_connectionArn = Lens.lens (\UpdateConnectionResponse' {connectionArn} -> connectionArn) (\s@UpdateConnectionResponse' {} a -> s {connectionArn = a} :: UpdateConnectionResponse)

-- | A time stamp for the time that the connection was last modified.
updateConnectionResponse_lastModifiedTime :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
updateConnectionResponse_lastModifiedTime = Lens.lens (\UpdateConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateConnectionResponse' {} a -> s {lastModifiedTime = a} :: UpdateConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | A time stamp for the time that the connection was last authorized.
updateConnectionResponse_lastAuthorizedTime :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
updateConnectionResponse_lastAuthorizedTime = Lens.lens (\UpdateConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@UpdateConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: UpdateConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
updateConnectionResponse_httpStatus :: Lens.Lens' UpdateConnectionResponse Prelude.Int
updateConnectionResponse_httpStatus = Lens.lens (\UpdateConnectionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectionResponse' {} a -> s {httpStatus = a} :: UpdateConnectionResponse)

instance Prelude.NFData UpdateConnectionResponse
