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
-- Module      : Amazonka.CloudWatchEvents.UpdateConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a connection.
module Amazonka.CloudWatchEvents.UpdateConnection
  ( -- * Creating a Request
    UpdateConnection (..),
    newUpdateConnection,

    -- * Request Lenses
    updateConnection_authParameters,
    updateConnection_authorizationType,
    updateConnection_description,
    updateConnection_name,

    -- * Destructuring the Response
    UpdateConnectionResponse (..),
    newUpdateConnectionResponse,

    -- * Response Lenses
    updateConnectionResponse_connectionArn,
    updateConnectionResponse_connectionState,
    updateConnectionResponse_creationTime,
    updateConnectionResponse_lastAuthorizedTime,
    updateConnectionResponse_lastModifiedTime,
    updateConnectionResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The authorization parameters to use for the connection.
    authParameters :: Prelude.Maybe UpdateConnectionAuthRequestParameters,
    -- | The type of authorization to use for the connection.
    authorizationType :: Prelude.Maybe ConnectionAuthorizationType,
    -- | A description for the connection.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'authParameters', 'updateConnection_authParameters' - The authorization parameters to use for the connection.
--
-- 'authorizationType', 'updateConnection_authorizationType' - The type of authorization to use for the connection.
--
-- 'description', 'updateConnection_description' - A description for the connection.
--
-- 'name', 'updateConnection_name' - The name of the connection to update.
newUpdateConnection ::
  -- | 'name'
  Prelude.Text ->
  UpdateConnection
newUpdateConnection pName_ =
  UpdateConnection'
    { authParameters = Prelude.Nothing,
      authorizationType = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | The authorization parameters to use for the connection.
updateConnection_authParameters :: Lens.Lens' UpdateConnection (Prelude.Maybe UpdateConnectionAuthRequestParameters)
updateConnection_authParameters = Lens.lens (\UpdateConnection' {authParameters} -> authParameters) (\s@UpdateConnection' {} a -> s {authParameters = a} :: UpdateConnection)

-- | The type of authorization to use for the connection.
updateConnection_authorizationType :: Lens.Lens' UpdateConnection (Prelude.Maybe ConnectionAuthorizationType)
updateConnection_authorizationType = Lens.lens (\UpdateConnection' {authorizationType} -> authorizationType) (\s@UpdateConnection' {} a -> s {authorizationType = a} :: UpdateConnection)

-- | A description for the connection.
updateConnection_description :: Lens.Lens' UpdateConnection (Prelude.Maybe Prelude.Text)
updateConnection_description = Lens.lens (\UpdateConnection' {description} -> description) (\s@UpdateConnection' {} a -> s {description = a} :: UpdateConnection)

-- | The name of the connection to update.
updateConnection_name :: Lens.Lens' UpdateConnection Prelude.Text
updateConnection_name = Lens.lens (\UpdateConnection' {name} -> name) (\s@UpdateConnection' {} a -> s {name = a} :: UpdateConnection)

instance Core.AWSRequest UpdateConnection where
  type
    AWSResponse UpdateConnection =
      UpdateConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectionResponse'
            Prelude.<$> (x Data..?> "ConnectionArn")
            Prelude.<*> (x Data..?> "ConnectionState")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "LastAuthorizedTime")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnection where
  hashWithSalt _salt UpdateConnection' {..} =
    _salt
      `Prelude.hashWithSalt` authParameters
      `Prelude.hashWithSalt` authorizationType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateConnection where
  rnf UpdateConnection' {..} =
    Prelude.rnf authParameters `Prelude.seq`
      Prelude.rnf authorizationType `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf name

instance Data.ToHeaders UpdateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.UpdateConnection" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnection where
  toJSON UpdateConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthParameters" Data..=)
              Prelude.<$> authParameters,
            ("AuthorizationType" Data..=)
              Prelude.<$> authorizationType,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectionResponse' smart constructor.
data UpdateConnectionResponse = UpdateConnectionResponse'
  { -- | The ARN of the connection that was updated.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the connection that was updated.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | A time stamp for the time that the connection was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Prelude.Maybe Data.POSIX,
    -- | A time stamp for the time that the connection was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
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
-- 'connectionArn', 'updateConnectionResponse_connectionArn' - The ARN of the connection that was updated.
--
-- 'connectionState', 'updateConnectionResponse_connectionState' - The state of the connection that was updated.
--
-- 'creationTime', 'updateConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'lastAuthorizedTime', 'updateConnectionResponse_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized.
--
-- 'lastModifiedTime', 'updateConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last modified.
--
-- 'httpStatus', 'updateConnectionResponse_httpStatus' - The response's http status code.
newUpdateConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectionResponse
newUpdateConnectionResponse pHttpStatus_ =
  UpdateConnectionResponse'
    { connectionArn =
        Prelude.Nothing,
      connectionState = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastAuthorizedTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the connection that was updated.
updateConnectionResponse_connectionArn :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.Text)
updateConnectionResponse_connectionArn = Lens.lens (\UpdateConnectionResponse' {connectionArn} -> connectionArn) (\s@UpdateConnectionResponse' {} a -> s {connectionArn = a} :: UpdateConnectionResponse)

-- | The state of the connection that was updated.
updateConnectionResponse_connectionState :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe ConnectionState)
updateConnectionResponse_connectionState = Lens.lens (\UpdateConnectionResponse' {connectionState} -> connectionState) (\s@UpdateConnectionResponse' {} a -> s {connectionState = a} :: UpdateConnectionResponse)

-- | A time stamp for the time that the connection was created.
updateConnectionResponse_creationTime :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
updateConnectionResponse_creationTime = Lens.lens (\UpdateConnectionResponse' {creationTime} -> creationTime) (\s@UpdateConnectionResponse' {} a -> s {creationTime = a} :: UpdateConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | A time stamp for the time that the connection was last authorized.
updateConnectionResponse_lastAuthorizedTime :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
updateConnectionResponse_lastAuthorizedTime = Lens.lens (\UpdateConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@UpdateConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: UpdateConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | A time stamp for the time that the connection was last modified.
updateConnectionResponse_lastModifiedTime :: Lens.Lens' UpdateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
updateConnectionResponse_lastModifiedTime = Lens.lens (\UpdateConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateConnectionResponse' {} a -> s {lastModifiedTime = a} :: UpdateConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateConnectionResponse_httpStatus :: Lens.Lens' UpdateConnectionResponse Prelude.Int
updateConnectionResponse_httpStatus = Lens.lens (\UpdateConnectionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectionResponse' {} a -> s {httpStatus = a} :: UpdateConnectionResponse)

instance Prelude.NFData UpdateConnectionResponse where
  rnf UpdateConnectionResponse' {..} =
    Prelude.rnf connectionArn `Prelude.seq`
      Prelude.rnf connectionState `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf lastAuthorizedTime `Prelude.seq`
            Prelude.rnf lastModifiedTime `Prelude.seq`
              Prelude.rnf httpStatus
