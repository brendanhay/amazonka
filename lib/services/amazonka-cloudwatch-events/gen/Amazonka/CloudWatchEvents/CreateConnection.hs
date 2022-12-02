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
-- Module      : Amazonka.CloudWatchEvents.CreateConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection. A connection defines the authorization type and
-- credentials to use for authorization with an API destination HTTP
-- endpoint.
module Amazonka.CloudWatchEvents.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_description,
    createConnection_name,
    createConnection_authorizationType,
    createConnection_authParameters,

    -- * Destructuring the Response
    CreateConnectionResponse (..),
    newCreateConnectionResponse,

    -- * Response Lenses
    createConnectionResponse_connectionState,
    createConnectionResponse_connectionArn,
    createConnectionResponse_lastModifiedTime,
    createConnectionResponse_creationTime,
    createConnectionResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | A description for the connection to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name for the connection to create.
    name :: Prelude.Text,
    -- | The type of authorization to use for the connection.
    authorizationType :: ConnectionAuthorizationType,
    -- | A @CreateConnectionAuthRequestParameters@ object that contains the
    -- authorization parameters to use to authorize with the endpoint.
    authParameters :: CreateConnectionAuthRequestParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createConnection_description' - A description for the connection to create.
--
-- 'name', 'createConnection_name' - The name for the connection to create.
--
-- 'authorizationType', 'createConnection_authorizationType' - The type of authorization to use for the connection.
--
-- 'authParameters', 'createConnection_authParameters' - A @CreateConnectionAuthRequestParameters@ object that contains the
-- authorization parameters to use to authorize with the endpoint.
newCreateConnection ::
  -- | 'name'
  Prelude.Text ->
  -- | 'authorizationType'
  ConnectionAuthorizationType ->
  -- | 'authParameters'
  CreateConnectionAuthRequestParameters ->
  CreateConnection
newCreateConnection
  pName_
  pAuthorizationType_
  pAuthParameters_ =
    CreateConnection'
      { description = Prelude.Nothing,
        name = pName_,
        authorizationType = pAuthorizationType_,
        authParameters = pAuthParameters_
      }

-- | A description for the connection to create.
createConnection_description :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_description = Lens.lens (\CreateConnection' {description} -> description) (\s@CreateConnection' {} a -> s {description = a} :: CreateConnection)

-- | The name for the connection to create.
createConnection_name :: Lens.Lens' CreateConnection Prelude.Text
createConnection_name = Lens.lens (\CreateConnection' {name} -> name) (\s@CreateConnection' {} a -> s {name = a} :: CreateConnection)

-- | The type of authorization to use for the connection.
createConnection_authorizationType :: Lens.Lens' CreateConnection ConnectionAuthorizationType
createConnection_authorizationType = Lens.lens (\CreateConnection' {authorizationType} -> authorizationType) (\s@CreateConnection' {} a -> s {authorizationType = a} :: CreateConnection)

-- | A @CreateConnectionAuthRequestParameters@ object that contains the
-- authorization parameters to use to authorize with the endpoint.
createConnection_authParameters :: Lens.Lens' CreateConnection CreateConnectionAuthRequestParameters
createConnection_authParameters = Lens.lens (\CreateConnection' {authParameters} -> authParameters) (\s@CreateConnection' {} a -> s {authParameters = a} :: CreateConnection)

instance Core.AWSRequest CreateConnection where
  type
    AWSResponse CreateConnection =
      CreateConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectionResponse'
            Prelude.<$> (x Data..?> "ConnectionState")
            Prelude.<*> (x Data..?> "ConnectionArn")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnection where
  hashWithSalt _salt CreateConnection' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` authorizationType
      `Prelude.hashWithSalt` authParameters

instance Prelude.NFData CreateConnection where
  rnf CreateConnection' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf authorizationType
      `Prelude.seq` Prelude.rnf authParameters

instance Data.ToHeaders CreateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.CreateConnection" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("AuthorizationType" Data..= authorizationType),
            Prelude.Just
              ("AuthParameters" Data..= authParameters)
          ]
      )

instance Data.ToPath CreateConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectionResponse' smart constructor.
data CreateConnectionResponse = CreateConnectionResponse'
  { -- | The state of the connection that was created by the request.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The ARN of the connection that was created by the request.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the connection was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A time stamp for the time that the connection was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionState', 'createConnectionResponse_connectionState' - The state of the connection that was created by the request.
--
-- 'connectionArn', 'createConnectionResponse_connectionArn' - The ARN of the connection that was created by the request.
--
-- 'lastModifiedTime', 'createConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last updated.
--
-- 'creationTime', 'createConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'httpStatus', 'createConnectionResponse_httpStatus' - The response's http status code.
newCreateConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectionResponse
newCreateConnectionResponse pHttpStatus_ =
  CreateConnectionResponse'
    { connectionState =
        Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the connection that was created by the request.
createConnectionResponse_connectionState :: Lens.Lens' CreateConnectionResponse (Prelude.Maybe ConnectionState)
createConnectionResponse_connectionState = Lens.lens (\CreateConnectionResponse' {connectionState} -> connectionState) (\s@CreateConnectionResponse' {} a -> s {connectionState = a} :: CreateConnectionResponse)

-- | The ARN of the connection that was created by the request.
createConnectionResponse_connectionArn :: Lens.Lens' CreateConnectionResponse (Prelude.Maybe Prelude.Text)
createConnectionResponse_connectionArn = Lens.lens (\CreateConnectionResponse' {connectionArn} -> connectionArn) (\s@CreateConnectionResponse' {} a -> s {connectionArn = a} :: CreateConnectionResponse)

-- | A time stamp for the time that the connection was last updated.
createConnectionResponse_lastModifiedTime :: Lens.Lens' CreateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
createConnectionResponse_lastModifiedTime = Lens.lens (\CreateConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateConnectionResponse' {} a -> s {lastModifiedTime = a} :: CreateConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | A time stamp for the time that the connection was created.
createConnectionResponse_creationTime :: Lens.Lens' CreateConnectionResponse (Prelude.Maybe Prelude.UTCTime)
createConnectionResponse_creationTime = Lens.lens (\CreateConnectionResponse' {creationTime} -> creationTime) (\s@CreateConnectionResponse' {} a -> s {creationTime = a} :: CreateConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createConnectionResponse_httpStatus :: Lens.Lens' CreateConnectionResponse Prelude.Int
createConnectionResponse_httpStatus = Lens.lens (\CreateConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionResponse' {} a -> s {httpStatus = a} :: CreateConnectionResponse)

instance Prelude.NFData CreateConnectionResponse where
  rnf CreateConnectionResponse' {..} =
    Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
