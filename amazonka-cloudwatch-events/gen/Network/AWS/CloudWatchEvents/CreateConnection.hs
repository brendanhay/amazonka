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
-- Module      : Network.AWS.CloudWatchEvents.CreateConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection. A connection defines the authorization type and
-- credentials to use for authorization with an API destination HTTP
-- endpoint.
module Network.AWS.CloudWatchEvents.CreateConnection
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
    createConnectionResponse_creationTime,
    createConnectionResponse_connectionState,
    createConnectionResponse_connectionArn,
    createConnectionResponse_lastModifiedTime,
    createConnectionResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | A description for the connection to create.
    description :: Core.Maybe Core.Text,
    -- | The name for the connection to create.
    name :: Core.Text,
    -- | The type of authorization to use for the connection.
    authorizationType :: ConnectionAuthorizationType,
    -- | A @CreateConnectionAuthRequestParameters@ object that contains the
    -- authorization parameters to use to authorize with the endpoint.
    authParameters :: CreateConnectionAuthRequestParameters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
      { description = Core.Nothing,
        name = pName_,
        authorizationType = pAuthorizationType_,
        authParameters = pAuthParameters_
      }

-- | A description for the connection to create.
createConnection_description :: Lens.Lens' CreateConnection (Core.Maybe Core.Text)
createConnection_description = Lens.lens (\CreateConnection' {description} -> description) (\s@CreateConnection' {} a -> s {description = a} :: CreateConnection)

-- | The name for the connection to create.
createConnection_name :: Lens.Lens' CreateConnection Core.Text
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectionResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ConnectionState")
            Core.<*> (x Core..?> "ConnectionArn")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateConnection

instance Core.NFData CreateConnection

instance Core.ToHeaders CreateConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.CreateConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("AuthorizationType" Core..= authorizationType),
            Core.Just ("AuthParameters" Core..= authParameters)
          ]
      )

instance Core.ToPath CreateConnection where
  toPath = Core.const "/"

instance Core.ToQuery CreateConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateConnectionResponse' smart constructor.
data CreateConnectionResponse = CreateConnectionResponse'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The state of the connection that was created by the request.
    connectionState :: Core.Maybe ConnectionState,
    -- | The ARN of the connection that was created by the request.
    connectionArn :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the connection was last updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'createConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'connectionState', 'createConnectionResponse_connectionState' - The state of the connection that was created by the request.
--
-- 'connectionArn', 'createConnectionResponse_connectionArn' - The ARN of the connection that was created by the request.
--
-- 'lastModifiedTime', 'createConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last updated.
--
-- 'httpStatus', 'createConnectionResponse_httpStatus' - The response's http status code.
newCreateConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConnectionResponse
newCreateConnectionResponse pHttpStatus_ =
  CreateConnectionResponse'
    { creationTime =
        Core.Nothing,
      connectionState = Core.Nothing,
      connectionArn = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the connection was created.
createConnectionResponse_creationTime :: Lens.Lens' CreateConnectionResponse (Core.Maybe Core.UTCTime)
createConnectionResponse_creationTime = Lens.lens (\CreateConnectionResponse' {creationTime} -> creationTime) (\s@CreateConnectionResponse' {} a -> s {creationTime = a} :: CreateConnectionResponse) Core.. Lens.mapping Core._Time

-- | The state of the connection that was created by the request.
createConnectionResponse_connectionState :: Lens.Lens' CreateConnectionResponse (Core.Maybe ConnectionState)
createConnectionResponse_connectionState = Lens.lens (\CreateConnectionResponse' {connectionState} -> connectionState) (\s@CreateConnectionResponse' {} a -> s {connectionState = a} :: CreateConnectionResponse)

-- | The ARN of the connection that was created by the request.
createConnectionResponse_connectionArn :: Lens.Lens' CreateConnectionResponse (Core.Maybe Core.Text)
createConnectionResponse_connectionArn = Lens.lens (\CreateConnectionResponse' {connectionArn} -> connectionArn) (\s@CreateConnectionResponse' {} a -> s {connectionArn = a} :: CreateConnectionResponse)

-- | A time stamp for the time that the connection was last updated.
createConnectionResponse_lastModifiedTime :: Lens.Lens' CreateConnectionResponse (Core.Maybe Core.UTCTime)
createConnectionResponse_lastModifiedTime = Lens.lens (\CreateConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateConnectionResponse' {} a -> s {lastModifiedTime = a} :: CreateConnectionResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
createConnectionResponse_httpStatus :: Lens.Lens' CreateConnectionResponse Core.Int
createConnectionResponse_httpStatus = Lens.lens (\CreateConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionResponse' {} a -> s {httpStatus = a} :: CreateConnectionResponse)

instance Core.NFData CreateConnectionResponse
