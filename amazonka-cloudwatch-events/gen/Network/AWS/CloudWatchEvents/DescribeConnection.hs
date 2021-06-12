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
-- Module      : Network.AWS.CloudWatchEvents.DescribeConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a connection.
module Network.AWS.CloudWatchEvents.DescribeConnection
  ( -- * Creating a Request
    DescribeConnection (..),
    newDescribeConnection,

    -- * Request Lenses
    describeConnection_name,

    -- * Destructuring the Response
    DescribeConnectionResponse (..),
    newDescribeConnectionResponse,

    -- * Response Lenses
    describeConnectionResponse_creationTime,
    describeConnectionResponse_connectionState,
    describeConnectionResponse_secretArn,
    describeConnectionResponse_stateReason,
    describeConnectionResponse_authorizationType,
    describeConnectionResponse_connectionArn,
    describeConnectionResponse_name,
    describeConnectionResponse_lastModifiedTime,
    describeConnectionResponse_description,
    describeConnectionResponse_lastAuthorizedTime,
    describeConnectionResponse_authParameters,
    describeConnectionResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConnection' smart constructor.
data DescribeConnection = DescribeConnection'
  { -- | The name of the connection to retrieve.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeConnection_name' - The name of the connection to retrieve.
newDescribeConnection ::
  -- | 'name'
  Core.Text ->
  DescribeConnection
newDescribeConnection pName_ =
  DescribeConnection' {name = pName_}

-- | The name of the connection to retrieve.
describeConnection_name :: Lens.Lens' DescribeConnection Core.Text
describeConnection_name = Lens.lens (\DescribeConnection' {name} -> name) (\s@DescribeConnection' {} a -> s {name = a} :: DescribeConnection)

instance Core.AWSRequest DescribeConnection where
  type
    AWSResponse DescribeConnection =
      DescribeConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ConnectionState")
            Core.<*> (x Core..?> "SecretArn")
            Core.<*> (x Core..?> "StateReason")
            Core.<*> (x Core..?> "AuthorizationType")
            Core.<*> (x Core..?> "ConnectionArn")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "LastAuthorizedTime")
            Core.<*> (x Core..?> "AuthParameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConnection

instance Core.NFData DescribeConnection

instance Core.ToHeaders DescribeConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DescribeConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConnection where
  toJSON DescribeConnection' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DescribeConnection where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeConnectionResponse' smart constructor.
data DescribeConnectionResponse = DescribeConnectionResponse'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The state of the connection retrieved.
    connectionState :: Core.Maybe ConnectionState,
    -- | The ARN of the secret created from the authorization parameters
    -- specified for the connection.
    secretArn :: Core.Maybe Core.Text,
    -- | The reason that the connection is in the current connection state.
    stateReason :: Core.Maybe Core.Text,
    -- | The type of authorization specified for the connection.
    authorizationType :: Core.Maybe ConnectionAuthorizationType,
    -- | The ARN of the connection retrieved.
    connectionArn :: Core.Maybe Core.Text,
    -- | The name of the connection retrieved.
    name :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the connection was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The description for the connection retrieved.
    description :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Core.Maybe Core.POSIX,
    -- | The parameters to use for authorization for the connection.
    authParameters :: Core.Maybe ConnectionAuthResponseParameters,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeConnectionResponse_creationTime' - A time stamp for the time that the connection was created.
--
-- 'connectionState', 'describeConnectionResponse_connectionState' - The state of the connection retrieved.
--
-- 'secretArn', 'describeConnectionResponse_secretArn' - The ARN of the secret created from the authorization parameters
-- specified for the connection.
--
-- 'stateReason', 'describeConnectionResponse_stateReason' - The reason that the connection is in the current connection state.
--
-- 'authorizationType', 'describeConnectionResponse_authorizationType' - The type of authorization specified for the connection.
--
-- 'connectionArn', 'describeConnectionResponse_connectionArn' - The ARN of the connection retrieved.
--
-- 'name', 'describeConnectionResponse_name' - The name of the connection retrieved.
--
-- 'lastModifiedTime', 'describeConnectionResponse_lastModifiedTime' - A time stamp for the time that the connection was last modified.
--
-- 'description', 'describeConnectionResponse_description' - The description for the connection retrieved.
--
-- 'lastAuthorizedTime', 'describeConnectionResponse_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized.
--
-- 'authParameters', 'describeConnectionResponse_authParameters' - The parameters to use for authorization for the connection.
--
-- 'httpStatus', 'describeConnectionResponse_httpStatus' - The response's http status code.
newDescribeConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConnectionResponse
newDescribeConnectionResponse pHttpStatus_ =
  DescribeConnectionResponse'
    { creationTime =
        Core.Nothing,
      connectionState = Core.Nothing,
      secretArn = Core.Nothing,
      stateReason = Core.Nothing,
      authorizationType = Core.Nothing,
      connectionArn = Core.Nothing,
      name = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      description = Core.Nothing,
      lastAuthorizedTime = Core.Nothing,
      authParameters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the connection was created.
describeConnectionResponse_creationTime :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.UTCTime)
describeConnectionResponse_creationTime = Lens.lens (\DescribeConnectionResponse' {creationTime} -> creationTime) (\s@DescribeConnectionResponse' {} a -> s {creationTime = a} :: DescribeConnectionResponse) Core.. Lens.mapping Core._Time

-- | The state of the connection retrieved.
describeConnectionResponse_connectionState :: Lens.Lens' DescribeConnectionResponse (Core.Maybe ConnectionState)
describeConnectionResponse_connectionState = Lens.lens (\DescribeConnectionResponse' {connectionState} -> connectionState) (\s@DescribeConnectionResponse' {} a -> s {connectionState = a} :: DescribeConnectionResponse)

-- | The ARN of the secret created from the authorization parameters
-- specified for the connection.
describeConnectionResponse_secretArn :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.Text)
describeConnectionResponse_secretArn = Lens.lens (\DescribeConnectionResponse' {secretArn} -> secretArn) (\s@DescribeConnectionResponse' {} a -> s {secretArn = a} :: DescribeConnectionResponse)

-- | The reason that the connection is in the current connection state.
describeConnectionResponse_stateReason :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.Text)
describeConnectionResponse_stateReason = Lens.lens (\DescribeConnectionResponse' {stateReason} -> stateReason) (\s@DescribeConnectionResponse' {} a -> s {stateReason = a} :: DescribeConnectionResponse)

-- | The type of authorization specified for the connection.
describeConnectionResponse_authorizationType :: Lens.Lens' DescribeConnectionResponse (Core.Maybe ConnectionAuthorizationType)
describeConnectionResponse_authorizationType = Lens.lens (\DescribeConnectionResponse' {authorizationType} -> authorizationType) (\s@DescribeConnectionResponse' {} a -> s {authorizationType = a} :: DescribeConnectionResponse)

-- | The ARN of the connection retrieved.
describeConnectionResponse_connectionArn :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.Text)
describeConnectionResponse_connectionArn = Lens.lens (\DescribeConnectionResponse' {connectionArn} -> connectionArn) (\s@DescribeConnectionResponse' {} a -> s {connectionArn = a} :: DescribeConnectionResponse)

-- | The name of the connection retrieved.
describeConnectionResponse_name :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.Text)
describeConnectionResponse_name = Lens.lens (\DescribeConnectionResponse' {name} -> name) (\s@DescribeConnectionResponse' {} a -> s {name = a} :: DescribeConnectionResponse)

-- | A time stamp for the time that the connection was last modified.
describeConnectionResponse_lastModifiedTime :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.UTCTime)
describeConnectionResponse_lastModifiedTime = Lens.lens (\DescribeConnectionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeConnectionResponse' {} a -> s {lastModifiedTime = a} :: DescribeConnectionResponse) Core.. Lens.mapping Core._Time

-- | The description for the connection retrieved.
describeConnectionResponse_description :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.Text)
describeConnectionResponse_description = Lens.lens (\DescribeConnectionResponse' {description} -> description) (\s@DescribeConnectionResponse' {} a -> s {description = a} :: DescribeConnectionResponse)

-- | A time stamp for the time that the connection was last authorized.
describeConnectionResponse_lastAuthorizedTime :: Lens.Lens' DescribeConnectionResponse (Core.Maybe Core.UTCTime)
describeConnectionResponse_lastAuthorizedTime = Lens.lens (\DescribeConnectionResponse' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@DescribeConnectionResponse' {} a -> s {lastAuthorizedTime = a} :: DescribeConnectionResponse) Core.. Lens.mapping Core._Time

-- | The parameters to use for authorization for the connection.
describeConnectionResponse_authParameters :: Lens.Lens' DescribeConnectionResponse (Core.Maybe ConnectionAuthResponseParameters)
describeConnectionResponse_authParameters = Lens.lens (\DescribeConnectionResponse' {authParameters} -> authParameters) (\s@DescribeConnectionResponse' {} a -> s {authParameters = a} :: DescribeConnectionResponse)

-- | The response's http status code.
describeConnectionResponse_httpStatus :: Lens.Lens' DescribeConnectionResponse Core.Int
describeConnectionResponse_httpStatus = Lens.lens (\DescribeConnectionResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectionResponse' {} a -> s {httpStatus = a} :: DescribeConnectionResponse)

instance Core.NFData DescribeConnectionResponse
