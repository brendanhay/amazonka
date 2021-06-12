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
-- Module      : Network.AWS.APIGatewayManagementAPI.GetConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about the connection with the provided id.
module Network.AWS.APIGatewayManagementAPI.GetConnection
  ( -- * Creating a Request
    GetConnection (..),
    newGetConnection,

    -- * Request Lenses
    getConnection_connectionId,

    -- * Destructuring the Response
    GetConnectionResponse (..),
    newGetConnectionResponse,

    -- * Response Lenses
    getConnectionResponse_identity,
    getConnectionResponse_lastActiveAt,
    getConnectionResponse_connectedAt,
    getConnectionResponse_httpStatus,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConnection' smart constructor.
data GetConnection = GetConnection'
  { connectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'getConnection_connectionId' - Undocumented member.
newGetConnection ::
  -- | 'connectionId'
  Core.Text ->
  GetConnection
newGetConnection pConnectionId_ =
  GetConnection' {connectionId = pConnectionId_}

-- | Undocumented member.
getConnection_connectionId :: Lens.Lens' GetConnection Core.Text
getConnection_connectionId = Lens.lens (\GetConnection' {connectionId} -> connectionId) (\s@GetConnection' {} a -> s {connectionId = a} :: GetConnection)

instance Core.AWSRequest GetConnection where
  type
    AWSResponse GetConnection =
      GetConnectionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionResponse'
            Core.<$> (x Core..?> "identity")
            Core.<*> (x Core..?> "lastActiveAt")
            Core.<*> (x Core..?> "connectedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConnection

instance Core.NFData GetConnection

instance Core.ToHeaders GetConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetConnection where
  toPath GetConnection' {..} =
    Core.mconcat
      ["/@connections/", Core.toBS connectionId]

instance Core.ToQuery GetConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { identity :: Core.Maybe Identity,
    -- | The time in ISO 8601 format for when the connection was last active.
    lastActiveAt :: Core.Maybe Core.POSIX,
    -- | The time in ISO 8601 format for when the connection was established.
    connectedAt :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'getConnectionResponse_identity' - Undocumented member.
--
-- 'lastActiveAt', 'getConnectionResponse_lastActiveAt' - The time in ISO 8601 format for when the connection was last active.
--
-- 'connectedAt', 'getConnectionResponse_connectedAt' - The time in ISO 8601 format for when the connection was established.
--
-- 'httpStatus', 'getConnectionResponse_httpStatus' - The response's http status code.
newGetConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConnectionResponse
newGetConnectionResponse pHttpStatus_ =
  GetConnectionResponse'
    { identity = Core.Nothing,
      lastActiveAt = Core.Nothing,
      connectedAt = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getConnectionResponse_identity :: Lens.Lens' GetConnectionResponse (Core.Maybe Identity)
getConnectionResponse_identity = Lens.lens (\GetConnectionResponse' {identity} -> identity) (\s@GetConnectionResponse' {} a -> s {identity = a} :: GetConnectionResponse)

-- | The time in ISO 8601 format for when the connection was last active.
getConnectionResponse_lastActiveAt :: Lens.Lens' GetConnectionResponse (Core.Maybe Core.UTCTime)
getConnectionResponse_lastActiveAt = Lens.lens (\GetConnectionResponse' {lastActiveAt} -> lastActiveAt) (\s@GetConnectionResponse' {} a -> s {lastActiveAt = a} :: GetConnectionResponse) Core.. Lens.mapping Core._Time

-- | The time in ISO 8601 format for when the connection was established.
getConnectionResponse_connectedAt :: Lens.Lens' GetConnectionResponse (Core.Maybe Core.UTCTime)
getConnectionResponse_connectedAt = Lens.lens (\GetConnectionResponse' {connectedAt} -> connectedAt) (\s@GetConnectionResponse' {} a -> s {connectedAt = a} :: GetConnectionResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
getConnectionResponse_httpStatus :: Lens.Lens' GetConnectionResponse Core.Int
getConnectionResponse_httpStatus = Lens.lens (\GetConnectionResponse' {httpStatus} -> httpStatus) (\s@GetConnectionResponse' {} a -> s {httpStatus = a} :: GetConnectionResponse)

instance Core.NFData GetConnectionResponse
