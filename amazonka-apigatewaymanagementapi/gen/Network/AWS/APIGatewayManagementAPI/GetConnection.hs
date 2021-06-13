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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConnection' smart constructor.
data GetConnection = GetConnection'
  { connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetConnection
newGetConnection pConnectionId_ =
  GetConnection' {connectionId = pConnectionId_}

-- | Undocumented member.
getConnection_connectionId :: Lens.Lens' GetConnection Prelude.Text
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
            Prelude.<$> (x Core..?> "identity")
            Prelude.<*> (x Core..?> "lastActiveAt")
            Prelude.<*> (x Core..?> "connectedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnection

instance Prelude.NFData GetConnection

instance Core.ToHeaders GetConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetConnection where
  toPath GetConnection' {..} =
    Prelude.mconcat
      ["/@connections/", Core.toBS connectionId]

instance Core.ToQuery GetConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { identity :: Prelude.Maybe Identity,
    -- | The time in ISO 8601 format for when the connection was last active.
    lastActiveAt :: Prelude.Maybe Core.POSIX,
    -- | The time in ISO 8601 format for when the connection was established.
    connectedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetConnectionResponse
newGetConnectionResponse pHttpStatus_ =
  GetConnectionResponse'
    { identity = Prelude.Nothing,
      lastActiveAt = Prelude.Nothing,
      connectedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getConnectionResponse_identity :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Identity)
getConnectionResponse_identity = Lens.lens (\GetConnectionResponse' {identity} -> identity) (\s@GetConnectionResponse' {} a -> s {identity = a} :: GetConnectionResponse)

-- | The time in ISO 8601 format for when the connection was last active.
getConnectionResponse_lastActiveAt :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Prelude.UTCTime)
getConnectionResponse_lastActiveAt = Lens.lens (\GetConnectionResponse' {lastActiveAt} -> lastActiveAt) (\s@GetConnectionResponse' {} a -> s {lastActiveAt = a} :: GetConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | The time in ISO 8601 format for when the connection was established.
getConnectionResponse_connectedAt :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Prelude.UTCTime)
getConnectionResponse_connectedAt = Lens.lens (\GetConnectionResponse' {connectedAt} -> connectedAt) (\s@GetConnectionResponse' {} a -> s {connectedAt = a} :: GetConnectionResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getConnectionResponse_httpStatus :: Lens.Lens' GetConnectionResponse Prelude.Int
getConnectionResponse_httpStatus = Lens.lens (\GetConnectionResponse' {httpStatus} -> httpStatus) (\s@GetConnectionResponse' {} a -> s {httpStatus = a} :: GetConnectionResponse)

instance Prelude.NFData GetConnectionResponse
