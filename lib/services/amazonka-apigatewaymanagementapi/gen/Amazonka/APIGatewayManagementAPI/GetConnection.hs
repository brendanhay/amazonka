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
-- Module      : Amazonka.APIGatewayManagementAPI.GetConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about the connection with the provided id.
module Amazonka.APIGatewayManagementAPI.GetConnection
  ( -- * Creating a Request
    GetConnection (..),
    newGetConnection,

    -- * Request Lenses
    getConnection_connectionId,

    -- * Destructuring the Response
    GetConnectionResponse (..),
    newGetConnectionResponse,

    -- * Response Lenses
    getConnectionResponse_connectedAt,
    getConnectionResponse_identity,
    getConnectionResponse_lastActiveAt,
    getConnectionResponse_httpStatus,
  )
where

import Amazonka.APIGatewayManagementAPI.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionResponse'
            Prelude.<$> (x Data..?> "connectedAt")
            Prelude.<*> (x Data..?> "identity")
            Prelude.<*> (x Data..?> "lastActiveAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnection where
  hashWithSalt _salt GetConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData GetConnection where
  rnf GetConnection' {..} = Prelude.rnf connectionId

instance Data.ToHeaders GetConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConnection where
  toPath GetConnection' {..} =
    Prelude.mconcat
      ["/@connections/", Data.toBS connectionId]

instance Data.ToQuery GetConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { -- | The time in ISO 8601 format for when the connection was established.
    connectedAt :: Prelude.Maybe Data.ISO8601,
    identity :: Prelude.Maybe Identity,
    -- | The time in ISO 8601 format for when the connection was last active.
    lastActiveAt :: Prelude.Maybe Data.ISO8601,
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
-- 'connectedAt', 'getConnectionResponse_connectedAt' - The time in ISO 8601 format for when the connection was established.
--
-- 'identity', 'getConnectionResponse_identity' - Undocumented member.
--
-- 'lastActiveAt', 'getConnectionResponse_lastActiveAt' - The time in ISO 8601 format for when the connection was last active.
--
-- 'httpStatus', 'getConnectionResponse_httpStatus' - The response's http status code.
newGetConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectionResponse
newGetConnectionResponse pHttpStatus_ =
  GetConnectionResponse'
    { connectedAt =
        Prelude.Nothing,
      identity = Prelude.Nothing,
      lastActiveAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time in ISO 8601 format for when the connection was established.
getConnectionResponse_connectedAt :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Prelude.UTCTime)
getConnectionResponse_connectedAt = Lens.lens (\GetConnectionResponse' {connectedAt} -> connectedAt) (\s@GetConnectionResponse' {} a -> s {connectedAt = a} :: GetConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
getConnectionResponse_identity :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Identity)
getConnectionResponse_identity = Lens.lens (\GetConnectionResponse' {identity} -> identity) (\s@GetConnectionResponse' {} a -> s {identity = a} :: GetConnectionResponse)

-- | The time in ISO 8601 format for when the connection was last active.
getConnectionResponse_lastActiveAt :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Prelude.UTCTime)
getConnectionResponse_lastActiveAt = Lens.lens (\GetConnectionResponse' {lastActiveAt} -> lastActiveAt) (\s@GetConnectionResponse' {} a -> s {lastActiveAt = a} :: GetConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getConnectionResponse_httpStatus :: Lens.Lens' GetConnectionResponse Prelude.Int
getConnectionResponse_httpStatus = Lens.lens (\GetConnectionResponse' {httpStatus} -> httpStatus) (\s@GetConnectionResponse' {} a -> s {httpStatus = a} :: GetConnectionResponse)

instance Prelude.NFData GetConnectionResponse where
  rnf GetConnectionResponse' {..} =
    Prelude.rnf connectedAt `Prelude.seq`
      Prelude.rnf identity `Prelude.seq`
        Prelude.rnf lastActiveAt `Prelude.seq`
          Prelude.rnf httpStatus
