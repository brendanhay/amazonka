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
-- Module      : Amazonka.Outposts.GetConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Web Services uses this action to install Outpost servers.
--
-- Gets information about the specified connection.
--
-- Use CloudTrail to monitor this action or Amazon Web Services managed
-- policy for Amazon Web Services Outposts to secure it. For more
-- information, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/security-iam-awsmanpol.html Amazon Web Services managed policies for Amazon Web Services Outposts>
-- and
-- <https://docs.aws.amazon.com/outposts/latest/userguide/logging-using-cloudtrail.html Logging Amazon Web Services Outposts API calls with Amazon Web Services CloudTrail>
-- in the /Amazon Web Services Outposts User Guide/.
module Amazonka.Outposts.GetConnection
  ( -- * Creating a Request
    GetConnection (..),
    newGetConnection,

    -- * Request Lenses
    getConnection_connectionId,

    -- * Destructuring the Response
    GetConnectionResponse (..),
    newGetConnectionResponse,

    -- * Response Lenses
    getConnectionResponse_connectionDetails,
    getConnectionResponse_connectionId,
    getConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnection' smart constructor.
data GetConnection = GetConnection'
  { -- | The ID of the connection.
    connectionId :: Prelude.Text
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
-- 'connectionId', 'getConnection_connectionId' - The ID of the connection.
newGetConnection ::
  -- | 'connectionId'
  Prelude.Text ->
  GetConnection
newGetConnection pConnectionId_ =
  GetConnection' {connectionId = pConnectionId_}

-- | The ID of the connection.
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
            Prelude.<$> (x Data..?> "ConnectionDetails")
            Prelude.<*> (x Data..?> "ConnectionId")
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
      ["/connections/", Data.toBS connectionId]

instance Data.ToQuery GetConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { -- | Information about the connection.
    connectionDetails :: Prelude.Maybe ConnectionDetails,
    -- | The ID of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
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
-- 'connectionDetails', 'getConnectionResponse_connectionDetails' - Information about the connection.
--
-- 'connectionId', 'getConnectionResponse_connectionId' - The ID of the connection.
--
-- 'httpStatus', 'getConnectionResponse_httpStatus' - The response's http status code.
newGetConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectionResponse
newGetConnectionResponse pHttpStatus_ =
  GetConnectionResponse'
    { connectionDetails =
        Prelude.Nothing,
      connectionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the connection.
getConnectionResponse_connectionDetails :: Lens.Lens' GetConnectionResponse (Prelude.Maybe ConnectionDetails)
getConnectionResponse_connectionDetails = Lens.lens (\GetConnectionResponse' {connectionDetails} -> connectionDetails) (\s@GetConnectionResponse' {} a -> s {connectionDetails = a} :: GetConnectionResponse)

-- | The ID of the connection.
getConnectionResponse_connectionId :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Prelude.Text)
getConnectionResponse_connectionId = Lens.lens (\GetConnectionResponse' {connectionId} -> connectionId) (\s@GetConnectionResponse' {} a -> s {connectionId = a} :: GetConnectionResponse)

-- | The response's http status code.
getConnectionResponse_httpStatus :: Lens.Lens' GetConnectionResponse Prelude.Int
getConnectionResponse_httpStatus = Lens.lens (\GetConnectionResponse' {httpStatus} -> httpStatus) (\s@GetConnectionResponse' {} a -> s {httpStatus = a} :: GetConnectionResponse)

instance Prelude.NFData GetConnectionResponse where
  rnf GetConnectionResponse' {..} =
    Prelude.rnf connectionDetails
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf httpStatus
