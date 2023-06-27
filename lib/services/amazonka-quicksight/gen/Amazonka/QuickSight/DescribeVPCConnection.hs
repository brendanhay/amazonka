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
-- Module      : Amazonka.QuickSight.DescribeVPCConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a VPC connection.
module Amazonka.QuickSight.DescribeVPCConnection
  ( -- * Creating a Request
    DescribeVPCConnection (..),
    newDescribeVPCConnection,

    -- * Request Lenses
    describeVPCConnection_awsAccountId,
    describeVPCConnection_vPCConnectionId,

    -- * Destructuring the Response
    DescribeVPCConnectionResponse (..),
    newDescribeVPCConnectionResponse,

    -- * Response Lenses
    describeVPCConnectionResponse_requestId,
    describeVPCConnectionResponse_status,
    describeVPCConnectionResponse_vPCConnection,
    describeVPCConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVPCConnection' smart constructor.
data DescribeVPCConnection = DescribeVPCConnection'
  { -- | The Amazon Web Services account ID of the account that contains the VPC
    -- connection that you want described.
    awsAccountId :: Prelude.Text,
    -- | The ID of the VPC connection that you\'re creating. This ID is a unique
    -- identifier for each Amazon Web Services Region in an Amazon Web Services
    -- account.
    vPCConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVPCConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeVPCConnection_awsAccountId' - The Amazon Web Services account ID of the account that contains the VPC
-- connection that you want described.
--
-- 'vPCConnectionId', 'describeVPCConnection_vPCConnectionId' - The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
newDescribeVPCConnection ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'vPCConnectionId'
  Prelude.Text ->
  DescribeVPCConnection
newDescribeVPCConnection
  pAwsAccountId_
  pVPCConnectionId_ =
    DescribeVPCConnection'
      { awsAccountId =
          pAwsAccountId_,
        vPCConnectionId = pVPCConnectionId_
      }

-- | The Amazon Web Services account ID of the account that contains the VPC
-- connection that you want described.
describeVPCConnection_awsAccountId :: Lens.Lens' DescribeVPCConnection Prelude.Text
describeVPCConnection_awsAccountId = Lens.lens (\DescribeVPCConnection' {awsAccountId} -> awsAccountId) (\s@DescribeVPCConnection' {} a -> s {awsAccountId = a} :: DescribeVPCConnection)

-- | The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
describeVPCConnection_vPCConnectionId :: Lens.Lens' DescribeVPCConnection Prelude.Text
describeVPCConnection_vPCConnectionId = Lens.lens (\DescribeVPCConnection' {vPCConnectionId} -> vPCConnectionId) (\s@DescribeVPCConnection' {} a -> s {vPCConnectionId = a} :: DescribeVPCConnection)

instance Core.AWSRequest DescribeVPCConnection where
  type
    AWSResponse DescribeVPCConnection =
      DescribeVPCConnectionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVPCConnectionResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "VPCConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVPCConnection where
  hashWithSalt _salt DescribeVPCConnection' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` vPCConnectionId

instance Prelude.NFData DescribeVPCConnection where
  rnf DescribeVPCConnection' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf vPCConnectionId

instance Data.ToHeaders DescribeVPCConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVPCConnection where
  toPath DescribeVPCConnection' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/vpc-connections/",
        Data.toBS vPCConnectionId
      ]

instance Data.ToQuery DescribeVPCConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVPCConnectionResponse' smart constructor.
data DescribeVPCConnectionResponse = DescribeVPCConnectionResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe Prelude.Int,
    -- | A response object that provides information for the specified VPC
    -- connection.
    vPCConnection :: Prelude.Maybe VPCConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVPCConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeVPCConnectionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeVPCConnectionResponse_status' - The HTTP status of the request.
--
-- 'vPCConnection', 'describeVPCConnectionResponse_vPCConnection' - A response object that provides information for the specified VPC
-- connection.
--
-- 'httpStatus', 'describeVPCConnectionResponse_httpStatus' - The response's http status code.
newDescribeVPCConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVPCConnectionResponse
newDescribeVPCConnectionResponse pHttpStatus_ =
  DescribeVPCConnectionResponse'
    { requestId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      vPCConnection = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeVPCConnectionResponse_requestId :: Lens.Lens' DescribeVPCConnectionResponse (Prelude.Maybe Prelude.Text)
describeVPCConnectionResponse_requestId = Lens.lens (\DescribeVPCConnectionResponse' {requestId} -> requestId) (\s@DescribeVPCConnectionResponse' {} a -> s {requestId = a} :: DescribeVPCConnectionResponse)

-- | The HTTP status of the request.
describeVPCConnectionResponse_status :: Lens.Lens' DescribeVPCConnectionResponse (Prelude.Maybe Prelude.Int)
describeVPCConnectionResponse_status = Lens.lens (\DescribeVPCConnectionResponse' {status} -> status) (\s@DescribeVPCConnectionResponse' {} a -> s {status = a} :: DescribeVPCConnectionResponse)

-- | A response object that provides information for the specified VPC
-- connection.
describeVPCConnectionResponse_vPCConnection :: Lens.Lens' DescribeVPCConnectionResponse (Prelude.Maybe VPCConnection)
describeVPCConnectionResponse_vPCConnection = Lens.lens (\DescribeVPCConnectionResponse' {vPCConnection} -> vPCConnection) (\s@DescribeVPCConnectionResponse' {} a -> s {vPCConnection = a} :: DescribeVPCConnectionResponse)

-- | The response's http status code.
describeVPCConnectionResponse_httpStatus :: Lens.Lens' DescribeVPCConnectionResponse Prelude.Int
describeVPCConnectionResponse_httpStatus = Lens.lens (\DescribeVPCConnectionResponse' {httpStatus} -> httpStatus) (\s@DescribeVPCConnectionResponse' {} a -> s {httpStatus = a} :: DescribeVPCConnectionResponse)

instance Prelude.NFData DescribeVPCConnectionResponse where
  rnf DescribeVPCConnectionResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vPCConnection
      `Prelude.seq` Prelude.rnf httpStatus
