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
-- Module      : Amazonka.MediaConnect.DescribeGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of a gateway. The response includes the gateway
-- ARN, name, and CIDR blocks, as well as details about the networks.
module Amazonka.MediaConnect.DescribeGateway
  ( -- * Creating a Request
    DescribeGateway (..),
    newDescribeGateway,

    -- * Request Lenses
    describeGateway_gatewayArn,

    -- * Destructuring the Response
    DescribeGatewayResponse (..),
    newDescribeGatewayResponse,

    -- * Response Lenses
    describeGatewayResponse_gateway,
    describeGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGateway' smart constructor.
data DescribeGateway = DescribeGateway'
  { -- | The Amazon Resource Name (ARN) of the gateway that you want to describe.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'describeGateway_gatewayArn' - The Amazon Resource Name (ARN) of the gateway that you want to describe.
newDescribeGateway ::
  -- | 'gatewayArn'
  Prelude.Text ->
  DescribeGateway
newDescribeGateway pGatewayArn_ =
  DescribeGateway' {gatewayArn = pGatewayArn_}

-- | The Amazon Resource Name (ARN) of the gateway that you want to describe.
describeGateway_gatewayArn :: Lens.Lens' DescribeGateway Prelude.Text
describeGateway_gatewayArn = Lens.lens (\DescribeGateway' {gatewayArn} -> gatewayArn) (\s@DescribeGateway' {} a -> s {gatewayArn = a} :: DescribeGateway)

instance Core.AWSRequest DescribeGateway where
  type
    AWSResponse DescribeGateway =
      DescribeGatewayResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayResponse'
            Prelude.<$> (x Data..?> "gateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGateway where
  hashWithSalt _salt DescribeGateway' {..} =
    _salt `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData DescribeGateway where
  rnf DescribeGateway' {..} = Prelude.rnf gatewayArn

instance Data.ToHeaders DescribeGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeGateway where
  toPath DescribeGateway' {..} =
    Prelude.mconcat
      ["/v1/gateways/", Data.toBS gatewayArn]

instance Data.ToQuery DescribeGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGatewayResponse' smart constructor.
data DescribeGatewayResponse = DescribeGatewayResponse'
  { gateway :: Prelude.Maybe Gateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gateway', 'describeGatewayResponse_gateway' - Undocumented member.
--
-- 'httpStatus', 'describeGatewayResponse_httpStatus' - The response's http status code.
newDescribeGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGatewayResponse
newDescribeGatewayResponse pHttpStatus_ =
  DescribeGatewayResponse'
    { gateway = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeGatewayResponse_gateway :: Lens.Lens' DescribeGatewayResponse (Prelude.Maybe Gateway)
describeGatewayResponse_gateway = Lens.lens (\DescribeGatewayResponse' {gateway} -> gateway) (\s@DescribeGatewayResponse' {} a -> s {gateway = a} :: DescribeGatewayResponse)

-- | The response's http status code.
describeGatewayResponse_httpStatus :: Lens.Lens' DescribeGatewayResponse Prelude.Int
describeGatewayResponse_httpStatus = Lens.lens (\DescribeGatewayResponse' {httpStatus} -> httpStatus) (\s@DescribeGatewayResponse' {} a -> s {httpStatus = a} :: DescribeGatewayResponse)

instance Prelude.NFData DescribeGatewayResponse where
  rnf DescribeGatewayResponse' {..} =
    Prelude.rnf gateway
      `Prelude.seq` Prelude.rnf httpStatus
