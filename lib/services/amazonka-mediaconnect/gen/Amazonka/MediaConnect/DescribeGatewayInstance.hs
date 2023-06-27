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
-- Module      : Amazonka.MediaConnect.DescribeGatewayInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of an instance.
module Amazonka.MediaConnect.DescribeGatewayInstance
  ( -- * Creating a Request
    DescribeGatewayInstance (..),
    newDescribeGatewayInstance,

    -- * Request Lenses
    describeGatewayInstance_gatewayInstanceArn,

    -- * Destructuring the Response
    DescribeGatewayInstanceResponse (..),
    newDescribeGatewayInstanceResponse,

    -- * Response Lenses
    describeGatewayInstanceResponse_gatewayInstance,
    describeGatewayInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGatewayInstance' smart constructor.
data DescribeGatewayInstance = DescribeGatewayInstance'
  { -- | The Amazon Resource Name (ARN) of the gateway instance that you want to
    -- describe.
    gatewayInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayInstanceArn', 'describeGatewayInstance_gatewayInstanceArn' - The Amazon Resource Name (ARN) of the gateway instance that you want to
-- describe.
newDescribeGatewayInstance ::
  -- | 'gatewayInstanceArn'
  Prelude.Text ->
  DescribeGatewayInstance
newDescribeGatewayInstance pGatewayInstanceArn_ =
  DescribeGatewayInstance'
    { gatewayInstanceArn =
        pGatewayInstanceArn_
    }

-- | The Amazon Resource Name (ARN) of the gateway instance that you want to
-- describe.
describeGatewayInstance_gatewayInstanceArn :: Lens.Lens' DescribeGatewayInstance Prelude.Text
describeGatewayInstance_gatewayInstanceArn = Lens.lens (\DescribeGatewayInstance' {gatewayInstanceArn} -> gatewayInstanceArn) (\s@DescribeGatewayInstance' {} a -> s {gatewayInstanceArn = a} :: DescribeGatewayInstance)

instance Core.AWSRequest DescribeGatewayInstance where
  type
    AWSResponse DescribeGatewayInstance =
      DescribeGatewayInstanceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayInstanceResponse'
            Prelude.<$> (x Data..?> "gatewayInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGatewayInstance where
  hashWithSalt _salt DescribeGatewayInstance' {..} =
    _salt `Prelude.hashWithSalt` gatewayInstanceArn

instance Prelude.NFData DescribeGatewayInstance where
  rnf DescribeGatewayInstance' {..} =
    Prelude.rnf gatewayInstanceArn

instance Data.ToHeaders DescribeGatewayInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeGatewayInstance where
  toPath DescribeGatewayInstance' {..} =
    Prelude.mconcat
      [ "/v1/gateway-instances/",
        Data.toBS gatewayInstanceArn
      ]

instance Data.ToQuery DescribeGatewayInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGatewayInstanceResponse' smart constructor.
data DescribeGatewayInstanceResponse = DescribeGatewayInstanceResponse'
  { gatewayInstance :: Prelude.Maybe GatewayInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayInstance', 'describeGatewayInstanceResponse_gatewayInstance' - Undocumented member.
--
-- 'httpStatus', 'describeGatewayInstanceResponse_httpStatus' - The response's http status code.
newDescribeGatewayInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGatewayInstanceResponse
newDescribeGatewayInstanceResponse pHttpStatus_ =
  DescribeGatewayInstanceResponse'
    { gatewayInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeGatewayInstanceResponse_gatewayInstance :: Lens.Lens' DescribeGatewayInstanceResponse (Prelude.Maybe GatewayInstance)
describeGatewayInstanceResponse_gatewayInstance = Lens.lens (\DescribeGatewayInstanceResponse' {gatewayInstance} -> gatewayInstance) (\s@DescribeGatewayInstanceResponse' {} a -> s {gatewayInstance = a} :: DescribeGatewayInstanceResponse)

-- | The response's http status code.
describeGatewayInstanceResponse_httpStatus :: Lens.Lens' DescribeGatewayInstanceResponse Prelude.Int
describeGatewayInstanceResponse_httpStatus = Lens.lens (\DescribeGatewayInstanceResponse' {httpStatus} -> httpStatus) (\s@DescribeGatewayInstanceResponse' {} a -> s {httpStatus = a} :: DescribeGatewayInstanceResponse)

instance
  Prelude.NFData
    DescribeGatewayInstanceResponse
  where
  rnf DescribeGatewayInstanceResponse' {..} =
    Prelude.rnf gatewayInstance
      `Prelude.seq` Prelude.rnf httpStatus
