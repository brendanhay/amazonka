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
-- Module      : Amazonka.BackupGateway.GetGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- By providing the ARN (Amazon Resource Name), this API returns the
-- gateway.
module Amazonka.BackupGateway.GetGateway
  ( -- * Creating a Request
    GetGateway (..),
    newGetGateway,

    -- * Request Lenses
    getGateway_gatewayArn,

    -- * Destructuring the Response
    GetGatewayResponse (..),
    newGetGatewayResponse,

    -- * Response Lenses
    getGatewayResponse_gateway,
    getGatewayResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGateway' smart constructor.
data GetGateway = GetGateway'
  { -- | The Amazon Resource Name (ARN) of the gateway.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'getGateway_gatewayArn' - The Amazon Resource Name (ARN) of the gateway.
newGetGateway ::
  -- | 'gatewayArn'
  Prelude.Text ->
  GetGateway
newGetGateway pGatewayArn_ =
  GetGateway' {gatewayArn = pGatewayArn_}

-- | The Amazon Resource Name (ARN) of the gateway.
getGateway_gatewayArn :: Lens.Lens' GetGateway Prelude.Text
getGateway_gatewayArn = Lens.lens (\GetGateway' {gatewayArn} -> gatewayArn) (\s@GetGateway' {} a -> s {gatewayArn = a} :: GetGateway)

instance Core.AWSRequest GetGateway where
  type AWSResponse GetGateway = GetGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayResponse'
            Prelude.<$> (x Data..?> "Gateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGateway where
  hashWithSalt _salt GetGateway' {..} =
    _salt `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData GetGateway where
  rnf GetGateway' {..} = Prelude.rnf gatewayArn

instance Data.ToHeaders GetGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.GetGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetGateway where
  toJSON GetGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayArn" Data..= gatewayArn)]
      )

instance Data.ToPath GetGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery GetGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { -- | By providing the ARN (Amazon Resource Name), this API returns the
    -- gateway.
    gateway :: Prelude.Maybe GatewayDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gateway', 'getGatewayResponse_gateway' - By providing the ARN (Amazon Resource Name), this API returns the
-- gateway.
--
-- 'httpStatus', 'getGatewayResponse_httpStatus' - The response's http status code.
newGetGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGatewayResponse
newGetGatewayResponse pHttpStatus_ =
  GetGatewayResponse'
    { gateway = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | By providing the ARN (Amazon Resource Name), this API returns the
-- gateway.
getGatewayResponse_gateway :: Lens.Lens' GetGatewayResponse (Prelude.Maybe GatewayDetails)
getGatewayResponse_gateway = Lens.lens (\GetGatewayResponse' {gateway} -> gateway) (\s@GetGatewayResponse' {} a -> s {gateway = a} :: GetGatewayResponse)

-- | The response's http status code.
getGatewayResponse_httpStatus :: Lens.Lens' GetGatewayResponse Prelude.Int
getGatewayResponse_httpStatus = Lens.lens (\GetGatewayResponse' {httpStatus} -> httpStatus) (\s@GetGatewayResponse' {} a -> s {httpStatus = a} :: GetGatewayResponse)

instance Prelude.NFData GetGatewayResponse where
  rnf GetGatewayResponse' {..} =
    Prelude.rnf gateway
      `Prelude.seq` Prelude.rnf httpStatus
