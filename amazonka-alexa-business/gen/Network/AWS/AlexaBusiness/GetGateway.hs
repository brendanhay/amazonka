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
-- Module      : Network.AWS.AlexaBusiness.GetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway.
module Network.AWS.AlexaBusiness.GetGateway
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGateway' smart constructor.
data GetGateway = GetGateway'
  { -- | The ARN of the gateway to get.
    gatewayArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'getGateway_gatewayArn' - The ARN of the gateway to get.
newGetGateway ::
  -- | 'gatewayArn'
  Core.Text ->
  GetGateway
newGetGateway pGatewayArn_ =
  GetGateway' {gatewayArn = pGatewayArn_}

-- | The ARN of the gateway to get.
getGateway_gatewayArn :: Lens.Lens' GetGateway Core.Text
getGateway_gatewayArn = Lens.lens (\GetGateway' {gatewayArn} -> gatewayArn) (\s@GetGateway' {} a -> s {gatewayArn = a} :: GetGateway)

instance Core.AWSRequest GetGateway where
  type AWSResponse GetGateway = GetGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayResponse'
            Core.<$> (x Core..?> "Gateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetGateway

instance Core.NFData GetGateway

instance Core.ToHeaders GetGateway where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.GetGateway" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetGateway where
  toJSON GetGateway' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayArn" Core..= gatewayArn)]
      )

instance Core.ToPath GetGateway where
  toPath = Core.const "/"

instance Core.ToQuery GetGateway where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { -- | The details of the gateway.
    gateway :: Core.Maybe Gateway,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gateway', 'getGatewayResponse_gateway' - The details of the gateway.
--
-- 'httpStatus', 'getGatewayResponse_httpStatus' - The response's http status code.
newGetGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGatewayResponse
newGetGatewayResponse pHttpStatus_ =
  GetGatewayResponse'
    { gateway = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the gateway.
getGatewayResponse_gateway :: Lens.Lens' GetGatewayResponse (Core.Maybe Gateway)
getGatewayResponse_gateway = Lens.lens (\GetGatewayResponse' {gateway} -> gateway) (\s@GetGatewayResponse' {} a -> s {gateway = a} :: GetGatewayResponse)

-- | The response's http status code.
getGatewayResponse_httpStatus :: Lens.Lens' GetGatewayResponse Core.Int
getGatewayResponse_httpStatus = Lens.lens (\GetGatewayResponse' {httpStatus} -> httpStatus) (\s@GetGatewayResponse' {} a -> s {httpStatus = a} :: GetGatewayResponse)

instance Core.NFData GetGatewayResponse
