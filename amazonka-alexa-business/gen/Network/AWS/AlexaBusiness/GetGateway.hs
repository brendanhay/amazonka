{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGateway' smart constructor.
data GetGateway = GetGateway'
  { -- | The ARN of the gateway to get.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetGateway
newGetGateway pGatewayArn_ =
  GetGateway' {gatewayArn = pGatewayArn_}

-- | The ARN of the gateway to get.
getGateway_gatewayArn :: Lens.Lens' GetGateway Prelude.Text
getGateway_gatewayArn = Lens.lens (\GetGateway' {gatewayArn} -> gatewayArn) (\s@GetGateway' {} a -> s {gatewayArn = a} :: GetGateway)

instance Prelude.AWSRequest GetGateway where
  type Rs GetGateway = GetGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayResponse'
            Prelude.<$> (x Prelude..?> "Gateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGateway

instance Prelude.NFData GetGateway

instance Prelude.ToHeaders GetGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.GetGateway" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetGateway where
  toJSON GetGateway' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayArn" Prelude..= gatewayArn)]
      )

instance Prelude.ToPath GetGateway where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { -- | The details of the gateway.
    gateway :: Prelude.Maybe Gateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetGatewayResponse
newGetGatewayResponse pHttpStatus_ =
  GetGatewayResponse'
    { gateway = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the gateway.
getGatewayResponse_gateway :: Lens.Lens' GetGatewayResponse (Prelude.Maybe Gateway)
getGatewayResponse_gateway = Lens.lens (\GetGatewayResponse' {gateway} -> gateway) (\s@GetGatewayResponse' {} a -> s {gateway = a} :: GetGatewayResponse)

-- | The response's http status code.
getGatewayResponse_httpStatus :: Lens.Lens' GetGatewayResponse Prelude.Int
getGatewayResponse_httpStatus = Lens.lens (\GetGatewayResponse' {httpStatus} -> httpStatus) (\s@GetGatewayResponse' {} a -> s {httpStatus = a} :: GetGatewayResponse)

instance Prelude.NFData GetGatewayResponse
