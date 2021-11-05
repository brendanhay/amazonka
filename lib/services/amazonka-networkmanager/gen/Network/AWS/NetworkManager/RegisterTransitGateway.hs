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
-- Module      : Network.AWS.NetworkManager.RegisterTransitGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a transit gateway in your global network. The transit gateway
-- can be in any AWS Region, but it must be owned by the same AWS account
-- that owns the global network. You cannot register a transit gateway in
-- more than one global network.
module Network.AWS.NetworkManager.RegisterTransitGateway
  ( -- * Creating a Request
    RegisterTransitGateway (..),
    newRegisterTransitGateway,

    -- * Request Lenses
    registerTransitGateway_globalNetworkId,
    registerTransitGateway_transitGatewayArn,

    -- * Destructuring the Response
    RegisterTransitGatewayResponse (..),
    newRegisterTransitGatewayResponse,

    -- * Response Lenses
    registerTransitGatewayResponse_transitGatewayRegistration,
    registerTransitGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkManager.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterTransitGateway' smart constructor.
data RegisterTransitGateway = RegisterTransitGateway'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transit gateway. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazonec2.html#amazonec2-resources-for-iam-policies Resources Defined by Amazon EC2>.
    transitGatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'registerTransitGateway_globalNetworkId' - The ID of the global network.
--
-- 'transitGatewayArn', 'registerTransitGateway_transitGatewayArn' - The Amazon Resource Name (ARN) of the transit gateway. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazonec2.html#amazonec2-resources-for-iam-policies Resources Defined by Amazon EC2>.
newRegisterTransitGateway ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'transitGatewayArn'
  Prelude.Text ->
  RegisterTransitGateway
newRegisterTransitGateway
  pGlobalNetworkId_
  pTransitGatewayArn_ =
    RegisterTransitGateway'
      { globalNetworkId =
          pGlobalNetworkId_,
        transitGatewayArn = pTransitGatewayArn_
      }

-- | The ID of the global network.
registerTransitGateway_globalNetworkId :: Lens.Lens' RegisterTransitGateway Prelude.Text
registerTransitGateway_globalNetworkId = Lens.lens (\RegisterTransitGateway' {globalNetworkId} -> globalNetworkId) (\s@RegisterTransitGateway' {} a -> s {globalNetworkId = a} :: RegisterTransitGateway)

-- | The Amazon Resource Name (ARN) of the transit gateway. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazonec2.html#amazonec2-resources-for-iam-policies Resources Defined by Amazon EC2>.
registerTransitGateway_transitGatewayArn :: Lens.Lens' RegisterTransitGateway Prelude.Text
registerTransitGateway_transitGatewayArn = Lens.lens (\RegisterTransitGateway' {transitGatewayArn} -> transitGatewayArn) (\s@RegisterTransitGateway' {} a -> s {transitGatewayArn = a} :: RegisterTransitGateway)

instance Core.AWSRequest RegisterTransitGateway where
  type
    AWSResponse RegisterTransitGateway =
      RegisterTransitGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTransitGatewayResponse'
            Prelude.<$> (x Core..?> "TransitGatewayRegistration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterTransitGateway

instance Prelude.NFData RegisterTransitGateway

instance Core.ToHeaders RegisterTransitGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterTransitGateway where
  toJSON RegisterTransitGateway' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransitGatewayArn" Core..= transitGatewayArn)
          ]
      )

instance Core.ToPath RegisterTransitGateway where
  toPath RegisterTransitGateway' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/transit-gateway-registrations"
      ]

instance Core.ToQuery RegisterTransitGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterTransitGatewayResponse' smart constructor.
data RegisterTransitGatewayResponse = RegisterTransitGatewayResponse'
  { -- | Information about the transit gateway registration.
    transitGatewayRegistration :: Prelude.Maybe TransitGatewayRegistration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTransitGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRegistration', 'registerTransitGatewayResponse_transitGatewayRegistration' - Information about the transit gateway registration.
--
-- 'httpStatus', 'registerTransitGatewayResponse_httpStatus' - The response's http status code.
newRegisterTransitGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTransitGatewayResponse
newRegisterTransitGatewayResponse pHttpStatus_ =
  RegisterTransitGatewayResponse'
    { transitGatewayRegistration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the transit gateway registration.
registerTransitGatewayResponse_transitGatewayRegistration :: Lens.Lens' RegisterTransitGatewayResponse (Prelude.Maybe TransitGatewayRegistration)
registerTransitGatewayResponse_transitGatewayRegistration = Lens.lens (\RegisterTransitGatewayResponse' {transitGatewayRegistration} -> transitGatewayRegistration) (\s@RegisterTransitGatewayResponse' {} a -> s {transitGatewayRegistration = a} :: RegisterTransitGatewayResponse)

-- | The response's http status code.
registerTransitGatewayResponse_httpStatus :: Lens.Lens' RegisterTransitGatewayResponse Prelude.Int
registerTransitGatewayResponse_httpStatus = Lens.lens (\RegisterTransitGatewayResponse' {httpStatus} -> httpStatus) (\s@RegisterTransitGatewayResponse' {} a -> s {httpStatus = a} :: RegisterTransitGatewayResponse)

instance
  Prelude.NFData
    RegisterTransitGatewayResponse
