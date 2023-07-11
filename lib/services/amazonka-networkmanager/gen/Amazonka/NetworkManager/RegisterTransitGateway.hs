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
-- Module      : Amazonka.NetworkManager.RegisterTransitGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a transit gateway in your global network. The transit gateway
-- can be in any Amazon Web Services Region, but it must be owned by the
-- same Amazon Web Services account that owns the global network. You
-- cannot register a transit gateway in more than one global network.
module Amazonka.NetworkManager.RegisterTransitGateway
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterTransitGateway' smart constructor.
data RegisterTransitGateway = RegisterTransitGateway'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transit gateway.
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
-- 'transitGatewayArn', 'registerTransitGateway_transitGatewayArn' - The Amazon Resource Name (ARN) of the transit gateway.
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

-- | The Amazon Resource Name (ARN) of the transit gateway.
registerTransitGateway_transitGatewayArn :: Lens.Lens' RegisterTransitGateway Prelude.Text
registerTransitGateway_transitGatewayArn = Lens.lens (\RegisterTransitGateway' {transitGatewayArn} -> transitGatewayArn) (\s@RegisterTransitGateway' {} a -> s {transitGatewayArn = a} :: RegisterTransitGateway)

instance Core.AWSRequest RegisterTransitGateway where
  type
    AWSResponse RegisterTransitGateway =
      RegisterTransitGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTransitGatewayResponse'
            Prelude.<$> (x Data..?> "TransitGatewayRegistration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterTransitGateway where
  hashWithSalt _salt RegisterTransitGateway' {..} =
    _salt
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` transitGatewayArn

instance Prelude.NFData RegisterTransitGateway where
  rnf RegisterTransitGateway' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf transitGatewayArn

instance Data.ToHeaders RegisterTransitGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterTransitGateway where
  toJSON RegisterTransitGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransitGatewayArn" Data..= transitGatewayArn)
          ]
      )

instance Data.ToPath RegisterTransitGateway where
  toPath RegisterTransitGateway' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/transit-gateway-registrations"
      ]

instance Data.ToQuery RegisterTransitGateway where
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
  where
  rnf RegisterTransitGatewayResponse' {..} =
    Prelude.rnf transitGatewayRegistration
      `Prelude.seq` Prelude.rnf httpStatus
