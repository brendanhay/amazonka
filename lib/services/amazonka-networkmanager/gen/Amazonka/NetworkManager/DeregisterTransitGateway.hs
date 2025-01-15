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
-- Module      : Amazonka.NetworkManager.DeregisterTransitGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a transit gateway from your global network. This action does
-- not delete your transit gateway, or modify any of its attachments. This
-- action removes any customer gateway associations.
module Amazonka.NetworkManager.DeregisterTransitGateway
  ( -- * Creating a Request
    DeregisterTransitGateway (..),
    newDeregisterTransitGateway,

    -- * Request Lenses
    deregisterTransitGateway_globalNetworkId,
    deregisterTransitGateway_transitGatewayArn,

    -- * Destructuring the Response
    DeregisterTransitGatewayResponse (..),
    newDeregisterTransitGatewayResponse,

    -- * Response Lenses
    deregisterTransitGatewayResponse_transitGatewayRegistration,
    deregisterTransitGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterTransitGateway' smart constructor.
data DeregisterTransitGateway = DeregisterTransitGateway'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transit gateway.
    transitGatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'deregisterTransitGateway_globalNetworkId' - The ID of the global network.
--
-- 'transitGatewayArn', 'deregisterTransitGateway_transitGatewayArn' - The Amazon Resource Name (ARN) of the transit gateway.
newDeregisterTransitGateway ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'transitGatewayArn'
  Prelude.Text ->
  DeregisterTransitGateway
newDeregisterTransitGateway
  pGlobalNetworkId_
  pTransitGatewayArn_ =
    DeregisterTransitGateway'
      { globalNetworkId =
          pGlobalNetworkId_,
        transitGatewayArn = pTransitGatewayArn_
      }

-- | The ID of the global network.
deregisterTransitGateway_globalNetworkId :: Lens.Lens' DeregisterTransitGateway Prelude.Text
deregisterTransitGateway_globalNetworkId = Lens.lens (\DeregisterTransitGateway' {globalNetworkId} -> globalNetworkId) (\s@DeregisterTransitGateway' {} a -> s {globalNetworkId = a} :: DeregisterTransitGateway)

-- | The Amazon Resource Name (ARN) of the transit gateway.
deregisterTransitGateway_transitGatewayArn :: Lens.Lens' DeregisterTransitGateway Prelude.Text
deregisterTransitGateway_transitGatewayArn = Lens.lens (\DeregisterTransitGateway' {transitGatewayArn} -> transitGatewayArn) (\s@DeregisterTransitGateway' {} a -> s {transitGatewayArn = a} :: DeregisterTransitGateway)

instance Core.AWSRequest DeregisterTransitGateway where
  type
    AWSResponse DeregisterTransitGateway =
      DeregisterTransitGatewayResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTransitGatewayResponse'
            Prelude.<$> (x Data..?> "TransitGatewayRegistration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterTransitGateway where
  hashWithSalt _salt DeregisterTransitGateway' {..} =
    _salt
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` transitGatewayArn

instance Prelude.NFData DeregisterTransitGateway where
  rnf DeregisterTransitGateway' {..} =
    Prelude.rnf globalNetworkId `Prelude.seq`
      Prelude.rnf transitGatewayArn

instance Data.ToHeaders DeregisterTransitGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeregisterTransitGateway where
  toPath DeregisterTransitGateway' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/transit-gateway-registrations/",
        Data.toBS transitGatewayArn
      ]

instance Data.ToQuery DeregisterTransitGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterTransitGatewayResponse' smart constructor.
data DeregisterTransitGatewayResponse = DeregisterTransitGatewayResponse'
  { -- | The transit gateway registration information.
    transitGatewayRegistration :: Prelude.Maybe TransitGatewayRegistration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTransitGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRegistration', 'deregisterTransitGatewayResponse_transitGatewayRegistration' - The transit gateway registration information.
--
-- 'httpStatus', 'deregisterTransitGatewayResponse_httpStatus' - The response's http status code.
newDeregisterTransitGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTransitGatewayResponse
newDeregisterTransitGatewayResponse pHttpStatus_ =
  DeregisterTransitGatewayResponse'
    { transitGatewayRegistration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The transit gateway registration information.
deregisterTransitGatewayResponse_transitGatewayRegistration :: Lens.Lens' DeregisterTransitGatewayResponse (Prelude.Maybe TransitGatewayRegistration)
deregisterTransitGatewayResponse_transitGatewayRegistration = Lens.lens (\DeregisterTransitGatewayResponse' {transitGatewayRegistration} -> transitGatewayRegistration) (\s@DeregisterTransitGatewayResponse' {} a -> s {transitGatewayRegistration = a} :: DeregisterTransitGatewayResponse)

-- | The response's http status code.
deregisterTransitGatewayResponse_httpStatus :: Lens.Lens' DeregisterTransitGatewayResponse Prelude.Int
deregisterTransitGatewayResponse_httpStatus = Lens.lens (\DeregisterTransitGatewayResponse' {httpStatus} -> httpStatus) (\s@DeregisterTransitGatewayResponse' {} a -> s {httpStatus = a} :: DeregisterTransitGatewayResponse)

instance
  Prelude.NFData
    DeregisterTransitGatewayResponse
  where
  rnf DeregisterTransitGatewayResponse' {..} =
    Prelude.rnf transitGatewayRegistration `Prelude.seq`
      Prelude.rnf httpStatus
