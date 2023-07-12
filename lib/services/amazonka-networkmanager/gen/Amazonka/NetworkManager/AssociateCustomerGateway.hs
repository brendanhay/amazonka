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
-- Module      : Amazonka.NetworkManager.AssociateCustomerGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a customer gateway with a device and optionally, with a link.
-- If you specify a link, it must be associated with the specified device.
--
-- You can only associate customer gateways that are connected to a VPN
-- attachment on a transit gateway or core network registered in your
-- global network. When you register a transit gateway or core network,
-- customer gateways that are connected to the transit gateway are
-- automatically included in the global network. To list customer gateways
-- that are connected to a transit gateway, use the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVpnConnections.html DescribeVpnConnections>
-- EC2 API and filter by @transit-gateway-id@.
--
-- You cannot associate a customer gateway with more than one device and
-- link.
module Amazonka.NetworkManager.AssociateCustomerGateway
  ( -- * Creating a Request
    AssociateCustomerGateway (..),
    newAssociateCustomerGateway,

    -- * Request Lenses
    associateCustomerGateway_linkId,
    associateCustomerGateway_customerGatewayArn,
    associateCustomerGateway_globalNetworkId,
    associateCustomerGateway_deviceId,

    -- * Destructuring the Response
    AssociateCustomerGatewayResponse (..),
    newAssociateCustomerGatewayResponse,

    -- * Response Lenses
    associateCustomerGatewayResponse_customerGatewayAssociation,
    associateCustomerGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateCustomerGateway' smart constructor.
data AssociateCustomerGateway = AssociateCustomerGateway'
  { -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the customer gateway.
    customerGatewayArn :: Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateCustomerGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkId', 'associateCustomerGateway_linkId' - The ID of the link.
--
-- 'customerGatewayArn', 'associateCustomerGateway_customerGatewayArn' - The Amazon Resource Name (ARN) of the customer gateway.
--
-- 'globalNetworkId', 'associateCustomerGateway_globalNetworkId' - The ID of the global network.
--
-- 'deviceId', 'associateCustomerGateway_deviceId' - The ID of the device.
newAssociateCustomerGateway ::
  -- | 'customerGatewayArn'
  Prelude.Text ->
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  AssociateCustomerGateway
newAssociateCustomerGateway
  pCustomerGatewayArn_
  pGlobalNetworkId_
  pDeviceId_ =
    AssociateCustomerGateway'
      { linkId = Prelude.Nothing,
        customerGatewayArn = pCustomerGatewayArn_,
        globalNetworkId = pGlobalNetworkId_,
        deviceId = pDeviceId_
      }

-- | The ID of the link.
associateCustomerGateway_linkId :: Lens.Lens' AssociateCustomerGateway (Prelude.Maybe Prelude.Text)
associateCustomerGateway_linkId = Lens.lens (\AssociateCustomerGateway' {linkId} -> linkId) (\s@AssociateCustomerGateway' {} a -> s {linkId = a} :: AssociateCustomerGateway)

-- | The Amazon Resource Name (ARN) of the customer gateway.
associateCustomerGateway_customerGatewayArn :: Lens.Lens' AssociateCustomerGateway Prelude.Text
associateCustomerGateway_customerGatewayArn = Lens.lens (\AssociateCustomerGateway' {customerGatewayArn} -> customerGatewayArn) (\s@AssociateCustomerGateway' {} a -> s {customerGatewayArn = a} :: AssociateCustomerGateway)

-- | The ID of the global network.
associateCustomerGateway_globalNetworkId :: Lens.Lens' AssociateCustomerGateway Prelude.Text
associateCustomerGateway_globalNetworkId = Lens.lens (\AssociateCustomerGateway' {globalNetworkId} -> globalNetworkId) (\s@AssociateCustomerGateway' {} a -> s {globalNetworkId = a} :: AssociateCustomerGateway)

-- | The ID of the device.
associateCustomerGateway_deviceId :: Lens.Lens' AssociateCustomerGateway Prelude.Text
associateCustomerGateway_deviceId = Lens.lens (\AssociateCustomerGateway' {deviceId} -> deviceId) (\s@AssociateCustomerGateway' {} a -> s {deviceId = a} :: AssociateCustomerGateway)

instance Core.AWSRequest AssociateCustomerGateway where
  type
    AWSResponse AssociateCustomerGateway =
      AssociateCustomerGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateCustomerGatewayResponse'
            Prelude.<$> (x Data..?> "CustomerGatewayAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateCustomerGateway where
  hashWithSalt _salt AssociateCustomerGateway' {..} =
    _salt
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` customerGatewayArn
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` deviceId

instance Prelude.NFData AssociateCustomerGateway where
  rnf AssociateCustomerGateway' {..} =
    Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf customerGatewayArn
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf deviceId

instance Data.ToHeaders AssociateCustomerGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateCustomerGateway where
  toJSON AssociateCustomerGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LinkId" Data..=) Prelude.<$> linkId,
            Prelude.Just
              ("CustomerGatewayArn" Data..= customerGatewayArn),
            Prelude.Just ("DeviceId" Data..= deviceId)
          ]
      )

instance Data.ToPath AssociateCustomerGateway where
  toPath AssociateCustomerGateway' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/customer-gateway-associations"
      ]

instance Data.ToQuery AssociateCustomerGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateCustomerGatewayResponse' smart constructor.
data AssociateCustomerGatewayResponse = AssociateCustomerGatewayResponse'
  { -- | The customer gateway association.
    customerGatewayAssociation :: Prelude.Maybe CustomerGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateCustomerGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayAssociation', 'associateCustomerGatewayResponse_customerGatewayAssociation' - The customer gateway association.
--
-- 'httpStatus', 'associateCustomerGatewayResponse_httpStatus' - The response's http status code.
newAssociateCustomerGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateCustomerGatewayResponse
newAssociateCustomerGatewayResponse pHttpStatus_ =
  AssociateCustomerGatewayResponse'
    { customerGatewayAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The customer gateway association.
associateCustomerGatewayResponse_customerGatewayAssociation :: Lens.Lens' AssociateCustomerGatewayResponse (Prelude.Maybe CustomerGatewayAssociation)
associateCustomerGatewayResponse_customerGatewayAssociation = Lens.lens (\AssociateCustomerGatewayResponse' {customerGatewayAssociation} -> customerGatewayAssociation) (\s@AssociateCustomerGatewayResponse' {} a -> s {customerGatewayAssociation = a} :: AssociateCustomerGatewayResponse)

-- | The response's http status code.
associateCustomerGatewayResponse_httpStatus :: Lens.Lens' AssociateCustomerGatewayResponse Prelude.Int
associateCustomerGatewayResponse_httpStatus = Lens.lens (\AssociateCustomerGatewayResponse' {httpStatus} -> httpStatus) (\s@AssociateCustomerGatewayResponse' {} a -> s {httpStatus = a} :: AssociateCustomerGatewayResponse)

instance
  Prelude.NFData
    AssociateCustomerGatewayResponse
  where
  rnf AssociateCustomerGatewayResponse' {..} =
    Prelude.rnf customerGatewayAssociation
      `Prelude.seq` Prelude.rnf httpStatus
