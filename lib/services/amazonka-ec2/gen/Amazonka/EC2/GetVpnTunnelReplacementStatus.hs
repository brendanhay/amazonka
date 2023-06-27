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
-- Module      : Amazonka.EC2.GetVpnTunnelReplacementStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details of available tunnel endpoint maintenance.
module Amazonka.EC2.GetVpnTunnelReplacementStatus
  ( -- * Creating a Request
    GetVpnTunnelReplacementStatus (..),
    newGetVpnTunnelReplacementStatus,

    -- * Request Lenses
    getVpnTunnelReplacementStatus_dryRun,
    getVpnTunnelReplacementStatus_vpnConnectionId,
    getVpnTunnelReplacementStatus_vpnTunnelOutsideIpAddress,

    -- * Destructuring the Response
    GetVpnTunnelReplacementStatusResponse (..),
    newGetVpnTunnelReplacementStatusResponse,

    -- * Response Lenses
    getVpnTunnelReplacementStatusResponse_customerGatewayId,
    getVpnTunnelReplacementStatusResponse_maintenanceDetails,
    getVpnTunnelReplacementStatusResponse_transitGatewayId,
    getVpnTunnelReplacementStatusResponse_vpnConnectionId,
    getVpnTunnelReplacementStatusResponse_vpnGatewayId,
    getVpnTunnelReplacementStatusResponse_vpnTunnelOutsideIpAddress,
    getVpnTunnelReplacementStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVpnTunnelReplacementStatus' smart constructor.
data GetVpnTunnelReplacementStatus = GetVpnTunnelReplacementStatus'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Site-to-Site VPN connection.
    vpnConnectionId :: Prelude.Text,
    -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIpAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpnTunnelReplacementStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getVpnTunnelReplacementStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpnConnectionId', 'getVpnTunnelReplacementStatus_vpnConnectionId' - The ID of the Site-to-Site VPN connection.
--
-- 'vpnTunnelOutsideIpAddress', 'getVpnTunnelReplacementStatus_vpnTunnelOutsideIpAddress' - The external IP address of the VPN tunnel.
newGetVpnTunnelReplacementStatus ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  -- | 'vpnTunnelOutsideIpAddress'
  Prelude.Text ->
  GetVpnTunnelReplacementStatus
newGetVpnTunnelReplacementStatus
  pVpnConnectionId_
  pVpnTunnelOutsideIpAddress_ =
    GetVpnTunnelReplacementStatus'
      { dryRun =
          Prelude.Nothing,
        vpnConnectionId = pVpnConnectionId_,
        vpnTunnelOutsideIpAddress =
          pVpnTunnelOutsideIpAddress_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getVpnTunnelReplacementStatus_dryRun :: Lens.Lens' GetVpnTunnelReplacementStatus (Prelude.Maybe Prelude.Bool)
getVpnTunnelReplacementStatus_dryRun = Lens.lens (\GetVpnTunnelReplacementStatus' {dryRun} -> dryRun) (\s@GetVpnTunnelReplacementStatus' {} a -> s {dryRun = a} :: GetVpnTunnelReplacementStatus)

-- | The ID of the Site-to-Site VPN connection.
getVpnTunnelReplacementStatus_vpnConnectionId :: Lens.Lens' GetVpnTunnelReplacementStatus Prelude.Text
getVpnTunnelReplacementStatus_vpnConnectionId = Lens.lens (\GetVpnTunnelReplacementStatus' {vpnConnectionId} -> vpnConnectionId) (\s@GetVpnTunnelReplacementStatus' {} a -> s {vpnConnectionId = a} :: GetVpnTunnelReplacementStatus)

-- | The external IP address of the VPN tunnel.
getVpnTunnelReplacementStatus_vpnTunnelOutsideIpAddress :: Lens.Lens' GetVpnTunnelReplacementStatus Prelude.Text
getVpnTunnelReplacementStatus_vpnTunnelOutsideIpAddress = Lens.lens (\GetVpnTunnelReplacementStatus' {vpnTunnelOutsideIpAddress} -> vpnTunnelOutsideIpAddress) (\s@GetVpnTunnelReplacementStatus' {} a -> s {vpnTunnelOutsideIpAddress = a} :: GetVpnTunnelReplacementStatus)

instance
  Core.AWSRequest
    GetVpnTunnelReplacementStatus
  where
  type
    AWSResponse GetVpnTunnelReplacementStatus =
      GetVpnTunnelReplacementStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetVpnTunnelReplacementStatusResponse'
            Prelude.<$> (x Data..@? "customerGatewayId")
            Prelude.<*> (x Data..@? "maintenanceDetails")
            Prelude.<*> (x Data..@? "transitGatewayId")
            Prelude.<*> (x Data..@? "vpnConnectionId")
            Prelude.<*> (x Data..@? "vpnGatewayId")
            Prelude.<*> (x Data..@? "vpnTunnelOutsideIpAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVpnTunnelReplacementStatus
  where
  hashWithSalt _salt GetVpnTunnelReplacementStatus' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpnConnectionId
      `Prelude.hashWithSalt` vpnTunnelOutsideIpAddress

instance Prelude.NFData GetVpnTunnelReplacementStatus where
  rnf GetVpnTunnelReplacementStatus' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf vpnConnectionId
      `Prelude.seq` Prelude.rnf vpnTunnelOutsideIpAddress

instance Data.ToHeaders GetVpnTunnelReplacementStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVpnTunnelReplacementStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetVpnTunnelReplacementStatus where
  toQuery GetVpnTunnelReplacementStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetVpnTunnelReplacementStatus" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpnConnectionId" Data.=: vpnConnectionId,
        "VpnTunnelOutsideIpAddress"
          Data.=: vpnTunnelOutsideIpAddress
      ]

-- | /See:/ 'newGetVpnTunnelReplacementStatusResponse' smart constructor.
data GetVpnTunnelReplacementStatusResponse = GetVpnTunnelReplacementStatusResponse'
  { -- | The ID of the customer gateway.
    customerGatewayId :: Prelude.Maybe Prelude.Text,
    -- | Get details of pending tunnel endpoint maintenance.
    maintenanceDetails :: Prelude.Maybe MaintenanceDetails,
    -- | The ID of the transit gateway associated with the VPN connection.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Site-to-Site VPN connection.
    vpnConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpnTunnelReplacementStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayId', 'getVpnTunnelReplacementStatusResponse_customerGatewayId' - The ID of the customer gateway.
--
-- 'maintenanceDetails', 'getVpnTunnelReplacementStatusResponse_maintenanceDetails' - Get details of pending tunnel endpoint maintenance.
--
-- 'transitGatewayId', 'getVpnTunnelReplacementStatusResponse_transitGatewayId' - The ID of the transit gateway associated with the VPN connection.
--
-- 'vpnConnectionId', 'getVpnTunnelReplacementStatusResponse_vpnConnectionId' - The ID of the Site-to-Site VPN connection.
--
-- 'vpnGatewayId', 'getVpnTunnelReplacementStatusResponse_vpnGatewayId' - The ID of the virtual private gateway.
--
-- 'vpnTunnelOutsideIpAddress', 'getVpnTunnelReplacementStatusResponse_vpnTunnelOutsideIpAddress' - The external IP address of the VPN tunnel.
--
-- 'httpStatus', 'getVpnTunnelReplacementStatusResponse_httpStatus' - The response's http status code.
newGetVpnTunnelReplacementStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVpnTunnelReplacementStatusResponse
newGetVpnTunnelReplacementStatusResponse pHttpStatus_ =
  GetVpnTunnelReplacementStatusResponse'
    { customerGatewayId =
        Prelude.Nothing,
      maintenanceDetails = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      vpnConnectionId = Prelude.Nothing,
      vpnGatewayId = Prelude.Nothing,
      vpnTunnelOutsideIpAddress =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the customer gateway.
getVpnTunnelReplacementStatusResponse_customerGatewayId :: Lens.Lens' GetVpnTunnelReplacementStatusResponse (Prelude.Maybe Prelude.Text)
getVpnTunnelReplacementStatusResponse_customerGatewayId = Lens.lens (\GetVpnTunnelReplacementStatusResponse' {customerGatewayId} -> customerGatewayId) (\s@GetVpnTunnelReplacementStatusResponse' {} a -> s {customerGatewayId = a} :: GetVpnTunnelReplacementStatusResponse)

-- | Get details of pending tunnel endpoint maintenance.
getVpnTunnelReplacementStatusResponse_maintenanceDetails :: Lens.Lens' GetVpnTunnelReplacementStatusResponse (Prelude.Maybe MaintenanceDetails)
getVpnTunnelReplacementStatusResponse_maintenanceDetails = Lens.lens (\GetVpnTunnelReplacementStatusResponse' {maintenanceDetails} -> maintenanceDetails) (\s@GetVpnTunnelReplacementStatusResponse' {} a -> s {maintenanceDetails = a} :: GetVpnTunnelReplacementStatusResponse)

-- | The ID of the transit gateway associated with the VPN connection.
getVpnTunnelReplacementStatusResponse_transitGatewayId :: Lens.Lens' GetVpnTunnelReplacementStatusResponse (Prelude.Maybe Prelude.Text)
getVpnTunnelReplacementStatusResponse_transitGatewayId = Lens.lens (\GetVpnTunnelReplacementStatusResponse' {transitGatewayId} -> transitGatewayId) (\s@GetVpnTunnelReplacementStatusResponse' {} a -> s {transitGatewayId = a} :: GetVpnTunnelReplacementStatusResponse)

-- | The ID of the Site-to-Site VPN connection.
getVpnTunnelReplacementStatusResponse_vpnConnectionId :: Lens.Lens' GetVpnTunnelReplacementStatusResponse (Prelude.Maybe Prelude.Text)
getVpnTunnelReplacementStatusResponse_vpnConnectionId = Lens.lens (\GetVpnTunnelReplacementStatusResponse' {vpnConnectionId} -> vpnConnectionId) (\s@GetVpnTunnelReplacementStatusResponse' {} a -> s {vpnConnectionId = a} :: GetVpnTunnelReplacementStatusResponse)

-- | The ID of the virtual private gateway.
getVpnTunnelReplacementStatusResponse_vpnGatewayId :: Lens.Lens' GetVpnTunnelReplacementStatusResponse (Prelude.Maybe Prelude.Text)
getVpnTunnelReplacementStatusResponse_vpnGatewayId = Lens.lens (\GetVpnTunnelReplacementStatusResponse' {vpnGatewayId} -> vpnGatewayId) (\s@GetVpnTunnelReplacementStatusResponse' {} a -> s {vpnGatewayId = a} :: GetVpnTunnelReplacementStatusResponse)

-- | The external IP address of the VPN tunnel.
getVpnTunnelReplacementStatusResponse_vpnTunnelOutsideIpAddress :: Lens.Lens' GetVpnTunnelReplacementStatusResponse (Prelude.Maybe Prelude.Text)
getVpnTunnelReplacementStatusResponse_vpnTunnelOutsideIpAddress = Lens.lens (\GetVpnTunnelReplacementStatusResponse' {vpnTunnelOutsideIpAddress} -> vpnTunnelOutsideIpAddress) (\s@GetVpnTunnelReplacementStatusResponse' {} a -> s {vpnTunnelOutsideIpAddress = a} :: GetVpnTunnelReplacementStatusResponse)

-- | The response's http status code.
getVpnTunnelReplacementStatusResponse_httpStatus :: Lens.Lens' GetVpnTunnelReplacementStatusResponse Prelude.Int
getVpnTunnelReplacementStatusResponse_httpStatus = Lens.lens (\GetVpnTunnelReplacementStatusResponse' {httpStatus} -> httpStatus) (\s@GetVpnTunnelReplacementStatusResponse' {} a -> s {httpStatus = a} :: GetVpnTunnelReplacementStatusResponse)

instance
  Prelude.NFData
    GetVpnTunnelReplacementStatusResponse
  where
  rnf GetVpnTunnelReplacementStatusResponse' {..} =
    Prelude.rnf customerGatewayId
      `Prelude.seq` Prelude.rnf maintenanceDetails
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf vpnConnectionId
      `Prelude.seq` Prelude.rnf vpnGatewayId
      `Prelude.seq` Prelude.rnf vpnTunnelOutsideIpAddress
      `Prelude.seq` Prelude.rnf httpStatus
