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
-- Module      : Network.AWS.EC2.AttachVpnGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a virtual private gateway to a VPC. You can attach one virtual
-- private gateway to one VPC at a time.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN>
-- in the /AWS Site-to-Site VPN User Guide/.
module Network.AWS.EC2.AttachVpnGateway
  ( -- * Creating a Request
    AttachVpnGateway (..),
    newAttachVpnGateway,

    -- * Request Lenses
    attachVpnGateway_dryRun,
    attachVpnGateway_vpcId,
    attachVpnGateway_vpnGatewayId,

    -- * Destructuring the Response
    AttachVpnGatewayResponse (..),
    newAttachVpnGatewayResponse,

    -- * Response Lenses
    attachVpnGatewayResponse_vpcAttachment,
    attachVpnGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AttachVpnGateway.
--
-- /See:/ 'newAttachVpnGateway' smart constructor.
data AttachVpnGateway = AttachVpnGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the VPC.
    vpcId :: Core.Text,
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachVpnGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'attachVpnGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'attachVpnGateway_vpcId' - The ID of the VPC.
--
-- 'vpnGatewayId', 'attachVpnGateway_vpnGatewayId' - The ID of the virtual private gateway.
newAttachVpnGateway ::
  -- | 'vpcId'
  Core.Text ->
  -- | 'vpnGatewayId'
  Core.Text ->
  AttachVpnGateway
newAttachVpnGateway pVpcId_ pVpnGatewayId_ =
  AttachVpnGateway'
    { dryRun = Core.Nothing,
      vpcId = pVpcId_,
      vpnGatewayId = pVpnGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
attachVpnGateway_dryRun :: Lens.Lens' AttachVpnGateway (Core.Maybe Core.Bool)
attachVpnGateway_dryRun = Lens.lens (\AttachVpnGateway' {dryRun} -> dryRun) (\s@AttachVpnGateway' {} a -> s {dryRun = a} :: AttachVpnGateway)

-- | The ID of the VPC.
attachVpnGateway_vpcId :: Lens.Lens' AttachVpnGateway Core.Text
attachVpnGateway_vpcId = Lens.lens (\AttachVpnGateway' {vpcId} -> vpcId) (\s@AttachVpnGateway' {} a -> s {vpcId = a} :: AttachVpnGateway)

-- | The ID of the virtual private gateway.
attachVpnGateway_vpnGatewayId :: Lens.Lens' AttachVpnGateway Core.Text
attachVpnGateway_vpnGatewayId = Lens.lens (\AttachVpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@AttachVpnGateway' {} a -> s {vpnGatewayId = a} :: AttachVpnGateway)

instance Core.AWSRequest AttachVpnGateway where
  type
    AWSResponse AttachVpnGateway =
      AttachVpnGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AttachVpnGatewayResponse'
            Core.<$> (x Core..@? "attachment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachVpnGateway

instance Core.NFData AttachVpnGateway

instance Core.ToHeaders AttachVpnGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AttachVpnGateway where
  toPath = Core.const "/"

instance Core.ToQuery AttachVpnGateway where
  toQuery AttachVpnGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AttachVpnGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "VpcId" Core.=: vpcId,
        "VpnGatewayId" Core.=: vpnGatewayId
      ]

-- | Contains the output of AttachVpnGateway.
--
-- /See:/ 'newAttachVpnGatewayResponse' smart constructor.
data AttachVpnGatewayResponse = AttachVpnGatewayResponse'
  { -- | Information about the attachment.
    vpcAttachment :: Core.Maybe VpcAttachment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachVpnGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcAttachment', 'attachVpnGatewayResponse_vpcAttachment' - Information about the attachment.
--
-- 'httpStatus', 'attachVpnGatewayResponse_httpStatus' - The response's http status code.
newAttachVpnGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AttachVpnGatewayResponse
newAttachVpnGatewayResponse pHttpStatus_ =
  AttachVpnGatewayResponse'
    { vpcAttachment =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attachment.
attachVpnGatewayResponse_vpcAttachment :: Lens.Lens' AttachVpnGatewayResponse (Core.Maybe VpcAttachment)
attachVpnGatewayResponse_vpcAttachment = Lens.lens (\AttachVpnGatewayResponse' {vpcAttachment} -> vpcAttachment) (\s@AttachVpnGatewayResponse' {} a -> s {vpcAttachment = a} :: AttachVpnGatewayResponse)

-- | The response's http status code.
attachVpnGatewayResponse_httpStatus :: Lens.Lens' AttachVpnGatewayResponse Core.Int
attachVpnGatewayResponse_httpStatus = Lens.lens (\AttachVpnGatewayResponse' {httpStatus} -> httpStatus) (\s@AttachVpnGatewayResponse' {} a -> s {httpStatus = a} :: AttachVpnGatewayResponse)

instance Core.NFData AttachVpnGatewayResponse
