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
-- Module      : Amazonka.EC2.AttachVpnGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a virtual private gateway to a VPC. You can attach one virtual
-- private gateway to one VPC at a time.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html Amazon Web Services Site-to-Site VPN>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
module Amazonka.EC2.AttachVpnGateway
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for AttachVpnGateway.
--
-- /See:/ 'newAttachVpnGateway' smart constructor.
data AttachVpnGateway = AttachVpnGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text,
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'vpnGatewayId'
  Prelude.Text ->
  AttachVpnGateway
newAttachVpnGateway pVpcId_ pVpnGatewayId_ =
  AttachVpnGateway'
    { dryRun = Prelude.Nothing,
      vpcId = pVpcId_,
      vpnGatewayId = pVpnGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
attachVpnGateway_dryRun :: Lens.Lens' AttachVpnGateway (Prelude.Maybe Prelude.Bool)
attachVpnGateway_dryRun = Lens.lens (\AttachVpnGateway' {dryRun} -> dryRun) (\s@AttachVpnGateway' {} a -> s {dryRun = a} :: AttachVpnGateway)

-- | The ID of the VPC.
attachVpnGateway_vpcId :: Lens.Lens' AttachVpnGateway Prelude.Text
attachVpnGateway_vpcId = Lens.lens (\AttachVpnGateway' {vpcId} -> vpcId) (\s@AttachVpnGateway' {} a -> s {vpcId = a} :: AttachVpnGateway)

-- | The ID of the virtual private gateway.
attachVpnGateway_vpnGatewayId :: Lens.Lens' AttachVpnGateway Prelude.Text
attachVpnGateway_vpnGatewayId = Lens.lens (\AttachVpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@AttachVpnGateway' {} a -> s {vpnGatewayId = a} :: AttachVpnGateway)

instance Core.AWSRequest AttachVpnGateway where
  type
    AWSResponse AttachVpnGateway =
      AttachVpnGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AttachVpnGatewayResponse'
            Prelude.<$> (x Data..@? "attachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachVpnGateway where
  hashWithSalt _salt AttachVpnGateway' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpnGatewayId

instance Prelude.NFData AttachVpnGateway where
  rnf AttachVpnGateway' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf vpcId `Prelude.seq`
        Prelude.rnf vpnGatewayId

instance Data.ToHeaders AttachVpnGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AttachVpnGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachVpnGateway where
  toQuery AttachVpnGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AttachVpnGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpcId" Data.=: vpcId,
        "VpnGatewayId" Data.=: vpnGatewayId
      ]

-- | Contains the output of AttachVpnGateway.
--
-- /See:/ 'newAttachVpnGatewayResponse' smart constructor.
data AttachVpnGatewayResponse = AttachVpnGatewayResponse'
  { -- | Information about the attachment.
    vpcAttachment :: Prelude.Maybe VpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AttachVpnGatewayResponse
newAttachVpnGatewayResponse pHttpStatus_ =
  AttachVpnGatewayResponse'
    { vpcAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attachment.
attachVpnGatewayResponse_vpcAttachment :: Lens.Lens' AttachVpnGatewayResponse (Prelude.Maybe VpcAttachment)
attachVpnGatewayResponse_vpcAttachment = Lens.lens (\AttachVpnGatewayResponse' {vpcAttachment} -> vpcAttachment) (\s@AttachVpnGatewayResponse' {} a -> s {vpcAttachment = a} :: AttachVpnGatewayResponse)

-- | The response's http status code.
attachVpnGatewayResponse_httpStatus :: Lens.Lens' AttachVpnGatewayResponse Prelude.Int
attachVpnGatewayResponse_httpStatus = Lens.lens (\AttachVpnGatewayResponse' {httpStatus} -> httpStatus) (\s@AttachVpnGatewayResponse' {} a -> s {httpStatus = a} :: AttachVpnGatewayResponse)

instance Prelude.NFData AttachVpnGatewayResponse where
  rnf AttachVpnGatewayResponse' {..} =
    Prelude.rnf vpcAttachment `Prelude.seq`
      Prelude.rnf httpStatus
