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
-- Module      : Network.AWS.EC2.DetachVpnGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a virtual private gateway from a VPC. You do this if you\'re
-- planning to turn off the VPC and not use it anymore. You can confirm a
-- virtual private gateway has been completely detached from a VPC by
-- describing the virtual private gateway (any attachments to the virtual
-- private gateway are also described).
--
-- You must wait for the attachment\'s state to switch to @detached@ before
-- you can delete the VPC or attach a different VPC to the virtual private
-- gateway.
module Network.AWS.EC2.DetachVpnGateway
  ( -- * Creating a Request
    DetachVpnGateway (..),
    newDetachVpnGateway,

    -- * Request Lenses
    detachVpnGateway_dryRun,
    detachVpnGateway_vpcId,
    detachVpnGateway_vpnGatewayId,

    -- * Destructuring the Response
    DetachVpnGatewayResponse (..),
    newDetachVpnGatewayResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DetachVpnGateway.
--
-- /See:/ 'newDetachVpnGateway' smart constructor.
data DetachVpnGateway = DetachVpnGateway'
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
-- Create a value of 'DetachVpnGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'detachVpnGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'detachVpnGateway_vpcId' - The ID of the VPC.
--
-- 'vpnGatewayId', 'detachVpnGateway_vpnGatewayId' - The ID of the virtual private gateway.
newDetachVpnGateway ::
  -- | 'vpcId'
  Core.Text ->
  -- | 'vpnGatewayId'
  Core.Text ->
  DetachVpnGateway
newDetachVpnGateway pVpcId_ pVpnGatewayId_ =
  DetachVpnGateway'
    { dryRun = Core.Nothing,
      vpcId = pVpcId_,
      vpnGatewayId = pVpnGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachVpnGateway_dryRun :: Lens.Lens' DetachVpnGateway (Core.Maybe Core.Bool)
detachVpnGateway_dryRun = Lens.lens (\DetachVpnGateway' {dryRun} -> dryRun) (\s@DetachVpnGateway' {} a -> s {dryRun = a} :: DetachVpnGateway)

-- | The ID of the VPC.
detachVpnGateway_vpcId :: Lens.Lens' DetachVpnGateway Core.Text
detachVpnGateway_vpcId = Lens.lens (\DetachVpnGateway' {vpcId} -> vpcId) (\s@DetachVpnGateway' {} a -> s {vpcId = a} :: DetachVpnGateway)

-- | The ID of the virtual private gateway.
detachVpnGateway_vpnGatewayId :: Lens.Lens' DetachVpnGateway Core.Text
detachVpnGateway_vpnGatewayId = Lens.lens (\DetachVpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@DetachVpnGateway' {} a -> s {vpnGatewayId = a} :: DetachVpnGateway)

instance Core.AWSRequest DetachVpnGateway where
  type
    AWSResponse DetachVpnGateway =
      DetachVpnGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DetachVpnGatewayResponse'

instance Core.Hashable DetachVpnGateway

instance Core.NFData DetachVpnGateway

instance Core.ToHeaders DetachVpnGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DetachVpnGateway where
  toPath = Core.const "/"

instance Core.ToQuery DetachVpnGateway where
  toQuery DetachVpnGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DetachVpnGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "VpcId" Core.=: vpcId,
        "VpnGatewayId" Core.=: vpnGatewayId
      ]

-- | /See:/ 'newDetachVpnGatewayResponse' smart constructor.
data DetachVpnGatewayResponse = DetachVpnGatewayResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachVpnGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachVpnGatewayResponse ::
  DetachVpnGatewayResponse
newDetachVpnGatewayResponse =
  DetachVpnGatewayResponse'

instance Core.NFData DetachVpnGatewayResponse
