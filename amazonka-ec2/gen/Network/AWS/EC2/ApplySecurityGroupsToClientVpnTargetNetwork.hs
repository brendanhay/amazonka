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
-- Module      : Network.AWS.EC2.ApplySecurityGroupsToClientVpnTargetNetwork
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a security group to the association between the target network
-- and the Client VPN endpoint. This action replaces the existing security
-- groups with the specified security groups.
module Network.AWS.EC2.ApplySecurityGroupsToClientVpnTargetNetwork
  ( -- * Creating a Request
    ApplySecurityGroupsToClientVpnTargetNetwork (..),
    newApplySecurityGroupsToClientVpnTargetNetwork,

    -- * Request Lenses
    applySecurityGroupsToClientVpnTargetNetwork_dryRun,
    applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId,
    applySecurityGroupsToClientVpnTargetNetwork_vpcId,
    applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds,

    -- * Destructuring the Response
    ApplySecurityGroupsToClientVpnTargetNetworkResponse (..),
    newApplySecurityGroupsToClientVpnTargetNetworkResponse,

    -- * Response Lenses
    applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds,
    applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newApplySecurityGroupsToClientVpnTargetNetwork' smart constructor.
data ApplySecurityGroupsToClientVpnTargetNetwork = ApplySecurityGroupsToClientVpnTargetNetwork'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Text,
    -- | The ID of the VPC in which the associated target network is located.
    vpcId :: Core.Text,
    -- | The IDs of the security groups to apply to the associated target
    -- network. Up to 5 security groups can be applied to an associated target
    -- network.
    securityGroupIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplySecurityGroupsToClientVpnTargetNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'applySecurityGroupsToClientVpnTargetNetwork_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientVpnEndpointId', 'applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId' - The ID of the Client VPN endpoint.
--
-- 'vpcId', 'applySecurityGroupsToClientVpnTargetNetwork_vpcId' - The ID of the VPC in which the associated target network is located.
--
-- 'securityGroupIds', 'applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds' - The IDs of the security groups to apply to the associated target
-- network. Up to 5 security groups can be applied to an associated target
-- network.
newApplySecurityGroupsToClientVpnTargetNetwork ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  -- | 'vpcId'
  Core.Text ->
  ApplySecurityGroupsToClientVpnTargetNetwork
newApplySecurityGroupsToClientVpnTargetNetwork
  pClientVpnEndpointId_
  pVpcId_ =
    ApplySecurityGroupsToClientVpnTargetNetwork'
      { dryRun =
          Core.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_,
        vpcId = pVpcId_,
        securityGroupIds = Core.mempty
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
applySecurityGroupsToClientVpnTargetNetwork_dryRun :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork (Core.Maybe Core.Bool)
applySecurityGroupsToClientVpnTargetNetwork_dryRun = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {dryRun} -> dryRun) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {dryRun = a} :: ApplySecurityGroupsToClientVpnTargetNetwork)

-- | The ID of the Client VPN endpoint.
applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork Core.Text
applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {clientVpnEndpointId = a} :: ApplySecurityGroupsToClientVpnTargetNetwork)

-- | The ID of the VPC in which the associated target network is located.
applySecurityGroupsToClientVpnTargetNetwork_vpcId :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork Core.Text
applySecurityGroupsToClientVpnTargetNetwork_vpcId = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {vpcId} -> vpcId) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {vpcId = a} :: ApplySecurityGroupsToClientVpnTargetNetwork)

-- | The IDs of the security groups to apply to the associated target
-- network. Up to 5 security groups can be applied to an associated target
-- network.
applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork [Core.Text]
applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {securityGroupIds} -> securityGroupIds) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {securityGroupIds = a} :: ApplySecurityGroupsToClientVpnTargetNetwork) Core.. Lens._Coerce

instance
  Core.AWSRequest
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  type
    AWSResponse
      ApplySecurityGroupsToClientVpnTargetNetwork =
      ApplySecurityGroupsToClientVpnTargetNetworkResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ApplySecurityGroupsToClientVpnTargetNetworkResponse'
            Core.<$> ( x Core..@? "securityGroupIds" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ApplySecurityGroupsToClientVpnTargetNetwork

instance
  Core.NFData
    ApplySecurityGroupsToClientVpnTargetNetwork

instance
  Core.ToHeaders
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  toQuery
    ApplySecurityGroupsToClientVpnTargetNetwork' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "ApplySecurityGroupsToClientVpnTargetNetwork" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "ClientVpnEndpointId" Core.=: clientVpnEndpointId,
          "VpcId" Core.=: vpcId,
          Core.toQueryList "SecurityGroupId" securityGroupIds
        ]

-- | /See:/ 'newApplySecurityGroupsToClientVpnTargetNetworkResponse' smart constructor.
data ApplySecurityGroupsToClientVpnTargetNetworkResponse = ApplySecurityGroupsToClientVpnTargetNetworkResponse'
  { -- | The IDs of the applied security groups.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplySecurityGroupsToClientVpnTargetNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds' - The IDs of the applied security groups.
--
-- 'httpStatus', 'applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus' - The response's http status code.
newApplySecurityGroupsToClientVpnTargetNetworkResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ApplySecurityGroupsToClientVpnTargetNetworkResponse
newApplySecurityGroupsToClientVpnTargetNetworkResponse
  pHttpStatus_ =
    ApplySecurityGroupsToClientVpnTargetNetworkResponse'
      { securityGroupIds =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The IDs of the applied security groups.
applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetworkResponse (Core.Maybe [Core.Text])
applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetworkResponse' {securityGroupIds} -> securityGroupIds) (\s@ApplySecurityGroupsToClientVpnTargetNetworkResponse' {} a -> s {securityGroupIds = a} :: ApplySecurityGroupsToClientVpnTargetNetworkResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetworkResponse Core.Int
applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetworkResponse' {httpStatus} -> httpStatus) (\s@ApplySecurityGroupsToClientVpnTargetNetworkResponse' {} a -> s {httpStatus = a} :: ApplySecurityGroupsToClientVpnTargetNetworkResponse)

instance
  Core.NFData
    ApplySecurityGroupsToClientVpnTargetNetworkResponse
