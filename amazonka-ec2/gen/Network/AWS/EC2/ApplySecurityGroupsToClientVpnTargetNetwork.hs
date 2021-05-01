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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newApplySecurityGroupsToClientVpnTargetNetwork' smart constructor.
data ApplySecurityGroupsToClientVpnTargetNetwork = ApplySecurityGroupsToClientVpnTargetNetwork'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text,
    -- | The ID of the VPC in which the associated target network is located.
    vpcId :: Prelude.Text,
    -- | The IDs of the security groups to apply to the associated target
    -- network. Up to 5 security groups can be applied to an associated target
    -- network.
    securityGroupIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  ApplySecurityGroupsToClientVpnTargetNetwork
newApplySecurityGroupsToClientVpnTargetNetwork
  pClientVpnEndpointId_
  pVpcId_ =
    ApplySecurityGroupsToClientVpnTargetNetwork'
      { dryRun =
          Prelude.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_,
        vpcId = pVpcId_,
        securityGroupIds =
          Prelude.mempty
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
applySecurityGroupsToClientVpnTargetNetwork_dryRun :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork (Prelude.Maybe Prelude.Bool)
applySecurityGroupsToClientVpnTargetNetwork_dryRun = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {dryRun} -> dryRun) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {dryRun = a} :: ApplySecurityGroupsToClientVpnTargetNetwork)

-- | The ID of the Client VPN endpoint.
applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork Prelude.Text
applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {clientVpnEndpointId = a} :: ApplySecurityGroupsToClientVpnTargetNetwork)

-- | The ID of the VPC in which the associated target network is located.
applySecurityGroupsToClientVpnTargetNetwork_vpcId :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork Prelude.Text
applySecurityGroupsToClientVpnTargetNetwork_vpcId = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {vpcId} -> vpcId) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {vpcId = a} :: ApplySecurityGroupsToClientVpnTargetNetwork)

-- | The IDs of the security groups to apply to the associated target
-- network. Up to 5 security groups can be applied to an associated target
-- network.
applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork [Prelude.Text]
applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetwork' {securityGroupIds} -> securityGroupIds) (\s@ApplySecurityGroupsToClientVpnTargetNetwork' {} a -> s {securityGroupIds = a} :: ApplySecurityGroupsToClientVpnTargetNetwork) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  type
    Rs ApplySecurityGroupsToClientVpnTargetNetwork =
      ApplySecurityGroupsToClientVpnTargetNetworkResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ApplySecurityGroupsToClientVpnTargetNetworkResponse'
            Prelude.<$> ( x Prelude..@? "securityGroupIds"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ApplySecurityGroupsToClientVpnTargetNetwork

instance
  Prelude.NFData
    ApplySecurityGroupsToClientVpnTargetNetwork

instance
  Prelude.ToHeaders
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ApplySecurityGroupsToClientVpnTargetNetwork
  where
  toQuery
    ApplySecurityGroupsToClientVpnTargetNetwork' {..} =
      Prelude.mconcat
        [ "Action"
            Prelude.=: ( "ApplySecurityGroupsToClientVpnTargetNetwork" ::
                           Prelude.ByteString
                       ),
          "Version"
            Prelude.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Prelude.=: dryRun,
          "ClientVpnEndpointId" Prelude.=: clientVpnEndpointId,
          "VpcId" Prelude.=: vpcId,
          Prelude.toQueryList
            "SecurityGroupId"
            securityGroupIds
        ]

-- | /See:/ 'newApplySecurityGroupsToClientVpnTargetNetworkResponse' smart constructor.
data ApplySecurityGroupsToClientVpnTargetNetworkResponse = ApplySecurityGroupsToClientVpnTargetNetworkResponse'
  { -- | The IDs of the applied security groups.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ApplySecurityGroupsToClientVpnTargetNetworkResponse
newApplySecurityGroupsToClientVpnTargetNetworkResponse
  pHttpStatus_ =
    ApplySecurityGroupsToClientVpnTargetNetworkResponse'
      { securityGroupIds =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The IDs of the applied security groups.
applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetworkResponse (Prelude.Maybe [Prelude.Text])
applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetworkResponse' {securityGroupIds} -> securityGroupIds) (\s@ApplySecurityGroupsToClientVpnTargetNetworkResponse' {} a -> s {securityGroupIds = a} :: ApplySecurityGroupsToClientVpnTargetNetworkResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetworkResponse Prelude.Int
applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus = Lens.lens (\ApplySecurityGroupsToClientVpnTargetNetworkResponse' {httpStatus} -> httpStatus) (\s@ApplySecurityGroupsToClientVpnTargetNetworkResponse' {} a -> s {httpStatus = a} :: ApplySecurityGroupsToClientVpnTargetNetworkResponse)

instance
  Prelude.NFData
    ApplySecurityGroupsToClientVpnTargetNetworkResponse
