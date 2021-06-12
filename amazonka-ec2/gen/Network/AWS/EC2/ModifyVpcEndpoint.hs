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
-- Module      : Network.AWS.EC2.ModifyVpcEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes of a specified VPC endpoint. The attributes that you
-- can modify depend on the type of VPC endpoint (interface, gateway, or
-- Gateway Load Balancer). For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html VPC Endpoints>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.ModifyVpcEndpoint
  ( -- * Creating a Request
    ModifyVpcEndpoint (..),
    newModifyVpcEndpoint,

    -- * Request Lenses
    modifyVpcEndpoint_policyDocument,
    modifyVpcEndpoint_dryRun,
    modifyVpcEndpoint_removeSubnetIds,
    modifyVpcEndpoint_addRouteTableIds,
    modifyVpcEndpoint_resetPolicy,
    modifyVpcEndpoint_removeRouteTableIds,
    modifyVpcEndpoint_addSubnetIds,
    modifyVpcEndpoint_privateDnsEnabled,
    modifyVpcEndpoint_removeSecurityGroupIds,
    modifyVpcEndpoint_addSecurityGroupIds,
    modifyVpcEndpoint_vpcEndpointId,

    -- * Destructuring the Response
    ModifyVpcEndpointResponse (..),
    newModifyVpcEndpointResponse,

    -- * Response Lenses
    modifyVpcEndpointResponse_return,
    modifyVpcEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyVpcEndpoint.
--
-- /See:/ 'newModifyVpcEndpoint' smart constructor.
data ModifyVpcEndpoint = ModifyVpcEndpoint'
  { -- | (Interface and gateway endpoints) A policy to attach to the endpoint
    -- that controls access to the service. The policy must be in valid JSON
    -- format.
    policyDocument :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | (Interface endpoint) One or more subnets IDs in which to remove the
    -- endpoint.
    removeSubnetIds :: Core.Maybe [Core.Text],
    -- | (Gateway endpoint) One or more route tables IDs to associate with the
    -- endpoint.
    addRouteTableIds :: Core.Maybe [Core.Text],
    -- | (Gateway endpoint) Specify @true@ to reset the policy document to the
    -- default policy. The default policy allows full access to the service.
    resetPolicy :: Core.Maybe Core.Bool,
    -- | (Gateway endpoint) One or more route table IDs to disassociate from the
    -- endpoint.
    removeRouteTableIds :: Core.Maybe [Core.Text],
    -- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
    -- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
    -- you can specify only one subnet.
    addSubnetIds :: Core.Maybe [Core.Text],
    -- | (Interface endpoint) Indicates whether a private hosted zone is
    -- associated with the VPC.
    privateDnsEnabled :: Core.Maybe Core.Bool,
    -- | (Interface endpoint) One or more security group IDs to disassociate from
    -- the network interface.
    removeSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | (Interface endpoint) One or more security group IDs to associate with
    -- the network interface.
    addSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | The ID of the endpoint.
    vpcEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDocument', 'modifyVpcEndpoint_policyDocument' - (Interface and gateway endpoints) A policy to attach to the endpoint
-- that controls access to the service. The policy must be in valid JSON
-- format.
--
-- 'dryRun', 'modifyVpcEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'removeSubnetIds', 'modifyVpcEndpoint_removeSubnetIds' - (Interface endpoint) One or more subnets IDs in which to remove the
-- endpoint.
--
-- 'addRouteTableIds', 'modifyVpcEndpoint_addRouteTableIds' - (Gateway endpoint) One or more route tables IDs to associate with the
-- endpoint.
--
-- 'resetPolicy', 'modifyVpcEndpoint_resetPolicy' - (Gateway endpoint) Specify @true@ to reset the policy document to the
-- default policy. The default policy allows full access to the service.
--
-- 'removeRouteTableIds', 'modifyVpcEndpoint_removeRouteTableIds' - (Gateway endpoint) One or more route table IDs to disassociate from the
-- endpoint.
--
-- 'addSubnetIds', 'modifyVpcEndpoint_addSubnetIds' - (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
-- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
-- you can specify only one subnet.
--
-- 'privateDnsEnabled', 'modifyVpcEndpoint_privateDnsEnabled' - (Interface endpoint) Indicates whether a private hosted zone is
-- associated with the VPC.
--
-- 'removeSecurityGroupIds', 'modifyVpcEndpoint_removeSecurityGroupIds' - (Interface endpoint) One or more security group IDs to disassociate from
-- the network interface.
--
-- 'addSecurityGroupIds', 'modifyVpcEndpoint_addSecurityGroupIds' - (Interface endpoint) One or more security group IDs to associate with
-- the network interface.
--
-- 'vpcEndpointId', 'modifyVpcEndpoint_vpcEndpointId' - The ID of the endpoint.
newModifyVpcEndpoint ::
  -- | 'vpcEndpointId'
  Core.Text ->
  ModifyVpcEndpoint
newModifyVpcEndpoint pVpcEndpointId_ =
  ModifyVpcEndpoint'
    { policyDocument = Core.Nothing,
      dryRun = Core.Nothing,
      removeSubnetIds = Core.Nothing,
      addRouteTableIds = Core.Nothing,
      resetPolicy = Core.Nothing,
      removeRouteTableIds = Core.Nothing,
      addSubnetIds = Core.Nothing,
      privateDnsEnabled = Core.Nothing,
      removeSecurityGroupIds = Core.Nothing,
      addSecurityGroupIds = Core.Nothing,
      vpcEndpointId = pVpcEndpointId_
    }

-- | (Interface and gateway endpoints) A policy to attach to the endpoint
-- that controls access to the service. The policy must be in valid JSON
-- format.
modifyVpcEndpoint_policyDocument :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Text)
modifyVpcEndpoint_policyDocument = Lens.lens (\ModifyVpcEndpoint' {policyDocument} -> policyDocument) (\s@ModifyVpcEndpoint' {} a -> s {policyDocument = a} :: ModifyVpcEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpoint_dryRun :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Bool)
modifyVpcEndpoint_dryRun = Lens.lens (\ModifyVpcEndpoint' {dryRun} -> dryRun) (\s@ModifyVpcEndpoint' {} a -> s {dryRun = a} :: ModifyVpcEndpoint)

-- | (Interface endpoint) One or more subnets IDs in which to remove the
-- endpoint.
modifyVpcEndpoint_removeSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Core.Text])
modifyVpcEndpoint_removeSubnetIds = Lens.lens (\ModifyVpcEndpoint' {removeSubnetIds} -> removeSubnetIds) (\s@ModifyVpcEndpoint' {} a -> s {removeSubnetIds = a} :: ModifyVpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | (Gateway endpoint) One or more route tables IDs to associate with the
-- endpoint.
modifyVpcEndpoint_addRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Core.Text])
modifyVpcEndpoint_addRouteTableIds = Lens.lens (\ModifyVpcEndpoint' {addRouteTableIds} -> addRouteTableIds) (\s@ModifyVpcEndpoint' {} a -> s {addRouteTableIds = a} :: ModifyVpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | (Gateway endpoint) Specify @true@ to reset the policy document to the
-- default policy. The default policy allows full access to the service.
modifyVpcEndpoint_resetPolicy :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Bool)
modifyVpcEndpoint_resetPolicy = Lens.lens (\ModifyVpcEndpoint' {resetPolicy} -> resetPolicy) (\s@ModifyVpcEndpoint' {} a -> s {resetPolicy = a} :: ModifyVpcEndpoint)

-- | (Gateway endpoint) One or more route table IDs to disassociate from the
-- endpoint.
modifyVpcEndpoint_removeRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Core.Text])
modifyVpcEndpoint_removeRouteTableIds = Lens.lens (\ModifyVpcEndpoint' {removeRouteTableIds} -> removeRouteTableIds) (\s@ModifyVpcEndpoint' {} a -> s {removeRouteTableIds = a} :: ModifyVpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
-- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
-- you can specify only one subnet.
modifyVpcEndpoint_addSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Core.Text])
modifyVpcEndpoint_addSubnetIds = Lens.lens (\ModifyVpcEndpoint' {addSubnetIds} -> addSubnetIds) (\s@ModifyVpcEndpoint' {} a -> s {addSubnetIds = a} :: ModifyVpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | (Interface endpoint) Indicates whether a private hosted zone is
-- associated with the VPC.
modifyVpcEndpoint_privateDnsEnabled :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Bool)
modifyVpcEndpoint_privateDnsEnabled = Lens.lens (\ModifyVpcEndpoint' {privateDnsEnabled} -> privateDnsEnabled) (\s@ModifyVpcEndpoint' {} a -> s {privateDnsEnabled = a} :: ModifyVpcEndpoint)

-- | (Interface endpoint) One or more security group IDs to disassociate from
-- the network interface.
modifyVpcEndpoint_removeSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Core.Text])
modifyVpcEndpoint_removeSecurityGroupIds = Lens.lens (\ModifyVpcEndpoint' {removeSecurityGroupIds} -> removeSecurityGroupIds) (\s@ModifyVpcEndpoint' {} a -> s {removeSecurityGroupIds = a} :: ModifyVpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | (Interface endpoint) One or more security group IDs to associate with
-- the network interface.
modifyVpcEndpoint_addSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Core.Text])
modifyVpcEndpoint_addSecurityGroupIds = Lens.lens (\ModifyVpcEndpoint' {addSecurityGroupIds} -> addSecurityGroupIds) (\s@ModifyVpcEndpoint' {} a -> s {addSecurityGroupIds = a} :: ModifyVpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The ID of the endpoint.
modifyVpcEndpoint_vpcEndpointId :: Lens.Lens' ModifyVpcEndpoint Core.Text
modifyVpcEndpoint_vpcEndpointId = Lens.lens (\ModifyVpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@ModifyVpcEndpoint' {} a -> s {vpcEndpointId = a} :: ModifyVpcEndpoint)

instance Core.AWSRequest ModifyVpcEndpoint where
  type
    AWSResponse ModifyVpcEndpoint =
      ModifyVpcEndpointResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyVpcEndpoint

instance Core.NFData ModifyVpcEndpoint

instance Core.ToHeaders ModifyVpcEndpoint where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyVpcEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery ModifyVpcEndpoint where
  toQuery ModifyVpcEndpoint' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyVpcEndpoint" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "PolicyDocument" Core.=: policyDocument,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "RemoveSubnetId"
              Core.<$> removeSubnetIds
          ),
        Core.toQuery
          ( Core.toQueryList "AddRouteTableId"
              Core.<$> addRouteTableIds
          ),
        "ResetPolicy" Core.=: resetPolicy,
        Core.toQuery
          ( Core.toQueryList "RemoveRouteTableId"
              Core.<$> removeRouteTableIds
          ),
        Core.toQuery
          ( Core.toQueryList "AddSubnetId"
              Core.<$> addSubnetIds
          ),
        "PrivateDnsEnabled" Core.=: privateDnsEnabled,
        Core.toQuery
          ( Core.toQueryList "RemoveSecurityGroupId"
              Core.<$> removeSecurityGroupIds
          ),
        Core.toQuery
          ( Core.toQueryList "AddSecurityGroupId"
              Core.<$> addSecurityGroupIds
          ),
        "VpcEndpointId" Core.=: vpcEndpointId
      ]

-- | /See:/ 'newModifyVpcEndpointResponse' smart constructor.
data ModifyVpcEndpointResponse = ModifyVpcEndpointResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyVpcEndpointResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyVpcEndpointResponse_httpStatus' - The response's http status code.
newModifyVpcEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyVpcEndpointResponse
newModifyVpcEndpointResponse pHttpStatus_ =
  ModifyVpcEndpointResponse'
    { return' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyVpcEndpointResponse_return :: Lens.Lens' ModifyVpcEndpointResponse (Core.Maybe Core.Bool)
modifyVpcEndpointResponse_return = Lens.lens (\ModifyVpcEndpointResponse' {return'} -> return') (\s@ModifyVpcEndpointResponse' {} a -> s {return' = a} :: ModifyVpcEndpointResponse)

-- | The response's http status code.
modifyVpcEndpointResponse_httpStatus :: Lens.Lens' ModifyVpcEndpointResponse Core.Int
modifyVpcEndpointResponse_httpStatus = Lens.lens (\ModifyVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcEndpointResponse' {} a -> s {httpStatus = a} :: ModifyVpcEndpointResponse)

instance Core.NFData ModifyVpcEndpointResponse
