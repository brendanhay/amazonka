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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyVpcEndpoint.
--
-- /See:/ 'newModifyVpcEndpoint' smart constructor.
data ModifyVpcEndpoint = ModifyVpcEndpoint'
  { -- | (Interface and gateway endpoints) A policy to attach to the endpoint
    -- that controls access to the service. The policy must be in valid JSON
    -- format.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | (Interface endpoint) One or more subnets IDs in which to remove the
    -- endpoint.
    removeSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | (Gateway endpoint) One or more route tables IDs to associate with the
    -- endpoint.
    addRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | (Gateway endpoint) Specify @true@ to reset the policy document to the
    -- default policy. The default policy allows full access to the service.
    resetPolicy :: Prelude.Maybe Prelude.Bool,
    -- | (Gateway endpoint) One or more route table IDs to disassociate from the
    -- endpoint.
    removeRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
    -- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
    -- you can specify only one subnet.
    addSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint) Indicates whether a private hosted zone is
    -- associated with the VPC.
    privateDnsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | (Interface endpoint) One or more security group IDs to disassociate from
    -- the network interface.
    removeSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint) One or more security group IDs to associate with
    -- the network interface.
    addSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the endpoint.
    vpcEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ModifyVpcEndpoint
newModifyVpcEndpoint pVpcEndpointId_ =
  ModifyVpcEndpoint'
    { policyDocument =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      removeSubnetIds = Prelude.Nothing,
      addRouteTableIds = Prelude.Nothing,
      resetPolicy = Prelude.Nothing,
      removeRouteTableIds = Prelude.Nothing,
      addSubnetIds = Prelude.Nothing,
      privateDnsEnabled = Prelude.Nothing,
      removeSecurityGroupIds = Prelude.Nothing,
      addSecurityGroupIds = Prelude.Nothing,
      vpcEndpointId = pVpcEndpointId_
    }

-- | (Interface and gateway endpoints) A policy to attach to the endpoint
-- that controls access to the service. The policy must be in valid JSON
-- format.
modifyVpcEndpoint_policyDocument :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Text)
modifyVpcEndpoint_policyDocument = Lens.lens (\ModifyVpcEndpoint' {policyDocument} -> policyDocument) (\s@ModifyVpcEndpoint' {} a -> s {policyDocument = a} :: ModifyVpcEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpoint_dryRun :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Bool)
modifyVpcEndpoint_dryRun = Lens.lens (\ModifyVpcEndpoint' {dryRun} -> dryRun) (\s@ModifyVpcEndpoint' {} a -> s {dryRun = a} :: ModifyVpcEndpoint)

-- | (Interface endpoint) One or more subnets IDs in which to remove the
-- endpoint.
modifyVpcEndpoint_removeSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_removeSubnetIds = Lens.lens (\ModifyVpcEndpoint' {removeSubnetIds} -> removeSubnetIds) (\s@ModifyVpcEndpoint' {} a -> s {removeSubnetIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | (Gateway endpoint) One or more route tables IDs to associate with the
-- endpoint.
modifyVpcEndpoint_addRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_addRouteTableIds = Lens.lens (\ModifyVpcEndpoint' {addRouteTableIds} -> addRouteTableIds) (\s@ModifyVpcEndpoint' {} a -> s {addRouteTableIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | (Gateway endpoint) Specify @true@ to reset the policy document to the
-- default policy. The default policy allows full access to the service.
modifyVpcEndpoint_resetPolicy :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Bool)
modifyVpcEndpoint_resetPolicy = Lens.lens (\ModifyVpcEndpoint' {resetPolicy} -> resetPolicy) (\s@ModifyVpcEndpoint' {} a -> s {resetPolicy = a} :: ModifyVpcEndpoint)

-- | (Gateway endpoint) One or more route table IDs to disassociate from the
-- endpoint.
modifyVpcEndpoint_removeRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_removeRouteTableIds = Lens.lens (\ModifyVpcEndpoint' {removeRouteTableIds} -> removeRouteTableIds) (\s@ModifyVpcEndpoint' {} a -> s {removeRouteTableIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
-- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
-- you can specify only one subnet.
modifyVpcEndpoint_addSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_addSubnetIds = Lens.lens (\ModifyVpcEndpoint' {addSubnetIds} -> addSubnetIds) (\s@ModifyVpcEndpoint' {} a -> s {addSubnetIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | (Interface endpoint) Indicates whether a private hosted zone is
-- associated with the VPC.
modifyVpcEndpoint_privateDnsEnabled :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Bool)
modifyVpcEndpoint_privateDnsEnabled = Lens.lens (\ModifyVpcEndpoint' {privateDnsEnabled} -> privateDnsEnabled) (\s@ModifyVpcEndpoint' {} a -> s {privateDnsEnabled = a} :: ModifyVpcEndpoint)

-- | (Interface endpoint) One or more security group IDs to disassociate from
-- the network interface.
modifyVpcEndpoint_removeSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_removeSecurityGroupIds = Lens.lens (\ModifyVpcEndpoint' {removeSecurityGroupIds} -> removeSecurityGroupIds) (\s@ModifyVpcEndpoint' {} a -> s {removeSecurityGroupIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | (Interface endpoint) One or more security group IDs to associate with
-- the network interface.
modifyVpcEndpoint_addSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_addSecurityGroupIds = Lens.lens (\ModifyVpcEndpoint' {addSecurityGroupIds} -> addSecurityGroupIds) (\s@ModifyVpcEndpoint' {} a -> s {addSecurityGroupIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the endpoint.
modifyVpcEndpoint_vpcEndpointId :: Lens.Lens' ModifyVpcEndpoint Prelude.Text
modifyVpcEndpoint_vpcEndpointId = Lens.lens (\ModifyVpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@ModifyVpcEndpoint' {} a -> s {vpcEndpointId = a} :: ModifyVpcEndpoint)

instance Prelude.AWSRequest ModifyVpcEndpoint where
  type Rs ModifyVpcEndpoint = ModifyVpcEndpointResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpcEndpoint

instance Prelude.NFData ModifyVpcEndpoint

instance Prelude.ToHeaders ModifyVpcEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyVpcEndpoint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyVpcEndpoint where
  toQuery ModifyVpcEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyVpcEndpoint" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "PolicyDocument" Prelude.=: policyDocument,
        "DryRun" Prelude.=: dryRun,
        Prelude.toQuery
          ( Prelude.toQueryList "RemoveSubnetId"
              Prelude.<$> removeSubnetIds
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "AddRouteTableId"
              Prelude.<$> addRouteTableIds
          ),
        "ResetPolicy" Prelude.=: resetPolicy,
        Prelude.toQuery
          ( Prelude.toQueryList "RemoveRouteTableId"
              Prelude.<$> removeRouteTableIds
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "AddSubnetId"
              Prelude.<$> addSubnetIds
          ),
        "PrivateDnsEnabled" Prelude.=: privateDnsEnabled,
        Prelude.toQuery
          ( Prelude.toQueryList "RemoveSecurityGroupId"
              Prelude.<$> removeSecurityGroupIds
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "AddSecurityGroupId"
              Prelude.<$> addSecurityGroupIds
          ),
        "VpcEndpointId" Prelude.=: vpcEndpointId
      ]

-- | /See:/ 'newModifyVpcEndpointResponse' smart constructor.
data ModifyVpcEndpointResponse = ModifyVpcEndpointResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ModifyVpcEndpointResponse
newModifyVpcEndpointResponse pHttpStatus_ =
  ModifyVpcEndpointResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyVpcEndpointResponse_return :: Lens.Lens' ModifyVpcEndpointResponse (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointResponse_return = Lens.lens (\ModifyVpcEndpointResponse' {return'} -> return') (\s@ModifyVpcEndpointResponse' {} a -> s {return' = a} :: ModifyVpcEndpointResponse)

-- | The response's http status code.
modifyVpcEndpointResponse_httpStatus :: Lens.Lens' ModifyVpcEndpointResponse Prelude.Int
modifyVpcEndpointResponse_httpStatus = Lens.lens (\ModifyVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcEndpointResponse' {} a -> s {httpStatus = a} :: ModifyVpcEndpointResponse)

instance Prelude.NFData ModifyVpcEndpointResponse
