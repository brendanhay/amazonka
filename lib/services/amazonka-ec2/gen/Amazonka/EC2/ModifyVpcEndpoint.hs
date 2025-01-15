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
-- Module      : Amazonka.EC2.ModifyVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes of a specified VPC endpoint. The attributes that you
-- can modify depend on the type of VPC endpoint (interface, gateway, or
-- Gateway Load Balancer). For more information, see the
-- <https://docs.aws.amazon.com/vpc/latest/privatelink/ Amazon Web Services PrivateLink Guide>.
module Amazonka.EC2.ModifyVpcEndpoint
  ( -- * Creating a Request
    ModifyVpcEndpoint (..),
    newModifyVpcEndpoint,

    -- * Request Lenses
    modifyVpcEndpoint_addRouteTableIds,
    modifyVpcEndpoint_addSecurityGroupIds,
    modifyVpcEndpoint_addSubnetIds,
    modifyVpcEndpoint_dnsOptions,
    modifyVpcEndpoint_dryRun,
    modifyVpcEndpoint_ipAddressType,
    modifyVpcEndpoint_policyDocument,
    modifyVpcEndpoint_privateDnsEnabled,
    modifyVpcEndpoint_removeRouteTableIds,
    modifyVpcEndpoint_removeSecurityGroupIds,
    modifyVpcEndpoint_removeSubnetIds,
    modifyVpcEndpoint_resetPolicy,
    modifyVpcEndpoint_vpcEndpointId,

    -- * Destructuring the Response
    ModifyVpcEndpointResponse (..),
    newModifyVpcEndpointResponse,

    -- * Response Lenses
    modifyVpcEndpointResponse_return,
    modifyVpcEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ModifyVpcEndpoint.
--
-- /See:/ 'newModifyVpcEndpoint' smart constructor.
data ModifyVpcEndpoint = ModifyVpcEndpoint'
  { -- | (Gateway endpoint) One or more route tables IDs to associate with the
    -- endpoint.
    addRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint) One or more security group IDs to associate with
    -- the network interface.
    addSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
    -- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
    -- you can specify only one subnet.
    addSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The DNS options for the endpoint.
    dnsOptions :: Prelude.Maybe DnsOptionsSpecification,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IP address type for the endpoint.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | (Interface and gateway endpoints) A policy to attach to the endpoint
    -- that controls access to the service. The policy must be in valid JSON
    -- format.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | (Interface endpoint) Indicates whether a private hosted zone is
    -- associated with the VPC.
    privateDnsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | (Gateway endpoint) One or more route table IDs to disassociate from the
    -- endpoint.
    removeRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint) One or more security group IDs to disassociate from
    -- the network interface.
    removeSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint) One or more subnets IDs in which to remove the
    -- endpoint.
    removeSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | (Gateway endpoint) Specify @true@ to reset the policy document to the
    -- default policy. The default policy allows full access to the service.
    resetPolicy :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the endpoint.
    vpcEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addRouteTableIds', 'modifyVpcEndpoint_addRouteTableIds' - (Gateway endpoint) One or more route tables IDs to associate with the
-- endpoint.
--
-- 'addSecurityGroupIds', 'modifyVpcEndpoint_addSecurityGroupIds' - (Interface endpoint) One or more security group IDs to associate with
-- the network interface.
--
-- 'addSubnetIds', 'modifyVpcEndpoint_addSubnetIds' - (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
-- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
-- you can specify only one subnet.
--
-- 'dnsOptions', 'modifyVpcEndpoint_dnsOptions' - The DNS options for the endpoint.
--
-- 'dryRun', 'modifyVpcEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipAddressType', 'modifyVpcEndpoint_ipAddressType' - The IP address type for the endpoint.
--
-- 'policyDocument', 'modifyVpcEndpoint_policyDocument' - (Interface and gateway endpoints) A policy to attach to the endpoint
-- that controls access to the service. The policy must be in valid JSON
-- format.
--
-- 'privateDnsEnabled', 'modifyVpcEndpoint_privateDnsEnabled' - (Interface endpoint) Indicates whether a private hosted zone is
-- associated with the VPC.
--
-- 'removeRouteTableIds', 'modifyVpcEndpoint_removeRouteTableIds' - (Gateway endpoint) One or more route table IDs to disassociate from the
-- endpoint.
--
-- 'removeSecurityGroupIds', 'modifyVpcEndpoint_removeSecurityGroupIds' - (Interface endpoint) One or more security group IDs to disassociate from
-- the network interface.
--
-- 'removeSubnetIds', 'modifyVpcEndpoint_removeSubnetIds' - (Interface endpoint) One or more subnets IDs in which to remove the
-- endpoint.
--
-- 'resetPolicy', 'modifyVpcEndpoint_resetPolicy' - (Gateway endpoint) Specify @true@ to reset the policy document to the
-- default policy. The default policy allows full access to the service.
--
-- 'vpcEndpointId', 'modifyVpcEndpoint_vpcEndpointId' - The ID of the endpoint.
newModifyVpcEndpoint ::
  -- | 'vpcEndpointId'
  Prelude.Text ->
  ModifyVpcEndpoint
newModifyVpcEndpoint pVpcEndpointId_ =
  ModifyVpcEndpoint'
    { addRouteTableIds =
        Prelude.Nothing,
      addSecurityGroupIds = Prelude.Nothing,
      addSubnetIds = Prelude.Nothing,
      dnsOptions = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      privateDnsEnabled = Prelude.Nothing,
      removeRouteTableIds = Prelude.Nothing,
      removeSecurityGroupIds = Prelude.Nothing,
      removeSubnetIds = Prelude.Nothing,
      resetPolicy = Prelude.Nothing,
      vpcEndpointId = pVpcEndpointId_
    }

-- | (Gateway endpoint) One or more route tables IDs to associate with the
-- endpoint.
modifyVpcEndpoint_addRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_addRouteTableIds = Lens.lens (\ModifyVpcEndpoint' {addRouteTableIds} -> addRouteTableIds) (\s@ModifyVpcEndpoint' {} a -> s {addRouteTableIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | (Interface endpoint) One or more security group IDs to associate with
-- the network interface.
modifyVpcEndpoint_addSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_addSecurityGroupIds = Lens.lens (\ModifyVpcEndpoint' {addSecurityGroupIds} -> addSecurityGroupIds) (\s@ModifyVpcEndpoint' {} a -> s {addSecurityGroupIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs
-- in which to serve the endpoint. For a Gateway Load Balancer endpoint,
-- you can specify only one subnet.
modifyVpcEndpoint_addSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_addSubnetIds = Lens.lens (\ModifyVpcEndpoint' {addSubnetIds} -> addSubnetIds) (\s@ModifyVpcEndpoint' {} a -> s {addSubnetIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The DNS options for the endpoint.
modifyVpcEndpoint_dnsOptions :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe DnsOptionsSpecification)
modifyVpcEndpoint_dnsOptions = Lens.lens (\ModifyVpcEndpoint' {dnsOptions} -> dnsOptions) (\s@ModifyVpcEndpoint' {} a -> s {dnsOptions = a} :: ModifyVpcEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpoint_dryRun :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Bool)
modifyVpcEndpoint_dryRun = Lens.lens (\ModifyVpcEndpoint' {dryRun} -> dryRun) (\s@ModifyVpcEndpoint' {} a -> s {dryRun = a} :: ModifyVpcEndpoint)

-- | The IP address type for the endpoint.
modifyVpcEndpoint_ipAddressType :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe IpAddressType)
modifyVpcEndpoint_ipAddressType = Lens.lens (\ModifyVpcEndpoint' {ipAddressType} -> ipAddressType) (\s@ModifyVpcEndpoint' {} a -> s {ipAddressType = a} :: ModifyVpcEndpoint)

-- | (Interface and gateway endpoints) A policy to attach to the endpoint
-- that controls access to the service. The policy must be in valid JSON
-- format.
modifyVpcEndpoint_policyDocument :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Text)
modifyVpcEndpoint_policyDocument = Lens.lens (\ModifyVpcEndpoint' {policyDocument} -> policyDocument) (\s@ModifyVpcEndpoint' {} a -> s {policyDocument = a} :: ModifyVpcEndpoint)

-- | (Interface endpoint) Indicates whether a private hosted zone is
-- associated with the VPC.
modifyVpcEndpoint_privateDnsEnabled :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Bool)
modifyVpcEndpoint_privateDnsEnabled = Lens.lens (\ModifyVpcEndpoint' {privateDnsEnabled} -> privateDnsEnabled) (\s@ModifyVpcEndpoint' {} a -> s {privateDnsEnabled = a} :: ModifyVpcEndpoint)

-- | (Gateway endpoint) One or more route table IDs to disassociate from the
-- endpoint.
modifyVpcEndpoint_removeRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_removeRouteTableIds = Lens.lens (\ModifyVpcEndpoint' {removeRouteTableIds} -> removeRouteTableIds) (\s@ModifyVpcEndpoint' {} a -> s {removeRouteTableIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | (Interface endpoint) One or more security group IDs to disassociate from
-- the network interface.
modifyVpcEndpoint_removeSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_removeSecurityGroupIds = Lens.lens (\ModifyVpcEndpoint' {removeSecurityGroupIds} -> removeSecurityGroupIds) (\s@ModifyVpcEndpoint' {} a -> s {removeSecurityGroupIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | (Interface endpoint) One or more subnets IDs in which to remove the
-- endpoint.
modifyVpcEndpoint_removeSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe [Prelude.Text])
modifyVpcEndpoint_removeSubnetIds = Lens.lens (\ModifyVpcEndpoint' {removeSubnetIds} -> removeSubnetIds) (\s@ModifyVpcEndpoint' {} a -> s {removeSubnetIds = a} :: ModifyVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | (Gateway endpoint) Specify @true@ to reset the policy document to the
-- default policy. The default policy allows full access to the service.
modifyVpcEndpoint_resetPolicy :: Lens.Lens' ModifyVpcEndpoint (Prelude.Maybe Prelude.Bool)
modifyVpcEndpoint_resetPolicy = Lens.lens (\ModifyVpcEndpoint' {resetPolicy} -> resetPolicy) (\s@ModifyVpcEndpoint' {} a -> s {resetPolicy = a} :: ModifyVpcEndpoint)

-- | The ID of the endpoint.
modifyVpcEndpoint_vpcEndpointId :: Lens.Lens' ModifyVpcEndpoint Prelude.Text
modifyVpcEndpoint_vpcEndpointId = Lens.lens (\ModifyVpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@ModifyVpcEndpoint' {} a -> s {vpcEndpointId = a} :: ModifyVpcEndpoint)

instance Core.AWSRequest ModifyVpcEndpoint where
  type
    AWSResponse ModifyVpcEndpoint =
      ModifyVpcEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpcEndpoint where
  hashWithSalt _salt ModifyVpcEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` addRouteTableIds
      `Prelude.hashWithSalt` addSecurityGroupIds
      `Prelude.hashWithSalt` addSubnetIds
      `Prelude.hashWithSalt` dnsOptions
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` privateDnsEnabled
      `Prelude.hashWithSalt` removeRouteTableIds
      `Prelude.hashWithSalt` removeSecurityGroupIds
      `Prelude.hashWithSalt` removeSubnetIds
      `Prelude.hashWithSalt` resetPolicy
      `Prelude.hashWithSalt` vpcEndpointId

instance Prelude.NFData ModifyVpcEndpoint where
  rnf ModifyVpcEndpoint' {..} =
    Prelude.rnf addRouteTableIds `Prelude.seq`
      Prelude.rnf addSecurityGroupIds `Prelude.seq`
        Prelude.rnf addSubnetIds `Prelude.seq`
          Prelude.rnf dnsOptions `Prelude.seq`
            Prelude.rnf dryRun `Prelude.seq`
              Prelude.rnf ipAddressType `Prelude.seq`
                Prelude.rnf policyDocument `Prelude.seq`
                  Prelude.rnf privateDnsEnabled `Prelude.seq`
                    Prelude.rnf removeRouteTableIds `Prelude.seq`
                      Prelude.rnf removeSecurityGroupIds `Prelude.seq`
                        Prelude.rnf removeSubnetIds `Prelude.seq`
                          Prelude.rnf resetPolicy `Prelude.seq`
                            Prelude.rnf vpcEndpointId

instance Data.ToHeaders ModifyVpcEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVpcEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVpcEndpoint where
  toQuery ModifyVpcEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVpcEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AddRouteTableId"
              Prelude.<$> addRouteTableIds
          ),
        Data.toQuery
          ( Data.toQueryList "AddSecurityGroupId"
              Prelude.<$> addSecurityGroupIds
          ),
        Data.toQuery
          ( Data.toQueryList "AddSubnetId"
              Prelude.<$> addSubnetIds
          ),
        "DnsOptions" Data.=: dnsOptions,
        "DryRun" Data.=: dryRun,
        "IpAddressType" Data.=: ipAddressType,
        "PolicyDocument" Data.=: policyDocument,
        "PrivateDnsEnabled" Data.=: privateDnsEnabled,
        Data.toQuery
          ( Data.toQueryList "RemoveRouteTableId"
              Prelude.<$> removeRouteTableIds
          ),
        Data.toQuery
          ( Data.toQueryList "RemoveSecurityGroupId"
              Prelude.<$> removeSecurityGroupIds
          ),
        Data.toQuery
          ( Data.toQueryList "RemoveSubnetId"
              Prelude.<$> removeSubnetIds
          ),
        "ResetPolicy" Data.=: resetPolicy,
        "VpcEndpointId" Data.=: vpcEndpointId
      ]

-- | /See:/ 'newModifyVpcEndpointResponse' smart constructor.
data ModifyVpcEndpointResponse = ModifyVpcEndpointResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData ModifyVpcEndpointResponse where
  rnf ModifyVpcEndpointResponse' {..} =
    Prelude.rnf return' `Prelude.seq`
      Prelude.rnf httpStatus
