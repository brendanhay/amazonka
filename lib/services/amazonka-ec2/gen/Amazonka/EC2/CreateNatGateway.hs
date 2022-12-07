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
-- Module      : Amazonka.EC2.CreateNatGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a NAT gateway in the specified subnet. This action creates a
-- network interface in the specified subnet with a private IP address from
-- the IP address range of the subnet. You can create either a public NAT
-- gateway or a private NAT gateway.
--
-- With a public NAT gateway, internet-bound traffic from a private subnet
-- can be routed to the NAT gateway, so that instances in a private subnet
-- can connect to the internet.
--
-- With a private NAT gateway, private communication is routed across VPCs
-- and on-premises networks through a transit gateway or virtual private
-- gateway. Common use cases include running large workloads behind a small
-- pool of allowlisted IPv4 addresses, preserving private IPv4 addresses,
-- and communicating between overlapping networks.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html NAT gateways>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateNatGateway
  ( -- * Creating a Request
    CreateNatGateway (..),
    newCreateNatGateway,

    -- * Request Lenses
    createNatGateway_clientToken,
    createNatGateway_allocationId,
    createNatGateway_dryRun,
    createNatGateway_privateIpAddress,
    createNatGateway_connectivityType,
    createNatGateway_tagSpecifications,
    createNatGateway_subnetId,

    -- * Destructuring the Response
    CreateNatGatewayResponse (..),
    newCreateNatGatewayResponse,

    -- * Response Lenses
    createNatGatewayResponse_clientToken,
    createNatGatewayResponse_natGateway,
    createNatGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNatGateway' smart constructor.
data CreateNatGateway = CreateNatGateway'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    --
    -- Constraint: Maximum 64 ASCII characters.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | [Public NAT gateways only] The allocation ID of an Elastic IP address to
    -- associate with the NAT gateway. You cannot specify an Elastic IP address
    -- with a private NAT gateway. If the Elastic IP address is associated with
    -- another resource, you must first disassociate it.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The private IPv4 address to assign to the NAT gateway. If you don\'t
    -- provide an address, a private IPv4 address will be automatically
    -- assigned.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the NAT gateway supports public or private
    -- connectivity. The default is public connectivity.
    connectivityType :: Prelude.Maybe ConnectivityType,
    -- | The tags to assign to the NAT gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The subnet in which to create the NAT gateway.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNatGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createNatGateway_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- Constraint: Maximum 64 ASCII characters.
--
-- 'allocationId', 'createNatGateway_allocationId' - [Public NAT gateways only] The allocation ID of an Elastic IP address to
-- associate with the NAT gateway. You cannot specify an Elastic IP address
-- with a private NAT gateway. If the Elastic IP address is associated with
-- another resource, you must first disassociate it.
--
-- 'dryRun', 'createNatGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'privateIpAddress', 'createNatGateway_privateIpAddress' - The private IPv4 address to assign to the NAT gateway. If you don\'t
-- provide an address, a private IPv4 address will be automatically
-- assigned.
--
-- 'connectivityType', 'createNatGateway_connectivityType' - Indicates whether the NAT gateway supports public or private
-- connectivity. The default is public connectivity.
--
-- 'tagSpecifications', 'createNatGateway_tagSpecifications' - The tags to assign to the NAT gateway.
--
-- 'subnetId', 'createNatGateway_subnetId' - The subnet in which to create the NAT gateway.
newCreateNatGateway ::
  -- | 'subnetId'
  Prelude.Text ->
  CreateNatGateway
newCreateNatGateway pSubnetId_ =
  CreateNatGateway'
    { clientToken = Prelude.Nothing,
      allocationId = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      connectivityType = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- Constraint: Maximum 64 ASCII characters.
createNatGateway_clientToken :: Lens.Lens' CreateNatGateway (Prelude.Maybe Prelude.Text)
createNatGateway_clientToken = Lens.lens (\CreateNatGateway' {clientToken} -> clientToken) (\s@CreateNatGateway' {} a -> s {clientToken = a} :: CreateNatGateway)

-- | [Public NAT gateways only] The allocation ID of an Elastic IP address to
-- associate with the NAT gateway. You cannot specify an Elastic IP address
-- with a private NAT gateway. If the Elastic IP address is associated with
-- another resource, you must first disassociate it.
createNatGateway_allocationId :: Lens.Lens' CreateNatGateway (Prelude.Maybe Prelude.Text)
createNatGateway_allocationId = Lens.lens (\CreateNatGateway' {allocationId} -> allocationId) (\s@CreateNatGateway' {} a -> s {allocationId = a} :: CreateNatGateway)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNatGateway_dryRun :: Lens.Lens' CreateNatGateway (Prelude.Maybe Prelude.Bool)
createNatGateway_dryRun = Lens.lens (\CreateNatGateway' {dryRun} -> dryRun) (\s@CreateNatGateway' {} a -> s {dryRun = a} :: CreateNatGateway)

-- | The private IPv4 address to assign to the NAT gateway. If you don\'t
-- provide an address, a private IPv4 address will be automatically
-- assigned.
createNatGateway_privateIpAddress :: Lens.Lens' CreateNatGateway (Prelude.Maybe Prelude.Text)
createNatGateway_privateIpAddress = Lens.lens (\CreateNatGateway' {privateIpAddress} -> privateIpAddress) (\s@CreateNatGateway' {} a -> s {privateIpAddress = a} :: CreateNatGateway)

-- | Indicates whether the NAT gateway supports public or private
-- connectivity. The default is public connectivity.
createNatGateway_connectivityType :: Lens.Lens' CreateNatGateway (Prelude.Maybe ConnectivityType)
createNatGateway_connectivityType = Lens.lens (\CreateNatGateway' {connectivityType} -> connectivityType) (\s@CreateNatGateway' {} a -> s {connectivityType = a} :: CreateNatGateway)

-- | The tags to assign to the NAT gateway.
createNatGateway_tagSpecifications :: Lens.Lens' CreateNatGateway (Prelude.Maybe [TagSpecification])
createNatGateway_tagSpecifications = Lens.lens (\CreateNatGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateNatGateway' {} a -> s {tagSpecifications = a} :: CreateNatGateway) Prelude.. Lens.mapping Lens.coerced

-- | The subnet in which to create the NAT gateway.
createNatGateway_subnetId :: Lens.Lens' CreateNatGateway Prelude.Text
createNatGateway_subnetId = Lens.lens (\CreateNatGateway' {subnetId} -> subnetId) (\s@CreateNatGateway' {} a -> s {subnetId = a} :: CreateNatGateway)

instance Core.AWSRequest CreateNatGateway where
  type
    AWSResponse CreateNatGateway =
      CreateNatGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNatGatewayResponse'
            Prelude.<$> (x Data..@? "clientToken")
            Prelude.<*> (x Data..@? "natGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNatGateway where
  hashWithSalt _salt CreateNatGateway' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` connectivityType
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData CreateNatGateway where
  rnf CreateNatGateway' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf connectivityType
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf subnetId

instance Data.ToHeaders CreateNatGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateNatGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNatGateway where
  toQuery CreateNatGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateNatGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "AllocationId" Data.=: allocationId,
        "DryRun" Data.=: dryRun,
        "PrivateIpAddress" Data.=: privateIpAddress,
        "ConnectivityType" Data.=: connectivityType,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "SubnetId" Data.=: subnetId
      ]

-- | /See:/ 'newCreateNatGatewayResponse' smart constructor.
data CreateNatGatewayResponse = CreateNatGatewayResponse'
  { -- | Unique, case-sensitive identifier to ensure the idempotency of the
    -- request. Only returned if a client token was provided in the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the NAT gateway.
    natGateway :: Prelude.Maybe NatGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNatGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createNatGatewayResponse_clientToken' - Unique, case-sensitive identifier to ensure the idempotency of the
-- request. Only returned if a client token was provided in the request.
--
-- 'natGateway', 'createNatGatewayResponse_natGateway' - Information about the NAT gateway.
--
-- 'httpStatus', 'createNatGatewayResponse_httpStatus' - The response's http status code.
newCreateNatGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNatGatewayResponse
newCreateNatGatewayResponse pHttpStatus_ =
  CreateNatGatewayResponse'
    { clientToken =
        Prelude.Nothing,
      natGateway = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive identifier to ensure the idempotency of the
-- request. Only returned if a client token was provided in the request.
createNatGatewayResponse_clientToken :: Lens.Lens' CreateNatGatewayResponse (Prelude.Maybe Prelude.Text)
createNatGatewayResponse_clientToken = Lens.lens (\CreateNatGatewayResponse' {clientToken} -> clientToken) (\s@CreateNatGatewayResponse' {} a -> s {clientToken = a} :: CreateNatGatewayResponse)

-- | Information about the NAT gateway.
createNatGatewayResponse_natGateway :: Lens.Lens' CreateNatGatewayResponse (Prelude.Maybe NatGateway)
createNatGatewayResponse_natGateway = Lens.lens (\CreateNatGatewayResponse' {natGateway} -> natGateway) (\s@CreateNatGatewayResponse' {} a -> s {natGateway = a} :: CreateNatGatewayResponse)

-- | The response's http status code.
createNatGatewayResponse_httpStatus :: Lens.Lens' CreateNatGatewayResponse Prelude.Int
createNatGatewayResponse_httpStatus = Lens.lens (\CreateNatGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateNatGatewayResponse' {} a -> s {httpStatus = a} :: CreateNatGatewayResponse)

instance Prelude.NFData CreateNatGatewayResponse where
  rnf CreateNatGatewayResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf natGateway
      `Prelude.seq` Prelude.rnf httpStatus
