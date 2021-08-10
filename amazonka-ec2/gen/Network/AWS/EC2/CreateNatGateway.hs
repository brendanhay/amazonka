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
-- Module      : Network.AWS.EC2.CreateNatGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a NAT gateway in the specified public subnet. This action
-- creates a network interface in the specified subnet with a private IP
-- address from the IP address range of the subnet. Internet-bound traffic
-- from a private subnet can be routed to the NAT gateway, therefore
-- enabling instances in the private subnet to connect to the internet. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html NAT Gateways>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateNatGateway
  ( -- * Creating a Request
    CreateNatGateway (..),
    newCreateNatGateway,

    -- * Request Lenses
    createNatGateway_tagSpecifications,
    createNatGateway_dryRun,
    createNatGateway_clientToken,
    createNatGateway_allocationId,
    createNatGateway_subnetId,

    -- * Destructuring the Response
    CreateNatGatewayResponse (..),
    newCreateNatGatewayResponse,

    -- * Response Lenses
    createNatGatewayResponse_natGateway,
    createNatGatewayResponse_clientToken,
    createNatGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNatGateway' smart constructor.
data CreateNatGateway = CreateNatGateway'
  { -- | The tags to assign to the NAT gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    --
    -- Constraint: Maximum 64 ASCII characters.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The allocation ID of an Elastic IP address to associate with the NAT
    -- gateway. If the Elastic IP address is associated with another resource,
    -- you must first disassociate it.
    allocationId :: Prelude.Text,
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
-- 'tagSpecifications', 'createNatGateway_tagSpecifications' - The tags to assign to the NAT gateway.
--
-- 'dryRun', 'createNatGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'createNatGateway_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- Constraint: Maximum 64 ASCII characters.
--
-- 'allocationId', 'createNatGateway_allocationId' - The allocation ID of an Elastic IP address to associate with the NAT
-- gateway. If the Elastic IP address is associated with another resource,
-- you must first disassociate it.
--
-- 'subnetId', 'createNatGateway_subnetId' - The subnet in which to create the NAT gateway.
newCreateNatGateway ::
  -- | 'allocationId'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  CreateNatGateway
newCreateNatGateway pAllocationId_ pSubnetId_ =
  CreateNatGateway'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      allocationId = pAllocationId_,
      subnetId = pSubnetId_
    }

-- | The tags to assign to the NAT gateway.
createNatGateway_tagSpecifications :: Lens.Lens' CreateNatGateway (Prelude.Maybe [TagSpecification])
createNatGateway_tagSpecifications = Lens.lens (\CreateNatGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateNatGateway' {} a -> s {tagSpecifications = a} :: CreateNatGateway) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNatGateway_dryRun :: Lens.Lens' CreateNatGateway (Prelude.Maybe Prelude.Bool)
createNatGateway_dryRun = Lens.lens (\CreateNatGateway' {dryRun} -> dryRun) (\s@CreateNatGateway' {} a -> s {dryRun = a} :: CreateNatGateway)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- Constraint: Maximum 64 ASCII characters.
createNatGateway_clientToken :: Lens.Lens' CreateNatGateway (Prelude.Maybe Prelude.Text)
createNatGateway_clientToken = Lens.lens (\CreateNatGateway' {clientToken} -> clientToken) (\s@CreateNatGateway' {} a -> s {clientToken = a} :: CreateNatGateway)

-- | The allocation ID of an Elastic IP address to associate with the NAT
-- gateway. If the Elastic IP address is associated with another resource,
-- you must first disassociate it.
createNatGateway_allocationId :: Lens.Lens' CreateNatGateway Prelude.Text
createNatGateway_allocationId = Lens.lens (\CreateNatGateway' {allocationId} -> allocationId) (\s@CreateNatGateway' {} a -> s {allocationId = a} :: CreateNatGateway)

-- | The subnet in which to create the NAT gateway.
createNatGateway_subnetId :: Lens.Lens' CreateNatGateway Prelude.Text
createNatGateway_subnetId = Lens.lens (\CreateNatGateway' {subnetId} -> subnetId) (\s@CreateNatGateway' {} a -> s {subnetId = a} :: CreateNatGateway)

instance Core.AWSRequest CreateNatGateway where
  type
    AWSResponse CreateNatGateway =
      CreateNatGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNatGatewayResponse'
            Prelude.<$> (x Core..@? "natGateway")
            Prelude.<*> (x Core..@? "clientToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNatGateway

instance Prelude.NFData CreateNatGateway

instance Core.ToHeaders CreateNatGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateNatGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateNatGateway where
  toQuery CreateNatGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateNatGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "ClientToken" Core.=: clientToken,
        "AllocationId" Core.=: allocationId,
        "SubnetId" Core.=: subnetId
      ]

-- | /See:/ 'newCreateNatGatewayResponse' smart constructor.
data CreateNatGatewayResponse = CreateNatGatewayResponse'
  { -- | Information about the NAT gateway.
    natGateway :: Prelude.Maybe NatGateway,
    -- | Unique, case-sensitive identifier to ensure the idempotency of the
    -- request. Only returned if a client token was provided in the request.
    clientToken :: Prelude.Maybe Prelude.Text,
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
-- 'natGateway', 'createNatGatewayResponse_natGateway' - Information about the NAT gateway.
--
-- 'clientToken', 'createNatGatewayResponse_clientToken' - Unique, case-sensitive identifier to ensure the idempotency of the
-- request. Only returned if a client token was provided in the request.
--
-- 'httpStatus', 'createNatGatewayResponse_httpStatus' - The response's http status code.
newCreateNatGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNatGatewayResponse
newCreateNatGatewayResponse pHttpStatus_ =
  CreateNatGatewayResponse'
    { natGateway =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the NAT gateway.
createNatGatewayResponse_natGateway :: Lens.Lens' CreateNatGatewayResponse (Prelude.Maybe NatGateway)
createNatGatewayResponse_natGateway = Lens.lens (\CreateNatGatewayResponse' {natGateway} -> natGateway) (\s@CreateNatGatewayResponse' {} a -> s {natGateway = a} :: CreateNatGatewayResponse)

-- | Unique, case-sensitive identifier to ensure the idempotency of the
-- request. Only returned if a client token was provided in the request.
createNatGatewayResponse_clientToken :: Lens.Lens' CreateNatGatewayResponse (Prelude.Maybe Prelude.Text)
createNatGatewayResponse_clientToken = Lens.lens (\CreateNatGatewayResponse' {clientToken} -> clientToken) (\s@CreateNatGatewayResponse' {} a -> s {clientToken = a} :: CreateNatGatewayResponse)

-- | The response's http status code.
createNatGatewayResponse_httpStatus :: Lens.Lens' CreateNatGatewayResponse Prelude.Int
createNatGatewayResponse_httpStatus = Lens.lens (\CreateNatGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateNatGatewayResponse' {} a -> s {httpStatus = a} :: CreateNatGatewayResponse)

instance Prelude.NFData CreateNatGatewayResponse
