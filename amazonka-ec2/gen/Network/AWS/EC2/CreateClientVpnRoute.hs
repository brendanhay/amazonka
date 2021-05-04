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
-- Module      : Network.AWS.EC2.CreateClientVpnRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a route to a network to a Client VPN endpoint. Each Client VPN
-- endpoint has a route table that describes the available destination
-- network routes. Each route in the route table specifies the path for
-- traﬃc to speciﬁc resources or networks.
module Network.AWS.EC2.CreateClientVpnRoute
  ( -- * Creating a Request
    CreateClientVpnRoute (..),
    newCreateClientVpnRoute,

    -- * Request Lenses
    createClientVpnRoute_dryRun,
    createClientVpnRoute_description,
    createClientVpnRoute_clientToken,
    createClientVpnRoute_clientVpnEndpointId,
    createClientVpnRoute_destinationCidrBlock,
    createClientVpnRoute_targetVpcSubnetId,

    -- * Destructuring the Response
    CreateClientVpnRouteResponse (..),
    newCreateClientVpnRouteResponse,

    -- * Response Lenses
    createClientVpnRouteResponse_status,
    createClientVpnRouteResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateClientVpnRoute' smart constructor.
data CreateClientVpnRoute = CreateClientVpnRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A brief description of the route.
    description :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint to which to add the route.
    clientVpnEndpointId :: Prelude.Text,
    -- | The IPv4 address range, in CIDR notation, of the route destination. For
    -- example:
    --
    -- -   To add a route for Internet access, enter @0.0.0.0\/0@
    --
    -- -   To add a route for a peered VPC, enter the peered VPC\'s IPv4 CIDR
    --     range
    --
    -- -   To add a route for an on-premises network, enter the AWS
    --     Site-to-Site VPN connection\'s IPv4 CIDR range
    --
    -- -   To add a route for the local network, enter the client CIDR range
    destinationCidrBlock :: Prelude.Text,
    -- | The ID of the subnet through which you want to route traffic. The
    -- specified subnet must be an existing target network of the Client VPN
    -- endpoint.
    --
    -- Alternatively, if you\'re adding a route for the local network, specify
    -- @local@.
    targetVpcSubnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateClientVpnRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createClientVpnRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'description', 'createClientVpnRoute_description' - A brief description of the route.
--
-- 'clientToken', 'createClientVpnRoute_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'clientVpnEndpointId', 'createClientVpnRoute_clientVpnEndpointId' - The ID of the Client VPN endpoint to which to add the route.
--
-- 'destinationCidrBlock', 'createClientVpnRoute_destinationCidrBlock' - The IPv4 address range, in CIDR notation, of the route destination. For
-- example:
--
-- -   To add a route for Internet access, enter @0.0.0.0\/0@
--
-- -   To add a route for a peered VPC, enter the peered VPC\'s IPv4 CIDR
--     range
--
-- -   To add a route for an on-premises network, enter the AWS
--     Site-to-Site VPN connection\'s IPv4 CIDR range
--
-- -   To add a route for the local network, enter the client CIDR range
--
-- 'targetVpcSubnetId', 'createClientVpnRoute_targetVpcSubnetId' - The ID of the subnet through which you want to route traffic. The
-- specified subnet must be an existing target network of the Client VPN
-- endpoint.
--
-- Alternatively, if you\'re adding a route for the local network, specify
-- @local@.
newCreateClientVpnRoute ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  -- | 'targetVpcSubnetId'
  Prelude.Text ->
  CreateClientVpnRoute
newCreateClientVpnRoute
  pClientVpnEndpointId_
  pDestinationCidrBlock_
  pTargetVpcSubnetId_ =
    CreateClientVpnRoute'
      { dryRun = Prelude.Nothing,
        description = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        destinationCidrBlock = pDestinationCidrBlock_,
        targetVpcSubnetId = pTargetVpcSubnetId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createClientVpnRoute_dryRun :: Lens.Lens' CreateClientVpnRoute (Prelude.Maybe Prelude.Bool)
createClientVpnRoute_dryRun = Lens.lens (\CreateClientVpnRoute' {dryRun} -> dryRun) (\s@CreateClientVpnRoute' {} a -> s {dryRun = a} :: CreateClientVpnRoute)

-- | A brief description of the route.
createClientVpnRoute_description :: Lens.Lens' CreateClientVpnRoute (Prelude.Maybe Prelude.Text)
createClientVpnRoute_description = Lens.lens (\CreateClientVpnRoute' {description} -> description) (\s@CreateClientVpnRoute' {} a -> s {description = a} :: CreateClientVpnRoute)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createClientVpnRoute_clientToken :: Lens.Lens' CreateClientVpnRoute (Prelude.Maybe Prelude.Text)
createClientVpnRoute_clientToken = Lens.lens (\CreateClientVpnRoute' {clientToken} -> clientToken) (\s@CreateClientVpnRoute' {} a -> s {clientToken = a} :: CreateClientVpnRoute)

-- | The ID of the Client VPN endpoint to which to add the route.
createClientVpnRoute_clientVpnEndpointId :: Lens.Lens' CreateClientVpnRoute Prelude.Text
createClientVpnRoute_clientVpnEndpointId = Lens.lens (\CreateClientVpnRoute' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@CreateClientVpnRoute' {} a -> s {clientVpnEndpointId = a} :: CreateClientVpnRoute)

-- | The IPv4 address range, in CIDR notation, of the route destination. For
-- example:
--
-- -   To add a route for Internet access, enter @0.0.0.0\/0@
--
-- -   To add a route for a peered VPC, enter the peered VPC\'s IPv4 CIDR
--     range
--
-- -   To add a route for an on-premises network, enter the AWS
--     Site-to-Site VPN connection\'s IPv4 CIDR range
--
-- -   To add a route for the local network, enter the client CIDR range
createClientVpnRoute_destinationCidrBlock :: Lens.Lens' CreateClientVpnRoute Prelude.Text
createClientVpnRoute_destinationCidrBlock = Lens.lens (\CreateClientVpnRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@CreateClientVpnRoute' {} a -> s {destinationCidrBlock = a} :: CreateClientVpnRoute)

-- | The ID of the subnet through which you want to route traffic. The
-- specified subnet must be an existing target network of the Client VPN
-- endpoint.
--
-- Alternatively, if you\'re adding a route for the local network, specify
-- @local@.
createClientVpnRoute_targetVpcSubnetId :: Lens.Lens' CreateClientVpnRoute Prelude.Text
createClientVpnRoute_targetVpcSubnetId = Lens.lens (\CreateClientVpnRoute' {targetVpcSubnetId} -> targetVpcSubnetId) (\s@CreateClientVpnRoute' {} a -> s {targetVpcSubnetId = a} :: CreateClientVpnRoute)

instance Prelude.AWSRequest CreateClientVpnRoute where
  type
    Rs CreateClientVpnRoute =
      CreateClientVpnRouteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateClientVpnRouteResponse'
            Prelude.<$> (x Prelude..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateClientVpnRoute

instance Prelude.NFData CreateClientVpnRoute

instance Prelude.ToHeaders CreateClientVpnRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateClientVpnRoute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateClientVpnRoute where
  toQuery CreateClientVpnRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateClientVpnRoute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "Description" Prelude.=: description,
        "ClientToken" Prelude.=: clientToken,
        "ClientVpnEndpointId" Prelude.=: clientVpnEndpointId,
        "DestinationCidrBlock"
          Prelude.=: destinationCidrBlock,
        "TargetVpcSubnetId" Prelude.=: targetVpcSubnetId
      ]

-- | /See:/ 'newCreateClientVpnRouteResponse' smart constructor.
data CreateClientVpnRouteResponse = CreateClientVpnRouteResponse'
  { -- | The current state of the route.
    status :: Prelude.Maybe ClientVpnRouteStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateClientVpnRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createClientVpnRouteResponse_status' - The current state of the route.
--
-- 'httpStatus', 'createClientVpnRouteResponse_httpStatus' - The response's http status code.
newCreateClientVpnRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClientVpnRouteResponse
newCreateClientVpnRouteResponse pHttpStatus_ =
  CreateClientVpnRouteResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the route.
createClientVpnRouteResponse_status :: Lens.Lens' CreateClientVpnRouteResponse (Prelude.Maybe ClientVpnRouteStatus)
createClientVpnRouteResponse_status = Lens.lens (\CreateClientVpnRouteResponse' {status} -> status) (\s@CreateClientVpnRouteResponse' {} a -> s {status = a} :: CreateClientVpnRouteResponse)

-- | The response's http status code.
createClientVpnRouteResponse_httpStatus :: Lens.Lens' CreateClientVpnRouteResponse Prelude.Int
createClientVpnRouteResponse_httpStatus = Lens.lens (\CreateClientVpnRouteResponse' {httpStatus} -> httpStatus) (\s@CreateClientVpnRouteResponse' {} a -> s {httpStatus = a} :: CreateClientVpnRouteResponse)

instance Prelude.NFData CreateClientVpnRouteResponse
