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
-- Module      : Network.AWS.GameLift.CreateVpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a VPC peering connection between a virtual private cloud
-- (VPC) in an AWS account with the VPC for your Amazon GameLift fleet. VPC
-- peering enables the game servers on your fleet to communicate directly
-- with other AWS resources. You can peer with VPCs in any AWS account that
-- you have access to, including the account that you use to manage your
-- Amazon GameLift fleets. You cannot peer with VPCs that are in different
-- Regions. For more information, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
--
-- Before calling this operation to establish the peering connection, you
-- first need to call CreateVpcPeeringAuthorization and identify the VPC
-- you want to peer with. Once the authorization for the specified VPC is
-- issued, you have 24 hours to establish the connection. These two
-- operations handle all tasks necessary to peer the two VPCs, including
-- acceptance, updating routing tables, etc.
--
-- To establish the connection, call this operation from the AWS account
-- that is used to manage the Amazon GameLift fleets. Identify the
-- following values: (1) The ID of the fleet you want to be enable a VPC
-- peering connection for; (2) The AWS account with the VPC that you want
-- to peer with; and (3) The ID of the VPC you want to peer with. This
-- operation is asynchronous. If successful, a VpcPeeringConnection request
-- is created. You can use continuous polling to track the request\'s
-- status using DescribeVpcPeeringConnections, or by monitoring fleet
-- events for success or failure using DescribeFleetEvents.
--
-- -   CreateVpcPeeringAuthorization
--
-- -   DescribeVpcPeeringAuthorizations
--
-- -   DeleteVpcPeeringAuthorization
--
-- -   CreateVpcPeeringConnection
--
-- -   DescribeVpcPeeringConnections
--
-- -   DeleteVpcPeeringConnection
module Network.AWS.GameLift.CreateVpcPeeringConnection
  ( -- * Creating a Request
    CreateVpcPeeringConnection (..),
    newCreateVpcPeeringConnection,

    -- * Request Lenses
    createVpcPeeringConnection_fleetId,
    createVpcPeeringConnection_peerVpcAwsAccountId,
    createVpcPeeringConnection_peerVpcId,

    -- * Destructuring the Response
    CreateVpcPeeringConnectionResponse (..),
    newCreateVpcPeeringConnectionResponse,

    -- * Response Lenses
    createVpcPeeringConnectionResponse_httpStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateVpcPeeringConnection' smart constructor.
data CreateVpcPeeringConnection = CreateVpcPeeringConnection'
  { -- | A unique identifier for a fleet. You can use either the fleet ID or ARN
    -- value. This tells Amazon GameLift which GameLift VPC to peer with.
    fleetId :: Prelude.Text,
    -- | A unique identifier for the AWS account with the VPC that you want to
    -- peer your Amazon GameLift fleet with. You can find your Account ID in
    -- the AWS Management Console under account settings.
    peerVpcAwsAccountId :: Prelude.Text,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- Amazon GameLift fleet. The VPC must be in the same Region where your
    -- fleet is deployed. Look up a VPC ID using the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
    -- Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
    peerVpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'createVpcPeeringConnection_fleetId' - A unique identifier for a fleet. You can use either the fleet ID or ARN
-- value. This tells Amazon GameLift which GameLift VPC to peer with.
--
-- 'peerVpcAwsAccountId', 'createVpcPeeringConnection_peerVpcAwsAccountId' - A unique identifier for the AWS account with the VPC that you want to
-- peer your Amazon GameLift fleet with. You can find your Account ID in
-- the AWS Management Console under account settings.
--
-- 'peerVpcId', 'createVpcPeeringConnection_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
newCreateVpcPeeringConnection ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'peerVpcAwsAccountId'
  Prelude.Text ->
  -- | 'peerVpcId'
  Prelude.Text ->
  CreateVpcPeeringConnection
newCreateVpcPeeringConnection
  pFleetId_
  pPeerVpcAwsAccountId_
  pPeerVpcId_ =
    CreateVpcPeeringConnection'
      { fleetId = pFleetId_,
        peerVpcAwsAccountId = pPeerVpcAwsAccountId_,
        peerVpcId = pPeerVpcId_
      }

-- | A unique identifier for a fleet. You can use either the fleet ID or ARN
-- value. This tells Amazon GameLift which GameLift VPC to peer with.
createVpcPeeringConnection_fleetId :: Lens.Lens' CreateVpcPeeringConnection Prelude.Text
createVpcPeeringConnection_fleetId = Lens.lens (\CreateVpcPeeringConnection' {fleetId} -> fleetId) (\s@CreateVpcPeeringConnection' {} a -> s {fleetId = a} :: CreateVpcPeeringConnection)

-- | A unique identifier for the AWS account with the VPC that you want to
-- peer your Amazon GameLift fleet with. You can find your Account ID in
-- the AWS Management Console under account settings.
createVpcPeeringConnection_peerVpcAwsAccountId :: Lens.Lens' CreateVpcPeeringConnection Prelude.Text
createVpcPeeringConnection_peerVpcAwsAccountId = Lens.lens (\CreateVpcPeeringConnection' {peerVpcAwsAccountId} -> peerVpcAwsAccountId) (\s@CreateVpcPeeringConnection' {} a -> s {peerVpcAwsAccountId = a} :: CreateVpcPeeringConnection)

-- | A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
createVpcPeeringConnection_peerVpcId :: Lens.Lens' CreateVpcPeeringConnection Prelude.Text
createVpcPeeringConnection_peerVpcId = Lens.lens (\CreateVpcPeeringConnection' {peerVpcId} -> peerVpcId) (\s@CreateVpcPeeringConnection' {} a -> s {peerVpcId = a} :: CreateVpcPeeringConnection)

instance
  Prelude.AWSRequest
    CreateVpcPeeringConnection
  where
  type
    Rs CreateVpcPeeringConnection =
      CreateVpcPeeringConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateVpcPeeringConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpcPeeringConnection

instance Prelude.NFData CreateVpcPeeringConnection

instance Prelude.ToHeaders CreateVpcPeeringConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.CreateVpcPeeringConnection" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateVpcPeeringConnection where
  toJSON CreateVpcPeeringConnection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Prelude..= fleetId),
            Prelude.Just
              ( "PeerVpcAwsAccountId"
                  Prelude..= peerVpcAwsAccountId
              ),
            Prelude.Just ("PeerVpcId" Prelude..= peerVpcId)
          ]
      )

instance Prelude.ToPath CreateVpcPeeringConnection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateVpcPeeringConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcPeeringConnectionResponse' smart constructor.
data CreateVpcPeeringConnectionResponse = CreateVpcPeeringConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcPeeringConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVpcPeeringConnectionResponse_httpStatus' - The response's http status code.
newCreateVpcPeeringConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcPeeringConnectionResponse
newCreateVpcPeeringConnectionResponse pHttpStatus_ =
  CreateVpcPeeringConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createVpcPeeringConnectionResponse_httpStatus :: Lens.Lens' CreateVpcPeeringConnectionResponse Prelude.Int
createVpcPeeringConnectionResponse_httpStatus = Lens.lens (\CreateVpcPeeringConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateVpcPeeringConnectionResponse' {} a -> s {httpStatus = a} :: CreateVpcPeeringConnectionResponse)

instance
  Prelude.NFData
    CreateVpcPeeringConnectionResponse
