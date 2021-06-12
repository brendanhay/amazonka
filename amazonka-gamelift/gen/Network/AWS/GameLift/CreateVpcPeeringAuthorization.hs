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
-- Module      : Network.AWS.GameLift.CreateVpcPeeringAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests authorization to create or delete a peer connection between the
-- VPC for your Amazon GameLift fleet and a virtual private cloud (VPC) in
-- your AWS account. VPC peering enables the game servers on your fleet to
-- communicate directly with other AWS resources. Once you\'ve received
-- authorization, call CreateVpcPeeringConnection to establish the peering
-- connection. For more information, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
--
-- You can peer with VPCs that are owned by any AWS account you have access
-- to, including the account that you use to manage your Amazon GameLift
-- fleets. You cannot peer with VPCs that are in different Regions.
--
-- To request authorization to create a connection, call this operation
-- from the AWS account with the VPC that you want to peer to your Amazon
-- GameLift fleet. For example, to enable your game servers to retrieve
-- data from a DynamoDB table, use the account that manages that DynamoDB
-- resource. Identify the following values: (1) The ID of the VPC that you
-- want to peer with, and (2) the ID of the AWS account that you use to
-- manage Amazon GameLift. If successful, VPC peering is authorized for the
-- specified VPC.
--
-- To request authorization to delete a connection, call this operation
-- from the AWS account with the VPC that is peered with your Amazon
-- GameLift fleet. Identify the following values: (1) VPC ID that you want
-- to delete the peering connection for, and (2) ID of the AWS account that
-- you use to manage Amazon GameLift.
--
-- The authorization remains valid for 24 hours unless it is canceled by a
-- call to DeleteVpcPeeringAuthorization. You must create or delete the
-- peering connection while the authorization is valid.
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
module Network.AWS.GameLift.CreateVpcPeeringAuthorization
  ( -- * Creating a Request
    CreateVpcPeeringAuthorization (..),
    newCreateVpcPeeringAuthorization,

    -- * Request Lenses
    createVpcPeeringAuthorization_gameLiftAwsAccountId,
    createVpcPeeringAuthorization_peerVpcId,

    -- * Destructuring the Response
    CreateVpcPeeringAuthorizationResponse (..),
    newCreateVpcPeeringAuthorizationResponse,

    -- * Response Lenses
    createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization,
    createVpcPeeringAuthorizationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateVpcPeeringAuthorization' smart constructor.
data CreateVpcPeeringAuthorization = CreateVpcPeeringAuthorization'
  { -- | A unique identifier for the AWS account that you use to manage your
    -- Amazon GameLift fleet. You can find your Account ID in the AWS
    -- Management Console under account settings.
    gameLiftAwsAccountId :: Core.Text,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- Amazon GameLift fleet. The VPC must be in the same Region where your
    -- fleet is deployed. Look up a VPC ID using the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
    -- Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
    peerVpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateVpcPeeringAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameLiftAwsAccountId', 'createVpcPeeringAuthorization_gameLiftAwsAccountId' - A unique identifier for the AWS account that you use to manage your
-- Amazon GameLift fleet. You can find your Account ID in the AWS
-- Management Console under account settings.
--
-- 'peerVpcId', 'createVpcPeeringAuthorization_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
newCreateVpcPeeringAuthorization ::
  -- | 'gameLiftAwsAccountId'
  Core.Text ->
  -- | 'peerVpcId'
  Core.Text ->
  CreateVpcPeeringAuthorization
newCreateVpcPeeringAuthorization
  pGameLiftAwsAccountId_
  pPeerVpcId_ =
    CreateVpcPeeringAuthorization'
      { gameLiftAwsAccountId =
          pGameLiftAwsAccountId_,
        peerVpcId = pPeerVpcId_
      }

-- | A unique identifier for the AWS account that you use to manage your
-- Amazon GameLift fleet. You can find your Account ID in the AWS
-- Management Console under account settings.
createVpcPeeringAuthorization_gameLiftAwsAccountId :: Lens.Lens' CreateVpcPeeringAuthorization Core.Text
createVpcPeeringAuthorization_gameLiftAwsAccountId = Lens.lens (\CreateVpcPeeringAuthorization' {gameLiftAwsAccountId} -> gameLiftAwsAccountId) (\s@CreateVpcPeeringAuthorization' {} a -> s {gameLiftAwsAccountId = a} :: CreateVpcPeeringAuthorization)

-- | A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
createVpcPeeringAuthorization_peerVpcId :: Lens.Lens' CreateVpcPeeringAuthorization Core.Text
createVpcPeeringAuthorization_peerVpcId = Lens.lens (\CreateVpcPeeringAuthorization' {peerVpcId} -> peerVpcId) (\s@CreateVpcPeeringAuthorization' {} a -> s {peerVpcId = a} :: CreateVpcPeeringAuthorization)

instance
  Core.AWSRequest
    CreateVpcPeeringAuthorization
  where
  type
    AWSResponse CreateVpcPeeringAuthorization =
      CreateVpcPeeringAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcPeeringAuthorizationResponse'
            Core.<$> (x Core..?> "VpcPeeringAuthorization")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateVpcPeeringAuthorization

instance Core.NFData CreateVpcPeeringAuthorization

instance Core.ToHeaders CreateVpcPeeringAuthorization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.CreateVpcPeeringAuthorization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateVpcPeeringAuthorization where
  toJSON CreateVpcPeeringAuthorization' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "GameLiftAwsAccountId"
                  Core..= gameLiftAwsAccountId
              ),
            Core.Just ("PeerVpcId" Core..= peerVpcId)
          ]
      )

instance Core.ToPath CreateVpcPeeringAuthorization where
  toPath = Core.const "/"

instance Core.ToQuery CreateVpcPeeringAuthorization where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateVpcPeeringAuthorizationResponse' smart constructor.
data CreateVpcPeeringAuthorizationResponse = CreateVpcPeeringAuthorizationResponse'
  { -- | Details on the requested VPC peering authorization, including
    -- expiration.
    vpcPeeringAuthorization :: Core.Maybe VpcPeeringAuthorization,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateVpcPeeringAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringAuthorization', 'createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization' - Details on the requested VPC peering authorization, including
-- expiration.
--
-- 'httpStatus', 'createVpcPeeringAuthorizationResponse_httpStatus' - The response's http status code.
newCreateVpcPeeringAuthorizationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateVpcPeeringAuthorizationResponse
newCreateVpcPeeringAuthorizationResponse pHttpStatus_ =
  CreateVpcPeeringAuthorizationResponse'
    { vpcPeeringAuthorization =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details on the requested VPC peering authorization, including
-- expiration.
createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization :: Lens.Lens' CreateVpcPeeringAuthorizationResponse (Core.Maybe VpcPeeringAuthorization)
createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization = Lens.lens (\CreateVpcPeeringAuthorizationResponse' {vpcPeeringAuthorization} -> vpcPeeringAuthorization) (\s@CreateVpcPeeringAuthorizationResponse' {} a -> s {vpcPeeringAuthorization = a} :: CreateVpcPeeringAuthorizationResponse)

-- | The response's http status code.
createVpcPeeringAuthorizationResponse_httpStatus :: Lens.Lens' CreateVpcPeeringAuthorizationResponse Core.Int
createVpcPeeringAuthorizationResponse_httpStatus = Lens.lens (\CreateVpcPeeringAuthorizationResponse' {httpStatus} -> httpStatus) (\s@CreateVpcPeeringAuthorizationResponse' {} a -> s {httpStatus = a} :: CreateVpcPeeringAuthorizationResponse)

instance
  Core.NFData
    CreateVpcPeeringAuthorizationResponse
