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
-- Module      : Amazonka.GameLift.CreateVpcPeeringAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests authorization to create or delete a peer connection between the
-- VPC for your Amazon GameLift fleet and a virtual private cloud (VPC) in
-- your Amazon Web Services account. VPC peering enables the game servers
-- on your fleet to communicate directly with other Amazon Web Services
-- resources. After you\'ve received authorization, use
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_CreateVpcPeeringConnection.html CreateVpcPeeringConnection>
-- to establish the peering connection. For more information, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
--
-- You can peer with VPCs that are owned by any Amazon Web Services account
-- you have access to, including the account that you use to manage your
-- Amazon GameLift fleets. You cannot peer with VPCs that are in different
-- Regions.
--
-- To request authorization to create a connection, call this operation
-- from the Amazon Web Services account with the VPC that you want to peer
-- to your Amazon GameLift fleet. For example, to enable your game servers
-- to retrieve data from a DynamoDB table, use the account that manages
-- that DynamoDB resource. Identify the following values: (1) The ID of the
-- VPC that you want to peer with, and (2) the ID of the Amazon Web
-- Services account that you use to manage Amazon GameLift. If successful,
-- VPC peering is authorized for the specified VPC.
--
-- To request authorization to delete a connection, call this operation
-- from the Amazon Web Services account with the VPC that is peered with
-- your Amazon GameLift fleet. Identify the following values: (1) VPC ID
-- that you want to delete the peering connection for, and (2) ID of the
-- Amazon Web Services account that you use to manage Amazon GameLift.
--
-- The authorization remains valid for 24 hours unless it is canceled. You
-- must create or delete the peering connection while the authorization is
-- valid.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateVpcPeeringAuthorization
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpcPeeringAuthorization' smart constructor.
data CreateVpcPeeringAuthorization = CreateVpcPeeringAuthorization'
  { -- | A unique identifier for the Amazon Web Services account that you use to
    -- manage your GameLift fleet. You can find your Account ID in the Amazon
    -- Web Services Management Console under account settings.
    gameLiftAwsAccountId :: Prelude.Text,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- GameLift fleet. The VPC must be in the same Region as your fleet. To
    -- look up a VPC ID, use the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
    -- Services Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
    peerVpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcPeeringAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameLiftAwsAccountId', 'createVpcPeeringAuthorization_gameLiftAwsAccountId' - A unique identifier for the Amazon Web Services account that you use to
-- manage your GameLift fleet. You can find your Account ID in the Amazon
-- Web Services Management Console under account settings.
--
-- 'peerVpcId', 'createVpcPeeringAuthorization_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
newCreateVpcPeeringAuthorization ::
  -- | 'gameLiftAwsAccountId'
  Prelude.Text ->
  -- | 'peerVpcId'
  Prelude.Text ->
  CreateVpcPeeringAuthorization
newCreateVpcPeeringAuthorization
  pGameLiftAwsAccountId_
  pPeerVpcId_ =
    CreateVpcPeeringAuthorization'
      { gameLiftAwsAccountId =
          pGameLiftAwsAccountId_,
        peerVpcId = pPeerVpcId_
      }

-- | A unique identifier for the Amazon Web Services account that you use to
-- manage your GameLift fleet. You can find your Account ID in the Amazon
-- Web Services Management Console under account settings.
createVpcPeeringAuthorization_gameLiftAwsAccountId :: Lens.Lens' CreateVpcPeeringAuthorization Prelude.Text
createVpcPeeringAuthorization_gameLiftAwsAccountId = Lens.lens (\CreateVpcPeeringAuthorization' {gameLiftAwsAccountId} -> gameLiftAwsAccountId) (\s@CreateVpcPeeringAuthorization' {} a -> s {gameLiftAwsAccountId = a} :: CreateVpcPeeringAuthorization)

-- | A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
createVpcPeeringAuthorization_peerVpcId :: Lens.Lens' CreateVpcPeeringAuthorization Prelude.Text
createVpcPeeringAuthorization_peerVpcId = Lens.lens (\CreateVpcPeeringAuthorization' {peerVpcId} -> peerVpcId) (\s@CreateVpcPeeringAuthorization' {} a -> s {peerVpcId = a} :: CreateVpcPeeringAuthorization)

instance
  Core.AWSRequest
    CreateVpcPeeringAuthorization
  where
  type
    AWSResponse CreateVpcPeeringAuthorization =
      CreateVpcPeeringAuthorizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcPeeringAuthorizationResponse'
            Prelude.<$> (x Data..?> "VpcPeeringAuthorization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateVpcPeeringAuthorization
  where
  hashWithSalt _salt CreateVpcPeeringAuthorization' {..} =
    _salt
      `Prelude.hashWithSalt` gameLiftAwsAccountId
      `Prelude.hashWithSalt` peerVpcId

instance Prelude.NFData CreateVpcPeeringAuthorization where
  rnf CreateVpcPeeringAuthorization' {..} =
    Prelude.rnf gameLiftAwsAccountId
      `Prelude.seq` Prelude.rnf peerVpcId

instance Data.ToHeaders CreateVpcPeeringAuthorization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.CreateVpcPeeringAuthorization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVpcPeeringAuthorization where
  toJSON CreateVpcPeeringAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "GameLiftAwsAccountId"
                  Data..= gameLiftAwsAccountId
              ),
            Prelude.Just ("PeerVpcId" Data..= peerVpcId)
          ]
      )

instance Data.ToPath CreateVpcPeeringAuthorization where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVpcPeeringAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcPeeringAuthorizationResponse' smart constructor.
data CreateVpcPeeringAuthorizationResponse = CreateVpcPeeringAuthorizationResponse'
  { -- | Details on the requested VPC peering authorization, including
    -- expiration.
    vpcPeeringAuthorization :: Prelude.Maybe VpcPeeringAuthorization,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateVpcPeeringAuthorizationResponse
newCreateVpcPeeringAuthorizationResponse pHttpStatus_ =
  CreateVpcPeeringAuthorizationResponse'
    { vpcPeeringAuthorization =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details on the requested VPC peering authorization, including
-- expiration.
createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization :: Lens.Lens' CreateVpcPeeringAuthorizationResponse (Prelude.Maybe VpcPeeringAuthorization)
createVpcPeeringAuthorizationResponse_vpcPeeringAuthorization = Lens.lens (\CreateVpcPeeringAuthorizationResponse' {vpcPeeringAuthorization} -> vpcPeeringAuthorization) (\s@CreateVpcPeeringAuthorizationResponse' {} a -> s {vpcPeeringAuthorization = a} :: CreateVpcPeeringAuthorizationResponse)

-- | The response's http status code.
createVpcPeeringAuthorizationResponse_httpStatus :: Lens.Lens' CreateVpcPeeringAuthorizationResponse Prelude.Int
createVpcPeeringAuthorizationResponse_httpStatus = Lens.lens (\CreateVpcPeeringAuthorizationResponse' {httpStatus} -> httpStatus) (\s@CreateVpcPeeringAuthorizationResponse' {} a -> s {httpStatus = a} :: CreateVpcPeeringAuthorizationResponse)

instance
  Prelude.NFData
    CreateVpcPeeringAuthorizationResponse
  where
  rnf CreateVpcPeeringAuthorizationResponse' {..} =
    Prelude.rnf vpcPeeringAuthorization
      `Prelude.seq` Prelude.rnf httpStatus
