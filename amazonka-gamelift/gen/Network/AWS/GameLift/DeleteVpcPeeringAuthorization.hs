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
-- Module      : Network.AWS.GameLift.DeleteVpcPeeringAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending VPC peering authorization for the specified VPC. If
-- you need to delete an existing VPC peering connection, call
-- DeleteVpcPeeringConnection.
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
module Network.AWS.GameLift.DeleteVpcPeeringAuthorization
  ( -- * Creating a Request
    DeleteVpcPeeringAuthorization (..),
    newDeleteVpcPeeringAuthorization,

    -- * Request Lenses
    deleteVpcPeeringAuthorization_gameLiftAwsAccountId,
    deleteVpcPeeringAuthorization_peerVpcId,

    -- * Destructuring the Response
    DeleteVpcPeeringAuthorizationResponse (..),
    newDeleteVpcPeeringAuthorizationResponse,

    -- * Response Lenses
    deleteVpcPeeringAuthorizationResponse_httpStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteVpcPeeringAuthorization' smart constructor.
data DeleteVpcPeeringAuthorization = DeleteVpcPeeringAuthorization'
  { -- | A unique identifier for the AWS account that you use to manage your
    -- Amazon GameLift fleet. You can find your Account ID in the AWS
    -- Management Console under account settings.
    gameLiftAwsAccountId :: Prelude.Text,
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
-- Create a value of 'DeleteVpcPeeringAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameLiftAwsAccountId', 'deleteVpcPeeringAuthorization_gameLiftAwsAccountId' - A unique identifier for the AWS account that you use to manage your
-- Amazon GameLift fleet. You can find your Account ID in the AWS
-- Management Console under account settings.
--
-- 'peerVpcId', 'deleteVpcPeeringAuthorization_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
newDeleteVpcPeeringAuthorization ::
  -- | 'gameLiftAwsAccountId'
  Prelude.Text ->
  -- | 'peerVpcId'
  Prelude.Text ->
  DeleteVpcPeeringAuthorization
newDeleteVpcPeeringAuthorization
  pGameLiftAwsAccountId_
  pPeerVpcId_ =
    DeleteVpcPeeringAuthorization'
      { gameLiftAwsAccountId =
          pGameLiftAwsAccountId_,
        peerVpcId = pPeerVpcId_
      }

-- | A unique identifier for the AWS account that you use to manage your
-- Amazon GameLift fleet. You can find your Account ID in the AWS
-- Management Console under account settings.
deleteVpcPeeringAuthorization_gameLiftAwsAccountId :: Lens.Lens' DeleteVpcPeeringAuthorization Prelude.Text
deleteVpcPeeringAuthorization_gameLiftAwsAccountId = Lens.lens (\DeleteVpcPeeringAuthorization' {gameLiftAwsAccountId} -> gameLiftAwsAccountId) (\s@DeleteVpcPeeringAuthorization' {} a -> s {gameLiftAwsAccountId = a} :: DeleteVpcPeeringAuthorization)

-- | A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
deleteVpcPeeringAuthorization_peerVpcId :: Lens.Lens' DeleteVpcPeeringAuthorization Prelude.Text
deleteVpcPeeringAuthorization_peerVpcId = Lens.lens (\DeleteVpcPeeringAuthorization' {peerVpcId} -> peerVpcId) (\s@DeleteVpcPeeringAuthorization' {} a -> s {peerVpcId = a} :: DeleteVpcPeeringAuthorization)

instance
  Prelude.AWSRequest
    DeleteVpcPeeringAuthorization
  where
  type
    Rs DeleteVpcPeeringAuthorization =
      DeleteVpcPeeringAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVpcPeeringAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVpcPeeringAuthorization

instance Prelude.NFData DeleteVpcPeeringAuthorization

instance
  Prelude.ToHeaders
    DeleteVpcPeeringAuthorization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.DeleteVpcPeeringAuthorization" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteVpcPeeringAuthorization where
  toJSON DeleteVpcPeeringAuthorization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "GameLiftAwsAccountId"
                  Prelude..= gameLiftAwsAccountId
              ),
            Prelude.Just ("PeerVpcId" Prelude..= peerVpcId)
          ]
      )

instance Prelude.ToPath DeleteVpcPeeringAuthorization where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteVpcPeeringAuthorization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcPeeringAuthorizationResponse' smart constructor.
data DeleteVpcPeeringAuthorizationResponse = DeleteVpcPeeringAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcPeeringAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVpcPeeringAuthorizationResponse_httpStatus' - The response's http status code.
newDeleteVpcPeeringAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcPeeringAuthorizationResponse
newDeleteVpcPeeringAuthorizationResponse pHttpStatus_ =
  DeleteVpcPeeringAuthorizationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteVpcPeeringAuthorizationResponse_httpStatus :: Lens.Lens' DeleteVpcPeeringAuthorizationResponse Prelude.Int
deleteVpcPeeringAuthorizationResponse_httpStatus = Lens.lens (\DeleteVpcPeeringAuthorizationResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcPeeringAuthorizationResponse' {} a -> s {httpStatus = a} :: DeleteVpcPeeringAuthorizationResponse)

instance
  Prelude.NFData
    DeleteVpcPeeringAuthorizationResponse
