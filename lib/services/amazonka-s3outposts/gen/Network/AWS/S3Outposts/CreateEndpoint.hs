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
-- Module      : Network.AWS.S3Outposts.CreateEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon S3 on Outposts Access Points simplify managing data access at
-- scale for shared datasets in S3 on Outposts. S3 on Outposts uses
-- endpoints to connect to Outposts buckets so that you can perform actions
-- within your virtual private cloud (VPC). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/AccessingS3Outposts.html Accessing S3 on Outposts using VPC only access points>.
--
-- This action creates an endpoint and associates it with the specified
-- Outposts.
--
-- It can take up to 5 minutes for this action to complete.
--
-- Related actions include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_DeleteEndpoint.html DeleteEndpoint>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_ListEndpoints.html ListEndpoints>
module Network.AWS.S3Outposts.CreateEndpoint
  ( -- * Creating a Request
    CreateEndpoint (..),
    newCreateEndpoint,

    -- * Request Lenses
    createEndpoint_accessType,
    createEndpoint_customerOwnedIpv4Pool,
    createEndpoint_outpostId,
    createEndpoint_subnetId,
    createEndpoint_securityGroupId,

    -- * Destructuring the Response
    CreateEndpointResponse (..),
    newCreateEndpointResponse,

    -- * Response Lenses
    createEndpointResponse_endpointArn,
    createEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3Outposts.Types

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | The type of access for the on-premise network connectivity for the
    -- Outpost endpoint. To access the endpoint from an on-premises network,
    -- you must specify the access type and provide the customer owned IPv4
    -- pool.
    accessType :: Prelude.Maybe EndpointAccessType,
    -- | The ID of the customer-owned IPv4 pool for the endpoint. IP addresses
    -- will be allocated from this pool for the endpoint.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS Outposts.
    outpostId :: Prelude.Text,
    -- | The ID of the subnet in the selected VPC. The endpoint subnet must
    -- belong to the Outpost that has the Amazon S3 on Outposts provisioned.
    subnetId :: Prelude.Text,
    -- | The ID of the security group to use with the endpoint.
    securityGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessType', 'createEndpoint_accessType' - The type of access for the on-premise network connectivity for the
-- Outpost endpoint. To access the endpoint from an on-premises network,
-- you must specify the access type and provide the customer owned IPv4
-- pool.
--
-- 'customerOwnedIpv4Pool', 'createEndpoint_customerOwnedIpv4Pool' - The ID of the customer-owned IPv4 pool for the endpoint. IP addresses
-- will be allocated from this pool for the endpoint.
--
-- 'outpostId', 'createEndpoint_outpostId' - The ID of the AWS Outposts.
--
-- 'subnetId', 'createEndpoint_subnetId' - The ID of the subnet in the selected VPC. The endpoint subnet must
-- belong to the Outpost that has the Amazon S3 on Outposts provisioned.
--
-- 'securityGroupId', 'createEndpoint_securityGroupId' - The ID of the security group to use with the endpoint.
newCreateEndpoint ::
  -- | 'outpostId'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  -- | 'securityGroupId'
  Prelude.Text ->
  CreateEndpoint
newCreateEndpoint
  pOutpostId_
  pSubnetId_
  pSecurityGroupId_ =
    CreateEndpoint'
      { accessType = Prelude.Nothing,
        customerOwnedIpv4Pool = Prelude.Nothing,
        outpostId = pOutpostId_,
        subnetId = pSubnetId_,
        securityGroupId = pSecurityGroupId_
      }

-- | The type of access for the on-premise network connectivity for the
-- Outpost endpoint. To access the endpoint from an on-premises network,
-- you must specify the access type and provide the customer owned IPv4
-- pool.
createEndpoint_accessType :: Lens.Lens' CreateEndpoint (Prelude.Maybe EndpointAccessType)
createEndpoint_accessType = Lens.lens (\CreateEndpoint' {accessType} -> accessType) (\s@CreateEndpoint' {} a -> s {accessType = a} :: CreateEndpoint)

-- | The ID of the customer-owned IPv4 pool for the endpoint. IP addresses
-- will be allocated from this pool for the endpoint.
createEndpoint_customerOwnedIpv4Pool :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_customerOwnedIpv4Pool = Lens.lens (\CreateEndpoint' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@CreateEndpoint' {} a -> s {customerOwnedIpv4Pool = a} :: CreateEndpoint)

-- | The ID of the AWS Outposts.
createEndpoint_outpostId :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_outpostId = Lens.lens (\CreateEndpoint' {outpostId} -> outpostId) (\s@CreateEndpoint' {} a -> s {outpostId = a} :: CreateEndpoint)

-- | The ID of the subnet in the selected VPC. The endpoint subnet must
-- belong to the Outpost that has the Amazon S3 on Outposts provisioned.
createEndpoint_subnetId :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_subnetId = Lens.lens (\CreateEndpoint' {subnetId} -> subnetId) (\s@CreateEndpoint' {} a -> s {subnetId = a} :: CreateEndpoint)

-- | The ID of the security group to use with the endpoint.
createEndpoint_securityGroupId :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_securityGroupId = Lens.lens (\CreateEndpoint' {securityGroupId} -> securityGroupId) (\s@CreateEndpoint' {} a -> s {securityGroupId = a} :: CreateEndpoint)

instance Core.AWSRequest CreateEndpoint where
  type
    AWSResponse CreateEndpoint =
      CreateEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Prelude.<$> (x Core..?> "EndpointArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpoint

instance Prelude.NFData CreateEndpoint

instance Core.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccessType" Core..=) Prelude.<$> accessType,
            ("CustomerOwnedIpv4Pool" Core..=)
              Prelude.<$> customerOwnedIpv4Pool,
            Prelude.Just ("OutpostId" Core..= outpostId),
            Prelude.Just ("SubnetId" Core..= subnetId),
            Prelude.Just
              ("SecurityGroupId" Core..= securityGroupId)
          ]
      )

instance Core.ToPath CreateEndpoint where
  toPath = Prelude.const "/S3Outposts/CreateEndpoint"

instance Core.ToQuery CreateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'createEndpointResponse_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
--
-- 'httpStatus', 'createEndpointResponse_httpStatus' - The response's http status code.
newCreateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEndpointResponse
newCreateEndpointResponse pHttpStatus_ =
  CreateEndpointResponse'
    { endpointArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the endpoint.
createEndpointResponse_endpointArn :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe Prelude.Text)
createEndpointResponse_endpointArn = Lens.lens (\CreateEndpointResponse' {endpointArn} -> endpointArn) (\s@CreateEndpointResponse' {} a -> s {endpointArn = a} :: CreateEndpointResponse)

-- | The response's http status code.
createEndpointResponse_httpStatus :: Lens.Lens' CreateEndpointResponse Prelude.Int
createEndpointResponse_httpStatus = Lens.lens (\CreateEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointResponse' {} a -> s {httpStatus = a} :: CreateEndpointResponse)

instance Prelude.NFData CreateEndpointResponse
