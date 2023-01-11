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
-- Module      : Amazonka.S3Outposts.CreateEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint and associates it with the specified Outpost.
--
-- It can take up to 5 minutes for this action to finish.
--
-- Related actions include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_DeleteEndpoint.html DeleteEndpoint>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_ListEndpoints.html ListEndpoints>
module Amazonka.S3Outposts.CreateEndpoint
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3Outposts.Types

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | The type of access for the network connectivity for the Amazon S3 on
    -- Outposts endpoint. To use the Amazon Web Services VPC, choose @Private@.
    -- To use the endpoint with an on-premises network, choose
    -- @CustomerOwnedIp@. If you choose @CustomerOwnedIp@, you must also
    -- provide the customer-owned IP address pool (CoIP pool).
    --
    -- @Private@ is the default access type value.
    accessType :: Prelude.Maybe EndpointAccessType,
    -- | The ID of the customer-owned IPv4 address pool (CoIP pool) for the
    -- endpoint. IP addresses are allocated from this pool for the endpoint.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Outposts.
    outpostId :: Prelude.Text,
    -- | The ID of the subnet in the selected VPC. The endpoint subnet must
    -- belong to the Outpost that has Amazon S3 on Outposts provisioned.
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
-- 'accessType', 'createEndpoint_accessType' - The type of access for the network connectivity for the Amazon S3 on
-- Outposts endpoint. To use the Amazon Web Services VPC, choose @Private@.
-- To use the endpoint with an on-premises network, choose
-- @CustomerOwnedIp@. If you choose @CustomerOwnedIp@, you must also
-- provide the customer-owned IP address pool (CoIP pool).
--
-- @Private@ is the default access type value.
--
-- 'customerOwnedIpv4Pool', 'createEndpoint_customerOwnedIpv4Pool' - The ID of the customer-owned IPv4 address pool (CoIP pool) for the
-- endpoint. IP addresses are allocated from this pool for the endpoint.
--
-- 'outpostId', 'createEndpoint_outpostId' - The ID of the Outposts.
--
-- 'subnetId', 'createEndpoint_subnetId' - The ID of the subnet in the selected VPC. The endpoint subnet must
-- belong to the Outpost that has Amazon S3 on Outposts provisioned.
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

-- | The type of access for the network connectivity for the Amazon S3 on
-- Outposts endpoint. To use the Amazon Web Services VPC, choose @Private@.
-- To use the endpoint with an on-premises network, choose
-- @CustomerOwnedIp@. If you choose @CustomerOwnedIp@, you must also
-- provide the customer-owned IP address pool (CoIP pool).
--
-- @Private@ is the default access type value.
createEndpoint_accessType :: Lens.Lens' CreateEndpoint (Prelude.Maybe EndpointAccessType)
createEndpoint_accessType = Lens.lens (\CreateEndpoint' {accessType} -> accessType) (\s@CreateEndpoint' {} a -> s {accessType = a} :: CreateEndpoint)

-- | The ID of the customer-owned IPv4 address pool (CoIP pool) for the
-- endpoint. IP addresses are allocated from this pool for the endpoint.
createEndpoint_customerOwnedIpv4Pool :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_customerOwnedIpv4Pool = Lens.lens (\CreateEndpoint' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@CreateEndpoint' {} a -> s {customerOwnedIpv4Pool = a} :: CreateEndpoint)

-- | The ID of the Outposts.
createEndpoint_outpostId :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_outpostId = Lens.lens (\CreateEndpoint' {outpostId} -> outpostId) (\s@CreateEndpoint' {} a -> s {outpostId = a} :: CreateEndpoint)

-- | The ID of the subnet in the selected VPC. The endpoint subnet must
-- belong to the Outpost that has Amazon S3 on Outposts provisioned.
createEndpoint_subnetId :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_subnetId = Lens.lens (\CreateEndpoint' {subnetId} -> subnetId) (\s@CreateEndpoint' {} a -> s {subnetId = a} :: CreateEndpoint)

-- | The ID of the security group to use with the endpoint.
createEndpoint_securityGroupId :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_securityGroupId = Lens.lens (\CreateEndpoint' {securityGroupId} -> securityGroupId) (\s@CreateEndpoint' {} a -> s {securityGroupId = a} :: CreateEndpoint)

instance Core.AWSRequest CreateEndpoint where
  type
    AWSResponse CreateEndpoint =
      CreateEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Prelude.<$> (x Data..?> "EndpointArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpoint where
  hashWithSalt _salt CreateEndpoint' {..} =
    _salt `Prelude.hashWithSalt` accessType
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` outpostId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` securityGroupId

instance Prelude.NFData CreateEndpoint where
  rnf CreateEndpoint' {..} =
    Prelude.rnf accessType
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf securityGroupId

instance Data.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessType" Data..=) Prelude.<$> accessType,
            ("CustomerOwnedIpv4Pool" Data..=)
              Prelude.<$> customerOwnedIpv4Pool,
            Prelude.Just ("OutpostId" Data..= outpostId),
            Prelude.Just ("SubnetId" Data..= subnetId),
            Prelude.Just
              ("SecurityGroupId" Data..= securityGroupId)
          ]
      )

instance Data.ToPath CreateEndpoint where
  toPath = Prelude.const "/S3Outposts/CreateEndpoint"

instance Data.ToQuery CreateEndpoint where
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

instance Prelude.NFData CreateEndpointResponse where
  rnf CreateEndpointResponse' {..} =
    Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf httpStatus
