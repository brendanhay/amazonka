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
-- Module      : Amazonka.OpenSearchServerless.CreateVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an OpenSearch Serverless-managed interface VPC endpoint. For
-- more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-vpc.html Access Amazon OpenSearch Serverless using an interface endpoint>.
module Amazonka.OpenSearchServerless.CreateVpcEndpoint
  ( -- * Creating a Request
    CreateVpcEndpoint (..),
    newCreateVpcEndpoint,

    -- * Request Lenses
    createVpcEndpoint_clientToken,
    createVpcEndpoint_securityGroupIds,
    createVpcEndpoint_name,
    createVpcEndpoint_subnetIds,
    createVpcEndpoint_vpcId,

    -- * Destructuring the Response
    CreateVpcEndpointResponse (..),
    newCreateVpcEndpointResponse,

    -- * Response Lenses
    createVpcEndpointResponse_createVpcEndpointDetail,
    createVpcEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpcEndpoint' smart constructor.
data CreateVpcEndpoint = CreateVpcEndpoint'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifiers of the security groups that define the ports,
    -- protocols, and sources for inbound traffic that you are authorizing into
    -- your endpoint.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the interface endpoint.
    name :: Prelude.Text,
    -- | The ID of one or more subnets from which you\'ll access OpenSearch
    -- Serverless.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the VPC from which you\'ll access OpenSearch Serverless.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVpcEndpoint_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'securityGroupIds', 'createVpcEndpoint_securityGroupIds' - The unique identifiers of the security groups that define the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
--
-- 'name', 'createVpcEndpoint_name' - The name of the interface endpoint.
--
-- 'subnetIds', 'createVpcEndpoint_subnetIds' - The ID of one or more subnets from which you\'ll access OpenSearch
-- Serverless.
--
-- 'vpcId', 'createVpcEndpoint_vpcId' - The ID of the VPC from which you\'ll access OpenSearch Serverless.
newCreateVpcEndpoint ::
  -- | 'name'
  Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  CreateVpcEndpoint
newCreateVpcEndpoint pName_ pSubnetIds_ pVpcId_ =
  CreateVpcEndpoint'
    { clientToken = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      name = pName_,
      subnetIds = Lens.coerced Lens.# pSubnetIds_,
      vpcId = pVpcId_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
createVpcEndpoint_clientToken :: Lens.Lens' CreateVpcEndpoint (Prelude.Maybe Prelude.Text)
createVpcEndpoint_clientToken = Lens.lens (\CreateVpcEndpoint' {clientToken} -> clientToken) (\s@CreateVpcEndpoint' {} a -> s {clientToken = a} :: CreateVpcEndpoint)

-- | The unique identifiers of the security groups that define the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
createVpcEndpoint_securityGroupIds :: Lens.Lens' CreateVpcEndpoint (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createVpcEndpoint_securityGroupIds = Lens.lens (\CreateVpcEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateVpcEndpoint' {} a -> s {securityGroupIds = a} :: CreateVpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The name of the interface endpoint.
createVpcEndpoint_name :: Lens.Lens' CreateVpcEndpoint Prelude.Text
createVpcEndpoint_name = Lens.lens (\CreateVpcEndpoint' {name} -> name) (\s@CreateVpcEndpoint' {} a -> s {name = a} :: CreateVpcEndpoint)

-- | The ID of one or more subnets from which you\'ll access OpenSearch
-- Serverless.
createVpcEndpoint_subnetIds :: Lens.Lens' CreateVpcEndpoint (Prelude.NonEmpty Prelude.Text)
createVpcEndpoint_subnetIds = Lens.lens (\CreateVpcEndpoint' {subnetIds} -> subnetIds) (\s@CreateVpcEndpoint' {} a -> s {subnetIds = a} :: CreateVpcEndpoint) Prelude.. Lens.coerced

-- | The ID of the VPC from which you\'ll access OpenSearch Serverless.
createVpcEndpoint_vpcId :: Lens.Lens' CreateVpcEndpoint Prelude.Text
createVpcEndpoint_vpcId = Lens.lens (\CreateVpcEndpoint' {vpcId} -> vpcId) (\s@CreateVpcEndpoint' {} a -> s {vpcId = a} :: CreateVpcEndpoint)

instance Core.AWSRequest CreateVpcEndpoint where
  type
    AWSResponse CreateVpcEndpoint =
      CreateVpcEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcEndpointResponse'
            Prelude.<$> (x Data..?> "createVpcEndpointDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpcEndpoint where
  hashWithSalt _salt CreateVpcEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData CreateVpcEndpoint where
  rnf CreateVpcEndpoint' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf securityGroupIds `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf subnetIds `Prelude.seq`
            Prelude.rnf vpcId

instance Data.ToHeaders CreateVpcEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.CreateVpcEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVpcEndpoint where
  toJSON CreateVpcEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("subnetIds" Data..= subnetIds),
            Prelude.Just ("vpcId" Data..= vpcId)
          ]
      )

instance Data.ToPath CreateVpcEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVpcEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcEndpointResponse' smart constructor.
data CreateVpcEndpointResponse = CreateVpcEndpointResponse'
  { -- | Details about the created interface VPC endpoint.
    createVpcEndpointDetail :: Prelude.Maybe CreateVpcEndpointDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createVpcEndpointDetail', 'createVpcEndpointResponse_createVpcEndpointDetail' - Details about the created interface VPC endpoint.
--
-- 'httpStatus', 'createVpcEndpointResponse_httpStatus' - The response's http status code.
newCreateVpcEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcEndpointResponse
newCreateVpcEndpointResponse pHttpStatus_ =
  CreateVpcEndpointResponse'
    { createVpcEndpointDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the created interface VPC endpoint.
createVpcEndpointResponse_createVpcEndpointDetail :: Lens.Lens' CreateVpcEndpointResponse (Prelude.Maybe CreateVpcEndpointDetail)
createVpcEndpointResponse_createVpcEndpointDetail = Lens.lens (\CreateVpcEndpointResponse' {createVpcEndpointDetail} -> createVpcEndpointDetail) (\s@CreateVpcEndpointResponse' {} a -> s {createVpcEndpointDetail = a} :: CreateVpcEndpointResponse)

-- | The response's http status code.
createVpcEndpointResponse_httpStatus :: Lens.Lens' CreateVpcEndpointResponse Prelude.Int
createVpcEndpointResponse_httpStatus = Lens.lens (\CreateVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateVpcEndpointResponse' {} a -> s {httpStatus = a} :: CreateVpcEndpointResponse)

instance Prelude.NFData CreateVpcEndpointResponse where
  rnf CreateVpcEndpointResponse' {..} =
    Prelude.rnf createVpcEndpointDetail `Prelude.seq`
      Prelude.rnf httpStatus
