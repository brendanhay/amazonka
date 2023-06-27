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
-- Module      : Amazonka.ElasticSearch.CreateVpcEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon OpenSearch Service-managed VPC endpoint.
module Amazonka.ElasticSearch.CreateVpcEndpoint
  ( -- * Creating a Request
    CreateVpcEndpoint (..),
    newCreateVpcEndpoint,

    -- * Request Lenses
    createVpcEndpoint_clientToken,
    createVpcEndpoint_domainArn,
    createVpcEndpoint_vpcOptions,

    -- * Destructuring the Response
    CreateVpcEndpointResponse (..),
    newCreateVpcEndpointResponse,

    -- * Response Lenses
    createVpcEndpointResponse_httpStatus,
    createVpcEndpointResponse_vpcEndpoint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @CreateVpcEndpointRequest@
-- operation.
--
-- /See:/ 'newCreateVpcEndpoint' smart constructor.
data CreateVpcEndpoint = CreateVpcEndpoint'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the domain to grant access to.
    domainArn :: Prelude.Text,
    -- | Options to specify the subnets and security groups for the endpoint.
    vpcOptions :: VPCOptions
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
-- 'domainArn', 'createVpcEndpoint_domainArn' - The Amazon Resource Name (ARN) of the domain to grant access to.
--
-- 'vpcOptions', 'createVpcEndpoint_vpcOptions' - Options to specify the subnets and security groups for the endpoint.
newCreateVpcEndpoint ::
  -- | 'domainArn'
  Prelude.Text ->
  -- | 'vpcOptions'
  VPCOptions ->
  CreateVpcEndpoint
newCreateVpcEndpoint pDomainArn_ pVpcOptions_ =
  CreateVpcEndpoint'
    { clientToken = Prelude.Nothing,
      domainArn = pDomainArn_,
      vpcOptions = pVpcOptions_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
createVpcEndpoint_clientToken :: Lens.Lens' CreateVpcEndpoint (Prelude.Maybe Prelude.Text)
createVpcEndpoint_clientToken = Lens.lens (\CreateVpcEndpoint' {clientToken} -> clientToken) (\s@CreateVpcEndpoint' {} a -> s {clientToken = a} :: CreateVpcEndpoint)

-- | The Amazon Resource Name (ARN) of the domain to grant access to.
createVpcEndpoint_domainArn :: Lens.Lens' CreateVpcEndpoint Prelude.Text
createVpcEndpoint_domainArn = Lens.lens (\CreateVpcEndpoint' {domainArn} -> domainArn) (\s@CreateVpcEndpoint' {} a -> s {domainArn = a} :: CreateVpcEndpoint)

-- | Options to specify the subnets and security groups for the endpoint.
createVpcEndpoint_vpcOptions :: Lens.Lens' CreateVpcEndpoint VPCOptions
createVpcEndpoint_vpcOptions = Lens.lens (\CreateVpcEndpoint' {vpcOptions} -> vpcOptions) (\s@CreateVpcEndpoint' {} a -> s {vpcOptions = a} :: CreateVpcEndpoint)

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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcEndpoint")
      )

instance Prelude.Hashable CreateVpcEndpoint where
  hashWithSalt _salt CreateVpcEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` domainArn
      `Prelude.hashWithSalt` vpcOptions

instance Prelude.NFData CreateVpcEndpoint where
  rnf CreateVpcEndpoint' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf domainArn
      `Prelude.seq` Prelude.rnf vpcOptions

instance Data.ToHeaders CreateVpcEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateVpcEndpoint where
  toJSON CreateVpcEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("DomainArn" Data..= domainArn),
            Prelude.Just ("VpcOptions" Data..= vpcOptions)
          ]
      )

instance Data.ToPath CreateVpcEndpoint where
  toPath = Prelude.const "/2015-01-01/es/vpcEndpoints"

instance Data.ToQuery CreateVpcEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response parameters to the @CreateVpcEndpoint@ operation.
-- Contains the configuration and status of the VPC Endpoint being created.
--
-- /See:/ 'newCreateVpcEndpointResponse' smart constructor.
data CreateVpcEndpointResponse = CreateVpcEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the newly created VPC endpoint.
    vpcEndpoint :: VpcEndpoint
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
-- 'httpStatus', 'createVpcEndpointResponse_httpStatus' - The response's http status code.
--
-- 'vpcEndpoint', 'createVpcEndpointResponse_vpcEndpoint' - Information about the newly created VPC endpoint.
newCreateVpcEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcEndpoint'
  VpcEndpoint ->
  CreateVpcEndpointResponse
newCreateVpcEndpointResponse
  pHttpStatus_
  pVpcEndpoint_ =
    CreateVpcEndpointResponse'
      { httpStatus =
          pHttpStatus_,
        vpcEndpoint = pVpcEndpoint_
      }

-- | The response's http status code.
createVpcEndpointResponse_httpStatus :: Lens.Lens' CreateVpcEndpointResponse Prelude.Int
createVpcEndpointResponse_httpStatus = Lens.lens (\CreateVpcEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateVpcEndpointResponse' {} a -> s {httpStatus = a} :: CreateVpcEndpointResponse)

-- | Information about the newly created VPC endpoint.
createVpcEndpointResponse_vpcEndpoint :: Lens.Lens' CreateVpcEndpointResponse VpcEndpoint
createVpcEndpointResponse_vpcEndpoint = Lens.lens (\CreateVpcEndpointResponse' {vpcEndpoint} -> vpcEndpoint) (\s@CreateVpcEndpointResponse' {} a -> s {vpcEndpoint = a} :: CreateVpcEndpointResponse)

instance Prelude.NFData CreateVpcEndpointResponse where
  rnf CreateVpcEndpointResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcEndpoint
