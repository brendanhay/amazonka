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
-- Module      : Amazonka.RedshiftServerLess.CreateEndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Redshift Serverless managed VPC endpoint.
module Amazonka.RedshiftServerLess.CreateEndpointAccess
  ( -- * Creating a Request
    CreateEndpointAccess (..),
    newCreateEndpointAccess,

    -- * Request Lenses
    createEndpointAccess_vpcSecurityGroupIds,
    createEndpointAccess_endpointName,
    createEndpointAccess_subnetIds,
    createEndpointAccess_workgroupName,

    -- * Destructuring the Response
    CreateEndpointAccessResponse (..),
    newCreateEndpointAccessResponse,

    -- * Response Lenses
    createEndpointAccessResponse_endpoint,
    createEndpointAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEndpointAccess' smart constructor.
data CreateEndpointAccess = CreateEndpointAccess'
  { -- | The unique identifiers of the security group that defines the ports,
    -- protocols, and sources for inbound traffic that you are authorizing into
    -- your endpoint.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the VPC endpoint. An endpoint name must contain 1-30
    -- characters. Valid characters are A-Z, a-z, 0-9, and hyphen(-). The first
    -- character must be a letter. The name can\'t contain two consecutive
    -- hyphens or end with a hyphen.
    endpointName :: Prelude.Text,
    -- | The unique identifers of subnets from which Amazon Redshift Serverless
    -- chooses one to deploy a VPC endpoint.
    subnetIds :: [Prelude.Text],
    -- | The name of the workgroup to associate with the VPC endpoint.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcSecurityGroupIds', 'createEndpointAccess_vpcSecurityGroupIds' - The unique identifiers of the security group that defines the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
--
-- 'endpointName', 'createEndpointAccess_endpointName' - The name of the VPC endpoint. An endpoint name must contain 1-30
-- characters. Valid characters are A-Z, a-z, 0-9, and hyphen(-). The first
-- character must be a letter. The name can\'t contain two consecutive
-- hyphens or end with a hyphen.
--
-- 'subnetIds', 'createEndpointAccess_subnetIds' - The unique identifers of subnets from which Amazon Redshift Serverless
-- chooses one to deploy a VPC endpoint.
--
-- 'workgroupName', 'createEndpointAccess_workgroupName' - The name of the workgroup to associate with the VPC endpoint.
newCreateEndpointAccess ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'workgroupName'
  Prelude.Text ->
  CreateEndpointAccess
newCreateEndpointAccess
  pEndpointName_
  pWorkgroupName_ =
    CreateEndpointAccess'
      { vpcSecurityGroupIds =
          Prelude.Nothing,
        endpointName = pEndpointName_,
        subnetIds = Prelude.mempty,
        workgroupName = pWorkgroupName_
      }

-- | The unique identifiers of the security group that defines the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
createEndpointAccess_vpcSecurityGroupIds :: Lens.Lens' CreateEndpointAccess (Prelude.Maybe [Prelude.Text])
createEndpointAccess_vpcSecurityGroupIds = Lens.lens (\CreateEndpointAccess' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateEndpointAccess' {} a -> s {vpcSecurityGroupIds = a} :: CreateEndpointAccess) Prelude.. Lens.mapping Lens.coerced

-- | The name of the VPC endpoint. An endpoint name must contain 1-30
-- characters. Valid characters are A-Z, a-z, 0-9, and hyphen(-). The first
-- character must be a letter. The name can\'t contain two consecutive
-- hyphens or end with a hyphen.
createEndpointAccess_endpointName :: Lens.Lens' CreateEndpointAccess Prelude.Text
createEndpointAccess_endpointName = Lens.lens (\CreateEndpointAccess' {endpointName} -> endpointName) (\s@CreateEndpointAccess' {} a -> s {endpointName = a} :: CreateEndpointAccess)

-- | The unique identifers of subnets from which Amazon Redshift Serverless
-- chooses one to deploy a VPC endpoint.
createEndpointAccess_subnetIds :: Lens.Lens' CreateEndpointAccess [Prelude.Text]
createEndpointAccess_subnetIds = Lens.lens (\CreateEndpointAccess' {subnetIds} -> subnetIds) (\s@CreateEndpointAccess' {} a -> s {subnetIds = a} :: CreateEndpointAccess) Prelude.. Lens.coerced

-- | The name of the workgroup to associate with the VPC endpoint.
createEndpointAccess_workgroupName :: Lens.Lens' CreateEndpointAccess Prelude.Text
createEndpointAccess_workgroupName = Lens.lens (\CreateEndpointAccess' {workgroupName} -> workgroupName) (\s@CreateEndpointAccess' {} a -> s {workgroupName = a} :: CreateEndpointAccess)

instance Core.AWSRequest CreateEndpointAccess where
  type
    AWSResponse CreateEndpointAccess =
      CreateEndpointAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointAccessResponse'
            Prelude.<$> (x Data..?> "endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpointAccess where
  hashWithSalt _salt CreateEndpointAccess' {..} =
    _salt
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData CreateEndpointAccess where
  rnf CreateEndpointAccess' {..} =
    Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf workgroupName

instance Data.ToHeaders CreateEndpointAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.CreateEndpointAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEndpointAccess where
  toJSON CreateEndpointAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("vpcSecurityGroupIds" Data..=)
              Prelude.<$> vpcSecurityGroupIds,
            Prelude.Just ("endpointName" Data..= endpointName),
            Prelude.Just ("subnetIds" Data..= subnetIds),
            Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath CreateEndpointAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEndpointAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointAccessResponse' smart constructor.
data CreateEndpointAccessResponse = CreateEndpointAccessResponse'
  { -- | The created VPC endpoint.
    endpoint :: Prelude.Maybe EndpointAccess,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'createEndpointAccessResponse_endpoint' - The created VPC endpoint.
--
-- 'httpStatus', 'createEndpointAccessResponse_httpStatus' - The response's http status code.
newCreateEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEndpointAccessResponse
newCreateEndpointAccessResponse pHttpStatus_ =
  CreateEndpointAccessResponse'
    { endpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The created VPC endpoint.
createEndpointAccessResponse_endpoint :: Lens.Lens' CreateEndpointAccessResponse (Prelude.Maybe EndpointAccess)
createEndpointAccessResponse_endpoint = Lens.lens (\CreateEndpointAccessResponse' {endpoint} -> endpoint) (\s@CreateEndpointAccessResponse' {} a -> s {endpoint = a} :: CreateEndpointAccessResponse)

-- | The response's http status code.
createEndpointAccessResponse_httpStatus :: Lens.Lens' CreateEndpointAccessResponse Prelude.Int
createEndpointAccessResponse_httpStatus = Lens.lens (\CreateEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointAccessResponse' {} a -> s {httpStatus = a} :: CreateEndpointAccessResponse)

instance Prelude.NFData CreateEndpointAccessResponse where
  rnf CreateEndpointAccessResponse' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf httpStatus
