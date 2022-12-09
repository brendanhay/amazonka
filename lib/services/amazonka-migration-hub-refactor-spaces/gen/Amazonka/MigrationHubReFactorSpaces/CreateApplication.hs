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
-- Module      : Amazonka.MigrationHubReFactorSpaces.CreateApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Web Services Migration Hub Refactor Spaces
-- application. The account that owns the environment also owns the
-- applications created inside the environment, regardless of the account
-- that creates the application. Refactor Spaces provisions an Amazon API
-- Gateway, API Gateway VPC link, and Network Load Balancer for the
-- application proxy inside your account.
module Amazonka.MigrationHubReFactorSpaces.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_apiGatewayProxy,
    createApplication_clientToken,
    createApplication_tags,
    createApplication_environmentIdentifier,
    createApplication_name,
    createApplication_proxyType,
    createApplication_vpcId,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_apiGatewayProxy,
    createApplicationResponse_applicationId,
    createApplicationResponse_arn,
    createApplicationResponse_createdByAccountId,
    createApplicationResponse_createdTime,
    createApplicationResponse_environmentId,
    createApplicationResponse_lastUpdatedTime,
    createApplicationResponse_name,
    createApplicationResponse_ownerAccountId,
    createApplicationResponse_proxyType,
    createApplicationResponse_state,
    createApplicationResponse_tags,
    createApplicationResponse_vpcId,
    createApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | A wrapper object holding the API Gateway endpoint type and stage name
    -- for the proxy.
    apiGatewayProxy :: Prelude.Maybe ApiGatewayProxyInput,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the application. A tag is a label that you assign
    -- to an Amazon Web Services resource. Each tag consists of a key-value
    -- pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The unique identifier of the environment.
    environmentIdentifier :: Prelude.Text,
    -- | The name to use for the application.
    name :: Prelude.Text,
    -- | The proxy type of the proxy created within the application.
    proxyType :: ProxyType,
    -- | The ID of the virtual private cloud (VPC).
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayProxy', 'createApplication_apiGatewayProxy' - A wrapper object holding the API Gateway endpoint type and stage name
-- for the proxy.
--
-- 'clientToken', 'createApplication_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'tags', 'createApplication_tags' - The tags to assign to the application. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
--
-- 'environmentIdentifier', 'createApplication_environmentIdentifier' - The unique identifier of the environment.
--
-- 'name', 'createApplication_name' - The name to use for the application.
--
-- 'proxyType', 'createApplication_proxyType' - The proxy type of the proxy created within the application.
--
-- 'vpcId', 'createApplication_vpcId' - The ID of the virtual private cloud (VPC).
newCreateApplication ::
  -- | 'environmentIdentifier'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'proxyType'
  ProxyType ->
  -- | 'vpcId'
  Prelude.Text ->
  CreateApplication
newCreateApplication
  pEnvironmentIdentifier_
  pName_
  pProxyType_
  pVpcId_ =
    CreateApplication'
      { apiGatewayProxy =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        environmentIdentifier = pEnvironmentIdentifier_,
        name = pName_,
        proxyType = pProxyType_,
        vpcId = pVpcId_
      }

-- | A wrapper object holding the API Gateway endpoint type and stage name
-- for the proxy.
createApplication_apiGatewayProxy :: Lens.Lens' CreateApplication (Prelude.Maybe ApiGatewayProxyInput)
createApplication_apiGatewayProxy = Lens.lens (\CreateApplication' {apiGatewayProxy} -> apiGatewayProxy) (\s@CreateApplication' {} a -> s {apiGatewayProxy = a} :: CreateApplication)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createApplication_clientToken :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_clientToken = Lens.lens (\CreateApplication' {clientToken} -> clientToken) (\s@CreateApplication' {} a -> s {clientToken = a} :: CreateApplication)

-- | The tags to assign to the application. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The unique identifier of the environment.
createApplication_environmentIdentifier :: Lens.Lens' CreateApplication Prelude.Text
createApplication_environmentIdentifier = Lens.lens (\CreateApplication' {environmentIdentifier} -> environmentIdentifier) (\s@CreateApplication' {} a -> s {environmentIdentifier = a} :: CreateApplication)

-- | The name to use for the application.
createApplication_name :: Lens.Lens' CreateApplication Prelude.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

-- | The proxy type of the proxy created within the application.
createApplication_proxyType :: Lens.Lens' CreateApplication ProxyType
createApplication_proxyType = Lens.lens (\CreateApplication' {proxyType} -> proxyType) (\s@CreateApplication' {} a -> s {proxyType = a} :: CreateApplication)

-- | The ID of the virtual private cloud (VPC).
createApplication_vpcId :: Lens.Lens' CreateApplication Prelude.Text
createApplication_vpcId = Lens.lens (\CreateApplication' {vpcId} -> vpcId) (\s@CreateApplication' {} a -> s {vpcId = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (x Data..?> "ApiGatewayProxy")
            Prelude.<*> (x Data..?> "ApplicationId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedByAccountId")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "EnvironmentId")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OwnerAccountId")
            Prelude.<*> (x Data..?> "ProxyType")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt `Prelude.hashWithSalt` apiGatewayProxy
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` environmentIdentifier
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` proxyType
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf apiGatewayProxy
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf environmentIdentifier
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf proxyType
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApiGatewayProxy" Data..=)
              Prelude.<$> apiGatewayProxy,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ProxyType" Data..= proxyType),
            Prelude.Just ("VpcId" Data..= vpcId)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath CreateApplication' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/applications"
      ]

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | A wrapper object holding the API Gateway endpoint type and stage name
    -- for the proxy.
    apiGatewayProxy :: Prelude.Maybe ApiGatewayProxyInput,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is
    -- @arn:aws:refactor-spaces:region:account-id:resource-type\/resource-id @.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of application creator.
    createdByAccountId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the application is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the environment in which the application is created.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the application was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the application owner (which is
    -- always the same as the environment owner account ID).
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The proxy type of the proxy created within the application.
    proxyType :: Prelude.Maybe ProxyType,
    -- | The current state of the application.
    state :: Prelude.Maybe ApplicationState,
    -- | The tags assigned to the application. A tag is a label that you assign
    -- to an Amazon Web Services resource. Each tag consists of a key-value
    -- pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The ID of the Amazon VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayProxy', 'createApplicationResponse_apiGatewayProxy' - A wrapper object holding the API Gateway endpoint type and stage name
-- for the proxy.
--
-- 'applicationId', 'createApplicationResponse_applicationId' - The unique identifier of the application.
--
-- 'arn', 'createApplicationResponse_arn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is
-- @arn:aws:refactor-spaces:region:account-id:resource-type\/resource-id @.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'createdByAccountId', 'createApplicationResponse_createdByAccountId' - The Amazon Web Services account ID of application creator.
--
-- 'createdTime', 'createApplicationResponse_createdTime' - A timestamp that indicates when the application is created.
--
-- 'environmentId', 'createApplicationResponse_environmentId' - The ID of the environment in which the application is created.
--
-- 'lastUpdatedTime', 'createApplicationResponse_lastUpdatedTime' - A timestamp that indicates when the application was last updated.
--
-- 'name', 'createApplicationResponse_name' - The name of the application.
--
-- 'ownerAccountId', 'createApplicationResponse_ownerAccountId' - The Amazon Web Services account ID of the application owner (which is
-- always the same as the environment owner account ID).
--
-- 'proxyType', 'createApplicationResponse_proxyType' - The proxy type of the proxy created within the application.
--
-- 'state', 'createApplicationResponse_state' - The current state of the application.
--
-- 'tags', 'createApplicationResponse_tags' - The tags assigned to the application. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
--
-- 'vpcId', 'createApplicationResponse_vpcId' - The ID of the Amazon VPC.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { apiGatewayProxy =
        Prelude.Nothing,
      applicationId = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdByAccountId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      proxyType = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A wrapper object holding the API Gateway endpoint type and stage name
-- for the proxy.
createApplicationResponse_apiGatewayProxy :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe ApiGatewayProxyInput)
createApplicationResponse_apiGatewayProxy = Lens.lens (\CreateApplicationResponse' {apiGatewayProxy} -> apiGatewayProxy) (\s@CreateApplicationResponse' {} a -> s {apiGatewayProxy = a} :: CreateApplicationResponse)

-- | The unique identifier of the application.
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is
-- @arn:aws:refactor-spaces:region:account-id:resource-type\/resource-id @.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
createApplicationResponse_arn :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_arn = Lens.lens (\CreateApplicationResponse' {arn} -> arn) (\s@CreateApplicationResponse' {} a -> s {arn = a} :: CreateApplicationResponse)

-- | The Amazon Web Services account ID of application creator.
createApplicationResponse_createdByAccountId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_createdByAccountId = Lens.lens (\CreateApplicationResponse' {createdByAccountId} -> createdByAccountId) (\s@CreateApplicationResponse' {} a -> s {createdByAccountId = a} :: CreateApplicationResponse)

-- | A timestamp that indicates when the application is created.
createApplicationResponse_createdTime :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.UTCTime)
createApplicationResponse_createdTime = Lens.lens (\CreateApplicationResponse' {createdTime} -> createdTime) (\s@CreateApplicationResponse' {} a -> s {createdTime = a} :: CreateApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the environment in which the application is created.
createApplicationResponse_environmentId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_environmentId = Lens.lens (\CreateApplicationResponse' {environmentId} -> environmentId) (\s@CreateApplicationResponse' {} a -> s {environmentId = a} :: CreateApplicationResponse)

-- | A timestamp that indicates when the application was last updated.
createApplicationResponse_lastUpdatedTime :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.UTCTime)
createApplicationResponse_lastUpdatedTime = Lens.lens (\CreateApplicationResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@CreateApplicationResponse' {} a -> s {lastUpdatedTime = a} :: CreateApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the application.
createApplicationResponse_name :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_name = Lens.lens (\CreateApplicationResponse' {name} -> name) (\s@CreateApplicationResponse' {} a -> s {name = a} :: CreateApplicationResponse)

-- | The Amazon Web Services account ID of the application owner (which is
-- always the same as the environment owner account ID).
createApplicationResponse_ownerAccountId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_ownerAccountId = Lens.lens (\CreateApplicationResponse' {ownerAccountId} -> ownerAccountId) (\s@CreateApplicationResponse' {} a -> s {ownerAccountId = a} :: CreateApplicationResponse)

-- | The proxy type of the proxy created within the application.
createApplicationResponse_proxyType :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe ProxyType)
createApplicationResponse_proxyType = Lens.lens (\CreateApplicationResponse' {proxyType} -> proxyType) (\s@CreateApplicationResponse' {} a -> s {proxyType = a} :: CreateApplicationResponse)

-- | The current state of the application.
createApplicationResponse_state :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe ApplicationState)
createApplicationResponse_state = Lens.lens (\CreateApplicationResponse' {state} -> state) (\s@CreateApplicationResponse' {} a -> s {state = a} :: CreateApplicationResponse)

-- | The tags assigned to the application. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
createApplicationResponse_tags :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplicationResponse_tags = Lens.lens (\CreateApplicationResponse' {tags} -> tags) (\s@CreateApplicationResponse' {} a -> s {tags = a} :: CreateApplicationResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ID of the Amazon VPC.
createApplicationResponse_vpcId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_vpcId = Lens.lens (\CreateApplicationResponse' {vpcId} -> vpcId) (\s@CreateApplicationResponse' {} a -> s {vpcId = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf apiGatewayProxy
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdByAccountId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf proxyType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
