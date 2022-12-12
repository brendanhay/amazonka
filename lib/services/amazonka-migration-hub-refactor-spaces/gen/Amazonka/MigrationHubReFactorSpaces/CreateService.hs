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
-- Module      : Amazonka.MigrationHubReFactorSpaces.CreateService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Web Services Migration Hub Refactor Spaces service.
-- The account owner of the service is always the environment owner,
-- regardless of which account in the environment creates the service.
-- Services have either a URL endpoint in a virtual private cloud (VPC), or
-- a Lambda function endpoint.
--
-- If an Amazon Web Services resource is launched in a service VPC, and you
-- want it to be accessible to all of an environment’s services with VPCs
-- and routes, apply the @RefactorSpacesSecurityGroup@ to the resource.
-- Alternatively, to add more cross-account constraints, apply your own
-- security group.
module Amazonka.MigrationHubReFactorSpaces.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_clientToken,
    createService_description,
    createService_lambdaEndpoint,
    createService_tags,
    createService_urlEndpoint,
    createService_vpcId,
    createService_applicationIdentifier,
    createService_endpointType,
    createService_environmentIdentifier,
    createService_name,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_applicationId,
    createServiceResponse_arn,
    createServiceResponse_createdByAccountId,
    createServiceResponse_createdTime,
    createServiceResponse_description,
    createServiceResponse_endpointType,
    createServiceResponse_environmentId,
    createServiceResponse_lambdaEndpoint,
    createServiceResponse_lastUpdatedTime,
    createServiceResponse_name,
    createServiceResponse_ownerAccountId,
    createServiceResponse_serviceId,
    createServiceResponse_state,
    createServiceResponse_tags,
    createServiceResponse_urlEndpoint,
    createServiceResponse_vpcId,
    createServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the service.
    description :: Prelude.Maybe Prelude.Text,
    -- | The configuration for the Lambda endpoint type.
    lambdaEndpoint :: Prelude.Maybe LambdaEndpointInput,
    -- | The tags to assign to the service. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key-value pair..
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The configuration for the URL endpoint type.
    urlEndpoint :: Prelude.Maybe UrlEndpointInput,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application which the service is created.
    applicationIdentifier :: Prelude.Text,
    -- | The type of endpoint to use for the service. The type can be a URL in a
    -- VPC or an Lambda function.
    endpointType :: ServiceEndpointType,
    -- | The ID of the environment in which the service is created.
    environmentIdentifier :: Prelude.Text,
    -- | The name of the service.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createService_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createService_description' - The description of the service.
--
-- 'lambdaEndpoint', 'createService_lambdaEndpoint' - The configuration for the Lambda endpoint type.
--
-- 'tags', 'createService_tags' - The tags to assign to the service. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key-value pair..
--
-- 'urlEndpoint', 'createService_urlEndpoint' - The configuration for the URL endpoint type.
--
-- 'vpcId', 'createService_vpcId' - The ID of the VPC.
--
-- 'applicationIdentifier', 'createService_applicationIdentifier' - The ID of the application which the service is created.
--
-- 'endpointType', 'createService_endpointType' - The type of endpoint to use for the service. The type can be a URL in a
-- VPC or an Lambda function.
--
-- 'environmentIdentifier', 'createService_environmentIdentifier' - The ID of the environment in which the service is created.
--
-- 'name', 'createService_name' - The name of the service.
newCreateService ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'endpointType'
  ServiceEndpointType ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateService
newCreateService
  pApplicationIdentifier_
  pEndpointType_
  pEnvironmentIdentifier_
  pName_ =
    CreateService'
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        lambdaEndpoint = Prelude.Nothing,
        tags = Prelude.Nothing,
        urlEndpoint = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        applicationIdentifier = pApplicationIdentifier_,
        endpointType = pEndpointType_,
        environmentIdentifier = pEnvironmentIdentifier_,
        name = pName_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createService_clientToken :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_clientToken = Lens.lens (\CreateService' {clientToken} -> clientToken) (\s@CreateService' {} a -> s {clientToken = a} :: CreateService)

-- | The description of the service.
createService_description :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_description = Lens.lens (\CreateService' {description} -> description) (\s@CreateService' {} a -> s {description = a} :: CreateService)

-- | The configuration for the Lambda endpoint type.
createService_lambdaEndpoint :: Lens.Lens' CreateService (Prelude.Maybe LambdaEndpointInput)
createService_lambdaEndpoint = Lens.lens (\CreateService' {lambdaEndpoint} -> lambdaEndpoint) (\s@CreateService' {} a -> s {lambdaEndpoint = a} :: CreateService)

-- | The tags to assign to the service. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key-value pair..
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The configuration for the URL endpoint type.
createService_urlEndpoint :: Lens.Lens' CreateService (Prelude.Maybe UrlEndpointInput)
createService_urlEndpoint = Lens.lens (\CreateService' {urlEndpoint} -> urlEndpoint) (\s@CreateService' {} a -> s {urlEndpoint = a} :: CreateService)

-- | The ID of the VPC.
createService_vpcId :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_vpcId = Lens.lens (\CreateService' {vpcId} -> vpcId) (\s@CreateService' {} a -> s {vpcId = a} :: CreateService)

-- | The ID of the application which the service is created.
createService_applicationIdentifier :: Lens.Lens' CreateService Prelude.Text
createService_applicationIdentifier = Lens.lens (\CreateService' {applicationIdentifier} -> applicationIdentifier) (\s@CreateService' {} a -> s {applicationIdentifier = a} :: CreateService)

-- | The type of endpoint to use for the service. The type can be a URL in a
-- VPC or an Lambda function.
createService_endpointType :: Lens.Lens' CreateService ServiceEndpointType
createService_endpointType = Lens.lens (\CreateService' {endpointType} -> endpointType) (\s@CreateService' {} a -> s {endpointType = a} :: CreateService)

-- | The ID of the environment in which the service is created.
createService_environmentIdentifier :: Lens.Lens' CreateService Prelude.Text
createService_environmentIdentifier = Lens.lens (\CreateService' {environmentIdentifier} -> environmentIdentifier) (\s@CreateService' {} a -> s {environmentIdentifier = a} :: CreateService)

-- | The name of the service.
createService_name :: Lens.Lens' CreateService Prelude.Text
createService_name = Lens.lens (\CreateService' {name} -> name) (\s@CreateService' {} a -> s {name = a} :: CreateService)

instance Core.AWSRequest CreateService where
  type
    AWSResponse CreateService =
      CreateServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (x Data..?> "ApplicationId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedByAccountId")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "EndpointType")
            Prelude.<*> (x Data..?> "EnvironmentId")
            Prelude.<*> (x Data..?> "LambdaEndpoint")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OwnerAccountId")
            Prelude.<*> (x Data..?> "ServiceId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "UrlEndpoint")
            Prelude.<*> (x Data..?> "VpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateService where
  hashWithSalt _salt CreateService' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lambdaEndpoint
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` urlEndpoint
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` environmentIdentifier
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateService where
  rnf CreateService' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lambdaEndpoint
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf urlEndpoint
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf applicationIdentifier
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf environmentIdentifier
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateService where
  toJSON CreateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("LambdaEndpoint" Data..=)
              Prelude.<$> lambdaEndpoint,
            ("Tags" Data..=) Prelude.<$> tags,
            ("UrlEndpoint" Data..=) Prelude.<$> urlEndpoint,
            ("VpcId" Data..=) Prelude.<$> vpcId,
            Prelude.Just ("EndpointType" Data..= endpointType),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateService where
  toPath CreateService' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/applications/",
        Data.toBS applicationIdentifier,
        "/services"
      ]

instance Data.ToQuery CreateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { -- | The ID of the application that the created service belongs to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the service creator.
    createdByAccountId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the service is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the created service.
    description :: Prelude.Maybe Prelude.Text,
    -- | The endpoint type of the service.
    endpointType :: Prelude.Maybe ServiceEndpointType,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The configuration for the Lambda endpoint type.
    lambdaEndpoint :: Prelude.Maybe LambdaEndpointInput,
    -- | A timestamp that indicates when the service was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the service owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the service.
    state :: Prelude.Maybe ServiceState,
    -- | The tags assigned to the created service. A tag is a label that you
    -- assign to an Amazon Web Services resource. Each tag consists of a
    -- key-value pair..
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The configuration for the URL endpoint type.
    urlEndpoint :: Prelude.Maybe UrlEndpointInput,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createServiceResponse_applicationId' - The ID of the application that the created service belongs to.
--
-- 'arn', 'createServiceResponse_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'createdByAccountId', 'createServiceResponse_createdByAccountId' - The Amazon Web Services account ID of the service creator.
--
-- 'createdTime', 'createServiceResponse_createdTime' - A timestamp that indicates when the service is created.
--
-- 'description', 'createServiceResponse_description' - The description of the created service.
--
-- 'endpointType', 'createServiceResponse_endpointType' - The endpoint type of the service.
--
-- 'environmentId', 'createServiceResponse_environmentId' - The unique identifier of the environment.
--
-- 'lambdaEndpoint', 'createServiceResponse_lambdaEndpoint' - The configuration for the Lambda endpoint type.
--
-- 'lastUpdatedTime', 'createServiceResponse_lastUpdatedTime' - A timestamp that indicates when the service was last updated.
--
-- 'name', 'createServiceResponse_name' - The name of the service.
--
-- 'ownerAccountId', 'createServiceResponse_ownerAccountId' - The Amazon Web Services account ID of the service owner.
--
-- 'serviceId', 'createServiceResponse_serviceId' - The unique identifier of the service.
--
-- 'state', 'createServiceResponse_state' - The current state of the service.
--
-- 'tags', 'createServiceResponse_tags' - The tags assigned to the created service. A tag is a label that you
-- assign to an Amazon Web Services resource. Each tag consists of a
-- key-value pair..
--
-- 'urlEndpoint', 'createServiceResponse_urlEndpoint' - The configuration for the URL endpoint type.
--
-- 'vpcId', 'createServiceResponse_vpcId' - The ID of the VPC.
--
-- 'httpStatus', 'createServiceResponse_httpStatus' - The response's http status code.
newCreateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceResponse
newCreateServiceResponse pHttpStatus_ =
  CreateServiceResponse'
    { applicationId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      createdByAccountId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      lambdaEndpoint = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      urlEndpoint = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the application that the created service belongs to.
createServiceResponse_applicationId :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_applicationId = Lens.lens (\CreateServiceResponse' {applicationId} -> applicationId) (\s@CreateServiceResponse' {} a -> s {applicationId = a} :: CreateServiceResponse)

-- | The Amazon Resource Name (ARN) of the service.
createServiceResponse_arn :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_arn = Lens.lens (\CreateServiceResponse' {arn} -> arn) (\s@CreateServiceResponse' {} a -> s {arn = a} :: CreateServiceResponse)

-- | The Amazon Web Services account ID of the service creator.
createServiceResponse_createdByAccountId :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_createdByAccountId = Lens.lens (\CreateServiceResponse' {createdByAccountId} -> createdByAccountId) (\s@CreateServiceResponse' {} a -> s {createdByAccountId = a} :: CreateServiceResponse)

-- | A timestamp that indicates when the service is created.
createServiceResponse_createdTime :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.UTCTime)
createServiceResponse_createdTime = Lens.lens (\CreateServiceResponse' {createdTime} -> createdTime) (\s@CreateServiceResponse' {} a -> s {createdTime = a} :: CreateServiceResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the created service.
createServiceResponse_description :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_description = Lens.lens (\CreateServiceResponse' {description} -> description) (\s@CreateServiceResponse' {} a -> s {description = a} :: CreateServiceResponse)

-- | The endpoint type of the service.
createServiceResponse_endpointType :: Lens.Lens' CreateServiceResponse (Prelude.Maybe ServiceEndpointType)
createServiceResponse_endpointType = Lens.lens (\CreateServiceResponse' {endpointType} -> endpointType) (\s@CreateServiceResponse' {} a -> s {endpointType = a} :: CreateServiceResponse)

-- | The unique identifier of the environment.
createServiceResponse_environmentId :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_environmentId = Lens.lens (\CreateServiceResponse' {environmentId} -> environmentId) (\s@CreateServiceResponse' {} a -> s {environmentId = a} :: CreateServiceResponse)

-- | The configuration for the Lambda endpoint type.
createServiceResponse_lambdaEndpoint :: Lens.Lens' CreateServiceResponse (Prelude.Maybe LambdaEndpointInput)
createServiceResponse_lambdaEndpoint = Lens.lens (\CreateServiceResponse' {lambdaEndpoint} -> lambdaEndpoint) (\s@CreateServiceResponse' {} a -> s {lambdaEndpoint = a} :: CreateServiceResponse)

-- | A timestamp that indicates when the service was last updated.
createServiceResponse_lastUpdatedTime :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.UTCTime)
createServiceResponse_lastUpdatedTime = Lens.lens (\CreateServiceResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@CreateServiceResponse' {} a -> s {lastUpdatedTime = a} :: CreateServiceResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the service.
createServiceResponse_name :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_name = Lens.lens (\CreateServiceResponse' {name} -> name) (\s@CreateServiceResponse' {} a -> s {name = a} :: CreateServiceResponse)

-- | The Amazon Web Services account ID of the service owner.
createServiceResponse_ownerAccountId :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_ownerAccountId = Lens.lens (\CreateServiceResponse' {ownerAccountId} -> ownerAccountId) (\s@CreateServiceResponse' {} a -> s {ownerAccountId = a} :: CreateServiceResponse)

-- | The unique identifier of the service.
createServiceResponse_serviceId :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_serviceId = Lens.lens (\CreateServiceResponse' {serviceId} -> serviceId) (\s@CreateServiceResponse' {} a -> s {serviceId = a} :: CreateServiceResponse)

-- | The current state of the service.
createServiceResponse_state :: Lens.Lens' CreateServiceResponse (Prelude.Maybe ServiceState)
createServiceResponse_state = Lens.lens (\CreateServiceResponse' {state} -> state) (\s@CreateServiceResponse' {} a -> s {state = a} :: CreateServiceResponse)

-- | The tags assigned to the created service. A tag is a label that you
-- assign to an Amazon Web Services resource. Each tag consists of a
-- key-value pair..
createServiceResponse_tags :: Lens.Lens' CreateServiceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createServiceResponse_tags = Lens.lens (\CreateServiceResponse' {tags} -> tags) (\s@CreateServiceResponse' {} a -> s {tags = a} :: CreateServiceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The configuration for the URL endpoint type.
createServiceResponse_urlEndpoint :: Lens.Lens' CreateServiceResponse (Prelude.Maybe UrlEndpointInput)
createServiceResponse_urlEndpoint = Lens.lens (\CreateServiceResponse' {urlEndpoint} -> urlEndpoint) (\s@CreateServiceResponse' {} a -> s {urlEndpoint = a} :: CreateServiceResponse)

-- | The ID of the VPC.
createServiceResponse_vpcId :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_vpcId = Lens.lens (\CreateServiceResponse' {vpcId} -> vpcId) (\s@CreateServiceResponse' {} a -> s {vpcId = a} :: CreateServiceResponse)

-- | The response's http status code.
createServiceResponse_httpStatus :: Lens.Lens' CreateServiceResponse Prelude.Int
createServiceResponse_httpStatus = Lens.lens (\CreateServiceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceResponse' {} a -> s {httpStatus = a} :: CreateServiceResponse)

instance Prelude.NFData CreateServiceResponse where
  rnf CreateServiceResponse' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdByAccountId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf lambdaEndpoint
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf urlEndpoint
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
