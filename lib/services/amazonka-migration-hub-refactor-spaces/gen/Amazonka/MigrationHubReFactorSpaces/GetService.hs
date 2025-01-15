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
-- Module      : Amazonka.MigrationHubReFactorSpaces.GetService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Amazon Web Services Migration Hub Refactor Spaces service.
module Amazonka.MigrationHubReFactorSpaces.GetService
  ( -- * Creating a Request
    GetService (..),
    newGetService,

    -- * Request Lenses
    getService_applicationIdentifier,
    getService_environmentIdentifier,
    getService_serviceIdentifier,

    -- * Destructuring the Response
    GetServiceResponse (..),
    newGetServiceResponse,

    -- * Response Lenses
    getServiceResponse_applicationId,
    getServiceResponse_arn,
    getServiceResponse_createdByAccountId,
    getServiceResponse_createdTime,
    getServiceResponse_description,
    getServiceResponse_endpointType,
    getServiceResponse_environmentId,
    getServiceResponse_error,
    getServiceResponse_lambdaEndpoint,
    getServiceResponse_lastUpdatedTime,
    getServiceResponse_name,
    getServiceResponse_ownerAccountId,
    getServiceResponse_serviceId,
    getServiceResponse_state,
    getServiceResponse_tags,
    getServiceResponse_urlEndpoint,
    getServiceResponse_vpcId,
    getServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetService' smart constructor.
data GetService = GetService'
  { -- | The ID of the application.
    applicationIdentifier :: Prelude.Text,
    -- | The ID of the environment.
    environmentIdentifier :: Prelude.Text,
    -- | The ID of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIdentifier', 'getService_applicationIdentifier' - The ID of the application.
--
-- 'environmentIdentifier', 'getService_environmentIdentifier' - The ID of the environment.
--
-- 'serviceIdentifier', 'getService_serviceIdentifier' - The ID of the service.
newGetService ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  GetService
newGetService
  pApplicationIdentifier_
  pEnvironmentIdentifier_
  pServiceIdentifier_ =
    GetService'
      { applicationIdentifier =
          pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | The ID of the application.
getService_applicationIdentifier :: Lens.Lens' GetService Prelude.Text
getService_applicationIdentifier = Lens.lens (\GetService' {applicationIdentifier} -> applicationIdentifier) (\s@GetService' {} a -> s {applicationIdentifier = a} :: GetService)

-- | The ID of the environment.
getService_environmentIdentifier :: Lens.Lens' GetService Prelude.Text
getService_environmentIdentifier = Lens.lens (\GetService' {environmentIdentifier} -> environmentIdentifier) (\s@GetService' {} a -> s {environmentIdentifier = a} :: GetService)

-- | The ID of the service.
getService_serviceIdentifier :: Lens.Lens' GetService Prelude.Text
getService_serviceIdentifier = Lens.lens (\GetService' {serviceIdentifier} -> serviceIdentifier) (\s@GetService' {} a -> s {serviceIdentifier = a} :: GetService)

instance Core.AWSRequest GetService where
  type AWSResponse GetService = GetServiceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceResponse'
            Prelude.<$> (x Data..?> "ApplicationId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedByAccountId")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "EndpointType")
            Prelude.<*> (x Data..?> "EnvironmentId")
            Prelude.<*> (x Data..?> "Error")
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

instance Prelude.Hashable GetService where
  hashWithSalt _salt GetService' {..} =
    _salt
      `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData GetService where
  rnf GetService' {..} =
    Prelude.rnf applicationIdentifier `Prelude.seq`
      Prelude.rnf environmentIdentifier `Prelude.seq`
        Prelude.rnf serviceIdentifier

instance Data.ToHeaders GetService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetService where
  toPath GetService' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/applications/",
        Data.toBS applicationIdentifier,
        "/services/",
        Data.toBS serviceIdentifier
      ]

instance Data.ToQuery GetService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceResponse' smart constructor.
data GetServiceResponse = GetServiceResponse'
  { -- | The ID of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the service creator.
    createdByAccountId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the service is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the service.
    description :: Prelude.Maybe Prelude.Text,
    -- | The endpoint type of the service.
    endpointType :: Prelude.Maybe ServiceEndpointType,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Any error associated with the service resource.
    error :: Prelude.Maybe ErrorResponse,
    -- | The configuration for the Lambda endpoint type.
    --
    -- The __Arn__ is the Amazon Resource Name (ARN) of the Lambda function
    -- associated with this service.
    lambdaEndpoint :: Prelude.Maybe LambdaEndpointConfig,
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
    -- | The tags assigned to the service. A tag is a label that you assign to an
    -- Amazon Web Services resource. Each tag consists of a key-value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The configuration for the URL endpoint type.
    --
    -- The __Url__ isthe URL of the endpoint type.
    --
    -- The __HealthUrl__ is the health check URL of the endpoint type.
    urlEndpoint :: Prelude.Maybe UrlEndpointConfig,
    -- | The ID of the virtual private cloud (VPC).
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getServiceResponse_applicationId' - The ID of the application.
--
-- 'arn', 'getServiceResponse_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'createdByAccountId', 'getServiceResponse_createdByAccountId' - The Amazon Web Services account ID of the service creator.
--
-- 'createdTime', 'getServiceResponse_createdTime' - The timestamp of when the service is created.
--
-- 'description', 'getServiceResponse_description' - The description of the service.
--
-- 'endpointType', 'getServiceResponse_endpointType' - The endpoint type of the service.
--
-- 'environmentId', 'getServiceResponse_environmentId' - The unique identifier of the environment.
--
-- 'error', 'getServiceResponse_error' - Any error associated with the service resource.
--
-- 'lambdaEndpoint', 'getServiceResponse_lambdaEndpoint' - The configuration for the Lambda endpoint type.
--
-- The __Arn__ is the Amazon Resource Name (ARN) of the Lambda function
-- associated with this service.
--
-- 'lastUpdatedTime', 'getServiceResponse_lastUpdatedTime' - A timestamp that indicates when the service was last updated.
--
-- 'name', 'getServiceResponse_name' - The name of the service.
--
-- 'ownerAccountId', 'getServiceResponse_ownerAccountId' - The Amazon Web Services account ID of the service owner.
--
-- 'serviceId', 'getServiceResponse_serviceId' - The unique identifier of the service.
--
-- 'state', 'getServiceResponse_state' - The current state of the service.
--
-- 'tags', 'getServiceResponse_tags' - The tags assigned to the service. A tag is a label that you assign to an
-- Amazon Web Services resource. Each tag consists of a key-value pair.
--
-- 'urlEndpoint', 'getServiceResponse_urlEndpoint' - The configuration for the URL endpoint type.
--
-- The __Url__ isthe URL of the endpoint type.
--
-- The __HealthUrl__ is the health check URL of the endpoint type.
--
-- 'vpcId', 'getServiceResponse_vpcId' - The ID of the virtual private cloud (VPC).
--
-- 'httpStatus', 'getServiceResponse_httpStatus' - The response's http status code.
newGetServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceResponse
newGetServiceResponse pHttpStatus_ =
  GetServiceResponse'
    { applicationId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      createdByAccountId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      error = Prelude.Nothing,
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

-- | The ID of the application.
getServiceResponse_applicationId :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_applicationId = Lens.lens (\GetServiceResponse' {applicationId} -> applicationId) (\s@GetServiceResponse' {} a -> s {applicationId = a} :: GetServiceResponse)

-- | The Amazon Resource Name (ARN) of the service.
getServiceResponse_arn :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_arn = Lens.lens (\GetServiceResponse' {arn} -> arn) (\s@GetServiceResponse' {} a -> s {arn = a} :: GetServiceResponse)

-- | The Amazon Web Services account ID of the service creator.
getServiceResponse_createdByAccountId :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_createdByAccountId = Lens.lens (\GetServiceResponse' {createdByAccountId} -> createdByAccountId) (\s@GetServiceResponse' {} a -> s {createdByAccountId = a} :: GetServiceResponse)

-- | The timestamp of when the service is created.
getServiceResponse_createdTime :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.UTCTime)
getServiceResponse_createdTime = Lens.lens (\GetServiceResponse' {createdTime} -> createdTime) (\s@GetServiceResponse' {} a -> s {createdTime = a} :: GetServiceResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the service.
getServiceResponse_description :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_description = Lens.lens (\GetServiceResponse' {description} -> description) (\s@GetServiceResponse' {} a -> s {description = a} :: GetServiceResponse)

-- | The endpoint type of the service.
getServiceResponse_endpointType :: Lens.Lens' GetServiceResponse (Prelude.Maybe ServiceEndpointType)
getServiceResponse_endpointType = Lens.lens (\GetServiceResponse' {endpointType} -> endpointType) (\s@GetServiceResponse' {} a -> s {endpointType = a} :: GetServiceResponse)

-- | The unique identifier of the environment.
getServiceResponse_environmentId :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_environmentId = Lens.lens (\GetServiceResponse' {environmentId} -> environmentId) (\s@GetServiceResponse' {} a -> s {environmentId = a} :: GetServiceResponse)

-- | Any error associated with the service resource.
getServiceResponse_error :: Lens.Lens' GetServiceResponse (Prelude.Maybe ErrorResponse)
getServiceResponse_error = Lens.lens (\GetServiceResponse' {error} -> error) (\s@GetServiceResponse' {} a -> s {error = a} :: GetServiceResponse)

-- | The configuration for the Lambda endpoint type.
--
-- The __Arn__ is the Amazon Resource Name (ARN) of the Lambda function
-- associated with this service.
getServiceResponse_lambdaEndpoint :: Lens.Lens' GetServiceResponse (Prelude.Maybe LambdaEndpointConfig)
getServiceResponse_lambdaEndpoint = Lens.lens (\GetServiceResponse' {lambdaEndpoint} -> lambdaEndpoint) (\s@GetServiceResponse' {} a -> s {lambdaEndpoint = a} :: GetServiceResponse)

-- | A timestamp that indicates when the service was last updated.
getServiceResponse_lastUpdatedTime :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.UTCTime)
getServiceResponse_lastUpdatedTime = Lens.lens (\GetServiceResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@GetServiceResponse' {} a -> s {lastUpdatedTime = a} :: GetServiceResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the service.
getServiceResponse_name :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_name = Lens.lens (\GetServiceResponse' {name} -> name) (\s@GetServiceResponse' {} a -> s {name = a} :: GetServiceResponse)

-- | The Amazon Web Services account ID of the service owner.
getServiceResponse_ownerAccountId :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_ownerAccountId = Lens.lens (\GetServiceResponse' {ownerAccountId} -> ownerAccountId) (\s@GetServiceResponse' {} a -> s {ownerAccountId = a} :: GetServiceResponse)

-- | The unique identifier of the service.
getServiceResponse_serviceId :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_serviceId = Lens.lens (\GetServiceResponse' {serviceId} -> serviceId) (\s@GetServiceResponse' {} a -> s {serviceId = a} :: GetServiceResponse)

-- | The current state of the service.
getServiceResponse_state :: Lens.Lens' GetServiceResponse (Prelude.Maybe ServiceState)
getServiceResponse_state = Lens.lens (\GetServiceResponse' {state} -> state) (\s@GetServiceResponse' {} a -> s {state = a} :: GetServiceResponse)

-- | The tags assigned to the service. A tag is a label that you assign to an
-- Amazon Web Services resource. Each tag consists of a key-value pair.
getServiceResponse_tags :: Lens.Lens' GetServiceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getServiceResponse_tags = Lens.lens (\GetServiceResponse' {tags} -> tags) (\s@GetServiceResponse' {} a -> s {tags = a} :: GetServiceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The configuration for the URL endpoint type.
--
-- The __Url__ isthe URL of the endpoint type.
--
-- The __HealthUrl__ is the health check URL of the endpoint type.
getServiceResponse_urlEndpoint :: Lens.Lens' GetServiceResponse (Prelude.Maybe UrlEndpointConfig)
getServiceResponse_urlEndpoint = Lens.lens (\GetServiceResponse' {urlEndpoint} -> urlEndpoint) (\s@GetServiceResponse' {} a -> s {urlEndpoint = a} :: GetServiceResponse)

-- | The ID of the virtual private cloud (VPC).
getServiceResponse_vpcId :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_vpcId = Lens.lens (\GetServiceResponse' {vpcId} -> vpcId) (\s@GetServiceResponse' {} a -> s {vpcId = a} :: GetServiceResponse)

-- | The response's http status code.
getServiceResponse_httpStatus :: Lens.Lens' GetServiceResponse Prelude.Int
getServiceResponse_httpStatus = Lens.lens (\GetServiceResponse' {httpStatus} -> httpStatus) (\s@GetServiceResponse' {} a -> s {httpStatus = a} :: GetServiceResponse)

instance Prelude.NFData GetServiceResponse where
  rnf GetServiceResponse' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf createdByAccountId `Prelude.seq`
          Prelude.rnf createdTime `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf endpointType `Prelude.seq`
                Prelude.rnf environmentId `Prelude.seq`
                  Prelude.rnf error `Prelude.seq`
                    Prelude.rnf lambdaEndpoint `Prelude.seq`
                      Prelude.rnf lastUpdatedTime `Prelude.seq`
                        Prelude.rnf name `Prelude.seq`
                          Prelude.rnf ownerAccountId `Prelude.seq`
                            Prelude.rnf serviceId `Prelude.seq`
                              Prelude.rnf state `Prelude.seq`
                                Prelude.rnf tags `Prelude.seq`
                                  Prelude.rnf urlEndpoint `Prelude.seq`
                                    Prelude.rnf vpcId `Prelude.seq`
                                      Prelude.rnf httpStatus
