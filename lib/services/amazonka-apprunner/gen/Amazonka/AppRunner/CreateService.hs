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
-- Module      : Amazonka.AppRunner.CreateService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an App Runner service. After the service is created, the action
-- also automatically starts a deployment.
--
-- This is an asynchronous operation. On a successful call, you can use the
-- returned @OperationId@ and the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_ListOperations.html ListOperations>
-- call to track the operation\'s progress.
module Amazonka.AppRunner.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_autoScalingConfigurationArn,
    createService_encryptionConfiguration,
    createService_healthCheckConfiguration,
    createService_instanceConfiguration,
    createService_networkConfiguration,
    createService_observabilityConfiguration,
    createService_tags,
    createService_serviceName,
    createService_sourceConfiguration,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_httpStatus,
    createServiceResponse_service,
    createServiceResponse_operationId,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | The Amazon Resource Name (ARN) of an App Runner automatic scaling
    -- configuration resource that you want to associate with your service. If
    -- not provided, App Runner associates the latest revision of a default
    -- auto scaling configuration.
    --
    -- Specify an ARN with a name and a revision number to associate that
    -- revision. For example:
    -- @arn:aws:apprunner:us-east-1:123456789012:autoscalingconfiguration\/high-availability\/3@
    --
    -- Specify just the name to associate the latest revision. For example:
    -- @arn:aws:apprunner:us-east-1:123456789012:autoscalingconfiguration\/high-availability@
    autoScalingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | An optional custom encryption key that App Runner uses to encrypt the
    -- copy of your source repository that it maintains and your service logs.
    -- By default, App Runner uses an Amazon Web Services managed key.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The settings for the health check that App Runner performs to monitor
    -- the health of the App Runner service.
    healthCheckConfiguration :: Prelude.Maybe HealthCheckConfiguration,
    -- | The runtime configuration of instances (scaling units) of your service.
    instanceConfiguration :: Prelude.Maybe InstanceConfiguration,
    -- | Configuration settings related to network traffic of the web application
    -- that the App Runner service runs.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The observability configuration of your service.
    observabilityConfiguration :: Prelude.Maybe ServiceObservabilityConfiguration,
    -- | An optional list of metadata items that you can associate with the App
    -- Runner service resource. A tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the App Runner service. It must be unique across all the
    -- running App Runner services in your Amazon Web Services account in the
    -- Amazon Web Services Region.
    serviceName :: Prelude.Text,
    -- | The source to deploy to the App Runner service. It can be a code or an
    -- image repository.
    sourceConfiguration :: SourceConfiguration
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
-- 'autoScalingConfigurationArn', 'createService_autoScalingConfigurationArn' - The Amazon Resource Name (ARN) of an App Runner automatic scaling
-- configuration resource that you want to associate with your service. If
-- not provided, App Runner associates the latest revision of a default
-- auto scaling configuration.
--
-- Specify an ARN with a name and a revision number to associate that
-- revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:autoscalingconfiguration\/high-availability\/3@
--
-- Specify just the name to associate the latest revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:autoscalingconfiguration\/high-availability@
--
-- 'encryptionConfiguration', 'createService_encryptionConfiguration' - An optional custom encryption key that App Runner uses to encrypt the
-- copy of your source repository that it maintains and your service logs.
-- By default, App Runner uses an Amazon Web Services managed key.
--
-- 'healthCheckConfiguration', 'createService_healthCheckConfiguration' - The settings for the health check that App Runner performs to monitor
-- the health of the App Runner service.
--
-- 'instanceConfiguration', 'createService_instanceConfiguration' - The runtime configuration of instances (scaling units) of your service.
--
-- 'networkConfiguration', 'createService_networkConfiguration' - Configuration settings related to network traffic of the web application
-- that the App Runner service runs.
--
-- 'observabilityConfiguration', 'createService_observabilityConfiguration' - The observability configuration of your service.
--
-- 'tags', 'createService_tags' - An optional list of metadata items that you can associate with the App
-- Runner service resource. A tag is a key-value pair.
--
-- 'serviceName', 'createService_serviceName' - A name for the App Runner service. It must be unique across all the
-- running App Runner services in your Amazon Web Services account in the
-- Amazon Web Services Region.
--
-- 'sourceConfiguration', 'createService_sourceConfiguration' - The source to deploy to the App Runner service. It can be a code or an
-- image repository.
newCreateService ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'sourceConfiguration'
  SourceConfiguration ->
  CreateService
newCreateService pServiceName_ pSourceConfiguration_ =
  CreateService'
    { autoScalingConfigurationArn =
        Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      healthCheckConfiguration = Prelude.Nothing,
      instanceConfiguration = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      observabilityConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      serviceName = pServiceName_,
      sourceConfiguration = pSourceConfiguration_
    }

-- | The Amazon Resource Name (ARN) of an App Runner automatic scaling
-- configuration resource that you want to associate with your service. If
-- not provided, App Runner associates the latest revision of a default
-- auto scaling configuration.
--
-- Specify an ARN with a name and a revision number to associate that
-- revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:autoscalingconfiguration\/high-availability\/3@
--
-- Specify just the name to associate the latest revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:autoscalingconfiguration\/high-availability@
createService_autoScalingConfigurationArn :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_autoScalingConfigurationArn = Lens.lens (\CreateService' {autoScalingConfigurationArn} -> autoScalingConfigurationArn) (\s@CreateService' {} a -> s {autoScalingConfigurationArn = a} :: CreateService)

-- | An optional custom encryption key that App Runner uses to encrypt the
-- copy of your source repository that it maintains and your service logs.
-- By default, App Runner uses an Amazon Web Services managed key.
createService_encryptionConfiguration :: Lens.Lens' CreateService (Prelude.Maybe EncryptionConfiguration)
createService_encryptionConfiguration = Lens.lens (\CreateService' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateService' {} a -> s {encryptionConfiguration = a} :: CreateService)

-- | The settings for the health check that App Runner performs to monitor
-- the health of the App Runner service.
createService_healthCheckConfiguration :: Lens.Lens' CreateService (Prelude.Maybe HealthCheckConfiguration)
createService_healthCheckConfiguration = Lens.lens (\CreateService' {healthCheckConfiguration} -> healthCheckConfiguration) (\s@CreateService' {} a -> s {healthCheckConfiguration = a} :: CreateService)

-- | The runtime configuration of instances (scaling units) of your service.
createService_instanceConfiguration :: Lens.Lens' CreateService (Prelude.Maybe InstanceConfiguration)
createService_instanceConfiguration = Lens.lens (\CreateService' {instanceConfiguration} -> instanceConfiguration) (\s@CreateService' {} a -> s {instanceConfiguration = a} :: CreateService)

-- | Configuration settings related to network traffic of the web application
-- that the App Runner service runs.
createService_networkConfiguration :: Lens.Lens' CreateService (Prelude.Maybe NetworkConfiguration)
createService_networkConfiguration = Lens.lens (\CreateService' {networkConfiguration} -> networkConfiguration) (\s@CreateService' {} a -> s {networkConfiguration = a} :: CreateService)

-- | The observability configuration of your service.
createService_observabilityConfiguration :: Lens.Lens' CreateService (Prelude.Maybe ServiceObservabilityConfiguration)
createService_observabilityConfiguration = Lens.lens (\CreateService' {observabilityConfiguration} -> observabilityConfiguration) (\s@CreateService' {} a -> s {observabilityConfiguration = a} :: CreateService)

-- | An optional list of metadata items that you can associate with the App
-- Runner service resource. A tag is a key-value pair.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe [Tag])
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | A name for the App Runner service. It must be unique across all the
-- running App Runner services in your Amazon Web Services account in the
-- Amazon Web Services Region.
createService_serviceName :: Lens.Lens' CreateService Prelude.Text
createService_serviceName = Lens.lens (\CreateService' {serviceName} -> serviceName) (\s@CreateService' {} a -> s {serviceName = a} :: CreateService)

-- | The source to deploy to the App Runner service. It can be a code or an
-- image repository.
createService_sourceConfiguration :: Lens.Lens' CreateService SourceConfiguration
createService_sourceConfiguration = Lens.lens (\CreateService' {sourceConfiguration} -> sourceConfiguration) (\s@CreateService' {} a -> s {sourceConfiguration = a} :: CreateService)

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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Service")
            Prelude.<*> (x Data..:> "OperationId")
      )

instance Prelude.Hashable CreateService where
  hashWithSalt _salt CreateService' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingConfigurationArn
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` healthCheckConfiguration
      `Prelude.hashWithSalt` instanceConfiguration
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` observabilityConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` sourceConfiguration

instance Prelude.NFData CreateService where
  rnf CreateService' {..} =
    Prelude.rnf autoScalingConfigurationArn `Prelude.seq`
      Prelude.rnf encryptionConfiguration `Prelude.seq`
        Prelude.rnf healthCheckConfiguration `Prelude.seq`
          Prelude.rnf instanceConfiguration `Prelude.seq`
            Prelude.rnf networkConfiguration `Prelude.seq`
              Prelude.rnf observabilityConfiguration `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf serviceName `Prelude.seq`
                    Prelude.rnf sourceConfiguration

instance Data.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AppRunner.CreateService" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateService where
  toJSON CreateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoScalingConfigurationArn" Data..=)
              Prelude.<$> autoScalingConfigurationArn,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("HealthCheckConfiguration" Data..=)
              Prelude.<$> healthCheckConfiguration,
            ("InstanceConfiguration" Data..=)
              Prelude.<$> instanceConfiguration,
            ("NetworkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("ObservabilityConfiguration" Data..=)
              Prelude.<$> observabilityConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ServiceName" Data..= serviceName),
            Prelude.Just
              ("SourceConfiguration" Data..= sourceConfiguration)
          ]
      )

instance Data.ToPath CreateService where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner service that\'s created by this request.
    service :: Service,
    -- | The unique ID of the asynchronous operation that this request started.
    -- You can use it combined with the
    -- <https://docs.aws.amazon.com/apprunner/latest/api/API_ListOperations.html ListOperations>
    -- call to track the operation\'s progress.
    operationId :: Prelude.Text
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
-- 'httpStatus', 'createServiceResponse_httpStatus' - The response's http status code.
--
-- 'service', 'createServiceResponse_service' - A description of the App Runner service that\'s created by this request.
--
-- 'operationId', 'createServiceResponse_operationId' - The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_ListOperations.html ListOperations>
-- call to track the operation\'s progress.
newCreateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'service'
  Service ->
  -- | 'operationId'
  Prelude.Text ->
  CreateServiceResponse
newCreateServiceResponse
  pHttpStatus_
  pService_
  pOperationId_ =
    CreateServiceResponse'
      { httpStatus = pHttpStatus_,
        service = pService_,
        operationId = pOperationId_
      }

-- | The response's http status code.
createServiceResponse_httpStatus :: Lens.Lens' CreateServiceResponse Prelude.Int
createServiceResponse_httpStatus = Lens.lens (\CreateServiceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceResponse' {} a -> s {httpStatus = a} :: CreateServiceResponse)

-- | A description of the App Runner service that\'s created by this request.
createServiceResponse_service :: Lens.Lens' CreateServiceResponse Service
createServiceResponse_service = Lens.lens (\CreateServiceResponse' {service} -> service) (\s@CreateServiceResponse' {} a -> s {service = a} :: CreateServiceResponse)

-- | The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_ListOperations.html ListOperations>
-- call to track the operation\'s progress.
createServiceResponse_operationId :: Lens.Lens' CreateServiceResponse Prelude.Text
createServiceResponse_operationId = Lens.lens (\CreateServiceResponse' {operationId} -> operationId) (\s@CreateServiceResponse' {} a -> s {operationId = a} :: CreateServiceResponse)

instance Prelude.NFData CreateServiceResponse where
  rnf CreateServiceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf service `Prelude.seq`
        Prelude.rnf operationId
