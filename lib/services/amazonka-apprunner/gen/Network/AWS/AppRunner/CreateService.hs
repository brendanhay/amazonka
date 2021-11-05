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
-- Module      : Network.AWS.AppRunner.CreateService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.AppRunner.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_autoScalingConfigurationArn,
    createService_encryptionConfiguration,
    createService_healthCheckConfiguration,
    createService_tags,
    createService_instanceConfiguration,
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

import Network.AWS.AppRunner.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | The Amazon Resource Name (ARN) of an App Runner automatic scaling
    -- configuration resource that you want to associate with your service. If
    -- not provided, App Runner associates the latest revision of a default
    -- auto scaling configuration.
    autoScalingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | An optional custom encryption key that App Runner uses to encrypt the
    -- copy of your source repository that it maintains and your service logs.
    -- By default, App Runner uses an Amazon Web Services managed CMK.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The settings for the health check that App Runner performs to monitor
    -- the health of your service.
    healthCheckConfiguration :: Prelude.Maybe HealthCheckConfiguration,
    -- | An optional list of metadata items that you can associate with your
    -- service resource. A tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | The runtime configuration of instances (scaling units) of the App Runner
    -- service.
    instanceConfiguration :: Prelude.Maybe InstanceConfiguration,
    -- | A name for the new service. It must be unique across all the running App
    -- Runner services in your Amazon Web Services account in the Amazon Web
    -- Services Region.
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
-- 'encryptionConfiguration', 'createService_encryptionConfiguration' - An optional custom encryption key that App Runner uses to encrypt the
-- copy of your source repository that it maintains and your service logs.
-- By default, App Runner uses an Amazon Web Services managed CMK.
--
-- 'healthCheckConfiguration', 'createService_healthCheckConfiguration' - The settings for the health check that App Runner performs to monitor
-- the health of your service.
--
-- 'tags', 'createService_tags' - An optional list of metadata items that you can associate with your
-- service resource. A tag is a key-value pair.
--
-- 'instanceConfiguration', 'createService_instanceConfiguration' - The runtime configuration of instances (scaling units) of the App Runner
-- service.
--
-- 'serviceName', 'createService_serviceName' - A name for the new service. It must be unique across all the running App
-- Runner services in your Amazon Web Services account in the Amazon Web
-- Services Region.
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
      tags = Prelude.Nothing,
      instanceConfiguration = Prelude.Nothing,
      serviceName = pServiceName_,
      sourceConfiguration = pSourceConfiguration_
    }

-- | The Amazon Resource Name (ARN) of an App Runner automatic scaling
-- configuration resource that you want to associate with your service. If
-- not provided, App Runner associates the latest revision of a default
-- auto scaling configuration.
createService_autoScalingConfigurationArn :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_autoScalingConfigurationArn = Lens.lens (\CreateService' {autoScalingConfigurationArn} -> autoScalingConfigurationArn) (\s@CreateService' {} a -> s {autoScalingConfigurationArn = a} :: CreateService)

-- | An optional custom encryption key that App Runner uses to encrypt the
-- copy of your source repository that it maintains and your service logs.
-- By default, App Runner uses an Amazon Web Services managed CMK.
createService_encryptionConfiguration :: Lens.Lens' CreateService (Prelude.Maybe EncryptionConfiguration)
createService_encryptionConfiguration = Lens.lens (\CreateService' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateService' {} a -> s {encryptionConfiguration = a} :: CreateService)

-- | The settings for the health check that App Runner performs to monitor
-- the health of your service.
createService_healthCheckConfiguration :: Lens.Lens' CreateService (Prelude.Maybe HealthCheckConfiguration)
createService_healthCheckConfiguration = Lens.lens (\CreateService' {healthCheckConfiguration} -> healthCheckConfiguration) (\s@CreateService' {} a -> s {healthCheckConfiguration = a} :: CreateService)

-- | An optional list of metadata items that you can associate with your
-- service resource. A tag is a key-value pair.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe [Tag])
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The runtime configuration of instances (scaling units) of the App Runner
-- service.
createService_instanceConfiguration :: Lens.Lens' CreateService (Prelude.Maybe InstanceConfiguration)
createService_instanceConfiguration = Lens.lens (\CreateService' {instanceConfiguration} -> instanceConfiguration) (\s@CreateService' {} a -> s {instanceConfiguration = a} :: CreateService)

-- | A name for the new service. It must be unique across all the running App
-- Runner services in your Amazon Web Services account in the Amazon Web
-- Services Region.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Service")
            Prelude.<*> (x Core..:> "OperationId")
      )

instance Prelude.Hashable CreateService

instance Prelude.NFData CreateService

instance Core.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AppRunner.CreateService" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateService where
  toJSON CreateService' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AutoScalingConfigurationArn" Core..=)
              Prelude.<$> autoScalingConfigurationArn,
            ("EncryptionConfiguration" Core..=)
              Prelude.<$> encryptionConfiguration,
            ("HealthCheckConfiguration" Core..=)
              Prelude.<$> healthCheckConfiguration,
            ("Tags" Core..=) Prelude.<$> tags,
            ("InstanceConfiguration" Core..=)
              Prelude.<$> instanceConfiguration,
            Prelude.Just ("ServiceName" Core..= serviceName),
            Prelude.Just
              ("SourceConfiguration" Core..= sourceConfiguration)
          ]
      )

instance Core.ToPath CreateService where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateService where
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

instance Prelude.NFData CreateServiceResponse
