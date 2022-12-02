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
-- Module      : Amazonka.Lightsail.CreateContainerService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lightsail container service.
--
-- A Lightsail container service is a compute resource to which you can
-- deploy containers. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-services Container services in Amazon Lightsail>
-- in the /Lightsail Dev Guide/.
module Amazonka.Lightsail.CreateContainerService
  ( -- * Creating a Request
    CreateContainerService (..),
    newCreateContainerService,

    -- * Request Lenses
    createContainerService_tags,
    createContainerService_deployment,
    createContainerService_publicDomainNames,
    createContainerService_privateRegistryAccess,
    createContainerService_serviceName,
    createContainerService_power,
    createContainerService_scale,

    -- * Destructuring the Response
    CreateContainerServiceResponse (..),
    newCreateContainerServiceResponse,

    -- * Response Lenses
    createContainerServiceResponse_containerService,
    createContainerServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateContainerService' smart constructor.
data CreateContainerService = CreateContainerService'
  { -- | The tag keys and optional values to add to the container service during
    -- create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    --
    -- For more information about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | An object that describes a deployment for the container service.
    --
    -- A deployment specifies the containers that will be launched on the
    -- container service and their settings, such as the ports to open, the
    -- environment variables to apply, and the launch command to run. It also
    -- specifies the container that will serve as the public endpoint of the
    -- deployment and its settings, such as the HTTP or HTTPS port to use, and
    -- the health check configuration.
    deployment :: Prelude.Maybe ContainerServiceDeploymentRequest,
    -- | The public domain names to use with the container service, such as
    -- @example.com@ and @www.example.com@.
    --
    -- You can specify up to four public domain names for a container service.
    -- The domain names that you specify are used when you create a deployment
    -- with a container configured as the public endpoint of your container
    -- service.
    --
    -- If you don\'t specify public domain names, then you can use the default
    -- domain of the container service.
    --
    -- You must create and validate an SSL\/TLS certificate before you can use
    -- public domain names with your container service. Use the
    -- @CreateCertificate@ action to create a certificate for the public domain
    -- names you want to use with your container service.
    --
    -- You can specify public domain names using a string to array map as shown
    -- in the example later on this page.
    publicDomainNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | An object to describe the configuration for the container service to
    -- access private container image repositories, such as Amazon Elastic
    -- Container Registry (Amazon ECR) private repositories.
    --
    -- For more information, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
    -- in the /Amazon Lightsail Developer Guide/.
    privateRegistryAccess :: Prelude.Maybe PrivateRegistryAccessRequest,
    -- | The name for the container service.
    --
    -- The name that you specify for your container service will make up part
    -- of its default domain. The default domain of a container service is
    -- typically
    -- @https:\/\/\<ServiceName>.\<RandomGUID>.\<AWSRegion>.cs.amazonlightsail.com@.
    -- If the name of your container service is @container-service-1@, and
    -- it\'s located in the US East (Ohio) Amazon Web Services Region
    -- (@us-east-2@), then the domain for your container service will be like
    -- the following example:
    -- @https:\/\/container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@
    --
    -- The following are the requirements for container service names:
    --
    -- -   Must be unique within each Amazon Web Services Region in your
    --     Lightsail account.
    --
    -- -   Must contain 1 to 63 characters.
    --
    -- -   Must contain only alphanumeric characters and hyphens.
    --
    -- -   A hyphen (-) can separate words but cannot be at the start or end of
    --     the name.
    serviceName :: Prelude.Text,
    -- | The power specification for the container service.
    --
    -- The power specifies the amount of memory, vCPUs, and base monthly cost
    -- of each node of the container service. The @power@ and @scale@ of a
    -- container service makes up its configured capacity. To determine the
    -- monthly price of your container service, multiply the base price of the
    -- @power@ with the @scale@ (the number of nodes) of the service.
    --
    -- Use the @GetContainerServicePowers@ action to get a list of power
    -- options that you can specify using this parameter, and their base
    -- monthly cost.
    power :: ContainerServicePowerName,
    -- | The scale specification for the container service.
    --
    -- The scale specifies the allocated compute nodes of the container
    -- service. The @power@ and @scale@ of a container service makes up its
    -- configured capacity. To determine the monthly price of your container
    -- service, multiply the base price of the @power@ with the @scale@ (the
    -- number of nodes) of the service.
    scale :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContainerService_tags' - The tag keys and optional values to add to the container service during
-- create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- For more information about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'deployment', 'createContainerService_deployment' - An object that describes a deployment for the container service.
--
-- A deployment specifies the containers that will be launched on the
-- container service and their settings, such as the ports to open, the
-- environment variables to apply, and the launch command to run. It also
-- specifies the container that will serve as the public endpoint of the
-- deployment and its settings, such as the HTTP or HTTPS port to use, and
-- the health check configuration.
--
-- 'publicDomainNames', 'createContainerService_publicDomainNames' - The public domain names to use with the container service, such as
-- @example.com@ and @www.example.com@.
--
-- You can specify up to four public domain names for a container service.
-- The domain names that you specify are used when you create a deployment
-- with a container configured as the public endpoint of your container
-- service.
--
-- If you don\'t specify public domain names, then you can use the default
-- domain of the container service.
--
-- You must create and validate an SSL\/TLS certificate before you can use
-- public domain names with your container service. Use the
-- @CreateCertificate@ action to create a certificate for the public domain
-- names you want to use with your container service.
--
-- You can specify public domain names using a string to array map as shown
-- in the example later on this page.
--
-- 'privateRegistryAccess', 'createContainerService_privateRegistryAccess' - An object to describe the configuration for the container service to
-- access private container image repositories, such as Amazon Elastic
-- Container Registry (Amazon ECR) private repositories.
--
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'serviceName', 'createContainerService_serviceName' - The name for the container service.
--
-- The name that you specify for your container service will make up part
-- of its default domain. The default domain of a container service is
-- typically
-- @https:\/\/\<ServiceName>.\<RandomGUID>.\<AWSRegion>.cs.amazonlightsail.com@.
-- If the name of your container service is @container-service-1@, and
-- it\'s located in the US East (Ohio) Amazon Web Services Region
-- (@us-east-2@), then the domain for your container service will be like
-- the following example:
-- @https:\/\/container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@
--
-- The following are the requirements for container service names:
--
-- -   Must be unique within each Amazon Web Services Region in your
--     Lightsail account.
--
-- -   Must contain 1 to 63 characters.
--
-- -   Must contain only alphanumeric characters and hyphens.
--
-- -   A hyphen (-) can separate words but cannot be at the start or end of
--     the name.
--
-- 'power', 'createContainerService_power' - The power specification for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost
-- of each node of the container service. The @power@ and @scale@ of a
-- container service makes up its configured capacity. To determine the
-- monthly price of your container service, multiply the base price of the
-- @power@ with the @scale@ (the number of nodes) of the service.
--
-- Use the @GetContainerServicePowers@ action to get a list of power
-- options that you can specify using this parameter, and their base
-- monthly cost.
--
-- 'scale', 'createContainerService_scale' - The scale specification for the container service.
--
-- The scale specifies the allocated compute nodes of the container
-- service. The @power@ and @scale@ of a container service makes up its
-- configured capacity. To determine the monthly price of your container
-- service, multiply the base price of the @power@ with the @scale@ (the
-- number of nodes) of the service.
newCreateContainerService ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'power'
  ContainerServicePowerName ->
  -- | 'scale'
  Prelude.Natural ->
  CreateContainerService
newCreateContainerService
  pServiceName_
  pPower_
  pScale_ =
    CreateContainerService'
      { tags = Prelude.Nothing,
        deployment = Prelude.Nothing,
        publicDomainNames = Prelude.Nothing,
        privateRegistryAccess = Prelude.Nothing,
        serviceName = pServiceName_,
        power = pPower_,
        scale = pScale_
      }

-- | The tag keys and optional values to add to the container service during
-- create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- For more information about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
createContainerService_tags :: Lens.Lens' CreateContainerService (Prelude.Maybe [Tag])
createContainerService_tags = Lens.lens (\CreateContainerService' {tags} -> tags) (\s@CreateContainerService' {} a -> s {tags = a} :: CreateContainerService) Prelude.. Lens.mapping Lens.coerced

-- | An object that describes a deployment for the container service.
--
-- A deployment specifies the containers that will be launched on the
-- container service and their settings, such as the ports to open, the
-- environment variables to apply, and the launch command to run. It also
-- specifies the container that will serve as the public endpoint of the
-- deployment and its settings, such as the HTTP or HTTPS port to use, and
-- the health check configuration.
createContainerService_deployment :: Lens.Lens' CreateContainerService (Prelude.Maybe ContainerServiceDeploymentRequest)
createContainerService_deployment = Lens.lens (\CreateContainerService' {deployment} -> deployment) (\s@CreateContainerService' {} a -> s {deployment = a} :: CreateContainerService)

-- | The public domain names to use with the container service, such as
-- @example.com@ and @www.example.com@.
--
-- You can specify up to four public domain names for a container service.
-- The domain names that you specify are used when you create a deployment
-- with a container configured as the public endpoint of your container
-- service.
--
-- If you don\'t specify public domain names, then you can use the default
-- domain of the container service.
--
-- You must create and validate an SSL\/TLS certificate before you can use
-- public domain names with your container service. Use the
-- @CreateCertificate@ action to create a certificate for the public domain
-- names you want to use with your container service.
--
-- You can specify public domain names using a string to array map as shown
-- in the example later on this page.
createContainerService_publicDomainNames :: Lens.Lens' CreateContainerService (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
createContainerService_publicDomainNames = Lens.lens (\CreateContainerService' {publicDomainNames} -> publicDomainNames) (\s@CreateContainerService' {} a -> s {publicDomainNames = a} :: CreateContainerService) Prelude.. Lens.mapping Lens.coerced

-- | An object to describe the configuration for the container service to
-- access private container image repositories, such as Amazon Elastic
-- Container Registry (Amazon ECR) private repositories.
--
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
-- in the /Amazon Lightsail Developer Guide/.
createContainerService_privateRegistryAccess :: Lens.Lens' CreateContainerService (Prelude.Maybe PrivateRegistryAccessRequest)
createContainerService_privateRegistryAccess = Lens.lens (\CreateContainerService' {privateRegistryAccess} -> privateRegistryAccess) (\s@CreateContainerService' {} a -> s {privateRegistryAccess = a} :: CreateContainerService)

-- | The name for the container service.
--
-- The name that you specify for your container service will make up part
-- of its default domain. The default domain of a container service is
-- typically
-- @https:\/\/\<ServiceName>.\<RandomGUID>.\<AWSRegion>.cs.amazonlightsail.com@.
-- If the name of your container service is @container-service-1@, and
-- it\'s located in the US East (Ohio) Amazon Web Services Region
-- (@us-east-2@), then the domain for your container service will be like
-- the following example:
-- @https:\/\/container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@
--
-- The following are the requirements for container service names:
--
-- -   Must be unique within each Amazon Web Services Region in your
--     Lightsail account.
--
-- -   Must contain 1 to 63 characters.
--
-- -   Must contain only alphanumeric characters and hyphens.
--
-- -   A hyphen (-) can separate words but cannot be at the start or end of
--     the name.
createContainerService_serviceName :: Lens.Lens' CreateContainerService Prelude.Text
createContainerService_serviceName = Lens.lens (\CreateContainerService' {serviceName} -> serviceName) (\s@CreateContainerService' {} a -> s {serviceName = a} :: CreateContainerService)

-- | The power specification for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost
-- of each node of the container service. The @power@ and @scale@ of a
-- container service makes up its configured capacity. To determine the
-- monthly price of your container service, multiply the base price of the
-- @power@ with the @scale@ (the number of nodes) of the service.
--
-- Use the @GetContainerServicePowers@ action to get a list of power
-- options that you can specify using this parameter, and their base
-- monthly cost.
createContainerService_power :: Lens.Lens' CreateContainerService ContainerServicePowerName
createContainerService_power = Lens.lens (\CreateContainerService' {power} -> power) (\s@CreateContainerService' {} a -> s {power = a} :: CreateContainerService)

-- | The scale specification for the container service.
--
-- The scale specifies the allocated compute nodes of the container
-- service. The @power@ and @scale@ of a container service makes up its
-- configured capacity. To determine the monthly price of your container
-- service, multiply the base price of the @power@ with the @scale@ (the
-- number of nodes) of the service.
createContainerService_scale :: Lens.Lens' CreateContainerService Prelude.Natural
createContainerService_scale = Lens.lens (\CreateContainerService' {scale} -> scale) (\s@CreateContainerService' {} a -> s {scale = a} :: CreateContainerService)

instance Core.AWSRequest CreateContainerService where
  type
    AWSResponse CreateContainerService =
      CreateContainerServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerServiceResponse'
            Prelude.<$> (x Data..?> "containerService")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContainerService where
  hashWithSalt _salt CreateContainerService' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` deployment
      `Prelude.hashWithSalt` publicDomainNames
      `Prelude.hashWithSalt` privateRegistryAccess
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` power
      `Prelude.hashWithSalt` scale

instance Prelude.NFData CreateContainerService where
  rnf CreateContainerService' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deployment
      `Prelude.seq` Prelude.rnf publicDomainNames
      `Prelude.seq` Prelude.rnf privateRegistryAccess
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf power
      `Prelude.seq` Prelude.rnf scale

instance Data.ToHeaders CreateContainerService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateContainerService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContainerService where
  toJSON CreateContainerService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("deployment" Data..=) Prelude.<$> deployment,
            ("publicDomainNames" Data..=)
              Prelude.<$> publicDomainNames,
            ("privateRegistryAccess" Data..=)
              Prelude.<$> privateRegistryAccess,
            Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("power" Data..= power),
            Prelude.Just ("scale" Data..= scale)
          ]
      )

instance Data.ToPath CreateContainerService where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateContainerService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContainerServiceResponse' smart constructor.
data CreateContainerServiceResponse = CreateContainerServiceResponse'
  { -- | An object that describes a container service.
    containerService :: Prelude.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerService', 'createContainerServiceResponse_containerService' - An object that describes a container service.
--
-- 'httpStatus', 'createContainerServiceResponse_httpStatus' - The response's http status code.
newCreateContainerServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContainerServiceResponse
newCreateContainerServiceResponse pHttpStatus_ =
  CreateContainerServiceResponse'
    { containerService =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes a container service.
createContainerServiceResponse_containerService :: Lens.Lens' CreateContainerServiceResponse (Prelude.Maybe ContainerService)
createContainerServiceResponse_containerService = Lens.lens (\CreateContainerServiceResponse' {containerService} -> containerService) (\s@CreateContainerServiceResponse' {} a -> s {containerService = a} :: CreateContainerServiceResponse)

-- | The response's http status code.
createContainerServiceResponse_httpStatus :: Lens.Lens' CreateContainerServiceResponse Prelude.Int
createContainerServiceResponse_httpStatus = Lens.lens (\CreateContainerServiceResponse' {httpStatus} -> httpStatus) (\s@CreateContainerServiceResponse' {} a -> s {httpStatus = a} :: CreateContainerServiceResponse)

instance
  Prelude.NFData
    CreateContainerServiceResponse
  where
  rnf CreateContainerServiceResponse' {..} =
    Prelude.rnf containerService
      `Prelude.seq` Prelude.rnf httpStatus
