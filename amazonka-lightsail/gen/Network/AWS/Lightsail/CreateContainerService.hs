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
-- Module      : Network.AWS.Lightsail.CreateContainerService
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Lightsail.CreateContainerService
  ( -- * Creating a Request
    CreateContainerService (..),
    newCreateContainerService,

    -- * Request Lenses
    createContainerService_deployment,
    createContainerService_tags,
    createContainerService_publicDomainNames,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateContainerService' smart constructor.
data CreateContainerService = CreateContainerService'
  { -- | An object that describes a deployment for the container service.
    --
    -- A deployment specifies the containers that will be launched on the
    -- container service and their settings, such as the ports to open, the
    -- environment variables to apply, and the launch command to run. It also
    -- specifies the container that will serve as the public endpoint of the
    -- deployment and its settings, such as the HTTP or HTTPS port to use, and
    -- the health check configuration.
    deployment :: Prelude.Maybe ContainerServiceDeploymentRequest,
    -- | The tag keys and optional values for the container service.
    --
    -- For more information about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
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
    -- | The name for the container service.
    --
    -- The name that you specify for your container service will make up part
    -- of its default domain. The default domain of a container service is
    -- typically
    -- @https:\/\/\<ServiceName>.\<RandomGUID>.\<AWSRegion>.cs.amazonlightsail.com@.
    -- If the name of your container service is @container-service-1@, and
    -- it\'s located in the US East (Ohio) AWS region (@us-east-2@), then the
    -- domain for your container service will be like the following example:
    -- @https:\/\/container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@
    --
    -- The following are the requirements for container service names:
    --
    -- -   Must be unique within each AWS Region in your Lightsail account.
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
-- 'deployment', 'createContainerService_deployment' - An object that describes a deployment for the container service.
--
-- A deployment specifies the containers that will be launched on the
-- container service and their settings, such as the ports to open, the
-- environment variables to apply, and the launch command to run. It also
-- specifies the container that will serve as the public endpoint of the
-- deployment and its settings, such as the HTTP or HTTPS port to use, and
-- the health check configuration.
--
-- 'tags', 'createContainerService_tags' - The tag keys and optional values for the container service.
--
-- For more information about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
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
-- 'serviceName', 'createContainerService_serviceName' - The name for the container service.
--
-- The name that you specify for your container service will make up part
-- of its default domain. The default domain of a container service is
-- typically
-- @https:\/\/\<ServiceName>.\<RandomGUID>.\<AWSRegion>.cs.amazonlightsail.com@.
-- If the name of your container service is @container-service-1@, and
-- it\'s located in the US East (Ohio) AWS region (@us-east-2@), then the
-- domain for your container service will be like the following example:
-- @https:\/\/container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@
--
-- The following are the requirements for container service names:
--
-- -   Must be unique within each AWS Region in your Lightsail account.
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
      { deployment =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        publicDomainNames = Prelude.Nothing,
        serviceName = pServiceName_,
        power = pPower_,
        scale = pScale_
      }

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

-- | The tag keys and optional values for the container service.
--
-- For more information about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
createContainerService_tags :: Lens.Lens' CreateContainerService (Prelude.Maybe [Tag])
createContainerService_tags = Lens.lens (\CreateContainerService' {tags} -> tags) (\s@CreateContainerService' {} a -> s {tags = a} :: CreateContainerService) Prelude.. Lens.mapping Lens._Coerce

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
createContainerService_publicDomainNames = Lens.lens (\CreateContainerService' {publicDomainNames} -> publicDomainNames) (\s@CreateContainerService' {} a -> s {publicDomainNames = a} :: CreateContainerService) Prelude.. Lens.mapping Lens._Coerce

-- | The name for the container service.
--
-- The name that you specify for your container service will make up part
-- of its default domain. The default domain of a container service is
-- typically
-- @https:\/\/\<ServiceName>.\<RandomGUID>.\<AWSRegion>.cs.amazonlightsail.com@.
-- If the name of your container service is @container-service-1@, and
-- it\'s located in the US East (Ohio) AWS region (@us-east-2@), then the
-- domain for your container service will be like the following example:
-- @https:\/\/container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@
--
-- The following are the requirements for container service names:
--
-- -   Must be unique within each AWS Region in your Lightsail account.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerServiceResponse'
            Prelude.<$> (x Core..?> "containerService")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContainerService

instance Prelude.NFData CreateContainerService

instance Core.ToHeaders CreateContainerService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateContainerService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateContainerService where
  toJSON CreateContainerService' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deployment" Core..=) Prelude.<$> deployment,
            ("tags" Core..=) Prelude.<$> tags,
            ("publicDomainNames" Core..=)
              Prelude.<$> publicDomainNames,
            Prelude.Just ("serviceName" Core..= serviceName),
            Prelude.Just ("power" Core..= power),
            Prelude.Just ("scale" Core..= scale)
          ]
      )

instance Core.ToPath CreateContainerService where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateContainerService where
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
