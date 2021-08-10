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
-- Module      : Network.AWS.Lightsail.CreateContainerServiceDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment for your Amazon Lightsail container service.
--
-- A deployment specifies the containers that will be launched on the
-- container service and their settings, such as the ports to open, the
-- environment variables to apply, and the launch command to run. It also
-- specifies the container that will serve as the public endpoint of the
-- deployment and its settings, such as the HTTP or HTTPS port to use, and
-- the health check configuration.
--
-- You can deploy containers to your container service using container
-- images from a public registry like Docker Hub, or from your local
-- machine. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-creating-container-images Creating container images for your Amazon Lightsail container services>
-- in the /Lightsail Dev Guide/.
module Network.AWS.Lightsail.CreateContainerServiceDeployment
  ( -- * Creating a Request
    CreateContainerServiceDeployment (..),
    newCreateContainerServiceDeployment,

    -- * Request Lenses
    createContainerServiceDeployment_publicEndpoint,
    createContainerServiceDeployment_containers,
    createContainerServiceDeployment_serviceName,

    -- * Destructuring the Response
    CreateContainerServiceDeploymentResponse (..),
    newCreateContainerServiceDeploymentResponse,

    -- * Response Lenses
    createContainerServiceDeploymentResponse_containerService,
    createContainerServiceDeploymentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateContainerServiceDeployment' smart constructor.
data CreateContainerServiceDeployment = CreateContainerServiceDeployment'
  { -- | An object that describes the settings of the public endpoint for the
    -- container service.
    publicEndpoint :: Prelude.Maybe EndpointRequest,
    -- | An object that describes the settings of the containers that will be
    -- launched on the container service.
    containers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Container),
    -- | The name of the container service for which to create the deployment.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerServiceDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicEndpoint', 'createContainerServiceDeployment_publicEndpoint' - An object that describes the settings of the public endpoint for the
-- container service.
--
-- 'containers', 'createContainerServiceDeployment_containers' - An object that describes the settings of the containers that will be
-- launched on the container service.
--
-- 'serviceName', 'createContainerServiceDeployment_serviceName' - The name of the container service for which to create the deployment.
newCreateContainerServiceDeployment ::
  -- | 'serviceName'
  Prelude.Text ->
  CreateContainerServiceDeployment
newCreateContainerServiceDeployment pServiceName_ =
  CreateContainerServiceDeployment'
    { publicEndpoint =
        Prelude.Nothing,
      containers = Prelude.Nothing,
      serviceName = pServiceName_
    }

-- | An object that describes the settings of the public endpoint for the
-- container service.
createContainerServiceDeployment_publicEndpoint :: Lens.Lens' CreateContainerServiceDeployment (Prelude.Maybe EndpointRequest)
createContainerServiceDeployment_publicEndpoint = Lens.lens (\CreateContainerServiceDeployment' {publicEndpoint} -> publicEndpoint) (\s@CreateContainerServiceDeployment' {} a -> s {publicEndpoint = a} :: CreateContainerServiceDeployment)

-- | An object that describes the settings of the containers that will be
-- launched on the container service.
createContainerServiceDeployment_containers :: Lens.Lens' CreateContainerServiceDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Container))
createContainerServiceDeployment_containers = Lens.lens (\CreateContainerServiceDeployment' {containers} -> containers) (\s@CreateContainerServiceDeployment' {} a -> s {containers = a} :: CreateContainerServiceDeployment) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the container service for which to create the deployment.
createContainerServiceDeployment_serviceName :: Lens.Lens' CreateContainerServiceDeployment Prelude.Text
createContainerServiceDeployment_serviceName = Lens.lens (\CreateContainerServiceDeployment' {serviceName} -> serviceName) (\s@CreateContainerServiceDeployment' {} a -> s {serviceName = a} :: CreateContainerServiceDeployment)

instance
  Core.AWSRequest
    CreateContainerServiceDeployment
  where
  type
    AWSResponse CreateContainerServiceDeployment =
      CreateContainerServiceDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerServiceDeploymentResponse'
            Prelude.<$> (x Core..?> "containerService")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateContainerServiceDeployment

instance
  Prelude.NFData
    CreateContainerServiceDeployment

instance
  Core.ToHeaders
    CreateContainerServiceDeployment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateContainerServiceDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateContainerServiceDeployment where
  toJSON CreateContainerServiceDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("publicEndpoint" Core..=)
              Prelude.<$> publicEndpoint,
            ("containers" Core..=) Prelude.<$> containers,
            Prelude.Just ("serviceName" Core..= serviceName)
          ]
      )

instance Core.ToPath CreateContainerServiceDeployment where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateContainerServiceDeployment
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContainerServiceDeploymentResponse' smart constructor.
data CreateContainerServiceDeploymentResponse = CreateContainerServiceDeploymentResponse'
  { -- | An object that describes a container service.
    containerService :: Prelude.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerServiceDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerService', 'createContainerServiceDeploymentResponse_containerService' - An object that describes a container service.
--
-- 'httpStatus', 'createContainerServiceDeploymentResponse_httpStatus' - The response's http status code.
newCreateContainerServiceDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContainerServiceDeploymentResponse
newCreateContainerServiceDeploymentResponse
  pHttpStatus_ =
    CreateContainerServiceDeploymentResponse'
      { containerService =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that describes a container service.
createContainerServiceDeploymentResponse_containerService :: Lens.Lens' CreateContainerServiceDeploymentResponse (Prelude.Maybe ContainerService)
createContainerServiceDeploymentResponse_containerService = Lens.lens (\CreateContainerServiceDeploymentResponse' {containerService} -> containerService) (\s@CreateContainerServiceDeploymentResponse' {} a -> s {containerService = a} :: CreateContainerServiceDeploymentResponse)

-- | The response's http status code.
createContainerServiceDeploymentResponse_httpStatus :: Lens.Lens' CreateContainerServiceDeploymentResponse Prelude.Int
createContainerServiceDeploymentResponse_httpStatus = Lens.lens (\CreateContainerServiceDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateContainerServiceDeploymentResponse' {} a -> s {httpStatus = a} :: CreateContainerServiceDeploymentResponse)

instance
  Prelude.NFData
    CreateContainerServiceDeploymentResponse
