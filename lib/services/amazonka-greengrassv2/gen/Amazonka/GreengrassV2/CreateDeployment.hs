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
-- Module      : Amazonka.GreengrassV2.CreateDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a continuous deployment for a target, which is a Greengrass core
-- device or group of core devices. When you add a new core device to a
-- group of core devices that has a deployment, IoT Greengrass deploys that
-- group\'s deployment to the new device.
--
-- You can define one deployment for each target. When you create a new
-- deployment for a target that has an existing deployment, you replace the
-- previous deployment. IoT Greengrass applies the new deployment to the
-- target devices.
--
-- Every deployment has a revision number that indicates how many
-- deployment revisions you define for a target. Use this operation to
-- create a new revision of an existing deployment. This operation returns
-- the revision number of the new deployment when you create it.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/create-deployments.html Create deployments>
-- in the /IoT Greengrass V2 Developer Guide/.
module Amazonka.GreengrassV2.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_components,
    createDeployment_clientToken,
    createDeployment_deploymentPolicies,
    createDeployment_iotJobConfiguration,
    createDeployment_deploymentName,
    createDeployment_tags,
    createDeployment_targetArn,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_iotJobId,
    createDeploymentResponse_iotJobArn,
    createDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | The components to deploy. This is a dictionary, where each key is the
    -- name of a component, and each key\'s value is the version and
    -- configuration to deploy for that component.
    components :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification),
    -- | A unique, case-sensitive identifier that you can provide to ensure that
    -- the request is idempotent. Idempotency means that the request is
    -- successfully processed only once, even if you send the request multiple
    -- times. When a request succeeds, and you specify the same client token
    -- for subsequent successful requests, the IoT Greengrass V2 service
    -- returns the successful response that it caches from the previous
    -- request. IoT Greengrass V2 caches successful responses for idempotent
    -- requests for up to 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The deployment policies for the deployment. These policies define how
    -- the deployment updates components and handles failure.
    deploymentPolicies :: Prelude.Maybe DeploymentPolicies,
    -- | The job configuration for the deployment configuration. The job
    -- configuration specifies the rollout, timeout, and stop configurations
    -- for the deployment configuration.
    iotJobConfiguration :: Prelude.Maybe DeploymentIoTJobConfiguration,
    -- | The name of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group.
    targetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'components', 'createDeployment_components' - The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
--
-- 'clientToken', 'createDeployment_clientToken' - A unique, case-sensitive identifier that you can provide to ensure that
-- the request is idempotent. Idempotency means that the request is
-- successfully processed only once, even if you send the request multiple
-- times. When a request succeeds, and you specify the same client token
-- for subsequent successful requests, the IoT Greengrass V2 service
-- returns the successful response that it caches from the previous
-- request. IoT Greengrass V2 caches successful responses for idempotent
-- requests for up to 8 hours.
--
-- 'deploymentPolicies', 'createDeployment_deploymentPolicies' - The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
--
-- 'iotJobConfiguration', 'createDeployment_iotJobConfiguration' - The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
--
-- 'deploymentName', 'createDeployment_deploymentName' - The name of the deployment.
--
-- 'tags', 'createDeployment_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'targetArn', 'createDeployment_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
newCreateDeployment ::
  -- | 'targetArn'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment pTargetArn_ =
  CreateDeployment'
    { components = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      deploymentPolicies = Prelude.Nothing,
      iotJobConfiguration = Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetArn = pTargetArn_
    }

-- | The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
createDeployment_components :: Lens.Lens' CreateDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification))
createDeployment_components = Lens.lens (\CreateDeployment' {components} -> components) (\s@CreateDeployment' {} a -> s {components = a} :: CreateDeployment) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive identifier that you can provide to ensure that
-- the request is idempotent. Idempotency means that the request is
-- successfully processed only once, even if you send the request multiple
-- times. When a request succeeds, and you specify the same client token
-- for subsequent successful requests, the IoT Greengrass V2 service
-- returns the successful response that it caches from the previous
-- request. IoT Greengrass V2 caches successful responses for idempotent
-- requests for up to 8 hours.
createDeployment_clientToken :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_clientToken = Lens.lens (\CreateDeployment' {clientToken} -> clientToken) (\s@CreateDeployment' {} a -> s {clientToken = a} :: CreateDeployment)

-- | The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
createDeployment_deploymentPolicies :: Lens.Lens' CreateDeployment (Prelude.Maybe DeploymentPolicies)
createDeployment_deploymentPolicies = Lens.lens (\CreateDeployment' {deploymentPolicies} -> deploymentPolicies) (\s@CreateDeployment' {} a -> s {deploymentPolicies = a} :: CreateDeployment)

-- | The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
createDeployment_iotJobConfiguration :: Lens.Lens' CreateDeployment (Prelude.Maybe DeploymentIoTJobConfiguration)
createDeployment_iotJobConfiguration = Lens.lens (\CreateDeployment' {iotJobConfiguration} -> iotJobConfiguration) (\s@CreateDeployment' {} a -> s {iotJobConfiguration = a} :: CreateDeployment)

-- | The name of the deployment.
createDeployment_deploymentName :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_deploymentName = Lens.lens (\CreateDeployment' {deploymentName} -> deploymentName) (\s@CreateDeployment' {} a -> s {deploymentName = a} :: CreateDeployment)

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
createDeployment_tags :: Lens.Lens' CreateDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeployment_tags = Lens.lens (\CreateDeployment' {tags} -> tags) (\s@CreateDeployment' {} a -> s {tags = a} :: CreateDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
createDeployment_targetArn :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_targetArn = Lens.lens (\CreateDeployment' {targetArn} -> targetArn) (\s@CreateDeployment' {} a -> s {targetArn = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type
    AWSResponse CreateDeployment =
      CreateDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Prelude.<$> (x Core..?> "deploymentId")
            Prelude.<*> (x Core..?> "iotJobId")
            Prelude.<*> (x Core..?> "iotJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeployment where
  hashWithSalt _salt CreateDeployment' {..} =
    _salt `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` deploymentPolicies
      `Prelude.hashWithSalt` iotJobConfiguration
      `Prelude.hashWithSalt` deploymentName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData CreateDeployment where
  rnf CreateDeployment' {..} =
    Prelude.rnf components
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf deploymentPolicies
      `Prelude.seq` Prelude.rnf iotJobConfiguration
      `Prelude.seq` Prelude.rnf deploymentName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetArn

instance Core.ToHeaders CreateDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("components" Core..=) Prelude.<$> components,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("deploymentPolicies" Core..=)
              Prelude.<$> deploymentPolicies,
            ("iotJobConfiguration" Core..=)
              Prelude.<$> iotJobConfiguration,
            ("deploymentName" Core..=)
              Prelude.<$> deploymentName,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("targetArn" Core..= targetArn)
          ]
      )

instance Core.ToPath CreateDeployment where
  toPath = Prelude.const "/greengrass/v2/deployments"

instance Core.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IoT job that applies the deployment to target devices.
    iotJobId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IoT job that applies the deployment to target devices.
    iotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'createDeploymentResponse_deploymentId' - The ID of the deployment.
--
-- 'iotJobId', 'createDeploymentResponse_iotJobId' - The ID of the IoT job that applies the deployment to target devices.
--
-- 'iotJobArn', 'createDeploymentResponse_iotJobArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
--
-- 'httpStatus', 'createDeploymentResponse_httpStatus' - The response's http status code.
newCreateDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeploymentResponse
newCreateDeploymentResponse pHttpStatus_ =
  CreateDeploymentResponse'
    { deploymentId =
        Prelude.Nothing,
      iotJobId = Prelude.Nothing,
      iotJobArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deployment.
createDeploymentResponse_deploymentId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_deploymentId = Lens.lens (\CreateDeploymentResponse' {deploymentId} -> deploymentId) (\s@CreateDeploymentResponse' {} a -> s {deploymentId = a} :: CreateDeploymentResponse)

-- | The ID of the IoT job that applies the deployment to target devices.
createDeploymentResponse_iotJobId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_iotJobId = Lens.lens (\CreateDeploymentResponse' {iotJobId} -> iotJobId) (\s@CreateDeploymentResponse' {} a -> s {iotJobId = a} :: CreateDeploymentResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
createDeploymentResponse_iotJobArn :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_iotJobArn = Lens.lens (\CreateDeploymentResponse' {iotJobArn} -> iotJobArn) (\s@CreateDeploymentResponse' {} a -> s {iotJobArn = a} :: CreateDeploymentResponse)

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Prelude.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

instance Prelude.NFData CreateDeploymentResponse where
  rnf CreateDeploymentResponse' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf iotJobId
      `Prelude.seq` Prelude.rnf iotJobArn
      `Prelude.seq` Prelude.rnf httpStatus
