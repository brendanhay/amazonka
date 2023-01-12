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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- create a new revision of an existing deployment.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/create-deployments.html Create deployments>
-- in the /IoT Greengrass V2 Developer Guide/.
module Amazonka.GreengrassV2.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_clientToken,
    createDeployment_components,
    createDeployment_deploymentName,
    createDeployment_deploymentPolicies,
    createDeployment_iotJobConfiguration,
    createDeployment_parentTargetArn,
    createDeployment_tags,
    createDeployment_targetArn,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_iotJobArn,
    createDeploymentResponse_iotJobId,
    createDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | A unique, case-sensitive identifier that you can provide to ensure that
    -- the request is idempotent. Idempotency means that the request is
    -- successfully processed only once, even if you send the request multiple
    -- times. When a request succeeds, and you specify the same client token
    -- for subsequent successful requests, the IoT Greengrass V2 service
    -- returns the successful response that it caches from the previous
    -- request. IoT Greengrass V2 caches successful responses for idempotent
    -- requests for up to 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The components to deploy. This is a dictionary, where each key is the
    -- name of a component, and each key\'s value is the version and
    -- configuration to deploy for that component.
    components :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification),
    -- | The name of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | The deployment policies for the deployment. These policies define how
    -- the deployment updates components and handles failure.
    deploymentPolicies :: Prelude.Maybe DeploymentPolicies,
    -- | The job configuration for the deployment configuration. The job
    -- configuration specifies the rollout, timeout, and stop configurations
    -- for the deployment configuration.
    iotJobConfiguration :: Prelude.Maybe DeploymentIoTJobConfiguration,
    -- | The parent deployment\'s target
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- within a subdeployment.
    parentTargetArn :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group. When creating a subdeployment,
    -- the targetARN can only be a thing group.
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
-- 'clientToken', 'createDeployment_clientToken' - A unique, case-sensitive identifier that you can provide to ensure that
-- the request is idempotent. Idempotency means that the request is
-- successfully processed only once, even if you send the request multiple
-- times. When a request succeeds, and you specify the same client token
-- for subsequent successful requests, the IoT Greengrass V2 service
-- returns the successful response that it caches from the previous
-- request. IoT Greengrass V2 caches successful responses for idempotent
-- requests for up to 8 hours.
--
-- 'components', 'createDeployment_components' - The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
--
-- 'deploymentName', 'createDeployment_deploymentName' - The name of the deployment.
--
-- 'deploymentPolicies', 'createDeployment_deploymentPolicies' - The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
--
-- 'iotJobConfiguration', 'createDeployment_iotJobConfiguration' - The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
--
-- 'parentTargetArn', 'createDeployment_parentTargetArn' - The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
--
-- 'tags', 'createDeployment_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'targetArn', 'createDeployment_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group. When creating a subdeployment,
-- the targetARN can only be a thing group.
newCreateDeployment ::
  -- | 'targetArn'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment pTargetArn_ =
  CreateDeployment'
    { clientToken = Prelude.Nothing,
      components = Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      deploymentPolicies = Prelude.Nothing,
      iotJobConfiguration = Prelude.Nothing,
      parentTargetArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetArn = pTargetArn_
    }

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

-- | The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
createDeployment_components :: Lens.Lens' CreateDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification))
createDeployment_components = Lens.lens (\CreateDeployment' {components} -> components) (\s@CreateDeployment' {} a -> s {components = a} :: CreateDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the deployment.
createDeployment_deploymentName :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_deploymentName = Lens.lens (\CreateDeployment' {deploymentName} -> deploymentName) (\s@CreateDeployment' {} a -> s {deploymentName = a} :: CreateDeployment)

-- | The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
createDeployment_deploymentPolicies :: Lens.Lens' CreateDeployment (Prelude.Maybe DeploymentPolicies)
createDeployment_deploymentPolicies = Lens.lens (\CreateDeployment' {deploymentPolicies} -> deploymentPolicies) (\s@CreateDeployment' {} a -> s {deploymentPolicies = a} :: CreateDeployment)

-- | The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
createDeployment_iotJobConfiguration :: Lens.Lens' CreateDeployment (Prelude.Maybe DeploymentIoTJobConfiguration)
createDeployment_iotJobConfiguration = Lens.lens (\CreateDeployment' {iotJobConfiguration} -> iotJobConfiguration) (\s@CreateDeployment' {} a -> s {iotJobConfiguration = a} :: CreateDeployment)

-- | The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
createDeployment_parentTargetArn :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_parentTargetArn = Lens.lens (\CreateDeployment' {parentTargetArn} -> parentTargetArn) (\s@CreateDeployment' {} a -> s {parentTargetArn = a} :: CreateDeployment)

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
createDeployment_tags :: Lens.Lens' CreateDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeployment_tags = Lens.lens (\CreateDeployment' {tags} -> tags) (\s@CreateDeployment' {} a -> s {tags = a} :: CreateDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group. When creating a subdeployment,
-- the targetARN can only be a thing group.
createDeployment_targetArn :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_targetArn = Lens.lens (\CreateDeployment' {targetArn} -> targetArn) (\s@CreateDeployment' {} a -> s {targetArn = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type
    AWSResponse CreateDeployment =
      CreateDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Prelude.<$> (x Data..?> "deploymentId")
            Prelude.<*> (x Data..?> "iotJobArn")
            Prelude.<*> (x Data..?> "iotJobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeployment where
  hashWithSalt _salt CreateDeployment' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` deploymentName
      `Prelude.hashWithSalt` deploymentPolicies
      `Prelude.hashWithSalt` iotJobConfiguration
      `Prelude.hashWithSalt` parentTargetArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData CreateDeployment where
  rnf CreateDeployment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf deploymentName
      `Prelude.seq` Prelude.rnf deploymentPolicies
      `Prelude.seq` Prelude.rnf iotJobConfiguration
      `Prelude.seq` Prelude.rnf parentTargetArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetArn

instance Data.ToHeaders CreateDeployment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("components" Data..=) Prelude.<$> components,
            ("deploymentName" Data..=)
              Prelude.<$> deploymentName,
            ("deploymentPolicies" Data..=)
              Prelude.<$> deploymentPolicies,
            ("iotJobConfiguration" Data..=)
              Prelude.<$> iotJobConfiguration,
            ("parentTargetArn" Data..=)
              Prelude.<$> parentTargetArn,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("targetArn" Data..= targetArn)
          ]
      )

instance Data.ToPath CreateDeployment where
  toPath = Prelude.const "/greengrass/v2/deployments"

instance Data.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IoT job that applies the deployment to target devices.
    iotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IoT job that applies the deployment to target devices.
    iotJobId :: Prelude.Maybe Prelude.Text,
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
-- 'iotJobArn', 'createDeploymentResponse_iotJobArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
--
-- 'iotJobId', 'createDeploymentResponse_iotJobId' - The ID of the IoT job that applies the deployment to target devices.
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
      iotJobArn = Prelude.Nothing,
      iotJobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deployment.
createDeploymentResponse_deploymentId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_deploymentId = Lens.lens (\CreateDeploymentResponse' {deploymentId} -> deploymentId) (\s@CreateDeploymentResponse' {} a -> s {deploymentId = a} :: CreateDeploymentResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
createDeploymentResponse_iotJobArn :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_iotJobArn = Lens.lens (\CreateDeploymentResponse' {iotJobArn} -> iotJobArn) (\s@CreateDeploymentResponse' {} a -> s {iotJobArn = a} :: CreateDeploymentResponse)

-- | The ID of the IoT job that applies the deployment to target devices.
createDeploymentResponse_iotJobId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_iotJobId = Lens.lens (\CreateDeploymentResponse' {iotJobId} -> iotJobId) (\s@CreateDeploymentResponse' {} a -> s {iotJobId = a} :: CreateDeploymentResponse)

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Prelude.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

instance Prelude.NFData CreateDeploymentResponse where
  rnf CreateDeploymentResponse' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf iotJobArn
      `Prelude.seq` Prelude.rnf iotJobId
      `Prelude.seq` Prelude.rnf httpStatus
