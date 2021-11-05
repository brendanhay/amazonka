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
-- Module      : Amazonka.GreengrassV2.GetDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a deployment. Deployments define the components that run on
-- Greengrass core devices.
module Amazonka.GreengrassV2.GetDeployment
  ( -- * Creating a Request
    GetDeployment (..),
    newGetDeployment,

    -- * Request Lenses
    getDeployment_deploymentId,

    -- * Destructuring the Response
    GetDeploymentResponse (..),
    newGetDeploymentResponse,

    -- * Response Lenses
    getDeploymentResponse_targetArn,
    getDeploymentResponse_components,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_iotJobId,
    getDeploymentResponse_iotJobArn,
    getDeploymentResponse_deploymentPolicies,
    getDeploymentResponse_creationTimestamp,
    getDeploymentResponse_iotJobConfiguration,
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_isLatestForTarget,
    getDeploymentResponse_revisionId,
    getDeploymentResponse_deploymentName,
    getDeploymentResponse_tags,
    getDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { -- | The ID of the deployment.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'getDeployment_deploymentId' - The ID of the deployment.
newGetDeployment ::
  -- | 'deploymentId'
  Prelude.Text ->
  GetDeployment
newGetDeployment pDeploymentId_ =
  GetDeployment' {deploymentId = pDeploymentId_}

-- | The ID of the deployment.
getDeployment_deploymentId :: Lens.Lens' GetDeployment Prelude.Text
getDeployment_deploymentId = Lens.lens (\GetDeployment' {deploymentId} -> deploymentId) (\s@GetDeployment' {} a -> s {deploymentId = a} :: GetDeployment)

instance Core.AWSRequest GetDeployment where
  type
    AWSResponse GetDeployment =
      GetDeploymentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentResponse'
            Prelude.<$> (x Core..?> "targetArn")
            Prelude.<*> (x Core..?> "components" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "deploymentId")
            Prelude.<*> (x Core..?> "iotJobId")
            Prelude.<*> (x Core..?> "iotJobArn")
            Prelude.<*> (x Core..?> "deploymentPolicies")
            Prelude.<*> (x Core..?> "creationTimestamp")
            Prelude.<*> (x Core..?> "iotJobConfiguration")
            Prelude.<*> (x Core..?> "deploymentStatus")
            Prelude.<*> (x Core..?> "isLatestForTarget")
            Prelude.<*> (x Core..?> "revisionId")
            Prelude.<*> (x Core..?> "deploymentName")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployment

instance Prelude.NFData GetDeployment

instance Core.ToHeaders GetDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDeployment where
  toPath GetDeployment' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/deployments/",
        Core.toBS deploymentId
      ]

instance Core.ToQuery GetDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The components to deploy. This is a dictionary, where each key is the
    -- name of a component, and each key\'s value is the version and
    -- configuration to deploy for that component.
    components :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification),
    -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IoT job that applies the deployment to target devices.
    iotJobId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IoT job that applies the deployment to target devices.
    iotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The deployment policies for the deployment. These policies define how
    -- the deployment updates components and handles failure.
    deploymentPolicies :: Prelude.Maybe DeploymentPolicies,
    -- | The time at which the deployment was created, expressed in ISO 8601
    -- format.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The job configuration for the deployment configuration. The job
    -- configuration specifies the rollout, timeout, and stop configurations
    -- for the deployment configuration.
    iotJobConfiguration :: Prelude.Maybe DeploymentIoTJobConfiguration,
    -- | The status of the deployment.
    deploymentStatus :: Prelude.Maybe DeploymentStatus,
    -- | Whether or not the deployment is the latest revision for its target.
    isLatestForTarget :: Prelude.Maybe Prelude.Bool,
    -- | The revision number of the deployment.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetArn', 'getDeploymentResponse_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
--
-- 'components', 'getDeploymentResponse_components' - The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
--
-- 'deploymentId', 'getDeploymentResponse_deploymentId' - The ID of the deployment.
--
-- 'iotJobId', 'getDeploymentResponse_iotJobId' - The ID of the IoT job that applies the deployment to target devices.
--
-- 'iotJobArn', 'getDeploymentResponse_iotJobArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
--
-- 'deploymentPolicies', 'getDeploymentResponse_deploymentPolicies' - The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
--
-- 'creationTimestamp', 'getDeploymentResponse_creationTimestamp' - The time at which the deployment was created, expressed in ISO 8601
-- format.
--
-- 'iotJobConfiguration', 'getDeploymentResponse_iotJobConfiguration' - The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
--
-- 'deploymentStatus', 'getDeploymentResponse_deploymentStatus' - The status of the deployment.
--
-- 'isLatestForTarget', 'getDeploymentResponse_isLatestForTarget' - Whether or not the deployment is the latest revision for its target.
--
-- 'revisionId', 'getDeploymentResponse_revisionId' - The revision number of the deployment.
--
-- 'deploymentName', 'getDeploymentResponse_deploymentName' - The name of the deployment.
--
-- 'tags', 'getDeploymentResponse_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'httpStatus', 'getDeploymentResponse_httpStatus' - The response's http status code.
newGetDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentResponse
newGetDeploymentResponse pHttpStatus_ =
  GetDeploymentResponse'
    { targetArn = Prelude.Nothing,
      components = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      iotJobId = Prelude.Nothing,
      iotJobArn = Prelude.Nothing,
      deploymentPolicies = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      iotJobConfiguration = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      isLatestForTarget = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
getDeploymentResponse_targetArn :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_targetArn = Lens.lens (\GetDeploymentResponse' {targetArn} -> targetArn) (\s@GetDeploymentResponse' {} a -> s {targetArn = a} :: GetDeploymentResponse)

-- | The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
getDeploymentResponse_components :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification))
getDeploymentResponse_components = Lens.lens (\GetDeploymentResponse' {components} -> components) (\s@GetDeploymentResponse' {} a -> s {components = a} :: GetDeploymentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the deployment.
getDeploymentResponse_deploymentId :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_deploymentId = Lens.lens (\GetDeploymentResponse' {deploymentId} -> deploymentId) (\s@GetDeploymentResponse' {} a -> s {deploymentId = a} :: GetDeploymentResponse)

-- | The ID of the IoT job that applies the deployment to target devices.
getDeploymentResponse_iotJobId :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_iotJobId = Lens.lens (\GetDeploymentResponse' {iotJobId} -> iotJobId) (\s@GetDeploymentResponse' {} a -> s {iotJobId = a} :: GetDeploymentResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
getDeploymentResponse_iotJobArn :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_iotJobArn = Lens.lens (\GetDeploymentResponse' {iotJobArn} -> iotJobArn) (\s@GetDeploymentResponse' {} a -> s {iotJobArn = a} :: GetDeploymentResponse)

-- | The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
getDeploymentResponse_deploymentPolicies :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentPolicies)
getDeploymentResponse_deploymentPolicies = Lens.lens (\GetDeploymentResponse' {deploymentPolicies} -> deploymentPolicies) (\s@GetDeploymentResponse' {} a -> s {deploymentPolicies = a} :: GetDeploymentResponse)

-- | The time at which the deployment was created, expressed in ISO 8601
-- format.
getDeploymentResponse_creationTimestamp :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.UTCTime)
getDeploymentResponse_creationTimestamp = Lens.lens (\GetDeploymentResponse' {creationTimestamp} -> creationTimestamp) (\s@GetDeploymentResponse' {} a -> s {creationTimestamp = a} :: GetDeploymentResponse) Prelude.. Lens.mapping Core._Time

-- | The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
getDeploymentResponse_iotJobConfiguration :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentIoTJobConfiguration)
getDeploymentResponse_iotJobConfiguration = Lens.lens (\GetDeploymentResponse' {iotJobConfiguration} -> iotJobConfiguration) (\s@GetDeploymentResponse' {} a -> s {iotJobConfiguration = a} :: GetDeploymentResponse)

-- | The status of the deployment.
getDeploymentResponse_deploymentStatus :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentStatus)
getDeploymentResponse_deploymentStatus = Lens.lens (\GetDeploymentResponse' {deploymentStatus} -> deploymentStatus) (\s@GetDeploymentResponse' {} a -> s {deploymentStatus = a} :: GetDeploymentResponse)

-- | Whether or not the deployment is the latest revision for its target.
getDeploymentResponse_isLatestForTarget :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Bool)
getDeploymentResponse_isLatestForTarget = Lens.lens (\GetDeploymentResponse' {isLatestForTarget} -> isLatestForTarget) (\s@GetDeploymentResponse' {} a -> s {isLatestForTarget = a} :: GetDeploymentResponse)

-- | The revision number of the deployment.
getDeploymentResponse_revisionId :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_revisionId = Lens.lens (\GetDeploymentResponse' {revisionId} -> revisionId) (\s@GetDeploymentResponse' {} a -> s {revisionId = a} :: GetDeploymentResponse)

-- | The name of the deployment.
getDeploymentResponse_deploymentName :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_deploymentName = Lens.lens (\GetDeploymentResponse' {deploymentName} -> deploymentName) (\s@GetDeploymentResponse' {} a -> s {deploymentName = a} :: GetDeploymentResponse)

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
getDeploymentResponse_tags :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDeploymentResponse_tags = Lens.lens (\GetDeploymentResponse' {tags} -> tags) (\s@GetDeploymentResponse' {} a -> s {tags = a} :: GetDeploymentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDeploymentResponse_httpStatus :: Lens.Lens' GetDeploymentResponse Prelude.Int
getDeploymentResponse_httpStatus = Lens.lens (\GetDeploymentResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentResponse' {} a -> s {httpStatus = a} :: GetDeploymentResponse)

instance Prelude.NFData GetDeploymentResponse
