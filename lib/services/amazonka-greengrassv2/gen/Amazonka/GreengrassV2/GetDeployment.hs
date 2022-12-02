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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_tags,
    getDeploymentResponse_iotJobConfiguration,
    getDeploymentResponse_iotJobArn,
    getDeploymentResponse_iotJobId,
    getDeploymentResponse_deploymentName,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_targetArn,
    getDeploymentResponse_isLatestForTarget,
    getDeploymentResponse_creationTimestamp,
    getDeploymentResponse_parentTargetArn,
    getDeploymentResponse_deploymentPolicies,
    getDeploymentResponse_components,
    getDeploymentResponse_revisionId,
    getDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentResponse'
            Prelude.<$> (x Data..?> "deploymentStatus")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "iotJobConfiguration")
            Prelude.<*> (x Data..?> "iotJobArn")
            Prelude.<*> (x Data..?> "iotJobId")
            Prelude.<*> (x Data..?> "deploymentName")
            Prelude.<*> (x Data..?> "deploymentId")
            Prelude.<*> (x Data..?> "targetArn")
            Prelude.<*> (x Data..?> "isLatestForTarget")
            Prelude.<*> (x Data..?> "creationTimestamp")
            Prelude.<*> (x Data..?> "parentTargetArn")
            Prelude.<*> (x Data..?> "deploymentPolicies")
            Prelude.<*> (x Data..?> "components" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployment where
  hashWithSalt _salt GetDeployment' {..} =
    _salt `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData GetDeployment where
  rnf GetDeployment' {..} = Prelude.rnf deploymentId

instance Data.ToHeaders GetDeployment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetDeployment where
  toPath GetDeployment' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/deployments/",
        Data.toBS deploymentId
      ]

instance Data.ToQuery GetDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { -- | The status of the deployment.
    deploymentStatus :: Prelude.Maybe DeploymentStatus,
    -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The job configuration for the deployment configuration. The job
    -- configuration specifies the rollout, timeout, and stop configurations
    -- for the deployment configuration.
    iotJobConfiguration :: Prelude.Maybe DeploymentIoTJobConfiguration,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IoT job that applies the deployment to target devices.
    iotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IoT job that applies the deployment to target devices.
    iotJobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the deployment is the latest revision for its target.
    isLatestForTarget :: Prelude.Maybe Prelude.Bool,
    -- | The time at which the deployment was created, expressed in ISO 8601
    -- format.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The parent deployment\'s target
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- within a subdeployment.
    parentTargetArn :: Prelude.Maybe Prelude.Text,
    -- | The deployment policies for the deployment. These policies define how
    -- the deployment updates components and handles failure.
    deploymentPolicies :: Prelude.Maybe DeploymentPolicies,
    -- | The components to deploy. This is a dictionary, where each key is the
    -- name of a component, and each key\'s value is the version and
    -- configuration to deploy for that component.
    components :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification),
    -- | The revision number of the deployment.
    revisionId :: Prelude.Maybe Prelude.Text,
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
-- 'deploymentStatus', 'getDeploymentResponse_deploymentStatus' - The status of the deployment.
--
-- 'tags', 'getDeploymentResponse_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'iotJobConfiguration', 'getDeploymentResponse_iotJobConfiguration' - The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
--
-- 'iotJobArn', 'getDeploymentResponse_iotJobArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
--
-- 'iotJobId', 'getDeploymentResponse_iotJobId' - The ID of the IoT job that applies the deployment to target devices.
--
-- 'deploymentName', 'getDeploymentResponse_deploymentName' - The name of the deployment.
--
-- 'deploymentId', 'getDeploymentResponse_deploymentId' - The ID of the deployment.
--
-- 'targetArn', 'getDeploymentResponse_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
--
-- 'isLatestForTarget', 'getDeploymentResponse_isLatestForTarget' - Whether or not the deployment is the latest revision for its target.
--
-- 'creationTimestamp', 'getDeploymentResponse_creationTimestamp' - The time at which the deployment was created, expressed in ISO 8601
-- format.
--
-- 'parentTargetArn', 'getDeploymentResponse_parentTargetArn' - The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
--
-- 'deploymentPolicies', 'getDeploymentResponse_deploymentPolicies' - The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
--
-- 'components', 'getDeploymentResponse_components' - The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
--
-- 'revisionId', 'getDeploymentResponse_revisionId' - The revision number of the deployment.
--
-- 'httpStatus', 'getDeploymentResponse_httpStatus' - The response's http status code.
newGetDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentResponse
newGetDeploymentResponse pHttpStatus_ =
  GetDeploymentResponse'
    { deploymentStatus =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      iotJobConfiguration = Prelude.Nothing,
      iotJobArn = Prelude.Nothing,
      iotJobId = Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      isLatestForTarget = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      parentTargetArn = Prelude.Nothing,
      deploymentPolicies = Prelude.Nothing,
      components = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the deployment.
getDeploymentResponse_deploymentStatus :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentStatus)
getDeploymentResponse_deploymentStatus = Lens.lens (\GetDeploymentResponse' {deploymentStatus} -> deploymentStatus) (\s@GetDeploymentResponse' {} a -> s {deploymentStatus = a} :: GetDeploymentResponse)

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
getDeploymentResponse_tags :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDeploymentResponse_tags = Lens.lens (\GetDeploymentResponse' {tags} -> tags) (\s@GetDeploymentResponse' {} a -> s {tags = a} :: GetDeploymentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The job configuration for the deployment configuration. The job
-- configuration specifies the rollout, timeout, and stop configurations
-- for the deployment configuration.
getDeploymentResponse_iotJobConfiguration :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentIoTJobConfiguration)
getDeploymentResponse_iotJobConfiguration = Lens.lens (\GetDeploymentResponse' {iotJobConfiguration} -> iotJobConfiguration) (\s@GetDeploymentResponse' {} a -> s {iotJobConfiguration = a} :: GetDeploymentResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
getDeploymentResponse_iotJobArn :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_iotJobArn = Lens.lens (\GetDeploymentResponse' {iotJobArn} -> iotJobArn) (\s@GetDeploymentResponse' {} a -> s {iotJobArn = a} :: GetDeploymentResponse)

-- | The ID of the IoT job that applies the deployment to target devices.
getDeploymentResponse_iotJobId :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_iotJobId = Lens.lens (\GetDeploymentResponse' {iotJobId} -> iotJobId) (\s@GetDeploymentResponse' {} a -> s {iotJobId = a} :: GetDeploymentResponse)

-- | The name of the deployment.
getDeploymentResponse_deploymentName :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_deploymentName = Lens.lens (\GetDeploymentResponse' {deploymentName} -> deploymentName) (\s@GetDeploymentResponse' {} a -> s {deploymentName = a} :: GetDeploymentResponse)

-- | The ID of the deployment.
getDeploymentResponse_deploymentId :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_deploymentId = Lens.lens (\GetDeploymentResponse' {deploymentId} -> deploymentId) (\s@GetDeploymentResponse' {} a -> s {deploymentId = a} :: GetDeploymentResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
getDeploymentResponse_targetArn :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_targetArn = Lens.lens (\GetDeploymentResponse' {targetArn} -> targetArn) (\s@GetDeploymentResponse' {} a -> s {targetArn = a} :: GetDeploymentResponse)

-- | Whether or not the deployment is the latest revision for its target.
getDeploymentResponse_isLatestForTarget :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Bool)
getDeploymentResponse_isLatestForTarget = Lens.lens (\GetDeploymentResponse' {isLatestForTarget} -> isLatestForTarget) (\s@GetDeploymentResponse' {} a -> s {isLatestForTarget = a} :: GetDeploymentResponse)

-- | The time at which the deployment was created, expressed in ISO 8601
-- format.
getDeploymentResponse_creationTimestamp :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.UTCTime)
getDeploymentResponse_creationTimestamp = Lens.lens (\GetDeploymentResponse' {creationTimestamp} -> creationTimestamp) (\s@GetDeploymentResponse' {} a -> s {creationTimestamp = a} :: GetDeploymentResponse) Prelude.. Lens.mapping Data._Time

-- | The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
getDeploymentResponse_parentTargetArn :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_parentTargetArn = Lens.lens (\GetDeploymentResponse' {parentTargetArn} -> parentTargetArn) (\s@GetDeploymentResponse' {} a -> s {parentTargetArn = a} :: GetDeploymentResponse)

-- | The deployment policies for the deployment. These policies define how
-- the deployment updates components and handles failure.
getDeploymentResponse_deploymentPolicies :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentPolicies)
getDeploymentResponse_deploymentPolicies = Lens.lens (\GetDeploymentResponse' {deploymentPolicies} -> deploymentPolicies) (\s@GetDeploymentResponse' {} a -> s {deploymentPolicies = a} :: GetDeploymentResponse)

-- | The components to deploy. This is a dictionary, where each key is the
-- name of a component, and each key\'s value is the version and
-- configuration to deploy for that component.
getDeploymentResponse_components :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDeploymentSpecification))
getDeploymentResponse_components = Lens.lens (\GetDeploymentResponse' {components} -> components) (\s@GetDeploymentResponse' {} a -> s {components = a} :: GetDeploymentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The revision number of the deployment.
getDeploymentResponse_revisionId :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_revisionId = Lens.lens (\GetDeploymentResponse' {revisionId} -> revisionId) (\s@GetDeploymentResponse' {} a -> s {revisionId = a} :: GetDeploymentResponse)

-- | The response's http status code.
getDeploymentResponse_httpStatus :: Lens.Lens' GetDeploymentResponse Prelude.Int
getDeploymentResponse_httpStatus = Lens.lens (\GetDeploymentResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentResponse' {} a -> s {httpStatus = a} :: GetDeploymentResponse)

instance Prelude.NFData GetDeploymentResponse where
  rnf GetDeploymentResponse' {..} =
    Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf iotJobConfiguration
      `Prelude.seq` Prelude.rnf iotJobArn
      `Prelude.seq` Prelude.rnf iotJobId
      `Prelude.seq` Prelude.rnf deploymentName
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf isLatestForTarget
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf parentTargetArn
      `Prelude.seq` Prelude.rnf deploymentPolicies
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf httpStatus
