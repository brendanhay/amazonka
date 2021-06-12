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
-- Module      : Network.AWS.CodeDeploy.BatchGetDeploymentTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of one or more targets associated with a deployment.
-- This method works with all compute types and should be used instead of
-- the deprecated @BatchGetDeploymentInstances@. The maximum number of
-- targets that can be returned is 25.
--
-- The type of targets returned depends on the deployment\'s compute
-- platform or deployment method:
--
-- -   __EC2\/On-premises__: Information about EC2 instance targets.
--
-- -   __AWS Lambda__: Information about Lambda functions targets.
--
-- -   __Amazon ECS__: Information about Amazon ECS service targets.
--
-- -   __CloudFormation__: Information about targets of blue\/green
--     deployments initiated by a CloudFormation stack update.
module Network.AWS.CodeDeploy.BatchGetDeploymentTargets
  ( -- * Creating a Request
    BatchGetDeploymentTargets (..),
    newBatchGetDeploymentTargets,

    -- * Request Lenses
    batchGetDeploymentTargets_targetIds,
    batchGetDeploymentTargets_deploymentId,

    -- * Destructuring the Response
    BatchGetDeploymentTargetsResponse (..),
    newBatchGetDeploymentTargetsResponse,

    -- * Response Lenses
    batchGetDeploymentTargetsResponse_deploymentTargets,
    batchGetDeploymentTargetsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetDeploymentTargets' smart constructor.
data BatchGetDeploymentTargets = BatchGetDeploymentTargets'
  { -- | The unique IDs of the deployment targets. The compute platform of the
    -- deployment determines the type of the targets and their formats. The
    -- maximum number of deployment target IDs you can specify is 25.
    --
    -- -   For deployments that use the EC2\/On-premises compute platform, the
    --     target IDs are EC2 or on-premises instances IDs, and their target
    --     type is @instanceTarget@.
    --
    -- -   For deployments that use the AWS Lambda compute platform, the target
    --     IDs are the names of Lambda functions, and their target type is
    --     @instanceTarget@.
    --
    -- -   For deployments that use the Amazon ECS compute platform, the target
    --     IDs are pairs of Amazon ECS clusters and services specified using
    --     the format @\<clustername>:\<servicename>@. Their target type is
    --     @ecsTarget@.
    --
    -- -   For deployments that are deployed with AWS CloudFormation, the
    --     target IDs are CloudFormation stack IDs. Their target type is
    --     @cloudFormationTarget@.
    targetIds :: Core.Maybe [Core.Text],
    -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetDeploymentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetIds', 'batchGetDeploymentTargets_targetIds' - The unique IDs of the deployment targets. The compute platform of the
-- deployment determines the type of the targets and their formats. The
-- maximum number of deployment target IDs you can specify is 25.
--
-- -   For deployments that use the EC2\/On-premises compute platform, the
--     target IDs are EC2 or on-premises instances IDs, and their target
--     type is @instanceTarget@.
--
-- -   For deployments that use the AWS Lambda compute platform, the target
--     IDs are the names of Lambda functions, and their target type is
--     @instanceTarget@.
--
-- -   For deployments that use the Amazon ECS compute platform, the target
--     IDs are pairs of Amazon ECS clusters and services specified using
--     the format @\<clustername>:\<servicename>@. Their target type is
--     @ecsTarget@.
--
-- -   For deployments that are deployed with AWS CloudFormation, the
--     target IDs are CloudFormation stack IDs. Their target type is
--     @cloudFormationTarget@.
--
-- 'deploymentId', 'batchGetDeploymentTargets_deploymentId' - The unique ID of a deployment.
newBatchGetDeploymentTargets ::
  BatchGetDeploymentTargets
newBatchGetDeploymentTargets =
  BatchGetDeploymentTargets'
    { targetIds =
        Core.Nothing,
      deploymentId = Core.Nothing
    }

-- | The unique IDs of the deployment targets. The compute platform of the
-- deployment determines the type of the targets and their formats. The
-- maximum number of deployment target IDs you can specify is 25.
--
-- -   For deployments that use the EC2\/On-premises compute platform, the
--     target IDs are EC2 or on-premises instances IDs, and their target
--     type is @instanceTarget@.
--
-- -   For deployments that use the AWS Lambda compute platform, the target
--     IDs are the names of Lambda functions, and their target type is
--     @instanceTarget@.
--
-- -   For deployments that use the Amazon ECS compute platform, the target
--     IDs are pairs of Amazon ECS clusters and services specified using
--     the format @\<clustername>:\<servicename>@. Their target type is
--     @ecsTarget@.
--
-- -   For deployments that are deployed with AWS CloudFormation, the
--     target IDs are CloudFormation stack IDs. Their target type is
--     @cloudFormationTarget@.
batchGetDeploymentTargets_targetIds :: Lens.Lens' BatchGetDeploymentTargets (Core.Maybe [Core.Text])
batchGetDeploymentTargets_targetIds = Lens.lens (\BatchGetDeploymentTargets' {targetIds} -> targetIds) (\s@BatchGetDeploymentTargets' {} a -> s {targetIds = a} :: BatchGetDeploymentTargets) Core.. Lens.mapping Lens._Coerce

-- | The unique ID of a deployment.
batchGetDeploymentTargets_deploymentId :: Lens.Lens' BatchGetDeploymentTargets (Core.Maybe Core.Text)
batchGetDeploymentTargets_deploymentId = Lens.lens (\BatchGetDeploymentTargets' {deploymentId} -> deploymentId) (\s@BatchGetDeploymentTargets' {} a -> s {deploymentId = a} :: BatchGetDeploymentTargets)

instance Core.AWSRequest BatchGetDeploymentTargets where
  type
    AWSResponse BatchGetDeploymentTargets =
      BatchGetDeploymentTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDeploymentTargetsResponse'
            Core.<$> (x Core..?> "deploymentTargets" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetDeploymentTargets

instance Core.NFData BatchGetDeploymentTargets

instance Core.ToHeaders BatchGetDeploymentTargets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.BatchGetDeploymentTargets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetDeploymentTargets where
  toJSON BatchGetDeploymentTargets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("targetIds" Core..=) Core.<$> targetIds,
            ("deploymentId" Core..=) Core.<$> deploymentId
          ]
      )

instance Core.ToPath BatchGetDeploymentTargets where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetDeploymentTargets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetDeploymentTargetsResponse' smart constructor.
data BatchGetDeploymentTargetsResponse = BatchGetDeploymentTargetsResponse'
  { -- | A list of target objects for a deployment. Each target object contains
    -- details about the target, such as its status and lifecycle events. The
    -- type of the target objects depends on the deployment\' compute platform.
    --
    -- -   __EC2\/On-premises__: Each target object is an EC2 or on-premises
    --     instance.
    --
    -- -   __AWS Lambda__: The target object is a specific version of an AWS
    --     Lambda function.
    --
    -- -   __Amazon ECS__: The target object is an Amazon ECS service.
    --
    -- -   __CloudFormation__: The target object is an AWS CloudFormation
    --     blue\/green deployment.
    deploymentTargets :: Core.Maybe [DeploymentTarget],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetDeploymentTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentTargets', 'batchGetDeploymentTargetsResponse_deploymentTargets' - A list of target objects for a deployment. Each target object contains
-- details about the target, such as its status and lifecycle events. The
-- type of the target objects depends on the deployment\' compute platform.
--
-- -   __EC2\/On-premises__: Each target object is an EC2 or on-premises
--     instance.
--
-- -   __AWS Lambda__: The target object is a specific version of an AWS
--     Lambda function.
--
-- -   __Amazon ECS__: The target object is an Amazon ECS service.
--
-- -   __CloudFormation__: The target object is an AWS CloudFormation
--     blue\/green deployment.
--
-- 'httpStatus', 'batchGetDeploymentTargetsResponse_httpStatus' - The response's http status code.
newBatchGetDeploymentTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetDeploymentTargetsResponse
newBatchGetDeploymentTargetsResponse pHttpStatus_ =
  BatchGetDeploymentTargetsResponse'
    { deploymentTargets =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of target objects for a deployment. Each target object contains
-- details about the target, such as its status and lifecycle events. The
-- type of the target objects depends on the deployment\' compute platform.
--
-- -   __EC2\/On-premises__: Each target object is an EC2 or on-premises
--     instance.
--
-- -   __AWS Lambda__: The target object is a specific version of an AWS
--     Lambda function.
--
-- -   __Amazon ECS__: The target object is an Amazon ECS service.
--
-- -   __CloudFormation__: The target object is an AWS CloudFormation
--     blue\/green deployment.
batchGetDeploymentTargetsResponse_deploymentTargets :: Lens.Lens' BatchGetDeploymentTargetsResponse (Core.Maybe [DeploymentTarget])
batchGetDeploymentTargetsResponse_deploymentTargets = Lens.lens (\BatchGetDeploymentTargetsResponse' {deploymentTargets} -> deploymentTargets) (\s@BatchGetDeploymentTargetsResponse' {} a -> s {deploymentTargets = a} :: BatchGetDeploymentTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetDeploymentTargetsResponse_httpStatus :: Lens.Lens' BatchGetDeploymentTargetsResponse Core.Int
batchGetDeploymentTargetsResponse_httpStatus = Lens.lens (\BatchGetDeploymentTargetsResponse' {httpStatus} -> httpStatus) (\s@BatchGetDeploymentTargetsResponse' {} a -> s {httpStatus = a} :: BatchGetDeploymentTargetsResponse)

instance
  Core.NFData
    BatchGetDeploymentTargetsResponse
