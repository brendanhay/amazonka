{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetDeploymentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of one or more targets associated with a deployment. This method works with all compute types and should be used instead of the deprecated @BatchGetDeploymentInstances@ . The maximum number of targets that can be returned is 25.
--
-- The type of targets returned depends on the deployment's compute platform or deployment method:
--
--     * __EC2/On-premises__ : Information about EC2 instance targets.
--
--
--     * __AWS Lambda__ : Information about Lambda functions targets.
--
--
--     * __Amazon ECS__ : Information about Amazon ECS service targets.
--
--
--     * __CloudFormation__ : Information about targets of blue/green deployments initiated by a CloudFormation stack update.
module Network.AWS.CodeDeploy.BatchGetDeploymentTargets
  ( -- * Creating a request
    BatchGetDeploymentTargets (..),
    mkBatchGetDeploymentTargets,

    -- ** Request lenses
    bgdtDeploymentId,
    bgdtTargetIds,

    -- * Destructuring the response
    BatchGetDeploymentTargetsResponse (..),
    mkBatchGetDeploymentTargetsResponse,

    -- ** Response lenses
    bgdtrrsDeploymentTargets,
    bgdtrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetDeploymentTargets' smart constructor.
data BatchGetDeploymentTargets = BatchGetDeploymentTargets'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | The unique IDs of the deployment targets. The compute platform of the deployment determines the type of the targets and their formats. The maximum number of deployment target IDs you can specify is 25.
    --
    --
    --     * For deployments that use the EC2/On-premises compute platform, the target IDs are EC2 or on-premises instances IDs, and their target type is @instanceTarget@ .
    --
    --
    --     * For deployments that use the AWS Lambda compute platform, the target IDs are the names of Lambda functions, and their target type is @instanceTarget@ .
    --
    --
    --     * For deployments that use the Amazon ECS compute platform, the target IDs are pairs of Amazon ECS clusters and services specified using the format @<clustername>:<servicename>@ . Their target type is @ecsTarget@ .
    --
    --
    --     * For deployments that are deployed with AWS CloudFormation, the target IDs are CloudFormation stack IDs. Their target type is @cloudFormationTarget@ .
    targetIds :: Core.Maybe [Types.TargetId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetDeploymentTargets' value with any optional fields omitted.
mkBatchGetDeploymentTargets ::
  BatchGetDeploymentTargets
mkBatchGetDeploymentTargets =
  BatchGetDeploymentTargets'
    { deploymentId = Core.Nothing,
      targetIds = Core.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdtDeploymentId :: Lens.Lens' BatchGetDeploymentTargets (Core.Maybe Types.DeploymentId)
bgdtDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED bgdtDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The unique IDs of the deployment targets. The compute platform of the deployment determines the type of the targets and their formats. The maximum number of deployment target IDs you can specify is 25.
--
--
--     * For deployments that use the EC2/On-premises compute platform, the target IDs are EC2 or on-premises instances IDs, and their target type is @instanceTarget@ .
--
--
--     * For deployments that use the AWS Lambda compute platform, the target IDs are the names of Lambda functions, and their target type is @instanceTarget@ .
--
--
--     * For deployments that use the Amazon ECS compute platform, the target IDs are pairs of Amazon ECS clusters and services specified using the format @<clustername>:<servicename>@ . Their target type is @ecsTarget@ .
--
--
--     * For deployments that are deployed with AWS CloudFormation, the target IDs are CloudFormation stack IDs. Their target type is @cloudFormationTarget@ .
--
--
--
-- /Note:/ Consider using 'targetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdtTargetIds :: Lens.Lens' BatchGetDeploymentTargets (Core.Maybe [Types.TargetId])
bgdtTargetIds = Lens.field @"targetIds"
{-# DEPRECATED bgdtTargetIds "Use generic-lens or generic-optics with 'targetIds' instead." #-}

instance Core.FromJSON BatchGetDeploymentTargets where
  toJSON BatchGetDeploymentTargets {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("targetIds" Core..=) Core.<$> targetIds
          ]
      )

instance Core.AWSRequest BatchGetDeploymentTargets where
  type
    Rs BatchGetDeploymentTargets =
      BatchGetDeploymentTargetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.BatchGetDeploymentTargets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDeploymentTargetsResponse'
            Core.<$> (x Core..:? "deploymentTargets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetDeploymentTargetsResponse' smart constructor.
data BatchGetDeploymentTargetsResponse = BatchGetDeploymentTargetsResponse'
  { -- | A list of target objects for a deployment. Each target object contains details about the target, such as its status and lifecycle events. The type of the target objects depends on the deployment' compute platform.
    --
    --
    --     * __EC2/On-premises__ : Each target object is an EC2 or on-premises instance.
    --
    --
    --     * __AWS Lambda__ : The target object is a specific version of an AWS Lambda function.
    --
    --
    --     * __Amazon ECS__ : The target object is an Amazon ECS service.
    --
    --
    --     * __CloudFormation__ : The target object is an AWS CloudFormation blue/green deployment.
    deploymentTargets :: Core.Maybe [Types.DeploymentTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetDeploymentTargetsResponse' value with any optional fields omitted.
mkBatchGetDeploymentTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetDeploymentTargetsResponse
mkBatchGetDeploymentTargetsResponse responseStatus =
  BatchGetDeploymentTargetsResponse'
    { deploymentTargets =
        Core.Nothing,
      responseStatus
    }

-- | A list of target objects for a deployment. Each target object contains details about the target, such as its status and lifecycle events. The type of the target objects depends on the deployment' compute platform.
--
--
--     * __EC2/On-premises__ : Each target object is an EC2 or on-premises instance.
--
--
--     * __AWS Lambda__ : The target object is a specific version of an AWS Lambda function.
--
--
--     * __Amazon ECS__ : The target object is an Amazon ECS service.
--
--
--     * __CloudFormation__ : The target object is an AWS CloudFormation blue/green deployment.
--
--
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdtrrsDeploymentTargets :: Lens.Lens' BatchGetDeploymentTargetsResponse (Core.Maybe [Types.DeploymentTarget])
bgdtrrsDeploymentTargets = Lens.field @"deploymentTargets"
{-# DEPRECATED bgdtrrsDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdtrrsResponseStatus :: Lens.Lens' BatchGetDeploymentTargetsResponse Core.Int
bgdtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgdtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
