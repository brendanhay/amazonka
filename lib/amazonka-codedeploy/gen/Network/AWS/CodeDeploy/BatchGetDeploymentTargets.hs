{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    bgdtrsDeploymentTargets,
    bgdtrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetDeploymentTargets' smart constructor.
data BatchGetDeploymentTargets = BatchGetDeploymentTargets'
  { deploymentId ::
      Lude.Maybe Lude.Text,
    targetIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetDeploymentTargets' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'targetIds' - The unique IDs of the deployment targets. The compute platform of the deployment determines the type of the targets and their formats. The maximum number of deployment target IDs you can specify is 25.
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
mkBatchGetDeploymentTargets ::
  BatchGetDeploymentTargets
mkBatchGetDeploymentTargets =
  BatchGetDeploymentTargets'
    { deploymentId = Lude.Nothing,
      targetIds = Lude.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdtDeploymentId :: Lens.Lens' BatchGetDeploymentTargets (Lude.Maybe Lude.Text)
bgdtDeploymentId = Lens.lens (deploymentId :: BatchGetDeploymentTargets -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: BatchGetDeploymentTargets)
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
bgdtTargetIds :: Lens.Lens' BatchGetDeploymentTargets (Lude.Maybe [Lude.Text])
bgdtTargetIds = Lens.lens (targetIds :: BatchGetDeploymentTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {targetIds = a} :: BatchGetDeploymentTargets)
{-# DEPRECATED bgdtTargetIds "Use generic-lens or generic-optics with 'targetIds' instead." #-}

instance Lude.AWSRequest BatchGetDeploymentTargets where
  type
    Rs BatchGetDeploymentTargets =
      BatchGetDeploymentTargetsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetDeploymentTargetsResponse'
            Lude.<$> (x Lude..?> "deploymentTargets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetDeploymentTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.BatchGetDeploymentTargets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetDeploymentTargets where
  toJSON BatchGetDeploymentTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deploymentId" Lude..=) Lude.<$> deploymentId,
            ("targetIds" Lude..=) Lude.<$> targetIds
          ]
      )

instance Lude.ToPath BatchGetDeploymentTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetDeploymentTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetDeploymentTargetsResponse' smart constructor.
data BatchGetDeploymentTargetsResponse = BatchGetDeploymentTargetsResponse'
  { deploymentTargets ::
      Lude.Maybe
        [DeploymentTarget],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetDeploymentTargetsResponse' with the minimum fields required to make a request.
--
-- * 'deploymentTargets' - A list of target objects for a deployment. Each target object contains details about the target, such as its status and lifecycle events. The type of the target objects depends on the deployment' compute platform.
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
-- * 'responseStatus' - The response status code.
mkBatchGetDeploymentTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetDeploymentTargetsResponse
mkBatchGetDeploymentTargetsResponse pResponseStatus_ =
  BatchGetDeploymentTargetsResponse'
    { deploymentTargets =
        Lude.Nothing,
      responseStatus = pResponseStatus_
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
bgdtrsDeploymentTargets :: Lens.Lens' BatchGetDeploymentTargetsResponse (Lude.Maybe [DeploymentTarget])
bgdtrsDeploymentTargets = Lens.lens (deploymentTargets :: BatchGetDeploymentTargetsResponse -> Lude.Maybe [DeploymentTarget]) (\s a -> s {deploymentTargets = a} :: BatchGetDeploymentTargetsResponse)
{-# DEPRECATED bgdtrsDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdtrsResponseStatus :: Lens.Lens' BatchGetDeploymentTargetsResponse Lude.Int
bgdtrsResponseStatus = Lens.lens (responseStatus :: BatchGetDeploymentTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetDeploymentTargetsResponse)
{-# DEPRECATED bgdtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
