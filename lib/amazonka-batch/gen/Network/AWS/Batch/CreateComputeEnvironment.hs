{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.CreateComputeEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Batch compute environment. You can create @MANAGED@ or @UNMANAGED@ compute environments.
--
-- In a managed compute environment, AWS Batch manages the capacity and instance types of the compute resources within the environment. This is based on the compute resource specification that you define or the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html launch template> that you specify when you create the compute environment. You can choose to use Amazon EC2 On-Demand Instances or Spot Instances in your managed compute environment. You can optionally set a maximum price so that Spot Instances only launch when the Spot Instance price is below a specified percentage of the On-Demand price.
-- In an unmanaged compute environment, you can manage your own compute resources. This provides more compute resource configuration options, such as using a custom AMI, but you must ensure that your AMI meets the Amazon ECS container instance AMI specification. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container_instance_AMIs.html Container Instance AMIs> in the /Amazon Elastic Container Service Developer Guide/ . After you have created your unmanaged compute environment, you can use the 'DescribeComputeEnvironments' operation to find the Amazon ECS cluster that is associated with it. Then, manually launch your container instances into that Amazon ECS cluster. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_container_instance.html Launching an Amazon ECS Container Instance> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.Batch.CreateComputeEnvironment
    (
    -- * Creating a request
      CreateComputeEnvironment (..)
    , mkCreateComputeEnvironment
    -- ** Request lenses
    , cceComputeEnvironmentName
    , cceType
    , cceServiceRole
    , cceComputeResources
    , cceState
    , cceTags

    -- * Destructuring the response
    , CreateComputeEnvironmentResponse (..)
    , mkCreateComputeEnvironmentResponse
    -- ** Response lenses
    , ccerrsComputeEnvironmentArn
    , ccerrsComputeEnvironmentName
    , ccerrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateComputeEnvironment' smart constructor.
data CreateComputeEnvironment = CreateComputeEnvironment'
  { computeEnvironmentName :: Core.Text
    -- ^ The name for your compute environment. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
  , type' :: Types.CEType
    -- ^ The type of the compute environment. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
  , serviceRole :: Core.Text
    -- ^ The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
  , computeResources :: Core.Maybe Types.ComputeResource
    -- ^ Details of the compute resources managed by the compute environment. This parameter is required for managed compute environments. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
  , state :: Core.Maybe Types.CEState
    -- ^ The state of the compute environment. If the state is @ENABLED@ , then the compute environment accepts jobs from a queue and can scale out automatically based on queues.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags that you apply to the compute environment to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- These tags can be updated or removed using the <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource> and <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource> API operations. These tags do not propagate to the underlying compute resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateComputeEnvironment' value with any optional fields omitted.
mkCreateComputeEnvironment
    :: Core.Text -- ^ 'computeEnvironmentName'
    -> Types.CEType -- ^ 'type\''
    -> Core.Text -- ^ 'serviceRole'
    -> CreateComputeEnvironment
mkCreateComputeEnvironment computeEnvironmentName type' serviceRole
  = CreateComputeEnvironment'{computeEnvironmentName, type',
                              serviceRole, computeResources = Core.Nothing, state = Core.Nothing,
                              tags = Core.Nothing}

-- | The name for your compute environment. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceComputeEnvironmentName :: Lens.Lens' CreateComputeEnvironment Core.Text
cceComputeEnvironmentName = Lens.field @"computeEnvironmentName"
{-# INLINEABLE cceComputeEnvironmentName #-}
{-# DEPRECATED computeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead"  #-}

-- | The type of the compute environment. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceType :: Lens.Lens' CreateComputeEnvironment Types.CEType
cceType = Lens.field @"type'"
{-# INLINEABLE cceType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceServiceRole :: Lens.Lens' CreateComputeEnvironment Core.Text
cceServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE cceServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | Details of the compute resources managed by the compute environment. This parameter is required for managed compute environments. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'computeResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceComputeResources :: Lens.Lens' CreateComputeEnvironment (Core.Maybe Types.ComputeResource)
cceComputeResources = Lens.field @"computeResources"
{-# INLINEABLE cceComputeResources #-}
{-# DEPRECATED computeResources "Use generic-lens or generic-optics with 'computeResources' instead"  #-}

-- | The state of the compute environment. If the state is @ENABLED@ , then the compute environment accepts jobs from a queue and can scale out automatically based on queues.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceState :: Lens.Lens' CreateComputeEnvironment (Core.Maybe Types.CEState)
cceState = Lens.field @"state"
{-# INLINEABLE cceState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The tags that you apply to the compute environment to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- These tags can be updated or removed using the <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource> and <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource> API operations. These tags do not propagate to the underlying compute resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceTags :: Lens.Lens' CreateComputeEnvironment (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cceTags = Lens.field @"tags"
{-# INLINEABLE cceTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateComputeEnvironment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateComputeEnvironment where
        toHeaders CreateComputeEnvironment{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateComputeEnvironment where
        toJSON CreateComputeEnvironment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("computeEnvironmentName" Core..= computeEnvironmentName),
                  Core.Just ("type" Core..= type'),
                  Core.Just ("serviceRole" Core..= serviceRole),
                  ("computeResources" Core..=) Core.<$> computeResources,
                  ("state" Core..=) Core.<$> state, ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateComputeEnvironment where
        type Rs CreateComputeEnvironment = CreateComputeEnvironmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/v1/createcomputeenvironment",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateComputeEnvironmentResponse' Core.<$>
                   (x Core..:? "computeEnvironmentArn") Core.<*>
                     x Core..:? "computeEnvironmentName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateComputeEnvironmentResponse' smart constructor.
data CreateComputeEnvironmentResponse = CreateComputeEnvironmentResponse'
  { computeEnvironmentArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the compute environment.
  , computeEnvironmentName :: Core.Maybe Core.Text
    -- ^ The name of the compute environment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateComputeEnvironmentResponse' value with any optional fields omitted.
mkCreateComputeEnvironmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateComputeEnvironmentResponse
mkCreateComputeEnvironmentResponse responseStatus
  = CreateComputeEnvironmentResponse'{computeEnvironmentArn =
                                        Core.Nothing,
                                      computeEnvironmentName = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccerrsComputeEnvironmentArn :: Lens.Lens' CreateComputeEnvironmentResponse (Core.Maybe Core.Text)
ccerrsComputeEnvironmentArn = Lens.field @"computeEnvironmentArn"
{-# INLINEABLE ccerrsComputeEnvironmentArn #-}
{-# DEPRECATED computeEnvironmentArn "Use generic-lens or generic-optics with 'computeEnvironmentArn' instead"  #-}

-- | The name of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccerrsComputeEnvironmentName :: Lens.Lens' CreateComputeEnvironmentResponse (Core.Maybe Core.Text)
ccerrsComputeEnvironmentName = Lens.field @"computeEnvironmentName"
{-# INLINEABLE ccerrsComputeEnvironmentName #-}
{-# DEPRECATED computeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccerrsResponseStatus :: Lens.Lens' CreateComputeEnvironmentResponse Core.Int
ccerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
