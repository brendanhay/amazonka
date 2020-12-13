{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateComputeEnvironment (..),
    mkCreateComputeEnvironment,

    -- ** Request lenses
    cceComputeEnvironmentName,
    cceState,
    cceComputeResources,
    cceType,
    cceServiceRole,
    cceTags,

    -- * Destructuring the response
    CreateComputeEnvironmentResponse (..),
    mkCreateComputeEnvironmentResponse,

    -- ** Response lenses
    ccersComputeEnvironmentName,
    ccersComputeEnvironmentARN,
    ccersResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateComputeEnvironment' smart constructor.
data CreateComputeEnvironment = CreateComputeEnvironment'
  { -- | The name for your compute environment. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
    computeEnvironmentName :: Lude.Text,
    -- | The state of the compute environment. If the state is @ENABLED@ , then the compute environment accepts jobs from a queue and can scale out automatically based on queues.
    state :: Lude.Maybe CEState,
    -- | Details of the compute resources managed by the compute environment. This parameter is required for managed compute environments. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
    computeResources :: Lude.Maybe ComputeResource,
    -- | The type of the compute environment. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
    type' :: CEType,
    -- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
    --
    -- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
    serviceRole :: Lude.Text,
    -- | The tags that you apply to the compute environment to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
    --
    -- These tags can be updated or removed using the <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource> and <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource> API operations. These tags do not propagate to the underlying compute resources.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateComputeEnvironment' with the minimum fields required to make a request.
--
-- * 'computeEnvironmentName' - The name for your compute environment. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
-- * 'state' - The state of the compute environment. If the state is @ENABLED@ , then the compute environment accepts jobs from a queue and can scale out automatically based on queues.
-- * 'computeResources' - Details of the compute resources managed by the compute environment. This parameter is required for managed compute environments. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
-- * 'type'' - The type of the compute environment. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
-- * 'serviceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
-- * 'tags' - The tags that you apply to the compute environment to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- These tags can be updated or removed using the <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource> and <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource> API operations. These tags do not propagate to the underlying compute resources.
mkCreateComputeEnvironment ::
  -- | 'computeEnvironmentName'
  Lude.Text ->
  -- | 'type''
  CEType ->
  -- | 'serviceRole'
  Lude.Text ->
  CreateComputeEnvironment
mkCreateComputeEnvironment
  pComputeEnvironmentName_
  pType_
  pServiceRole_ =
    CreateComputeEnvironment'
      { computeEnvironmentName =
          pComputeEnvironmentName_,
        state = Lude.Nothing,
        computeResources = Lude.Nothing,
        type' = pType_,
        serviceRole = pServiceRole_,
        tags = Lude.Nothing
      }

-- | The name for your compute environment. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceComputeEnvironmentName :: Lens.Lens' CreateComputeEnvironment Lude.Text
cceComputeEnvironmentName = Lens.lens (computeEnvironmentName :: CreateComputeEnvironment -> Lude.Text) (\s a -> s {computeEnvironmentName = a} :: CreateComputeEnvironment)
{-# DEPRECATED cceComputeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead." #-}

-- | The state of the compute environment. If the state is @ENABLED@ , then the compute environment accepts jobs from a queue and can scale out automatically based on queues.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceState :: Lens.Lens' CreateComputeEnvironment (Lude.Maybe CEState)
cceState = Lens.lens (state :: CreateComputeEnvironment -> Lude.Maybe CEState) (\s a -> s {state = a} :: CreateComputeEnvironment)
{-# DEPRECATED cceState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Details of the compute resources managed by the compute environment. This parameter is required for managed compute environments. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'computeResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceComputeResources :: Lens.Lens' CreateComputeEnvironment (Lude.Maybe ComputeResource)
cceComputeResources = Lens.lens (computeResources :: CreateComputeEnvironment -> Lude.Maybe ComputeResource) (\s a -> s {computeResources = a} :: CreateComputeEnvironment)
{-# DEPRECATED cceComputeResources "Use generic-lens or generic-optics with 'computeResources' instead." #-}

-- | The type of the compute environment. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceType :: Lens.Lens' CreateComputeEnvironment CEType
cceType = Lens.lens (type' :: CreateComputeEnvironment -> CEType) (\s a -> s {type' = a} :: CreateComputeEnvironment)
{-# DEPRECATED cceType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceServiceRole :: Lens.Lens' CreateComputeEnvironment Lude.Text
cceServiceRole = Lens.lens (serviceRole :: CreateComputeEnvironment -> Lude.Text) (\s a -> s {serviceRole = a} :: CreateComputeEnvironment)
{-# DEPRECATED cceServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The tags that you apply to the compute environment to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- These tags can be updated or removed using the <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource> and <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource> API operations. These tags do not propagate to the underlying compute resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cceTags :: Lens.Lens' CreateComputeEnvironment (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cceTags = Lens.lens (tags :: CreateComputeEnvironment -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateComputeEnvironment)
{-# DEPRECATED cceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateComputeEnvironment where
  type Rs CreateComputeEnvironment = CreateComputeEnvironmentResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateComputeEnvironmentResponse'
            Lude.<$> (x Lude..?> "computeEnvironmentName")
            Lude.<*> (x Lude..?> "computeEnvironmentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateComputeEnvironment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateComputeEnvironment where
  toJSON CreateComputeEnvironment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("computeEnvironmentName" Lude..= computeEnvironmentName),
            ("state" Lude..=) Lude.<$> state,
            ("computeResources" Lude..=) Lude.<$> computeResources,
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("serviceRole" Lude..= serviceRole),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateComputeEnvironment where
  toPath = Lude.const "/v1/createcomputeenvironment"

instance Lude.ToQuery CreateComputeEnvironment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateComputeEnvironmentResponse' smart constructor.
data CreateComputeEnvironmentResponse = CreateComputeEnvironmentResponse'
  { -- | The name of the compute environment.
    computeEnvironmentName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironmentARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateComputeEnvironmentResponse' with the minimum fields required to make a request.
--
-- * 'computeEnvironmentName' - The name of the compute environment.
-- * 'computeEnvironmentARN' - The Amazon Resource Name (ARN) of the compute environment.
-- * 'responseStatus' - The response status code.
mkCreateComputeEnvironmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateComputeEnvironmentResponse
mkCreateComputeEnvironmentResponse pResponseStatus_ =
  CreateComputeEnvironmentResponse'
    { computeEnvironmentName =
        Lude.Nothing,
      computeEnvironmentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccersComputeEnvironmentName :: Lens.Lens' CreateComputeEnvironmentResponse (Lude.Maybe Lude.Text)
ccersComputeEnvironmentName = Lens.lens (computeEnvironmentName :: CreateComputeEnvironmentResponse -> Lude.Maybe Lude.Text) (\s a -> s {computeEnvironmentName = a} :: CreateComputeEnvironmentResponse)
{-# DEPRECATED ccersComputeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead." #-}

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccersComputeEnvironmentARN :: Lens.Lens' CreateComputeEnvironmentResponse (Lude.Maybe Lude.Text)
ccersComputeEnvironmentARN = Lens.lens (computeEnvironmentARN :: CreateComputeEnvironmentResponse -> Lude.Maybe Lude.Text) (\s a -> s {computeEnvironmentARN = a} :: CreateComputeEnvironmentResponse)
{-# DEPRECATED ccersComputeEnvironmentARN "Use generic-lens or generic-optics with 'computeEnvironmentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccersResponseStatus :: Lens.Lens' CreateComputeEnvironmentResponse Lude.Int
ccersResponseStatus = Lens.lens (responseStatus :: CreateComputeEnvironmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateComputeEnvironmentResponse)
{-# DEPRECATED ccersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
