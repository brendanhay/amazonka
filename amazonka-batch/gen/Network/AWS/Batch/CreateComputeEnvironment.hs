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
-- Module      : Network.AWS.Batch.CreateComputeEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Batch compute environment. You can create @MANAGED@ or
-- @UNMANAGED@ compute environments. @MANAGED@ compute environments can use
-- Amazon EC2 or AWS Fargate resources. @UNMANAGED@ compute environments
-- can only use EC2 resources.
--
-- In a managed compute environment, AWS Batch manages the capacity and
-- instance types of the compute resources within the environment. This is
-- based on the compute resource specification that you define or the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html launch template>
-- that you specify when you create the compute environment. You can choose
-- either to use EC2 On-Demand Instances and EC2 Spot Instances, or to use
-- Fargate and Fargate Spot capacity in your managed compute environment.
-- You can optionally set a maximum price so that Spot Instances only
-- launch when the Spot Instance price is less than a specified percentage
-- of the On-Demand price.
--
-- Multi-node parallel jobs are not supported on Spot Instances.
--
-- In an unmanaged compute environment, you can manage your own EC2 compute
-- resources and have a lot of flexibility with how you configure your
-- compute resources. For example, you can use custom AMI. However, you
-- need to verify that your AMI meets the Amazon ECS container instance AMI
-- specification. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container_instance_AMIs.html container instance AMIs>
-- in the /Amazon Elastic Container Service Developer Guide/. After you
-- have created your unmanaged compute environment, you can use the
-- DescribeComputeEnvironments operation to find the Amazon ECS cluster
-- that\'s associated with it. Then, manually launch your container
-- instances into that Amazon ECS cluster. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_container_instance.html Launching an Amazon ECS container instance>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- AWS Batch doesn\'t upgrade the AMIs in a compute environment after it\'s
-- created. For example, it doesn\'t update the AMIs when a newer version
-- of the Amazon ECS-optimized AMI is available. Therefore, you\'re
-- responsible for the management of the guest operating system (including
-- updates and security patches) and any additional application software or
-- utilities that you install on the compute resources. To use a new AMI
-- for your AWS Batch jobs, complete these steps:
--
-- 1.  Create a new compute environment with the new AMI.
--
-- 2.  Add the compute environment to an existing job queue.
--
-- 3.  Remove the earlier compute environment from your job queue.
--
-- 4.  Delete the earlier compute environment.
module Network.AWS.Batch.CreateComputeEnvironment
  ( -- * Creating a Request
    CreateComputeEnvironment (..),
    newCreateComputeEnvironment,

    -- * Request Lenses
    createComputeEnvironment_state,
    createComputeEnvironment_computeResources,
    createComputeEnvironment_tags,
    createComputeEnvironment_computeEnvironmentName,
    createComputeEnvironment_type,
    createComputeEnvironment_serviceRole,

    -- * Destructuring the Response
    CreateComputeEnvironmentResponse (..),
    newCreateComputeEnvironmentResponse,

    -- * Response Lenses
    createComputeEnvironmentResponse_computeEnvironmentName,
    createComputeEnvironmentResponse_computeEnvironmentArn,
    createComputeEnvironmentResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @CreateComputeEnvironment@.
--
-- /See:/ 'newCreateComputeEnvironment' smart constructor.
data CreateComputeEnvironment = CreateComputeEnvironment'
  { -- | The state of the compute environment. If the state is @ENABLED@, then
    -- the compute environment accepts jobs from a queue and can scale out
    -- automatically based on queues.
    --
    -- If the state is @ENABLED@, then the AWS Batch scheduler can attempt to
    -- place jobs from an associated job queue on the compute resources within
    -- the environment. If the compute environment is managed, then it can
    -- scale its instances out or in automatically, based on the job queue
    -- demand.
    --
    -- If the state is @DISABLED@, then the AWS Batch scheduler doesn\'t
    -- attempt to place jobs within the environment. Jobs in a @STARTING@ or
    -- @RUNNING@ state continue to progress normally. Managed compute
    -- environments in the @DISABLED@ state don\'t scale out. However, they
    -- scale in to @minvCpus@ value after instances become idle.
    state :: Core.Maybe CEState,
    -- | Details about the compute resources managed by the compute environment.
    -- This parameter is required for managed compute environments. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /AWS Batch User Guide/.
    computeResources :: Core.Maybe ComputeResource,
    -- | The tags that you apply to the compute environment to help you
    -- categorize and organize your resources. Each tag consists of a key and
    -- an optional value. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in /AWS General Reference/.
    --
    -- These tags can be updated or removed using the
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
    -- and
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
    -- API operations. These tags don\'t propagate to the underlying compute
    -- resources.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name for your compute environment. Up to 128 letters (uppercase and
    -- lowercase), numbers, hyphens, and underscores are allowed.
    computeEnvironmentName :: Core.Text,
    -- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /AWS Batch User Guide/.
    type' :: CEType,
    -- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS
    -- Batch to make calls to other AWS services on your behalf. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html AWS Batch service IAM role>
    -- in the /AWS Batch User Guide/.
    --
    -- If your specified role has a path other than @\/@, then you must either
    -- specify the full role ARN (this is recommended) or prefix the role name
    -- with the path.
    --
    -- Depending on how you created your AWS Batch service role, its ARN might
    -- contain the @service-role@ path prefix. When you only specify the name
    -- of the service role, AWS Batch assumes that your ARN doesn\'t use the
    -- @service-role@ path prefix. Because of this, we recommend that you
    -- specify the full ARN of your service role when you create compute
    -- environments.
    serviceRole :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateComputeEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'createComputeEnvironment_state' - The state of the compute environment. If the state is @ENABLED@, then
-- the compute environment accepts jobs from a queue and can scale out
-- automatically based on queues.
--
-- If the state is @ENABLED@, then the AWS Batch scheduler can attempt to
-- place jobs from an associated job queue on the compute resources within
-- the environment. If the compute environment is managed, then it can
-- scale its instances out or in automatically, based on the job queue
-- demand.
--
-- If the state is @DISABLED@, then the AWS Batch scheduler doesn\'t
-- attempt to place jobs within the environment. Jobs in a @STARTING@ or
-- @RUNNING@ state continue to progress normally. Managed compute
-- environments in the @DISABLED@ state don\'t scale out. However, they
-- scale in to @minvCpus@ value after instances become idle.
--
-- 'computeResources', 'createComputeEnvironment_computeResources' - Details about the compute resources managed by the compute environment.
-- This parameter is required for managed compute environments. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
--
-- 'tags', 'createComputeEnvironment_tags' - The tags that you apply to the compute environment to help you
-- categorize and organize your resources. Each tag consists of a key and
-- an optional value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in /AWS General Reference/.
--
-- These tags can be updated or removed using the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
-- and
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
-- API operations. These tags don\'t propagate to the underlying compute
-- resources.
--
-- 'computeEnvironmentName', 'createComputeEnvironment_computeEnvironmentName' - The name for your compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
--
-- 'type'', 'createComputeEnvironment_type' - The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
--
-- 'serviceRole', 'createComputeEnvironment_serviceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows AWS
-- Batch to make calls to other AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html AWS Batch service IAM role>
-- in the /AWS Batch User Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path.
--
-- Depending on how you created your AWS Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, AWS Batch assumes that your ARN doesn\'t use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
newCreateComputeEnvironment ::
  -- | 'computeEnvironmentName'
  Core.Text ->
  -- | 'type''
  CEType ->
  -- | 'serviceRole'
  Core.Text ->
  CreateComputeEnvironment
newCreateComputeEnvironment
  pComputeEnvironmentName_
  pType_
  pServiceRole_ =
    CreateComputeEnvironment'
      { state = Core.Nothing,
        computeResources = Core.Nothing,
        tags = Core.Nothing,
        computeEnvironmentName = pComputeEnvironmentName_,
        type' = pType_,
        serviceRole = pServiceRole_
      }

-- | The state of the compute environment. If the state is @ENABLED@, then
-- the compute environment accepts jobs from a queue and can scale out
-- automatically based on queues.
--
-- If the state is @ENABLED@, then the AWS Batch scheduler can attempt to
-- place jobs from an associated job queue on the compute resources within
-- the environment. If the compute environment is managed, then it can
-- scale its instances out or in automatically, based on the job queue
-- demand.
--
-- If the state is @DISABLED@, then the AWS Batch scheduler doesn\'t
-- attempt to place jobs within the environment. Jobs in a @STARTING@ or
-- @RUNNING@ state continue to progress normally. Managed compute
-- environments in the @DISABLED@ state don\'t scale out. However, they
-- scale in to @minvCpus@ value after instances become idle.
createComputeEnvironment_state :: Lens.Lens' CreateComputeEnvironment (Core.Maybe CEState)
createComputeEnvironment_state = Lens.lens (\CreateComputeEnvironment' {state} -> state) (\s@CreateComputeEnvironment' {} a -> s {state = a} :: CreateComputeEnvironment)

-- | Details about the compute resources managed by the compute environment.
-- This parameter is required for managed compute environments. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
createComputeEnvironment_computeResources :: Lens.Lens' CreateComputeEnvironment (Core.Maybe ComputeResource)
createComputeEnvironment_computeResources = Lens.lens (\CreateComputeEnvironment' {computeResources} -> computeResources) (\s@CreateComputeEnvironment' {} a -> s {computeResources = a} :: CreateComputeEnvironment)

-- | The tags that you apply to the compute environment to help you
-- categorize and organize your resources. Each tag consists of a key and
-- an optional value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in /AWS General Reference/.
--
-- These tags can be updated or removed using the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
-- and
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
-- API operations. These tags don\'t propagate to the underlying compute
-- resources.
createComputeEnvironment_tags :: Lens.Lens' CreateComputeEnvironment (Core.Maybe (Core.HashMap Core.Text Core.Text))
createComputeEnvironment_tags = Lens.lens (\CreateComputeEnvironment' {tags} -> tags) (\s@CreateComputeEnvironment' {} a -> s {tags = a} :: CreateComputeEnvironment) Core.. Lens.mapping Lens._Coerce

-- | The name for your compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
createComputeEnvironment_computeEnvironmentName :: Lens.Lens' CreateComputeEnvironment Core.Text
createComputeEnvironment_computeEnvironmentName = Lens.lens (\CreateComputeEnvironment' {computeEnvironmentName} -> computeEnvironmentName) (\s@CreateComputeEnvironment' {} a -> s {computeEnvironmentName = a} :: CreateComputeEnvironment)

-- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
createComputeEnvironment_type :: Lens.Lens' CreateComputeEnvironment CEType
createComputeEnvironment_type = Lens.lens (\CreateComputeEnvironment' {type'} -> type') (\s@CreateComputeEnvironment' {} a -> s {type' = a} :: CreateComputeEnvironment)

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS
-- Batch to make calls to other AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html AWS Batch service IAM role>
-- in the /AWS Batch User Guide/.
--
-- If your specified role has a path other than @\/@, then you must either
-- specify the full role ARN (this is recommended) or prefix the role name
-- with the path.
--
-- Depending on how you created your AWS Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, AWS Batch assumes that your ARN doesn\'t use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
createComputeEnvironment_serviceRole :: Lens.Lens' CreateComputeEnvironment Core.Text
createComputeEnvironment_serviceRole = Lens.lens (\CreateComputeEnvironment' {serviceRole} -> serviceRole) (\s@CreateComputeEnvironment' {} a -> s {serviceRole = a} :: CreateComputeEnvironment)

instance Core.AWSRequest CreateComputeEnvironment where
  type
    AWSResponse CreateComputeEnvironment =
      CreateComputeEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComputeEnvironmentResponse'
            Core.<$> (x Core..?> "computeEnvironmentName")
            Core.<*> (x Core..?> "computeEnvironmentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateComputeEnvironment

instance Core.NFData CreateComputeEnvironment

instance Core.ToHeaders CreateComputeEnvironment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateComputeEnvironment where
  toJSON CreateComputeEnvironment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("state" Core..=) Core.<$> state,
            ("computeResources" Core..=)
              Core.<$> computeResources,
            ("tags" Core..=) Core.<$> tags,
            Core.Just
              ( "computeEnvironmentName"
                  Core..= computeEnvironmentName
              ),
            Core.Just ("type" Core..= type'),
            Core.Just ("serviceRole" Core..= serviceRole)
          ]
      )

instance Core.ToPath CreateComputeEnvironment where
  toPath = Core.const "/v1/createcomputeenvironment"

instance Core.ToQuery CreateComputeEnvironment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateComputeEnvironmentResponse' smart constructor.
data CreateComputeEnvironmentResponse = CreateComputeEnvironmentResponse'
  { -- | The name of the compute environment. Up to 128 letters (uppercase and
    -- lowercase), numbers, hyphens, and underscores are allowed.
    computeEnvironmentName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironmentArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateComputeEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeEnvironmentName', 'createComputeEnvironmentResponse_computeEnvironmentName' - The name of the compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
--
-- 'computeEnvironmentArn', 'createComputeEnvironmentResponse_computeEnvironmentArn' - The Amazon Resource Name (ARN) of the compute environment.
--
-- 'httpStatus', 'createComputeEnvironmentResponse_httpStatus' - The response's http status code.
newCreateComputeEnvironmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateComputeEnvironmentResponse
newCreateComputeEnvironmentResponse pHttpStatus_ =
  CreateComputeEnvironmentResponse'
    { computeEnvironmentName =
        Core.Nothing,
      computeEnvironmentArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the compute environment. Up to 128 letters (uppercase and
-- lowercase), numbers, hyphens, and underscores are allowed.
createComputeEnvironmentResponse_computeEnvironmentName :: Lens.Lens' CreateComputeEnvironmentResponse (Core.Maybe Core.Text)
createComputeEnvironmentResponse_computeEnvironmentName = Lens.lens (\CreateComputeEnvironmentResponse' {computeEnvironmentName} -> computeEnvironmentName) (\s@CreateComputeEnvironmentResponse' {} a -> s {computeEnvironmentName = a} :: CreateComputeEnvironmentResponse)

-- | The Amazon Resource Name (ARN) of the compute environment.
createComputeEnvironmentResponse_computeEnvironmentArn :: Lens.Lens' CreateComputeEnvironmentResponse (Core.Maybe Core.Text)
createComputeEnvironmentResponse_computeEnvironmentArn = Lens.lens (\CreateComputeEnvironmentResponse' {computeEnvironmentArn} -> computeEnvironmentArn) (\s@CreateComputeEnvironmentResponse' {} a -> s {computeEnvironmentArn = a} :: CreateComputeEnvironmentResponse)

-- | The response's http status code.
createComputeEnvironmentResponse_httpStatus :: Lens.Lens' CreateComputeEnvironmentResponse Core.Int
createComputeEnvironmentResponse_httpStatus = Lens.lens (\CreateComputeEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateComputeEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateComputeEnvironmentResponse)

instance Core.NFData CreateComputeEnvironmentResponse
