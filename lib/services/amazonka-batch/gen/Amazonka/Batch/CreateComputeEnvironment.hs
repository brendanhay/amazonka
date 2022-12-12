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
-- Module      : Amazonka.Batch.CreateComputeEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Batch compute environment. You can create @MANAGED@ or
-- @UNMANAGED@ compute environments. @MANAGED@ compute environments can use
-- Amazon EC2 or Fargate resources. @UNMANAGED@ compute environments can
-- only use EC2 resources.
--
-- In a managed compute environment, Batch manages the capacity and
-- instance types of the compute resources within the environment. This is
-- based on the compute resource specification that you define or the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html launch template>
-- that you specify when you create the compute environment. Either, you
-- can choose to use EC2 On-Demand Instances and EC2 Spot Instances. Or,
-- you can use Fargate and Fargate Spot capacity in your managed compute
-- environment. You can optionally set a maximum price so that Spot
-- Instances only launch when the Spot Instance price is less than a
-- specified percentage of the On-Demand price.
--
-- Multi-node parallel jobs aren\'t supported on Spot Instances.
--
-- In an unmanaged compute environment, you can manage your own EC2 compute
-- resources and have flexibility with how you configure your compute
-- resources. For example, you can use custom AMIs. However, you must
-- verify that each of your AMIs meet the Amazon ECS container instance AMI
-- specification. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container_instance_AMIs.html container instance AMIs>
-- in the /Amazon Elastic Container Service Developer Guide/. After you
-- created your unmanaged compute environment, you can use the
-- DescribeComputeEnvironments operation to find the Amazon ECS cluster
-- that\'s associated with it. Then, launch your container instances into
-- that Amazon ECS cluster. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_container_instance.html Launching an Amazon ECS container instance>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- To create a compute environment that uses EKS resources, the caller must
-- have permissions to call @eks:DescribeCluster@.
--
-- Batch doesn\'t automatically upgrade the AMIs in a compute environment
-- after it\'s created. For example, it also doesn\'t update the AMIs in
-- your compute environment when a newer version of the Amazon ECS
-- optimized AMI is available. You\'re responsible for the management of
-- the guest operating system. This includes any updates and security
-- patches. You\'re also responsible for any additional application
-- software or utilities that you install on the compute resources. There
-- are two ways to use a new AMI for your Batch jobs. The original method
-- is to complete these steps:
--
-- 1.  Create a new compute environment with the new AMI.
--
-- 2.  Add the compute environment to an existing job queue.
--
-- 3.  Remove the earlier compute environment from your job queue.
--
-- 4.  Delete the earlier compute environment.
--
-- In April 2022, Batch added enhanced support for updating compute
-- environments. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>.
-- To use the enhanced updating of compute environments to update AMIs,
-- follow these rules:
--
-- -   Either don\'t set the service role (@serviceRole@) parameter or set
--     it to the __AWSBatchServiceRole__ service-linked role.
--
-- -   Set the allocation strategy (@allocationStrategy@) parameter to
--     @BEST_FIT_PROGRESSIVE@ or @SPOT_CAPACITY_OPTIMIZED@.
--
-- -   Set the update to latest image version
--     (@updateToLatestImageVersion@) parameter to @true@.
--
-- -   Don\'t specify an AMI ID in @imageId@, @imageIdOverride@ (in
--     <https://docs.aws.amazon.com/batch/latest/APIReference/API_Ec2Configuration.html ec2Configuration>
--     ), or in the launch template (@launchTemplate@). In that case, Batch
--     selects the latest Amazon ECS optimized AMI that\'s supported by
--     Batch at the time the infrastructure update is initiated.
--     Alternatively, you can specify the AMI ID in the @imageId@ or
--     @imageIdOverride@ parameters, or the launch template identified by
--     the @LaunchTemplate@ properties. Changing any of these properties
--     starts an infrastructure update. If the AMI ID is specified in the
--     launch template, it can\'t be replaced by specifying an AMI ID in
--     either the @imageId@ or @imageIdOverride@ parameters. It can only be
--     replaced by specifying a different launch template, or if the launch
--     template version is set to @$Default@ or @$Latest@, by setting
--     either a new default version for the launch template (if @$Default@)
--     or by adding a new version to the launch template (if @$Latest@).
--
-- If these rules are followed, any update that starts an infrastructure
-- update causes the AMI ID to be re-selected. If the @version@ setting in
-- the launch template (@launchTemplate@) is set to @$Latest@ or
-- @$Default@, the latest or default version of the launch template is
-- evaluated up at the time of the infrastructure update, even if the
-- @launchTemplate@ wasn\'t updated.
module Amazonka.Batch.CreateComputeEnvironment
  ( -- * Creating a Request
    CreateComputeEnvironment (..),
    newCreateComputeEnvironment,

    -- * Request Lenses
    createComputeEnvironment_computeResources,
    createComputeEnvironment_eksConfiguration,
    createComputeEnvironment_serviceRole,
    createComputeEnvironment_state,
    createComputeEnvironment_tags,
    createComputeEnvironment_unmanagedvCpus,
    createComputeEnvironment_computeEnvironmentName,
    createComputeEnvironment_type,

    -- * Destructuring the Response
    CreateComputeEnvironmentResponse (..),
    newCreateComputeEnvironmentResponse,

    -- * Response Lenses
    createComputeEnvironmentResponse_computeEnvironmentArn,
    createComputeEnvironmentResponse_computeEnvironmentName,
    createComputeEnvironmentResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @CreateComputeEnvironment@.
--
-- /See:/ 'newCreateComputeEnvironment' smart constructor.
data CreateComputeEnvironment = CreateComputeEnvironment'
  { -- | Details about the compute resources managed by the compute environment.
    -- This parameter is required for managed compute environments. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /Batch User Guide/.
    computeResources :: Prelude.Maybe ComputeResource,
    -- | The details for the Amazon EKS cluster that supports the compute
    -- environment.
    eksConfiguration :: Prelude.Maybe EksConfiguration,
    -- | The full Amazon Resource Name (ARN) of the IAM role that allows Batch to
    -- make calls to other Amazon Web Services services on your behalf. For
    -- more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
    -- in the /Batch User Guide/.
    --
    -- If your account already created the Batch service-linked role, that role
    -- is used by default for your compute environment unless you specify a
    -- different role here. If the Batch service-linked role doesn\'t exist in
    -- your account, and no role is specified here, the service attempts to
    -- create the Batch service-linked role in your account.
    --
    -- If your specified role has a path other than @\/@, then you must specify
    -- either the full role ARN (recommended) or prefix the role name with the
    -- path. For example, if a role with the name @bar@ has a path of
    -- @\/foo\/@, specify @\/foo\/bar@ as the role name. For more information,
    -- see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
    -- in the /IAM User Guide/.
    --
    -- Depending on how you created your Batch service role, its ARN might
    -- contain the @service-role@ path prefix. When you only specify the name
    -- of the service role, Batch assumes that your ARN doesn\'t use the
    -- @service-role@ path prefix. Because of this, we recommend that you
    -- specify the full ARN of your service role when you create compute
    -- environments.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The state of the compute environment. If the state is @ENABLED@, then
    -- the compute environment accepts jobs from a queue and can scale out
    -- automatically based on queues.
    --
    -- If the state is @ENABLED@, then the Batch scheduler can attempt to place
    -- jobs from an associated job queue on the compute resources within the
    -- environment. If the compute environment is managed, then it can scale
    -- its instances out or in automatically, based on the job queue demand.
    --
    -- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
    -- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
    -- state continue to progress normally. Managed compute environments in the
    -- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
    -- value after instances become idle.
    state :: Prelude.Maybe CEState,
    -- | The tags that you apply to the compute environment to help you
    -- categorize and organize your resources. Each tag consists of a key and
    -- an optional value. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in /Amazon Web Services General Reference/.
    --
    -- These tags can be updated or removed using the
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
    -- and
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
    -- API operations. These tags don\'t propagate to the underlying compute
    -- resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The maximum number of vCPUs for an unmanaged compute environment. This
    -- parameter is only used for fair share scheduling to reserve vCPU
    -- capacity for new share identifiers. If this parameter isn\'t provided
    -- for a fair share job queue, no vCPU capacity is reserved.
    --
    -- This parameter is only supported when the @type@ parameter is set to
    -- @UNMANAGED@.
    unmanagedvCpus :: Prelude.Maybe Prelude.Int,
    -- | The name for your compute environment. It can be up to 128 characters
    -- long. It can contain uppercase and lowercase letters, numbers, hyphens
    -- (-), and underscores (_).
    computeEnvironmentName :: Prelude.Text,
    -- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /Batch User Guide/.
    type' :: CEType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComputeEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeResources', 'createComputeEnvironment_computeResources' - Details about the compute resources managed by the compute environment.
-- This parameter is required for managed compute environments. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
--
-- 'eksConfiguration', 'createComputeEnvironment_eksConfiguration' - The details for the Amazon EKS cluster that supports the compute
-- environment.
--
-- 'serviceRole', 'createComputeEnvironment_serviceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows Batch to
-- make calls to other Amazon Web Services services on your behalf. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
--
-- If your account already created the Batch service-linked role, that role
-- is used by default for your compute environment unless you specify a
-- different role here. If the Batch service-linked role doesn\'t exist in
-- your account, and no role is specified here, the service attempts to
-- create the Batch service-linked role in your account.
--
-- If your specified role has a path other than @\/@, then you must specify
-- either the full role ARN (recommended) or prefix the role name with the
-- path. For example, if a role with the name @bar@ has a path of
-- @\/foo\/@, specify @\/foo\/bar@ as the role name. For more information,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
-- in the /IAM User Guide/.
--
-- Depending on how you created your Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, Batch assumes that your ARN doesn\'t use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
--
-- 'state', 'createComputeEnvironment_state' - The state of the compute environment. If the state is @ENABLED@, then
-- the compute environment accepts jobs from a queue and can scale out
-- automatically based on queues.
--
-- If the state is @ENABLED@, then the Batch scheduler can attempt to place
-- jobs from an associated job queue on the compute resources within the
-- environment. If the compute environment is managed, then it can scale
-- its instances out or in automatically, based on the job queue demand.
--
-- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
-- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
-- state continue to progress normally. Managed compute environments in the
-- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
-- value after instances become idle.
--
-- 'tags', 'createComputeEnvironment_tags' - The tags that you apply to the compute environment to help you
-- categorize and organize your resources. Each tag consists of a key and
-- an optional value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
--
-- These tags can be updated or removed using the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
-- and
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
-- API operations. These tags don\'t propagate to the underlying compute
-- resources.
--
-- 'unmanagedvCpus', 'createComputeEnvironment_unmanagedvCpus' - The maximum number of vCPUs for an unmanaged compute environment. This
-- parameter is only used for fair share scheduling to reserve vCPU
-- capacity for new share identifiers. If this parameter isn\'t provided
-- for a fair share job queue, no vCPU capacity is reserved.
--
-- This parameter is only supported when the @type@ parameter is set to
-- @UNMANAGED@.
--
-- 'computeEnvironmentName', 'createComputeEnvironment_computeEnvironmentName' - The name for your compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
--
-- 'type'', 'createComputeEnvironment_type' - The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
newCreateComputeEnvironment ::
  -- | 'computeEnvironmentName'
  Prelude.Text ->
  -- | 'type''
  CEType ->
  CreateComputeEnvironment
newCreateComputeEnvironment
  pComputeEnvironmentName_
  pType_ =
    CreateComputeEnvironment'
      { computeResources =
          Prelude.Nothing,
        eksConfiguration = Prelude.Nothing,
        serviceRole = Prelude.Nothing,
        state = Prelude.Nothing,
        tags = Prelude.Nothing,
        unmanagedvCpus = Prelude.Nothing,
        computeEnvironmentName = pComputeEnvironmentName_,
        type' = pType_
      }

-- | Details about the compute resources managed by the compute environment.
-- This parameter is required for managed compute environments. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
createComputeEnvironment_computeResources :: Lens.Lens' CreateComputeEnvironment (Prelude.Maybe ComputeResource)
createComputeEnvironment_computeResources = Lens.lens (\CreateComputeEnvironment' {computeResources} -> computeResources) (\s@CreateComputeEnvironment' {} a -> s {computeResources = a} :: CreateComputeEnvironment)

-- | The details for the Amazon EKS cluster that supports the compute
-- environment.
createComputeEnvironment_eksConfiguration :: Lens.Lens' CreateComputeEnvironment (Prelude.Maybe EksConfiguration)
createComputeEnvironment_eksConfiguration = Lens.lens (\CreateComputeEnvironment' {eksConfiguration} -> eksConfiguration) (\s@CreateComputeEnvironment' {} a -> s {eksConfiguration = a} :: CreateComputeEnvironment)

-- | The full Amazon Resource Name (ARN) of the IAM role that allows Batch to
-- make calls to other Amazon Web Services services on your behalf. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/service_IAM_role.html Batch service IAM role>
-- in the /Batch User Guide/.
--
-- If your account already created the Batch service-linked role, that role
-- is used by default for your compute environment unless you specify a
-- different role here. If the Batch service-linked role doesn\'t exist in
-- your account, and no role is specified here, the service attempts to
-- create the Batch service-linked role in your account.
--
-- If your specified role has a path other than @\/@, then you must specify
-- either the full role ARN (recommended) or prefix the role name with the
-- path. For example, if a role with the name @bar@ has a path of
-- @\/foo\/@, specify @\/foo\/bar@ as the role name. For more information,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly names and paths>
-- in the /IAM User Guide/.
--
-- Depending on how you created your Batch service role, its ARN might
-- contain the @service-role@ path prefix. When you only specify the name
-- of the service role, Batch assumes that your ARN doesn\'t use the
-- @service-role@ path prefix. Because of this, we recommend that you
-- specify the full ARN of your service role when you create compute
-- environments.
createComputeEnvironment_serviceRole :: Lens.Lens' CreateComputeEnvironment (Prelude.Maybe Prelude.Text)
createComputeEnvironment_serviceRole = Lens.lens (\CreateComputeEnvironment' {serviceRole} -> serviceRole) (\s@CreateComputeEnvironment' {} a -> s {serviceRole = a} :: CreateComputeEnvironment)

-- | The state of the compute environment. If the state is @ENABLED@, then
-- the compute environment accepts jobs from a queue and can scale out
-- automatically based on queues.
--
-- If the state is @ENABLED@, then the Batch scheduler can attempt to place
-- jobs from an associated job queue on the compute resources within the
-- environment. If the compute environment is managed, then it can scale
-- its instances out or in automatically, based on the job queue demand.
--
-- If the state is @DISABLED@, then the Batch scheduler doesn\'t attempt to
-- place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@
-- state continue to progress normally. Managed compute environments in the
-- @DISABLED@ state don\'t scale out. However, they scale in to @minvCpus@
-- value after instances become idle.
createComputeEnvironment_state :: Lens.Lens' CreateComputeEnvironment (Prelude.Maybe CEState)
createComputeEnvironment_state = Lens.lens (\CreateComputeEnvironment' {state} -> state) (\s@CreateComputeEnvironment' {} a -> s {state = a} :: CreateComputeEnvironment)

-- | The tags that you apply to the compute environment to help you
-- categorize and organize your resources. Each tag consists of a key and
-- an optional value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
--
-- These tags can be updated or removed using the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_TagResource.html TagResource>
-- and
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_UntagResource.html UntagResource>
-- API operations. These tags don\'t propagate to the underlying compute
-- resources.
createComputeEnvironment_tags :: Lens.Lens' CreateComputeEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createComputeEnvironment_tags = Lens.lens (\CreateComputeEnvironment' {tags} -> tags) (\s@CreateComputeEnvironment' {} a -> s {tags = a} :: CreateComputeEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of vCPUs for an unmanaged compute environment. This
-- parameter is only used for fair share scheduling to reserve vCPU
-- capacity for new share identifiers. If this parameter isn\'t provided
-- for a fair share job queue, no vCPU capacity is reserved.
--
-- This parameter is only supported when the @type@ parameter is set to
-- @UNMANAGED@.
createComputeEnvironment_unmanagedvCpus :: Lens.Lens' CreateComputeEnvironment (Prelude.Maybe Prelude.Int)
createComputeEnvironment_unmanagedvCpus = Lens.lens (\CreateComputeEnvironment' {unmanagedvCpus} -> unmanagedvCpus) (\s@CreateComputeEnvironment' {} a -> s {unmanagedvCpus = a} :: CreateComputeEnvironment)

-- | The name for your compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
createComputeEnvironment_computeEnvironmentName :: Lens.Lens' CreateComputeEnvironment Prelude.Text
createComputeEnvironment_computeEnvironmentName = Lens.lens (\CreateComputeEnvironment' {computeEnvironmentName} -> computeEnvironmentName) (\s@CreateComputeEnvironment' {} a -> s {computeEnvironmentName = a} :: CreateComputeEnvironment)

-- | The type of the compute environment: @MANAGED@ or @UNMANAGED@. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
createComputeEnvironment_type :: Lens.Lens' CreateComputeEnvironment CEType
createComputeEnvironment_type = Lens.lens (\CreateComputeEnvironment' {type'} -> type') (\s@CreateComputeEnvironment' {} a -> s {type' = a} :: CreateComputeEnvironment)

instance Core.AWSRequest CreateComputeEnvironment where
  type
    AWSResponse CreateComputeEnvironment =
      CreateComputeEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComputeEnvironmentResponse'
            Prelude.<$> (x Data..?> "computeEnvironmentArn")
            Prelude.<*> (x Data..?> "computeEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateComputeEnvironment where
  hashWithSalt _salt CreateComputeEnvironment' {..} =
    _salt `Prelude.hashWithSalt` computeResources
      `Prelude.hashWithSalt` eksConfiguration
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` unmanagedvCpus
      `Prelude.hashWithSalt` computeEnvironmentName
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateComputeEnvironment where
  rnf CreateComputeEnvironment' {..} =
    Prelude.rnf computeResources
      `Prelude.seq` Prelude.rnf eksConfiguration
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf unmanagedvCpus
      `Prelude.seq` Prelude.rnf computeEnvironmentName
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateComputeEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateComputeEnvironment where
  toJSON CreateComputeEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("computeResources" Data..=)
              Prelude.<$> computeResources,
            ("eksConfiguration" Data..=)
              Prelude.<$> eksConfiguration,
            ("serviceRole" Data..=) Prelude.<$> serviceRole,
            ("state" Data..=) Prelude.<$> state,
            ("tags" Data..=) Prelude.<$> tags,
            ("unmanagedvCpus" Data..=)
              Prelude.<$> unmanagedvCpus,
            Prelude.Just
              ( "computeEnvironmentName"
                  Data..= computeEnvironmentName
              ),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateComputeEnvironment where
  toPath = Prelude.const "/v1/createcomputeenvironment"

instance Data.ToQuery CreateComputeEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateComputeEnvironmentResponse' smart constructor.
data CreateComputeEnvironmentResponse = CreateComputeEnvironmentResponse'
  { -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironmentArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute environment. It can be up to 128 characters
    -- long. It can contain uppercase and lowercase letters, numbers, hyphens
    -- (-), and underscores (_).
    computeEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComputeEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeEnvironmentArn', 'createComputeEnvironmentResponse_computeEnvironmentArn' - The Amazon Resource Name (ARN) of the compute environment.
--
-- 'computeEnvironmentName', 'createComputeEnvironmentResponse_computeEnvironmentName' - The name of the compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
--
-- 'httpStatus', 'createComputeEnvironmentResponse_httpStatus' - The response's http status code.
newCreateComputeEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateComputeEnvironmentResponse
newCreateComputeEnvironmentResponse pHttpStatus_ =
  CreateComputeEnvironmentResponse'
    { computeEnvironmentArn =
        Prelude.Nothing,
      computeEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the compute environment.
createComputeEnvironmentResponse_computeEnvironmentArn :: Lens.Lens' CreateComputeEnvironmentResponse (Prelude.Maybe Prelude.Text)
createComputeEnvironmentResponse_computeEnvironmentArn = Lens.lens (\CreateComputeEnvironmentResponse' {computeEnvironmentArn} -> computeEnvironmentArn) (\s@CreateComputeEnvironmentResponse' {} a -> s {computeEnvironmentArn = a} :: CreateComputeEnvironmentResponse)

-- | The name of the compute environment. It can be up to 128 characters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
createComputeEnvironmentResponse_computeEnvironmentName :: Lens.Lens' CreateComputeEnvironmentResponse (Prelude.Maybe Prelude.Text)
createComputeEnvironmentResponse_computeEnvironmentName = Lens.lens (\CreateComputeEnvironmentResponse' {computeEnvironmentName} -> computeEnvironmentName) (\s@CreateComputeEnvironmentResponse' {} a -> s {computeEnvironmentName = a} :: CreateComputeEnvironmentResponse)

-- | The response's http status code.
createComputeEnvironmentResponse_httpStatus :: Lens.Lens' CreateComputeEnvironmentResponse Prelude.Int
createComputeEnvironmentResponse_httpStatus = Lens.lens (\CreateComputeEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateComputeEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateComputeEnvironmentResponse)

instance
  Prelude.NFData
    CreateComputeEnvironmentResponse
  where
  rnf CreateComputeEnvironmentResponse' {..} =
    Prelude.rnf computeEnvironmentArn
      `Prelude.seq` Prelude.rnf computeEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
