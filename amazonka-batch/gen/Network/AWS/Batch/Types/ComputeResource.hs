{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeResource where

import Network.AWS.Batch.Types.CRAllocationStrategy
import Network.AWS.Batch.Types.CRType
import Network.AWS.Batch.Types.Ec2Configuration
import Network.AWS.Batch.Types.LaunchTemplateSpecification
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an AWS Batch compute resource. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
--
-- /See:/ 'newComputeResource' smart constructor.
data ComputeResource = ComputeResource'
  { -- | The Amazon EC2 security groups associated with instances launched in the
    -- compute environment. One or more security groups must be specified,
    -- either in @securityGroupIds@ or using a launch template referenced in
    -- @launchTemplate@. This parameter is required for jobs running on Fargate
    -- resources and must contain at least one security group. (Fargate does
    -- not support launch templates.) If security groups are specified using
    -- both @securityGroupIds@ and @launchTemplate@, the values in
    -- @securityGroupIds@ will be used.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum percentage that a Spot Instance price can be when compared
    -- with the On-Demand price for that instance type before instances are
    -- launched. For example, if your maximum percentage is 20%, then the Spot
    -- price must be less than 20% of the current On-Demand price for that
    -- Amazon EC2 instance. You always pay the lowest (market) price and never
    -- more than your maximum percentage. If you leave this field empty, the
    -- default value is 100% of the On-Demand price.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    bidPercentage :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of Amazon EC2 vCPUs that an environment should
    -- maintain (even if the compute environment is @DISABLED@).
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    minvCpus :: Prelude.Maybe Prelude.Int,
    -- | The Amazon EC2 key pair that\'s used for instances launched in the
    -- compute environment. You can use this key pair to log in to your
    -- instances with SSH.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    ec2KeyPair :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 placement group to associate with your compute resources.
    -- If you intend to submit multi-node parallel jobs to your compute
    -- environment, you should consider creating a cluster placement group and
    -- associate it with your compute resources. This keeps your multi-node
    -- parallel job on a logical grouping of instances within a single
    -- Availability Zone with high network flow potential. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The launch template to use for your compute resources. Any other compute
    -- resource parameters that you specify in a CreateComputeEnvironment API
    -- operation override the same parameters in the launch template. You must
    -- specify either the launch template ID or launch template name in the
    -- request, but not both. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch Template Support>
    -- in the /AWS Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The Amazon Machine Image (AMI) ID used for instances launched in the
    -- compute environment. This parameter is overridden by the
    -- @imageIdOverride@ member of the @Ec2Configuration@ structure.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    --
    -- The AMI that you choose for a compute environment must match the
    -- architecture of the instance types that you intend to use for that
    -- compute environment. For example, if your compute environment uses A1
    -- instance types, the compute resource AMI that you choose must support
    -- ARM instances. Amazon ECS vends both x86 and ARM versions of the Amazon
    -- ECS-optimized Amazon Linux 2 AMI. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#ecs-optimized-ami-linux-variants.html Amazon ECS-optimized Amazon Linux 2 AMI>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role
    -- applied to a @SPOT@ compute environment. This role is required if the
    -- allocation strategy set to @BEST_FIT@ or if the allocation strategy
    -- isn\'t specified. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet Role>
    -- in the /AWS Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    --
    -- To tag your Spot Instances on creation, the Spot Fleet IAM role
    -- specified here must use the newer __AmazonEC2SpotFleetTaggingRole__
    -- managed policy. The previously recommended __AmazonEC2SpotFleetRole__
    -- managed policy doesn\'t have the required permissions to tag Spot
    -- Instances. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#spot-instance-no-tag Spot Instances not tagged on creation>
    -- in the /AWS Batch User Guide/.
    spotIamFleetRole :: Prelude.Maybe Prelude.Text,
    -- | Provides information used to select Amazon Machine Images (AMIs) for EC2
    -- instances in the compute environment. If @Ec2Configuration@ isn\'t
    -- specified, the default is @ECS_AL1@.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    ec2Configuration :: Prelude.Maybe [Ec2Configuration],
    -- | Key-value pair tags to be applied to EC2 resources that are launched in
    -- the compute environment. For AWS Batch, these take the form of
    -- \"String1\": \"String2\", where String1 is the tag key and String2 is
    -- the tag value−for example, { \"Name\": \"AWS Batch Instance -
    -- C4OnDemand\" }. This is helpful for recognizing your AWS Batch instances
    -- in the Amazon EC2 console. These tags can\'t be updated or removed after
    -- the compute environment has been created; any changes require creating a
    -- new compute environment and removing the old compute environment. These
    -- tags are not seen when using the AWS Batch @ListTagsForResource@ API
    -- operation.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The desired number of Amazon EC2 vCPUS in the compute environment. AWS
    -- Batch modifies this value between the minimum and maximum values, based
    -- on job queue demand.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    desiredvCpus :: Prelude.Maybe Prelude.Int,
    -- | The allocation strategy to use for the compute resource if not enough
    -- instances of the best fitting instance type can be allocated. This might
    -- be because of availability of the instance type in the Region or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation Strategies>
    -- in the /AWS Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    --
    -- [BEST_FIT (default)]
    --     AWS Batch selects an instance type that best fits the needs of the
    --     jobs with a preference for the lowest-cost instance type. If
    --     additional instances of the selected instance type aren\'t
    --     available, AWS Batch will wait for the additional instances to be
    --     available. If there are not enough instances available, or if the
    --     user is hitting
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>
    --     then additional jobs aren\'t run until currently running jobs have
    --     completed. This allocation strategy keeps costs lower but can limit
    --     scaling. If you are using Spot Fleets with @BEST_FIT@ then the Spot
    --     Fleet IAM Role must be specified.
    --
    -- [BEST_FIT_PROGRESSIVE]
    --     AWS Batch will select additional instance types that are large
    --     enough to meet the requirements of the jobs in the queue, with a
    --     preference for instance types with a lower cost per unit vCPU. If
    --     additional instances of the previously selected instance types
    --     aren\'t available, AWS Batch will select new instance types.
    --
    -- [SPOT_CAPACITY_OPTIMIZED]
    --     AWS Batch will select one or more instance types that are large
    --     enough to meet the requirements of the jobs in the queue, with a
    --     preference for instance types that are less likely to be
    --     interrupted. This allocation strategy is only available for Spot
    --     Instance compute resources.
    --
    -- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
    -- strategies, AWS Batch might need to go above @maxvCpus@ to meet your
    -- capacity requirements. In this event, AWS Batch never exceeds @maxvCpus@
    -- by more than a single instance.
    allocationStrategy :: Prelude.Maybe CRAllocationStrategy,
    -- | The Amazon ECS instance profile applied to Amazon EC2 instances in a
    -- compute environment. You can specify the short name or full Amazon
    -- Resource Name (ARN) of an instance profile. For example,
    -- @ ecsInstanceRole @ or
    -- @arn:aws:iam::\<aws_account_id>:instance-profile\/ecsInstanceRole @. For
    -- more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS Instance Role>
    -- in the /AWS Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    instanceRole :: Prelude.Maybe Prelude.Text,
    -- | The instances types that can be launched. You can specify instance
    -- families to launch any instance type within those families (for example,
    -- @c5@ or @p3@), or you can specify specific sizes within a family (such
    -- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
    -- (from the C4, M4, and R4 instance families) on the fly that match the
    -- demand of your job queues.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    --
    -- When you create a compute environment, the instance types that you
    -- select for the compute environment must share the same architecture. For
    -- example, you can\'t mix x86 and ARM instances in the same compute
    -- environment.
    --
    -- Currently, @optimal@ uses instance types from the C4, M4, and R4
    -- instance families. In Regions that don\'t have instance types from those
    -- instance families, instance types from the C5, M5. and R5 instance
    -- families are used.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
    -- @FARGATE_SPOT@. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
    -- in the /AWS Batch User Guide/.
    --
    -- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
    -- role with the @spotIamFleetRole@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet role>
    -- in the /AWS Batch User Guide/.
    type' :: CRType,
    -- | The maximum number of Amazon EC2 vCPUs that a compute environment can
    -- reach.
    --
    -- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
    -- allocation strategies, AWS Batch might need to go above @maxvCpus@ to
    -- meet your capacity requirements. In this event, AWS Batch will never go
    -- above @maxvCpus@ by more than a single instance (e.g., no more than a
    -- single instance from among those specified in your compute environment).
    maxvCpus :: Prelude.Int,
    -- | The VPC subnets into which the compute resources are launched. These
    -- subnets must be within the same VPC. This parameter is required for jobs
    -- running on Fargate resources, where it can contain up to 16 subnets. For
    -- more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
    -- in the /Amazon VPC User Guide/.
    subnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'computeResource_securityGroupIds' - The Amazon EC2 security groups associated with instances launched in the
-- compute environment. One or more security groups must be specified,
-- either in @securityGroupIds@ or using a launch template referenced in
-- @launchTemplate@. This parameter is required for jobs running on Fargate
-- resources and must contain at least one security group. (Fargate does
-- not support launch templates.) If security groups are specified using
-- both @securityGroupIds@ and @launchTemplate@, the values in
-- @securityGroupIds@ will be used.
--
-- 'bidPercentage', 'computeResource_bidPercentage' - The maximum percentage that a Spot Instance price can be when compared
-- with the On-Demand price for that instance type before instances are
-- launched. For example, if your maximum percentage is 20%, then the Spot
-- price must be less than 20% of the current On-Demand price for that
-- Amazon EC2 instance. You always pay the lowest (market) price and never
-- more than your maximum percentage. If you leave this field empty, the
-- default value is 100% of the On-Demand price.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'minvCpus', 'computeResource_minvCpus' - The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain (even if the compute environment is @DISABLED@).
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'ec2KeyPair', 'computeResource_ec2KeyPair' - The Amazon EC2 key pair that\'s used for instances launched in the
-- compute environment. You can use this key pair to log in to your
-- instances with SSH.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'placementGroup', 'computeResource_placementGroup' - The Amazon EC2 placement group to associate with your compute resources.
-- If you intend to submit multi-node parallel jobs to your compute
-- environment, you should consider creating a cluster placement group and
-- associate it with your compute resources. This keeps your multi-node
-- parallel job on a logical grouping of instances within a single
-- Availability Zone with high network flow potential. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'launchTemplate', 'computeResource_launchTemplate' - The launch template to use for your compute resources. Any other compute
-- resource parameters that you specify in a CreateComputeEnvironment API
-- operation override the same parameters in the launch template. You must
-- specify either the launch template ID or launch template name in the
-- request, but not both. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch Template Support>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'imageId', 'computeResource_imageId' - The Amazon Machine Image (AMI) ID used for instances launched in the
-- compute environment. This parameter is overridden by the
-- @imageIdOverride@ member of the @Ec2Configuration@ structure.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- The AMI that you choose for a compute environment must match the
-- architecture of the instance types that you intend to use for that
-- compute environment. For example, if your compute environment uses A1
-- instance types, the compute resource AMI that you choose must support
-- ARM instances. Amazon ECS vends both x86 and ARM versions of the Amazon
-- ECS-optimized Amazon Linux 2 AMI. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#ecs-optimized-ami-linux-variants.html Amazon ECS-optimized Amazon Linux 2 AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'spotIamFleetRole', 'computeResource_spotIamFleetRole' - The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role
-- applied to a @SPOT@ compute environment. This role is required if the
-- allocation strategy set to @BEST_FIT@ or if the allocation strategy
-- isn\'t specified. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet Role>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- To tag your Spot Instances on creation, the Spot Fleet IAM role
-- specified here must use the newer __AmazonEC2SpotFleetTaggingRole__
-- managed policy. The previously recommended __AmazonEC2SpotFleetRole__
-- managed policy doesn\'t have the required permissions to tag Spot
-- Instances. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#spot-instance-no-tag Spot Instances not tagged on creation>
-- in the /AWS Batch User Guide/.
--
-- 'ec2Configuration', 'computeResource_ec2Configuration' - Provides information used to select Amazon Machine Images (AMIs) for EC2
-- instances in the compute environment. If @Ec2Configuration@ isn\'t
-- specified, the default is @ECS_AL1@.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'tags', 'computeResource_tags' - Key-value pair tags to be applied to EC2 resources that are launched in
-- the compute environment. For AWS Batch, these take the form of
-- \"String1\": \"String2\", where String1 is the tag key and String2 is
-- the tag value−for example, { \"Name\": \"AWS Batch Instance -
-- C4OnDemand\" }. This is helpful for recognizing your AWS Batch instances
-- in the Amazon EC2 console. These tags can\'t be updated or removed after
-- the compute environment has been created; any changes require creating a
-- new compute environment and removing the old compute environment. These
-- tags are not seen when using the AWS Batch @ListTagsForResource@ API
-- operation.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'desiredvCpus', 'computeResource_desiredvCpus' - The desired number of Amazon EC2 vCPUS in the compute environment. AWS
-- Batch modifies this value between the minimum and maximum values, based
-- on job queue demand.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'allocationStrategy', 'computeResource_allocationStrategy' - The allocation strategy to use for the compute resource if not enough
-- instances of the best fitting instance type can be allocated. This might
-- be because of availability of the instance type in the Region or
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation Strategies>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- [BEST_FIT (default)]
--     AWS Batch selects an instance type that best fits the needs of the
--     jobs with a preference for the lowest-cost instance type. If
--     additional instances of the selected instance type aren\'t
--     available, AWS Batch will wait for the additional instances to be
--     available. If there are not enough instances available, or if the
--     user is hitting
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>
--     then additional jobs aren\'t run until currently running jobs have
--     completed. This allocation strategy keeps costs lower but can limit
--     scaling. If you are using Spot Fleets with @BEST_FIT@ then the Spot
--     Fleet IAM Role must be specified.
--
-- [BEST_FIT_PROGRESSIVE]
--     AWS Batch will select additional instance types that are large
--     enough to meet the requirements of the jobs in the queue, with a
--     preference for instance types with a lower cost per unit vCPU. If
--     additional instances of the previously selected instance types
--     aren\'t available, AWS Batch will select new instance types.
--
-- [SPOT_CAPACITY_OPTIMIZED]
--     AWS Batch will select one or more instance types that are large
--     enough to meet the requirements of the jobs in the queue, with a
--     preference for instance types that are less likely to be
--     interrupted. This allocation strategy is only available for Spot
--     Instance compute resources.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- strategies, AWS Batch might need to go above @maxvCpus@ to meet your
-- capacity requirements. In this event, AWS Batch never exceeds @maxvCpus@
-- by more than a single instance.
--
-- 'instanceRole', 'computeResource_instanceRole' - The Amazon ECS instance profile applied to Amazon EC2 instances in a
-- compute environment. You can specify the short name or full Amazon
-- Resource Name (ARN) of an instance profile. For example,
-- @ ecsInstanceRole @ or
-- @arn:aws:iam::\<aws_account_id>:instance-profile\/ecsInstanceRole @. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS Instance Role>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'instanceTypes', 'computeResource_instanceTypes' - The instances types that can be launched. You can specify instance
-- families to launch any instance type within those families (for example,
-- @c5@ or @p3@), or you can specify specific sizes within a family (such
-- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
-- (from the C4, M4, and R4 instance families) on the fly that match the
-- demand of your job queues.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- When you create a compute environment, the instance types that you
-- select for the compute environment must share the same architecture. For
-- example, you can\'t mix x86 and ARM instances in the same compute
-- environment.
--
-- Currently, @optimal@ uses instance types from the C4, M4, and R4
-- instance families. In Regions that don\'t have instance types from those
-- instance families, instance types from the C5, M5. and R5 instance
-- families are used.
--
-- 'type'', 'computeResource_type' - The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
-- @FARGATE_SPOT@. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
--
-- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
-- role with the @spotIamFleetRole@ parameter. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet role>
-- in the /AWS Batch User Guide/.
--
-- 'maxvCpus', 'computeResource_maxvCpus' - The maximum number of Amazon EC2 vCPUs that a compute environment can
-- reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies, AWS Batch might need to go above @maxvCpus@ to
-- meet your capacity requirements. In this event, AWS Batch will never go
-- above @maxvCpus@ by more than a single instance (e.g., no more than a
-- single instance from among those specified in your compute environment).
--
-- 'subnets', 'computeResource_subnets' - The VPC subnets into which the compute resources are launched. These
-- subnets must be within the same VPC. This parameter is required for jobs
-- running on Fargate resources, where it can contain up to 16 subnets. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
-- in the /Amazon VPC User Guide/.
newComputeResource ::
  -- | 'type''
  CRType ->
  -- | 'maxvCpus'
  Prelude.Int ->
  ComputeResource
newComputeResource pType_ pMaxvCpus_ =
  ComputeResource'
    { securityGroupIds =
        Prelude.Nothing,
      bidPercentage = Prelude.Nothing,
      minvCpus = Prelude.Nothing,
      ec2KeyPair = Prelude.Nothing,
      placementGroup = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      imageId = Prelude.Nothing,
      spotIamFleetRole = Prelude.Nothing,
      ec2Configuration = Prelude.Nothing,
      tags = Prelude.Nothing,
      desiredvCpus = Prelude.Nothing,
      allocationStrategy = Prelude.Nothing,
      instanceRole = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      type' = pType_,
      maxvCpus = pMaxvCpus_,
      subnets = Prelude.mempty
    }

-- | The Amazon EC2 security groups associated with instances launched in the
-- compute environment. One or more security groups must be specified,
-- either in @securityGroupIds@ or using a launch template referenced in
-- @launchTemplate@. This parameter is required for jobs running on Fargate
-- resources and must contain at least one security group. (Fargate does
-- not support launch templates.) If security groups are specified using
-- both @securityGroupIds@ and @launchTemplate@, the values in
-- @securityGroupIds@ will be used.
computeResource_securityGroupIds :: Lens.Lens' ComputeResource (Prelude.Maybe [Prelude.Text])
computeResource_securityGroupIds = Lens.lens (\ComputeResource' {securityGroupIds} -> securityGroupIds) (\s@ComputeResource' {} a -> s {securityGroupIds = a} :: ComputeResource) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum percentage that a Spot Instance price can be when compared
-- with the On-Demand price for that instance type before instances are
-- launched. For example, if your maximum percentage is 20%, then the Spot
-- price must be less than 20% of the current On-Demand price for that
-- Amazon EC2 instance. You always pay the lowest (market) price and never
-- more than your maximum percentage. If you leave this field empty, the
-- default value is 100% of the On-Demand price.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_bidPercentage :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Int)
computeResource_bidPercentage = Lens.lens (\ComputeResource' {bidPercentage} -> bidPercentage) (\s@ComputeResource' {} a -> s {bidPercentage = a} :: ComputeResource)

-- | The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain (even if the compute environment is @DISABLED@).
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_minvCpus :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Int)
computeResource_minvCpus = Lens.lens (\ComputeResource' {minvCpus} -> minvCpus) (\s@ComputeResource' {} a -> s {minvCpus = a} :: ComputeResource)

-- | The Amazon EC2 key pair that\'s used for instances launched in the
-- compute environment. You can use this key pair to log in to your
-- instances with SSH.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_ec2KeyPair :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_ec2KeyPair = Lens.lens (\ComputeResource' {ec2KeyPair} -> ec2KeyPair) (\s@ComputeResource' {} a -> s {ec2KeyPair = a} :: ComputeResource)

-- | The Amazon EC2 placement group to associate with your compute resources.
-- If you intend to submit multi-node parallel jobs to your compute
-- environment, you should consider creating a cluster placement group and
-- associate it with your compute resources. This keeps your multi-node
-- parallel job on a logical grouping of instances within a single
-- Availability Zone with high network flow potential. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_placementGroup :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_placementGroup = Lens.lens (\ComputeResource' {placementGroup} -> placementGroup) (\s@ComputeResource' {} a -> s {placementGroup = a} :: ComputeResource)

-- | The launch template to use for your compute resources. Any other compute
-- resource parameters that you specify in a CreateComputeEnvironment API
-- operation override the same parameters in the launch template. You must
-- specify either the launch template ID or launch template name in the
-- request, but not both. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch Template Support>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_launchTemplate :: Lens.Lens' ComputeResource (Prelude.Maybe LaunchTemplateSpecification)
computeResource_launchTemplate = Lens.lens (\ComputeResource' {launchTemplate} -> launchTemplate) (\s@ComputeResource' {} a -> s {launchTemplate = a} :: ComputeResource)

-- | The Amazon Machine Image (AMI) ID used for instances launched in the
-- compute environment. This parameter is overridden by the
-- @imageIdOverride@ member of the @Ec2Configuration@ structure.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- The AMI that you choose for a compute environment must match the
-- architecture of the instance types that you intend to use for that
-- compute environment. For example, if your compute environment uses A1
-- instance types, the compute resource AMI that you choose must support
-- ARM instances. Amazon ECS vends both x86 and ARM versions of the Amazon
-- ECS-optimized Amazon Linux 2 AMI. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#ecs-optimized-ami-linux-variants.html Amazon ECS-optimized Amazon Linux 2 AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
computeResource_imageId :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_imageId = Lens.lens (\ComputeResource' {imageId} -> imageId) (\s@ComputeResource' {} a -> s {imageId = a} :: ComputeResource)

-- | The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role
-- applied to a @SPOT@ compute environment. This role is required if the
-- allocation strategy set to @BEST_FIT@ or if the allocation strategy
-- isn\'t specified. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet Role>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- To tag your Spot Instances on creation, the Spot Fleet IAM role
-- specified here must use the newer __AmazonEC2SpotFleetTaggingRole__
-- managed policy. The previously recommended __AmazonEC2SpotFleetRole__
-- managed policy doesn\'t have the required permissions to tag Spot
-- Instances. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#spot-instance-no-tag Spot Instances not tagged on creation>
-- in the /AWS Batch User Guide/.
computeResource_spotIamFleetRole :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_spotIamFleetRole = Lens.lens (\ComputeResource' {spotIamFleetRole} -> spotIamFleetRole) (\s@ComputeResource' {} a -> s {spotIamFleetRole = a} :: ComputeResource)

-- | Provides information used to select Amazon Machine Images (AMIs) for EC2
-- instances in the compute environment. If @Ec2Configuration@ isn\'t
-- specified, the default is @ECS_AL1@.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_ec2Configuration :: Lens.Lens' ComputeResource (Prelude.Maybe [Ec2Configuration])
computeResource_ec2Configuration = Lens.lens (\ComputeResource' {ec2Configuration} -> ec2Configuration) (\s@ComputeResource' {} a -> s {ec2Configuration = a} :: ComputeResource) Prelude.. Lens.mapping Lens._Coerce

-- | Key-value pair tags to be applied to EC2 resources that are launched in
-- the compute environment. For AWS Batch, these take the form of
-- \"String1\": \"String2\", where String1 is the tag key and String2 is
-- the tag value−for example, { \"Name\": \"AWS Batch Instance -
-- C4OnDemand\" }. This is helpful for recognizing your AWS Batch instances
-- in the Amazon EC2 console. These tags can\'t be updated or removed after
-- the compute environment has been created; any changes require creating a
-- new compute environment and removing the old compute environment. These
-- tags are not seen when using the AWS Batch @ListTagsForResource@ API
-- operation.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_tags :: Lens.Lens' ComputeResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
computeResource_tags = Lens.lens (\ComputeResource' {tags} -> tags) (\s@ComputeResource' {} a -> s {tags = a} :: ComputeResource) Prelude.. Lens.mapping Lens._Coerce

-- | The desired number of Amazon EC2 vCPUS in the compute environment. AWS
-- Batch modifies this value between the minimum and maximum values, based
-- on job queue demand.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_desiredvCpus :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Int)
computeResource_desiredvCpus = Lens.lens (\ComputeResource' {desiredvCpus} -> desiredvCpus) (\s@ComputeResource' {} a -> s {desiredvCpus = a} :: ComputeResource)

-- | The allocation strategy to use for the compute resource if not enough
-- instances of the best fitting instance type can be allocated. This might
-- be because of availability of the instance type in the Region or
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation Strategies>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- [BEST_FIT (default)]
--     AWS Batch selects an instance type that best fits the needs of the
--     jobs with a preference for the lowest-cost instance type. If
--     additional instances of the selected instance type aren\'t
--     available, AWS Batch will wait for the additional instances to be
--     available. If there are not enough instances available, or if the
--     user is hitting
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>
--     then additional jobs aren\'t run until currently running jobs have
--     completed. This allocation strategy keeps costs lower but can limit
--     scaling. If you are using Spot Fleets with @BEST_FIT@ then the Spot
--     Fleet IAM Role must be specified.
--
-- [BEST_FIT_PROGRESSIVE]
--     AWS Batch will select additional instance types that are large
--     enough to meet the requirements of the jobs in the queue, with a
--     preference for instance types with a lower cost per unit vCPU. If
--     additional instances of the previously selected instance types
--     aren\'t available, AWS Batch will select new instance types.
--
-- [SPOT_CAPACITY_OPTIMIZED]
--     AWS Batch will select one or more instance types that are large
--     enough to meet the requirements of the jobs in the queue, with a
--     preference for instance types that are less likely to be
--     interrupted. This allocation strategy is only available for Spot
--     Instance compute resources.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- strategies, AWS Batch might need to go above @maxvCpus@ to meet your
-- capacity requirements. In this event, AWS Batch never exceeds @maxvCpus@
-- by more than a single instance.
computeResource_allocationStrategy :: Lens.Lens' ComputeResource (Prelude.Maybe CRAllocationStrategy)
computeResource_allocationStrategy = Lens.lens (\ComputeResource' {allocationStrategy} -> allocationStrategy) (\s@ComputeResource' {} a -> s {allocationStrategy = a} :: ComputeResource)

-- | The Amazon ECS instance profile applied to Amazon EC2 instances in a
-- compute environment. You can specify the short name or full Amazon
-- Resource Name (ARN) of an instance profile. For example,
-- @ ecsInstanceRole @ or
-- @arn:aws:iam::\<aws_account_id>:instance-profile\/ecsInstanceRole @. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS Instance Role>
-- in the /AWS Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResource_instanceRole :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_instanceRole = Lens.lens (\ComputeResource' {instanceRole} -> instanceRole) (\s@ComputeResource' {} a -> s {instanceRole = a} :: ComputeResource)

-- | The instances types that can be launched. You can specify instance
-- families to launch any instance type within those families (for example,
-- @c5@ or @p3@), or you can specify specific sizes within a family (such
-- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
-- (from the C4, M4, and R4 instance families) on the fly that match the
-- demand of your job queues.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- When you create a compute environment, the instance types that you
-- select for the compute environment must share the same architecture. For
-- example, you can\'t mix x86 and ARM instances in the same compute
-- environment.
--
-- Currently, @optimal@ uses instance types from the C4, M4, and R4
-- instance families. In Regions that don\'t have instance types from those
-- instance families, instance types from the C5, M5. and R5 instance
-- families are used.
computeResource_instanceTypes :: Lens.Lens' ComputeResource (Prelude.Maybe [Prelude.Text])
computeResource_instanceTypes = Lens.lens (\ComputeResource' {instanceTypes} -> instanceTypes) (\s@ComputeResource' {} a -> s {instanceTypes = a} :: ComputeResource) Prelude.. Lens.mapping Lens._Coerce

-- | The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
-- @FARGATE_SPOT@. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
--
-- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
-- role with the @spotIamFleetRole@ parameter. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet role>
-- in the /AWS Batch User Guide/.
computeResource_type :: Lens.Lens' ComputeResource CRType
computeResource_type = Lens.lens (\ComputeResource' {type'} -> type') (\s@ComputeResource' {} a -> s {type' = a} :: ComputeResource)

-- | The maximum number of Amazon EC2 vCPUs that a compute environment can
-- reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies, AWS Batch might need to go above @maxvCpus@ to
-- meet your capacity requirements. In this event, AWS Batch will never go
-- above @maxvCpus@ by more than a single instance (e.g., no more than a
-- single instance from among those specified in your compute environment).
computeResource_maxvCpus :: Lens.Lens' ComputeResource Prelude.Int
computeResource_maxvCpus = Lens.lens (\ComputeResource' {maxvCpus} -> maxvCpus) (\s@ComputeResource' {} a -> s {maxvCpus = a} :: ComputeResource)

-- | The VPC subnets into which the compute resources are launched. These
-- subnets must be within the same VPC. This parameter is required for jobs
-- running on Fargate resources, where it can contain up to 16 subnets. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
-- in the /Amazon VPC User Guide/.
computeResource_subnets :: Lens.Lens' ComputeResource [Prelude.Text]
computeResource_subnets = Lens.lens (\ComputeResource' {subnets} -> subnets) (\s@ComputeResource' {} a -> s {subnets = a} :: ComputeResource) Prelude.. Lens._Coerce

instance Core.FromJSON ComputeResource where
  parseJSON =
    Core.withObject
      "ComputeResource"
      ( \x ->
          ComputeResource'
            Prelude.<$> ( x Core..:? "securityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "bidPercentage")
            Prelude.<*> (x Core..:? "minvCpus")
            Prelude.<*> (x Core..:? "ec2KeyPair")
            Prelude.<*> (x Core..:? "placementGroup")
            Prelude.<*> (x Core..:? "launchTemplate")
            Prelude.<*> (x Core..:? "imageId")
            Prelude.<*> (x Core..:? "spotIamFleetRole")
            Prelude.<*> ( x Core..:? "ec2Configuration"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "desiredvCpus")
            Prelude.<*> (x Core..:? "allocationStrategy")
            Prelude.<*> (x Core..:? "instanceRole")
            Prelude.<*> (x Core..:? "instanceTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "maxvCpus")
            Prelude.<*> (x Core..:? "subnets" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ComputeResource

instance Prelude.NFData ComputeResource

instance Core.ToJSON ComputeResource where
  toJSON ComputeResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("bidPercentage" Core..=) Prelude.<$> bidPercentage,
            ("minvCpus" Core..=) Prelude.<$> minvCpus,
            ("ec2KeyPair" Core..=) Prelude.<$> ec2KeyPair,
            ("placementGroup" Core..=)
              Prelude.<$> placementGroup,
            ("launchTemplate" Core..=)
              Prelude.<$> launchTemplate,
            ("imageId" Core..=) Prelude.<$> imageId,
            ("spotIamFleetRole" Core..=)
              Prelude.<$> spotIamFleetRole,
            ("ec2Configuration" Core..=)
              Prelude.<$> ec2Configuration,
            ("tags" Core..=) Prelude.<$> tags,
            ("desiredvCpus" Core..=) Prelude.<$> desiredvCpus,
            ("allocationStrategy" Core..=)
              Prelude.<$> allocationStrategy,
            ("instanceRole" Core..=) Prelude.<$> instanceRole,
            ("instanceTypes" Core..=) Prelude.<$> instanceTypes,
            Prelude.Just ("type" Core..= type'),
            Prelude.Just ("maxvCpus" Core..= maxvCpus),
            Prelude.Just ("subnets" Core..= subnets)
          ]
      )
