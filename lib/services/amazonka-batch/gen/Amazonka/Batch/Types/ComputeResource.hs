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
-- Module      : Amazonka.Batch.Types.ComputeResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ComputeResource where

import Amazonka.Batch.Types.CRAllocationStrategy
import Amazonka.Batch.Types.CRType
import Amazonka.Batch.Types.Ec2Configuration
import Amazonka.Batch.Types.LaunchTemplateSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Batch compute resource. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
--
-- /See:/ 'newComputeResource' smart constructor.
data ComputeResource = ComputeResource'
  { -- | The allocation strategy to use for the compute resource if not enough
    -- instances of the best fitting instance type can be allocated. This might
    -- be because of availability of the instance type in the Region or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation strategies>
    -- in the /Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    --
    -- [BEST_FIT (default)]
    --     Batch selects an instance type that best fits the needs of the jobs
    --     with a preference for the lowest-cost instance type. If additional
    --     instances of the selected instance type aren\'t available, Batch
    --     waits for the additional instances to be available. If there aren\'t
    --     enough instances available or the user is reaching
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>,
    --     additional jobs aren\'t run until the currently running jobs are
    --     completed. This allocation strategy keeps costs lower but can limit
    --     scaling. If you\'re using Spot Fleets with @BEST_FIT@, the Spot
    --     Fleet IAM Role must be specified. Compute resources that use a
    --     @BEST_FIT@ allocation strategy don\'t support infrastructure updates
    --     and can\'t update some parameters. For more information, see
    --     <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    --     in the /Batch User Guide/.
    --
    -- [BEST_FIT_PROGRESSIVE]
    --     Batch selects additional instance types that are large enough to
    --     meet the requirements of the jobs in the queue. Its preference is
    --     for instance types with lower cost vCPUs. If additional instances of
    --     the previously selected instance types aren\'t available, Batch
    --     selects new instance types.
    --
    -- [SPOT_CAPACITY_OPTIMIZED]
    --     Batch selects one or more instance types that are large enough to
    --     meet the requirements of the jobs in the queue. Its preference is
    --     for instance types that are less likely to be interrupted. This
    --     allocation strategy is only available for Spot Instance compute
    --     resources.
    --
    -- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
    -- strategies using On-Demand or Spot Instances, and the @BEST_FIT@
    -- strategy using Spot Instances, Batch might need to exceed @maxvCpus@ to
    -- meet your capacity requirements. In this event, Batch never exceeds
    -- @maxvCpus@ by more than a single instance.
    allocationStrategy :: Prelude.Maybe CRAllocationStrategy,
    -- | The maximum percentage that a Spot Instance price can be when compared
    -- with the On-Demand price for that instance type before instances are
    -- launched. For example, if your maximum percentage is 20%, then the Spot
    -- price must be less than 20% of the current On-Demand price for that
    -- Amazon EC2 instance. You always pay the lowest (market) price and never
    -- more than your maximum percentage. If you leave this field empty, the
    -- default value is 100% of the On-Demand price. For most use cases, we
    -- recommend leaving this field empty.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    bidPercentage :: Prelude.Maybe Prelude.Int,
    -- | The desired number of Amazon EC2 vCPUS in the compute environment. Batch
    -- modifies this value between the minimum and maximum values based on job
    -- queue demand.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    desiredvCpus :: Prelude.Maybe Prelude.Int,
    -- | Provides information that\'s used to select Amazon Machine Images (AMIs)
    -- for EC2 instances in the compute environment. If @Ec2Configuration@
    -- isn\'t specified, the default is @ECS_AL2@.
    --
    -- One or two values can be provided.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    ec2Configuration :: Prelude.Maybe [Ec2Configuration],
    -- | The Amazon EC2 key pair that\'s used for instances launched in the
    -- compute environment. You can use this key pair to log in to your
    -- instances with SSH.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    ec2KeyPair :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Machine Image (AMI) ID used for instances launched in the
    -- compute environment. This parameter is overridden by the
    -- @imageIdOverride@ member of the @Ec2Configuration@ structure.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
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
    -- | The Amazon ECS instance profile applied to Amazon EC2 instances in a
    -- compute environment. You can specify the short name or full Amazon
    -- Resource Name (ARN) of an instance profile. For example,
    -- @ ecsInstanceRole @ or
    -- @arn:aws:iam::\<aws_account_id>:instance-profile\/ecsInstanceRole @. For
    -- more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS instance role>
    -- in the /Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    instanceRole :: Prelude.Maybe Prelude.Text,
    -- | The instances types that can be launched. You can specify instance
    -- families to launch any instance type within those families (for example,
    -- @c5@ or @p3@), or you can specify specific sizes within a family (such
    -- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
    -- (from the C4, M4, and R4 instance families) that match the demand of
    -- your job queues.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    --
    -- When you create a compute environment, the instance types that you
    -- select for the compute environment must share the same architecture. For
    -- example, you can\'t mix x86 and ARM instances in the same compute
    -- environment.
    --
    -- Currently, @optimal@ uses instance types from the C4, M4, and R4
    -- instance families. In Regions that don\'t have instance types from those
    -- instance families, instance types from the C5, M5, and R5 instance
    -- families are used.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The launch template to use for your compute resources. Any other compute
    -- resource parameters that you specify in a CreateComputeEnvironment API
    -- operation override the same parameters in the launch template. You must
    -- specify either the launch template ID or launch template name in the
    -- request, but not both. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch template support>
    -- in the /Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The minimum number of Amazon EC2 vCPUs that an environment should
    -- maintain (even if the compute environment is @DISABLED@).
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    minvCpus :: Prelude.Maybe Prelude.Int,
    -- | The Amazon EC2 placement group to associate with your compute resources.
    -- If you intend to submit multi-node parallel jobs to your compute
    -- environment, you should consider creating a cluster placement group and
    -- associate it with your compute resources. This keeps your multi-node
    -- parallel job on a logical grouping of instances within a single
    -- Availability Zone with high network flow potential. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 security groups that are associated with instances
    -- launched in the compute environment. One or more security groups must be
    -- specified, either in @securityGroupIds@ or using a launch template
    -- referenced in @launchTemplate@. This parameter is required for jobs that
    -- are running on Fargate resources and must contain at least one security
    -- group. Fargate doesn\'t support launch templates. If security groups are
    -- specified using both @securityGroupIds@ and @launchTemplate@, the values
    -- in @securityGroupIds@ are used.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role
    -- applied to a @SPOT@ compute environment. This role is required if the
    -- allocation strategy set to @BEST_FIT@ or if the allocation strategy
    -- isn\'t specified. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
    -- in the /Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    --
    -- To tag your Spot Instances on creation, the Spot Fleet IAM role
    -- specified here must use the newer __AmazonEC2SpotFleetTaggingRole__
    -- managed policy. The previously recommended __AmazonEC2SpotFleetRole__
    -- managed policy doesn\'t have the required permissions to tag Spot
    -- Instances. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#spot-instance-no-tag Spot instances not tagged on creation>
    -- in the /Batch User Guide/.
    spotIamFleetRole :: Prelude.Maybe Prelude.Text,
    -- | Key-value pair tags to be applied to EC2 resources that are launched in
    -- the compute environment. For Batch, these take the form of
    -- @\"String1\": \"String2\"@, where @String1@ is the tag key and @String2@
    -- is the tag value-for example,
    -- @{ \"Name\": \"Batch Instance - C4OnDemand\" }@. This is helpful for
    -- recognizing your Batch instances in the Amazon EC2 console. Updating
    -- these tags requires an infrastructure update to the compute environment.
    -- For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/. These tags aren\'t seen when using the Batch
    -- @ListTagsForResource@ API operation.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
    -- @FARGATE_SPOT@. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
    -- in the /Batch User Guide/.
    --
    -- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
    -- role with the @spotIamFleetRole@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
    -- in the /Batch User Guide/.
    type' :: CRType,
    -- | The maximum number of Amazon EC2 vCPUs that a compute environment can
    -- reach.
    --
    -- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
    -- allocation strategies using On-Demand or Spot Instances, and the
    -- @BEST_FIT@ strategy using Spot Instances, Batch might need to exceed
    -- @maxvCpus@ to meet your capacity requirements. In this event, Batch
    -- never exceeds @maxvCpus@ by more than a single instance. For example, no
    -- more than a single instance from among those specified in your compute
    -- environment is allocated.
    maxvCpus :: Prelude.Int,
    -- | The VPC subnets where the compute resources are launched. These subnets
    -- must be within the same VPC. Fargate compute resources can contain up to
    -- 16 subnets. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and subnets>
    -- in the /Amazon VPC User Guide/.
    --
    -- Batch on Amazon EC2 and Batch on Amazon EKS support Local Zones. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-local-zones Local Zones>
    -- in the /Amazon EC2 User Guide for Linux Instances/,
    -- <https://docs.aws.amazon.com/eks/latest/userguide/local-zones.html Amazon EKS and Amazon Web Services Local Zones>
    -- in the /Amazon EKS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-regions-zones.html#clusters-local-zones Amazon ECS clusters in Local Zones, Wavelength Zones, and Amazon Web Services Outposts>
    -- in the /Amazon ECS Developer Guide/.
    --
    -- Batch on Fargate doesn\'t currently support Local Zones.
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
-- 'allocationStrategy', 'computeResource_allocationStrategy' - The allocation strategy to use for the compute resource if not enough
-- instances of the best fitting instance type can be allocated. This might
-- be because of availability of the instance type in the Region or
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation strategies>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- [BEST_FIT (default)]
--     Batch selects an instance type that best fits the needs of the jobs
--     with a preference for the lowest-cost instance type. If additional
--     instances of the selected instance type aren\'t available, Batch
--     waits for the additional instances to be available. If there aren\'t
--     enough instances available or the user is reaching
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>,
--     additional jobs aren\'t run until the currently running jobs are
--     completed. This allocation strategy keeps costs lower but can limit
--     scaling. If you\'re using Spot Fleets with @BEST_FIT@, the Spot
--     Fleet IAM Role must be specified. Compute resources that use a
--     @BEST_FIT@ allocation strategy don\'t support infrastructure updates
--     and can\'t update some parameters. For more information, see
--     <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
--     in the /Batch User Guide/.
--
-- [BEST_FIT_PROGRESSIVE]
--     Batch selects additional instance types that are large enough to
--     meet the requirements of the jobs in the queue. Its preference is
--     for instance types with lower cost vCPUs. If additional instances of
--     the previously selected instance types aren\'t available, Batch
--     selects new instance types.
--
-- [SPOT_CAPACITY_OPTIMIZED]
--     Batch selects one or more instance types that are large enough to
--     meet the requirements of the jobs in the queue. Its preference is
--     for instance types that are less likely to be interrupted. This
--     allocation strategy is only available for Spot Instance compute
--     resources.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- strategies using On-Demand or Spot Instances, and the @BEST_FIT@
-- strategy using Spot Instances, Batch might need to exceed @maxvCpus@ to
-- meet your capacity requirements. In this event, Batch never exceeds
-- @maxvCpus@ by more than a single instance.
--
-- 'bidPercentage', 'computeResource_bidPercentage' - The maximum percentage that a Spot Instance price can be when compared
-- with the On-Demand price for that instance type before instances are
-- launched. For example, if your maximum percentage is 20%, then the Spot
-- price must be less than 20% of the current On-Demand price for that
-- Amazon EC2 instance. You always pay the lowest (market) price and never
-- more than your maximum percentage. If you leave this field empty, the
-- default value is 100% of the On-Demand price. For most use cases, we
-- recommend leaving this field empty.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'desiredvCpus', 'computeResource_desiredvCpus' - The desired number of Amazon EC2 vCPUS in the compute environment. Batch
-- modifies this value between the minimum and maximum values based on job
-- queue demand.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'ec2Configuration', 'computeResource_ec2Configuration' - Provides information that\'s used to select Amazon Machine Images (AMIs)
-- for EC2 instances in the compute environment. If @Ec2Configuration@
-- isn\'t specified, the default is @ECS_AL2@.
--
-- One or two values can be provided.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'ec2KeyPair', 'computeResource_ec2KeyPair' - The Amazon EC2 key pair that\'s used for instances launched in the
-- compute environment. You can use this key pair to log in to your
-- instances with SSH.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'imageId', 'computeResource_imageId' - The Amazon Machine Image (AMI) ID used for instances launched in the
-- compute environment. This parameter is overridden by the
-- @imageIdOverride@ member of the @Ec2Configuration@ structure.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
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
-- 'instanceRole', 'computeResource_instanceRole' - The Amazon ECS instance profile applied to Amazon EC2 instances in a
-- compute environment. You can specify the short name or full Amazon
-- Resource Name (ARN) of an instance profile. For example,
-- @ ecsInstanceRole @ or
-- @arn:aws:iam::\<aws_account_id>:instance-profile\/ecsInstanceRole @. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS instance role>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'instanceTypes', 'computeResource_instanceTypes' - The instances types that can be launched. You can specify instance
-- families to launch any instance type within those families (for example,
-- @c5@ or @p3@), or you can specify specific sizes within a family (such
-- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
-- (from the C4, M4, and R4 instance families) that match the demand of
-- your job queues.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- When you create a compute environment, the instance types that you
-- select for the compute environment must share the same architecture. For
-- example, you can\'t mix x86 and ARM instances in the same compute
-- environment.
--
-- Currently, @optimal@ uses instance types from the C4, M4, and R4
-- instance families. In Regions that don\'t have instance types from those
-- instance families, instance types from the C5, M5, and R5 instance
-- families are used.
--
-- 'launchTemplate', 'computeResource_launchTemplate' - The launch template to use for your compute resources. Any other compute
-- resource parameters that you specify in a CreateComputeEnvironment API
-- operation override the same parameters in the launch template. You must
-- specify either the launch template ID or launch template name in the
-- request, but not both. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch template support>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'minvCpus', 'computeResource_minvCpus' - The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain (even if the compute environment is @DISABLED@).
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'placementGroup', 'computeResource_placementGroup' - The Amazon EC2 placement group to associate with your compute resources.
-- If you intend to submit multi-node parallel jobs to your compute
-- environment, you should consider creating a cluster placement group and
-- associate it with your compute resources. This keeps your multi-node
-- parallel job on a logical grouping of instances within a single
-- Availability Zone with high network flow potential. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'securityGroupIds', 'computeResource_securityGroupIds' - The Amazon EC2 security groups that are associated with instances
-- launched in the compute environment. One or more security groups must be
-- specified, either in @securityGroupIds@ or using a launch template
-- referenced in @launchTemplate@. This parameter is required for jobs that
-- are running on Fargate resources and must contain at least one security
-- group. Fargate doesn\'t support launch templates. If security groups are
-- specified using both @securityGroupIds@ and @launchTemplate@, the values
-- in @securityGroupIds@ are used.
--
-- 'spotIamFleetRole', 'computeResource_spotIamFleetRole' - The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role
-- applied to a @SPOT@ compute environment. This role is required if the
-- allocation strategy set to @BEST_FIT@ or if the allocation strategy
-- isn\'t specified. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- To tag your Spot Instances on creation, the Spot Fleet IAM role
-- specified here must use the newer __AmazonEC2SpotFleetTaggingRole__
-- managed policy. The previously recommended __AmazonEC2SpotFleetRole__
-- managed policy doesn\'t have the required permissions to tag Spot
-- Instances. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#spot-instance-no-tag Spot instances not tagged on creation>
-- in the /Batch User Guide/.
--
-- 'tags', 'computeResource_tags' - Key-value pair tags to be applied to EC2 resources that are launched in
-- the compute environment. For Batch, these take the form of
-- @\"String1\": \"String2\"@, where @String1@ is the tag key and @String2@
-- is the tag value-for example,
-- @{ \"Name\": \"Batch Instance - C4OnDemand\" }@. This is helpful for
-- recognizing your Batch instances in the Amazon EC2 console. Updating
-- these tags requires an infrastructure update to the compute environment.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/. These tags aren\'t seen when using the Batch
-- @ListTagsForResource@ API operation.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'type'', 'computeResource_type' - The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
-- @FARGATE_SPOT@. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
--
-- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
-- role with the @spotIamFleetRole@ parameter. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
-- in the /Batch User Guide/.
--
-- 'maxvCpus', 'computeResource_maxvCpus' - The maximum number of Amazon EC2 vCPUs that a compute environment can
-- reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies using On-Demand or Spot Instances, and the
-- @BEST_FIT@ strategy using Spot Instances, Batch might need to exceed
-- @maxvCpus@ to meet your capacity requirements. In this event, Batch
-- never exceeds @maxvCpus@ by more than a single instance. For example, no
-- more than a single instance from among those specified in your compute
-- environment is allocated.
--
-- 'subnets', 'computeResource_subnets' - The VPC subnets where the compute resources are launched. These subnets
-- must be within the same VPC. Fargate compute resources can contain up to
-- 16 subnets. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and subnets>
-- in the /Amazon VPC User Guide/.
--
-- Batch on Amazon EC2 and Batch on Amazon EKS support Local Zones. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-local-zones Local Zones>
-- in the /Amazon EC2 User Guide for Linux Instances/,
-- <https://docs.aws.amazon.com/eks/latest/userguide/local-zones.html Amazon EKS and Amazon Web Services Local Zones>
-- in the /Amazon EKS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-regions-zones.html#clusters-local-zones Amazon ECS clusters in Local Zones, Wavelength Zones, and Amazon Web Services Outposts>
-- in the /Amazon ECS Developer Guide/.
--
-- Batch on Fargate doesn\'t currently support Local Zones.
newComputeResource ::
  -- | 'type''
  CRType ->
  -- | 'maxvCpus'
  Prelude.Int ->
  ComputeResource
newComputeResource pType_ pMaxvCpus_ =
  ComputeResource'
    { allocationStrategy =
        Prelude.Nothing,
      bidPercentage = Prelude.Nothing,
      desiredvCpus = Prelude.Nothing,
      ec2Configuration = Prelude.Nothing,
      ec2KeyPair = Prelude.Nothing,
      imageId = Prelude.Nothing,
      instanceRole = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      minvCpus = Prelude.Nothing,
      placementGroup = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      spotIamFleetRole = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = pType_,
      maxvCpus = pMaxvCpus_,
      subnets = Prelude.mempty
    }

-- | The allocation strategy to use for the compute resource if not enough
-- instances of the best fitting instance type can be allocated. This might
-- be because of availability of the instance type in the Region or
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation strategies>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- [BEST_FIT (default)]
--     Batch selects an instance type that best fits the needs of the jobs
--     with a preference for the lowest-cost instance type. If additional
--     instances of the selected instance type aren\'t available, Batch
--     waits for the additional instances to be available. If there aren\'t
--     enough instances available or the user is reaching
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>,
--     additional jobs aren\'t run until the currently running jobs are
--     completed. This allocation strategy keeps costs lower but can limit
--     scaling. If you\'re using Spot Fleets with @BEST_FIT@, the Spot
--     Fleet IAM Role must be specified. Compute resources that use a
--     @BEST_FIT@ allocation strategy don\'t support infrastructure updates
--     and can\'t update some parameters. For more information, see
--     <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
--     in the /Batch User Guide/.
--
-- [BEST_FIT_PROGRESSIVE]
--     Batch selects additional instance types that are large enough to
--     meet the requirements of the jobs in the queue. Its preference is
--     for instance types with lower cost vCPUs. If additional instances of
--     the previously selected instance types aren\'t available, Batch
--     selects new instance types.
--
-- [SPOT_CAPACITY_OPTIMIZED]
--     Batch selects one or more instance types that are large enough to
--     meet the requirements of the jobs in the queue. Its preference is
--     for instance types that are less likely to be interrupted. This
--     allocation strategy is only available for Spot Instance compute
--     resources.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- strategies using On-Demand or Spot Instances, and the @BEST_FIT@
-- strategy using Spot Instances, Batch might need to exceed @maxvCpus@ to
-- meet your capacity requirements. In this event, Batch never exceeds
-- @maxvCpus@ by more than a single instance.
computeResource_allocationStrategy :: Lens.Lens' ComputeResource (Prelude.Maybe CRAllocationStrategy)
computeResource_allocationStrategy = Lens.lens (\ComputeResource' {allocationStrategy} -> allocationStrategy) (\s@ComputeResource' {} a -> s {allocationStrategy = a} :: ComputeResource)

-- | The maximum percentage that a Spot Instance price can be when compared
-- with the On-Demand price for that instance type before instances are
-- launched. For example, if your maximum percentage is 20%, then the Spot
-- price must be less than 20% of the current On-Demand price for that
-- Amazon EC2 instance. You always pay the lowest (market) price and never
-- more than your maximum percentage. If you leave this field empty, the
-- default value is 100% of the On-Demand price. For most use cases, we
-- recommend leaving this field empty.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_bidPercentage :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Int)
computeResource_bidPercentage = Lens.lens (\ComputeResource' {bidPercentage} -> bidPercentage) (\s@ComputeResource' {} a -> s {bidPercentage = a} :: ComputeResource)

-- | The desired number of Amazon EC2 vCPUS in the compute environment. Batch
-- modifies this value between the minimum and maximum values based on job
-- queue demand.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_desiredvCpus :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Int)
computeResource_desiredvCpus = Lens.lens (\ComputeResource' {desiredvCpus} -> desiredvCpus) (\s@ComputeResource' {} a -> s {desiredvCpus = a} :: ComputeResource)

-- | Provides information that\'s used to select Amazon Machine Images (AMIs)
-- for EC2 instances in the compute environment. If @Ec2Configuration@
-- isn\'t specified, the default is @ECS_AL2@.
--
-- One or two values can be provided.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_ec2Configuration :: Lens.Lens' ComputeResource (Prelude.Maybe [Ec2Configuration])
computeResource_ec2Configuration = Lens.lens (\ComputeResource' {ec2Configuration} -> ec2Configuration) (\s@ComputeResource' {} a -> s {ec2Configuration = a} :: ComputeResource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon EC2 key pair that\'s used for instances launched in the
-- compute environment. You can use this key pair to log in to your
-- instances with SSH.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_ec2KeyPair :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_ec2KeyPair = Lens.lens (\ComputeResource' {ec2KeyPair} -> ec2KeyPair) (\s@ComputeResource' {} a -> s {ec2KeyPair = a} :: ComputeResource)

-- | The Amazon Machine Image (AMI) ID used for instances launched in the
-- compute environment. This parameter is overridden by the
-- @imageIdOverride@ member of the @Ec2Configuration@ structure.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
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

-- | The Amazon ECS instance profile applied to Amazon EC2 instances in a
-- compute environment. You can specify the short name or full Amazon
-- Resource Name (ARN) of an instance profile. For example,
-- @ ecsInstanceRole @ or
-- @arn:aws:iam::\<aws_account_id>:instance-profile\/ecsInstanceRole @. For
-- more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS instance role>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_instanceRole :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_instanceRole = Lens.lens (\ComputeResource' {instanceRole} -> instanceRole) (\s@ComputeResource' {} a -> s {instanceRole = a} :: ComputeResource)

-- | The instances types that can be launched. You can specify instance
-- families to launch any instance type within those families (for example,
-- @c5@ or @p3@), or you can specify specific sizes within a family (such
-- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
-- (from the C4, M4, and R4 instance families) that match the demand of
-- your job queues.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- When you create a compute environment, the instance types that you
-- select for the compute environment must share the same architecture. For
-- example, you can\'t mix x86 and ARM instances in the same compute
-- environment.
--
-- Currently, @optimal@ uses instance types from the C4, M4, and R4
-- instance families. In Regions that don\'t have instance types from those
-- instance families, instance types from the C5, M5, and R5 instance
-- families are used.
computeResource_instanceTypes :: Lens.Lens' ComputeResource (Prelude.Maybe [Prelude.Text])
computeResource_instanceTypes = Lens.lens (\ComputeResource' {instanceTypes} -> instanceTypes) (\s@ComputeResource' {} a -> s {instanceTypes = a} :: ComputeResource) Prelude.. Lens.mapping Lens.coerced

-- | The launch template to use for your compute resources. Any other compute
-- resource parameters that you specify in a CreateComputeEnvironment API
-- operation override the same parameters in the launch template. You must
-- specify either the launch template ID or launch template name in the
-- request, but not both. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch template support>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_launchTemplate :: Lens.Lens' ComputeResource (Prelude.Maybe LaunchTemplateSpecification)
computeResource_launchTemplate = Lens.lens (\ComputeResource' {launchTemplate} -> launchTemplate) (\s@ComputeResource' {} a -> s {launchTemplate = a} :: ComputeResource)

-- | The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain (even if the compute environment is @DISABLED@).
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_minvCpus :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Int)
computeResource_minvCpus = Lens.lens (\ComputeResource' {minvCpus} -> minvCpus) (\s@ComputeResource' {} a -> s {minvCpus = a} :: ComputeResource)

-- | The Amazon EC2 placement group to associate with your compute resources.
-- If you intend to submit multi-node parallel jobs to your compute
-- environment, you should consider creating a cluster placement group and
-- associate it with your compute resources. This keeps your multi-node
-- parallel job on a logical grouping of instances within a single
-- Availability Zone with high network flow potential. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_placementGroup :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_placementGroup = Lens.lens (\ComputeResource' {placementGroup} -> placementGroup) (\s@ComputeResource' {} a -> s {placementGroup = a} :: ComputeResource)

-- | The Amazon EC2 security groups that are associated with instances
-- launched in the compute environment. One or more security groups must be
-- specified, either in @securityGroupIds@ or using a launch template
-- referenced in @launchTemplate@. This parameter is required for jobs that
-- are running on Fargate resources and must contain at least one security
-- group. Fargate doesn\'t support launch templates. If security groups are
-- specified using both @securityGroupIds@ and @launchTemplate@, the values
-- in @securityGroupIds@ are used.
computeResource_securityGroupIds :: Lens.Lens' ComputeResource (Prelude.Maybe [Prelude.Text])
computeResource_securityGroupIds = Lens.lens (\ComputeResource' {securityGroupIds} -> securityGroupIds) (\s@ComputeResource' {} a -> s {securityGroupIds = a} :: ComputeResource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role
-- applied to a @SPOT@ compute environment. This role is required if the
-- allocation strategy set to @BEST_FIT@ or if the allocation strategy
-- isn\'t specified. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- To tag your Spot Instances on creation, the Spot Fleet IAM role
-- specified here must use the newer __AmazonEC2SpotFleetTaggingRole__
-- managed policy. The previously recommended __AmazonEC2SpotFleetRole__
-- managed policy doesn\'t have the required permissions to tag Spot
-- Instances. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#spot-instance-no-tag Spot instances not tagged on creation>
-- in the /Batch User Guide/.
computeResource_spotIamFleetRole :: Lens.Lens' ComputeResource (Prelude.Maybe Prelude.Text)
computeResource_spotIamFleetRole = Lens.lens (\ComputeResource' {spotIamFleetRole} -> spotIamFleetRole) (\s@ComputeResource' {} a -> s {spotIamFleetRole = a} :: ComputeResource)

-- | Key-value pair tags to be applied to EC2 resources that are launched in
-- the compute environment. For Batch, these take the form of
-- @\"String1\": \"String2\"@, where @String1@ is the tag key and @String2@
-- is the tag value-for example,
-- @{ \"Name\": \"Batch Instance - C4OnDemand\" }@. This is helpful for
-- recognizing your Batch instances in the Amazon EC2 console. Updating
-- these tags requires an infrastructure update to the compute environment.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/. These tags aren\'t seen when using the Batch
-- @ListTagsForResource@ API operation.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResource_tags :: Lens.Lens' ComputeResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
computeResource_tags = Lens.lens (\ComputeResource' {tags} -> tags) (\s@ComputeResource' {} a -> s {tags = a} :: ComputeResource) Prelude.. Lens.mapping Lens.coerced

-- | The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
-- @FARGATE_SPOT@. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
--
-- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
-- role with the @spotIamFleetRole@ parameter. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
-- in the /Batch User Guide/.
computeResource_type :: Lens.Lens' ComputeResource CRType
computeResource_type = Lens.lens (\ComputeResource' {type'} -> type') (\s@ComputeResource' {} a -> s {type' = a} :: ComputeResource)

-- | The maximum number of Amazon EC2 vCPUs that a compute environment can
-- reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies using On-Demand or Spot Instances, and the
-- @BEST_FIT@ strategy using Spot Instances, Batch might need to exceed
-- @maxvCpus@ to meet your capacity requirements. In this event, Batch
-- never exceeds @maxvCpus@ by more than a single instance. For example, no
-- more than a single instance from among those specified in your compute
-- environment is allocated.
computeResource_maxvCpus :: Lens.Lens' ComputeResource Prelude.Int
computeResource_maxvCpus = Lens.lens (\ComputeResource' {maxvCpus} -> maxvCpus) (\s@ComputeResource' {} a -> s {maxvCpus = a} :: ComputeResource)

-- | The VPC subnets where the compute resources are launched. These subnets
-- must be within the same VPC. Fargate compute resources can contain up to
-- 16 subnets. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and subnets>
-- in the /Amazon VPC User Guide/.
--
-- Batch on Amazon EC2 and Batch on Amazon EKS support Local Zones. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-local-zones Local Zones>
-- in the /Amazon EC2 User Guide for Linux Instances/,
-- <https://docs.aws.amazon.com/eks/latest/userguide/local-zones.html Amazon EKS and Amazon Web Services Local Zones>
-- in the /Amazon EKS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-regions-zones.html#clusters-local-zones Amazon ECS clusters in Local Zones, Wavelength Zones, and Amazon Web Services Outposts>
-- in the /Amazon ECS Developer Guide/.
--
-- Batch on Fargate doesn\'t currently support Local Zones.
computeResource_subnets :: Lens.Lens' ComputeResource [Prelude.Text]
computeResource_subnets = Lens.lens (\ComputeResource' {subnets} -> subnets) (\s@ComputeResource' {} a -> s {subnets = a} :: ComputeResource) Prelude.. Lens.coerced

instance Data.FromJSON ComputeResource where
  parseJSON =
    Data.withObject
      "ComputeResource"
      ( \x ->
          ComputeResource'
            Prelude.<$> (x Data..:? "allocationStrategy")
            Prelude.<*> (x Data..:? "bidPercentage")
            Prelude.<*> (x Data..:? "desiredvCpus")
            Prelude.<*> ( x Data..:? "ec2Configuration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ec2KeyPair")
            Prelude.<*> (x Data..:? "imageId")
            Prelude.<*> (x Data..:? "instanceRole")
            Prelude.<*> (x Data..:? "instanceTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "launchTemplate")
            Prelude.<*> (x Data..:? "minvCpus")
            Prelude.<*> (x Data..:? "placementGroup")
            Prelude.<*> ( x Data..:? "securityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "spotIamFleetRole")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "maxvCpus")
            Prelude.<*> (x Data..:? "subnets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ComputeResource where
  hashWithSalt _salt ComputeResource' {..} =
    _salt `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` bidPercentage
      `Prelude.hashWithSalt` desiredvCpus
      `Prelude.hashWithSalt` ec2Configuration
      `Prelude.hashWithSalt` ec2KeyPair
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` instanceRole
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` minvCpus
      `Prelude.hashWithSalt` placementGroup
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` spotIamFleetRole
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` maxvCpus
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData ComputeResource where
  rnf ComputeResource' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf bidPercentage
      `Prelude.seq` Prelude.rnf desiredvCpus
      `Prelude.seq` Prelude.rnf ec2Configuration
      `Prelude.seq` Prelude.rnf ec2KeyPair
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf instanceRole
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf minvCpus
      `Prelude.seq` Prelude.rnf placementGroup
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf spotIamFleetRole
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf maxvCpus
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON ComputeResource where
  toJSON ComputeResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allocationStrategy" Data..=)
              Prelude.<$> allocationStrategy,
            ("bidPercentage" Data..=) Prelude.<$> bidPercentage,
            ("desiredvCpus" Data..=) Prelude.<$> desiredvCpus,
            ("ec2Configuration" Data..=)
              Prelude.<$> ec2Configuration,
            ("ec2KeyPair" Data..=) Prelude.<$> ec2KeyPair,
            ("imageId" Data..=) Prelude.<$> imageId,
            ("instanceRole" Data..=) Prelude.<$> instanceRole,
            ("instanceTypes" Data..=) Prelude.<$> instanceTypes,
            ("launchTemplate" Data..=)
              Prelude.<$> launchTemplate,
            ("minvCpus" Data..=) Prelude.<$> minvCpus,
            ("placementGroup" Data..=)
              Prelude.<$> placementGroup,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("spotIamFleetRole" Data..=)
              Prelude.<$> spotIamFleetRole,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("type" Data..= type'),
            Prelude.Just ("maxvCpus" Data..= maxvCpus),
            Prelude.Just ("subnets" Data..= subnets)
          ]
      )
