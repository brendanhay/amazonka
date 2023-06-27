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
-- Module      : Amazonka.Batch.Types.ComputeResourceUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ComputeResourceUpdate where

import Amazonka.Batch.Types.CRType
import Amazonka.Batch.Types.CRUpdateAllocationStrategy
import Amazonka.Batch.Types.Ec2Configuration
import Amazonka.Batch.Types.LaunchTemplateSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the attributes of a compute environment that
-- can be updated. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- /See:/ 'newComputeResourceUpdate' smart constructor.
data ComputeResourceUpdate = ComputeResourceUpdate'
  { -- | The allocation strategy to use for the compute resource if there\'s not
    -- enough instances of the best fitting instance type that can be
    -- allocated. This might be because of availability of the instance type in
    -- the Region or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation strategies>
    -- in the /Batch User Guide/.
    --
    -- When updating a compute environment, changing the allocation strategy
    -- requires an infrastructure update of the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/. @BEST_FIT@ isn\'t supported when updating a
    -- compute environment.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
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
    allocationStrategy :: Prelude.Maybe CRUpdateAllocationStrategy,
    -- | The maximum percentage that a Spot Instance price can be when compared
    -- with the On-Demand price for that instance type before instances are
    -- launched. For example, if your maximum percentage is 20%, the Spot price
    -- must be less than 20% of the current On-Demand price for that Amazon EC2
    -- instance. You always pay the lowest (market) price and never more than
    -- your maximum percentage. For most use cases, we recommend leaving this
    -- field empty.
    --
    -- When updating a compute environment, changing the bid percentage
    -- requires an infrastructure update of the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
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
    --
    -- Batch doesn\'t support changing the desired number of vCPUs of an
    -- existing compute environment. Don\'t specify this parameter for compute
    -- environments using Amazon EKS clusters.
    --
    -- When you update the @desiredvCpus@ setting, the value must be between
    -- the @minvCpus@ and @maxvCpus@ values.
    --
    -- Additionally, the updated @desiredvCpus@ value must be greater than or
    -- equal to the current @desiredvCpus@ value. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#error-desired-vcpus-update Troubleshooting Batch>
    -- in the /Batch User Guide/.
    desiredvCpus :: Prelude.Maybe Prelude.Int,
    -- | Provides information used to select Amazon Machine Images (AMIs) for EC2
    -- instances in the compute environment. If @Ec2Configuration@ isn\'t
    -- specified, the default is @ECS_AL2@.
    --
    -- When updating a compute environment, changing this setting requires an
    -- infrastructure update of the compute environment. For more information,
    -- see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/. To remove the EC2 configuration and any
    -- custom AMI ID specified in @imageIdOverride@, set this value to an empty
    -- string.
    --
    -- One or two values can be provided.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    ec2Configuration :: Prelude.Maybe [Ec2Configuration],
    -- | The Amazon EC2 key pair that\'s used for instances launched in the
    -- compute environment. You can use this key pair to log in to your
    -- instances with SSH. To remove the Amazon EC2 key pair, set this value to
    -- an empty string.
    --
    -- When updating a compute environment, changing the EC2 key pair requires
    -- an infrastructure update of the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    ec2KeyPair :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Machine Image (AMI) ID used for instances launched in the
    -- compute environment. This parameter is overridden by the
    -- @imageIdOverride@ member of the @Ec2Configuration@ structure. To remove
    -- the custom AMI ID and use the default AMI ID, set this value to an empty
    -- string.
    --
    -- When updating a compute environment, changing the AMI ID requires an
    -- infrastructure update of the compute environment. For more information,
    -- see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
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
    -- @ @/@ecsInstanceRole@/@ @ or
    -- @arn:aws:iam::@/@\<aws_account_id>@/@:instance-profile\/@/@ecsInstanceRole@/@ @.
    -- For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS instance role>
    -- in the /Batch User Guide/.
    --
    -- When updating a compute environment, changing this setting requires an
    -- infrastructure update of the compute environment. For more information,
    -- see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
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
    -- When updating a compute environment, changing this setting requires an
    -- infrastructure update of the compute environment. For more information,
    -- see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
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
    -- | The updated launch template to use for your compute resources. You must
    -- specify either the launch template ID or launch template name in the
    -- request, but not both. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch template support>
    -- in the /Batch User Guide/. To remove the custom launch template and use
    -- the default launch template, set @launchTemplateId@ or
    -- @launchTemplateName@ member of the launch template specification to an
    -- empty string. Removing the launch template from a compute environment
    -- will not remove the AMI specified in the launch template. In order to
    -- update the AMI specified in a launch template, the
    -- @updateToLatestImageVersion@ parameter must be set to @true@.
    --
    -- When updating a compute environment, changing the launch template
    -- requires an infrastructure update of the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
    --
    -- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
    -- allocation strategies using On-Demand or Spot Instances, and the
    -- @BEST_FIT@ strategy using Spot Instances, Batch might need to exceed
    -- @maxvCpus@ to meet your capacity requirements. In this event, Batch
    -- never exceeds @maxvCpus@ by more than a single instance. That is, no
    -- more than a single instance from among those specified in your compute
    -- environment.
    maxvCpus :: Prelude.Maybe Prelude.Int,
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
    -- When updating a compute environment, changing the placement group
    -- requires an infrastructure update of the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources. Don\'t specify it.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 security groups that are associated with instances
    -- launched in the compute environment. This parameter is required for
    -- Fargate compute resources, where it can contain up to 5 security groups.
    -- For Fargate compute resources, providing an empty list is handled as if
    -- this parameter wasn\'t specified and no change is made. For EC2 compute
    -- resources, providing an empty list removes the security groups from the
    -- compute resource.
    --
    -- When updating a compute environment, changing the EC2 security groups
    -- requires an infrastructure update of the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The VPC subnets where the compute resources are launched. Fargate
    -- compute resources can contain up to 16 subnets. For Fargate compute
    -- resources, providing an empty list will be handled as if this parameter
    -- wasn\'t specified and no change is made. For EC2 compute resources,
    -- providing an empty list removes the VPC subnets from the compute
    -- resource. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and subnets>
    -- in the /Amazon VPC User Guide/.
    --
    -- When updating a compute environment, changing the VPC subnets requires
    -- an infrastructure update of the compute environment. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
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
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | Key-value pair tags to be applied to EC2 resources that are launched in
    -- the compute environment. For Batch, these take the form of
    -- @\"String1\": \"String2\"@, where @String1@ is the tag key and @String2@
    -- is the tag value-for example,
    -- @{ \"Name\": \"Batch Instance - C4OnDemand\" }@. This is helpful for
    -- recognizing your Batch instances in the Amazon EC2 console. These tags
    -- aren\'t seen when using the Batch @ListTagsForResource@ API operation.
    --
    -- When updating a compute environment, changing this setting requires an
    -- infrastructure update of the compute environment. For more information,
    -- see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
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
    --
    -- When updating a compute environment, changing the type of a compute
    -- environment requires an infrastructure update of the compute
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    type' :: Prelude.Maybe CRType,
    -- | Specifies whether the AMI ID is updated to the latest one that\'s
    -- supported by Batch when the compute environment has an infrastructure
    -- update. The default value is @false@.
    --
    -- An AMI ID can either be specified in the @imageId@ or @imageIdOverride@
    -- parameters or be determined by the launch template that\'s specified in
    -- the @launchTemplate@ parameter. If an AMI ID is specified any of these
    -- ways, this parameter is ignored. For more information about to update
    -- AMI IDs during an infrastructure update, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html#updating-compute-environments-ami Updating the AMI ID>
    -- in the /Batch User Guide/.
    --
    -- When updating a compute environment, changing this setting requires an
    -- infrastructure update of the compute environment. For more information,
    -- see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    updateToLatestImageVersion :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeResourceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationStrategy', 'computeResourceUpdate_allocationStrategy' - The allocation strategy to use for the compute resource if there\'s not
-- enough instances of the best fitting instance type that can be
-- allocated. This might be because of availability of the instance type in
-- the Region or
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation strategies>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing the allocation strategy
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/. @BEST_FIT@ isn\'t supported when updating a
-- compute environment.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
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
-- 'bidPercentage', 'computeResourceUpdate_bidPercentage' - The maximum percentage that a Spot Instance price can be when compared
-- with the On-Demand price for that instance type before instances are
-- launched. For example, if your maximum percentage is 20%, the Spot price
-- must be less than 20% of the current On-Demand price for that Amazon EC2
-- instance. You always pay the lowest (market) price and never more than
-- your maximum percentage. For most use cases, we recommend leaving this
-- field empty.
--
-- When updating a compute environment, changing the bid percentage
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'desiredvCpus', 'computeResourceUpdate_desiredvCpus' - The desired number of Amazon EC2 vCPUS in the compute environment. Batch
-- modifies this value between the minimum and maximum values based on job
-- queue demand.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- Batch doesn\'t support changing the desired number of vCPUs of an
-- existing compute environment. Don\'t specify this parameter for compute
-- environments using Amazon EKS clusters.
--
-- When you update the @desiredvCpus@ setting, the value must be between
-- the @minvCpus@ and @maxvCpus@ values.
--
-- Additionally, the updated @desiredvCpus@ value must be greater than or
-- equal to the current @desiredvCpus@ value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#error-desired-vcpus-update Troubleshooting Batch>
-- in the /Batch User Guide/.
--
-- 'ec2Configuration', 'computeResourceUpdate_ec2Configuration' - Provides information used to select Amazon Machine Images (AMIs) for EC2
-- instances in the compute environment. If @Ec2Configuration@ isn\'t
-- specified, the default is @ECS_AL2@.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/. To remove the EC2 configuration and any
-- custom AMI ID specified in @imageIdOverride@, set this value to an empty
-- string.
--
-- One or two values can be provided.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'ec2KeyPair', 'computeResourceUpdate_ec2KeyPair' - The Amazon EC2 key pair that\'s used for instances launched in the
-- compute environment. You can use this key pair to log in to your
-- instances with SSH. To remove the Amazon EC2 key pair, set this value to
-- an empty string.
--
-- When updating a compute environment, changing the EC2 key pair requires
-- an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'imageId', 'computeResourceUpdate_imageId' - The Amazon Machine Image (AMI) ID used for instances launched in the
-- compute environment. This parameter is overridden by the
-- @imageIdOverride@ member of the @Ec2Configuration@ structure. To remove
-- the custom AMI ID and use the default AMI ID, set this value to an empty
-- string.
--
-- When updating a compute environment, changing the AMI ID requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
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
-- 'instanceRole', 'computeResourceUpdate_instanceRole' - The Amazon ECS instance profile applied to Amazon EC2 instances in a
-- compute environment. You can specify the short name or full Amazon
-- Resource Name (ARN) of an instance profile. For example,
-- @ @/@ecsInstanceRole@/@ @ or
-- @arn:aws:iam::@/@\<aws_account_id>@/@:instance-profile\/@/@ecsInstanceRole@/@ @.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS instance role>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'instanceTypes', 'computeResourceUpdate_instanceTypes' - The instances types that can be launched. You can specify instance
-- families to launch any instance type within those families (for example,
-- @c5@ or @p3@), or you can specify specific sizes within a family (such
-- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
-- (from the C4, M4, and R4 instance families) that match the demand of
-- your job queues.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
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
-- 'launchTemplate', 'computeResourceUpdate_launchTemplate' - The updated launch template to use for your compute resources. You must
-- specify either the launch template ID or launch template name in the
-- request, but not both. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch template support>
-- in the /Batch User Guide/. To remove the custom launch template and use
-- the default launch template, set @launchTemplateId@ or
-- @launchTemplateName@ member of the launch template specification to an
-- empty string. Removing the launch template from a compute environment
-- will not remove the AMI specified in the launch template. In order to
-- update the AMI specified in a launch template, the
-- @updateToLatestImageVersion@ parameter must be set to @true@.
--
-- When updating a compute environment, changing the launch template
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'maxvCpus', 'computeResourceUpdate_maxvCpus' - The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies using On-Demand or Spot Instances, and the
-- @BEST_FIT@ strategy using Spot Instances, Batch might need to exceed
-- @maxvCpus@ to meet your capacity requirements. In this event, Batch
-- never exceeds @maxvCpus@ by more than a single instance. That is, no
-- more than a single instance from among those specified in your compute
-- environment.
--
-- 'minvCpus', 'computeResourceUpdate_minvCpus' - The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain (even if the compute environment is @DISABLED@).
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'placementGroup', 'computeResourceUpdate_placementGroup' - The Amazon EC2 placement group to associate with your compute resources.
-- If you intend to submit multi-node parallel jobs to your compute
-- environment, you should consider creating a cluster placement group and
-- associate it with your compute resources. This keeps your multi-node
-- parallel job on a logical grouping of instances within a single
-- Availability Zone with high network flow potential. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- When updating a compute environment, changing the placement group
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'securityGroupIds', 'computeResourceUpdate_securityGroupIds' - The Amazon EC2 security groups that are associated with instances
-- launched in the compute environment. This parameter is required for
-- Fargate compute resources, where it can contain up to 5 security groups.
-- For Fargate compute resources, providing an empty list is handled as if
-- this parameter wasn\'t specified and no change is made. For EC2 compute
-- resources, providing an empty list removes the security groups from the
-- compute resource.
--
-- When updating a compute environment, changing the EC2 security groups
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- 'subnets', 'computeResourceUpdate_subnets' - The VPC subnets where the compute resources are launched. Fargate
-- compute resources can contain up to 16 subnets. For Fargate compute
-- resources, providing an empty list will be handled as if this parameter
-- wasn\'t specified and no change is made. For EC2 compute resources,
-- providing an empty list removes the VPC subnets from the compute
-- resource. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and subnets>
-- in the /Amazon VPC User Guide/.
--
-- When updating a compute environment, changing the VPC subnets requires
-- an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
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
--
-- 'tags', 'computeResourceUpdate_tags' - Key-value pair tags to be applied to EC2 resources that are launched in
-- the compute environment. For Batch, these take the form of
-- @\"String1\": \"String2\"@, where @String1@ is the tag key and @String2@
-- is the tag value-for example,
-- @{ \"Name\": \"Batch Instance - C4OnDemand\" }@. This is helpful for
-- recognizing your Batch instances in the Amazon EC2 console. These tags
-- aren\'t seen when using the Batch @ListTagsForResource@ API operation.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- 'type'', 'computeResourceUpdate_type' - The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
-- @FARGATE_SPOT@. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
--
-- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
-- role with the @spotIamFleetRole@ parameter. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing the type of a compute
-- environment requires an infrastructure update of the compute
-- environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- 'updateToLatestImageVersion', 'computeResourceUpdate_updateToLatestImageVersion' - Specifies whether the AMI ID is updated to the latest one that\'s
-- supported by Batch when the compute environment has an infrastructure
-- update. The default value is @false@.
--
-- An AMI ID can either be specified in the @imageId@ or @imageIdOverride@
-- parameters or be determined by the launch template that\'s specified in
-- the @launchTemplate@ parameter. If an AMI ID is specified any of these
-- ways, this parameter is ignored. For more information about to update
-- AMI IDs during an infrastructure update, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html#updating-compute-environments-ami Updating the AMI ID>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
newComputeResourceUpdate ::
  ComputeResourceUpdate
newComputeResourceUpdate =
  ComputeResourceUpdate'
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
      maxvCpus = Prelude.Nothing,
      minvCpus = Prelude.Nothing,
      placementGroup = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnets = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      updateToLatestImageVersion = Prelude.Nothing
    }

-- | The allocation strategy to use for the compute resource if there\'s not
-- enough instances of the best fitting instance type that can be
-- allocated. This might be because of availability of the instance type in
-- the Region or
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation strategies>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing the allocation strategy
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/. @BEST_FIT@ isn\'t supported when updating a
-- compute environment.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
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
computeResourceUpdate_allocationStrategy :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe CRUpdateAllocationStrategy)
computeResourceUpdate_allocationStrategy = Lens.lens (\ComputeResourceUpdate' {allocationStrategy} -> allocationStrategy) (\s@ComputeResourceUpdate' {} a -> s {allocationStrategy = a} :: ComputeResourceUpdate)

-- | The maximum percentage that a Spot Instance price can be when compared
-- with the On-Demand price for that instance type before instances are
-- launched. For example, if your maximum percentage is 20%, the Spot price
-- must be less than 20% of the current On-Demand price for that Amazon EC2
-- instance. You always pay the lowest (market) price and never more than
-- your maximum percentage. For most use cases, we recommend leaving this
-- field empty.
--
-- When updating a compute environment, changing the bid percentage
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_bidPercentage :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_bidPercentage = Lens.lens (\ComputeResourceUpdate' {bidPercentage} -> bidPercentage) (\s@ComputeResourceUpdate' {} a -> s {bidPercentage = a} :: ComputeResourceUpdate)

-- | The desired number of Amazon EC2 vCPUS in the compute environment. Batch
-- modifies this value between the minimum and maximum values based on job
-- queue demand.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
--
-- Batch doesn\'t support changing the desired number of vCPUs of an
-- existing compute environment. Don\'t specify this parameter for compute
-- environments using Amazon EKS clusters.
--
-- When you update the @desiredvCpus@ setting, the value must be between
-- the @minvCpus@ and @maxvCpus@ values.
--
-- Additionally, the updated @desiredvCpus@ value must be greater than or
-- equal to the current @desiredvCpus@ value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#error-desired-vcpus-update Troubleshooting Batch>
-- in the /Batch User Guide/.
computeResourceUpdate_desiredvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_desiredvCpus = Lens.lens (\ComputeResourceUpdate' {desiredvCpus} -> desiredvCpus) (\s@ComputeResourceUpdate' {} a -> s {desiredvCpus = a} :: ComputeResourceUpdate)

-- | Provides information used to select Amazon Machine Images (AMIs) for EC2
-- instances in the compute environment. If @Ec2Configuration@ isn\'t
-- specified, the default is @ECS_AL2@.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/. To remove the EC2 configuration and any
-- custom AMI ID specified in @imageIdOverride@, set this value to an empty
-- string.
--
-- One or two values can be provided.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_ec2Configuration :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Ec2Configuration])
computeResourceUpdate_ec2Configuration = Lens.lens (\ComputeResourceUpdate' {ec2Configuration} -> ec2Configuration) (\s@ComputeResourceUpdate' {} a -> s {ec2Configuration = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon EC2 key pair that\'s used for instances launched in the
-- compute environment. You can use this key pair to log in to your
-- instances with SSH. To remove the Amazon EC2 key pair, set this value to
-- an empty string.
--
-- When updating a compute environment, changing the EC2 key pair requires
-- an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_ec2KeyPair :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Text)
computeResourceUpdate_ec2KeyPair = Lens.lens (\ComputeResourceUpdate' {ec2KeyPair} -> ec2KeyPair) (\s@ComputeResourceUpdate' {} a -> s {ec2KeyPair = a} :: ComputeResourceUpdate)

-- | The Amazon Machine Image (AMI) ID used for instances launched in the
-- compute environment. This parameter is overridden by the
-- @imageIdOverride@ member of the @Ec2Configuration@ structure. To remove
-- the custom AMI ID and use the default AMI ID, set this value to an empty
-- string.
--
-- When updating a compute environment, changing the AMI ID requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
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
computeResourceUpdate_imageId :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Text)
computeResourceUpdate_imageId = Lens.lens (\ComputeResourceUpdate' {imageId} -> imageId) (\s@ComputeResourceUpdate' {} a -> s {imageId = a} :: ComputeResourceUpdate)

-- | The Amazon ECS instance profile applied to Amazon EC2 instances in a
-- compute environment. You can specify the short name or full Amazon
-- Resource Name (ARN) of an instance profile. For example,
-- @ @/@ecsInstanceRole@/@ @ or
-- @arn:aws:iam::@/@\<aws_account_id>@/@:instance-profile\/@/@ecsInstanceRole@/@ @.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS instance role>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_instanceRole :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Text)
computeResourceUpdate_instanceRole = Lens.lens (\ComputeResourceUpdate' {instanceRole} -> instanceRole) (\s@ComputeResourceUpdate' {} a -> s {instanceRole = a} :: ComputeResourceUpdate)

-- | The instances types that can be launched. You can specify instance
-- families to launch any instance type within those families (for example,
-- @c5@ or @p3@), or you can specify specific sizes within a family (such
-- as @c5.8xlarge@). You can also choose @optimal@ to select instance types
-- (from the C4, M4, and R4 instance families) that match the demand of
-- your job queues.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
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
computeResourceUpdate_instanceTypes :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Prelude.Text])
computeResourceUpdate_instanceTypes = Lens.lens (\ComputeResourceUpdate' {instanceTypes} -> instanceTypes) (\s@ComputeResourceUpdate' {} a -> s {instanceTypes = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The updated launch template to use for your compute resources. You must
-- specify either the launch template ID or launch template name in the
-- request, but not both. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch template support>
-- in the /Batch User Guide/. To remove the custom launch template and use
-- the default launch template, set @launchTemplateId@ or
-- @launchTemplateName@ member of the launch template specification to an
-- empty string. Removing the launch template from a compute environment
-- will not remove the AMI specified in the launch template. In order to
-- update the AMI specified in a launch template, the
-- @updateToLatestImageVersion@ parameter must be set to @true@.
--
-- When updating a compute environment, changing the launch template
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_launchTemplate :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe LaunchTemplateSpecification)
computeResourceUpdate_launchTemplate = Lens.lens (\ComputeResourceUpdate' {launchTemplate} -> launchTemplate) (\s@ComputeResourceUpdate' {} a -> s {launchTemplate = a} :: ComputeResourceUpdate)

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies using On-Demand or Spot Instances, and the
-- @BEST_FIT@ strategy using Spot Instances, Batch might need to exceed
-- @maxvCpus@ to meet your capacity requirements. In this event, Batch
-- never exceeds @maxvCpus@ by more than a single instance. That is, no
-- more than a single instance from among those specified in your compute
-- environment.
computeResourceUpdate_maxvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_maxvCpus = Lens.lens (\ComputeResourceUpdate' {maxvCpus} -> maxvCpus) (\s@ComputeResourceUpdate' {} a -> s {maxvCpus = a} :: ComputeResourceUpdate)

-- | The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain (even if the compute environment is @DISABLED@).
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_minvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_minvCpus = Lens.lens (\ComputeResourceUpdate' {minvCpus} -> minvCpus) (\s@ComputeResourceUpdate' {} a -> s {minvCpus = a} :: ComputeResourceUpdate)

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
-- When updating a compute environment, changing the placement group
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_placementGroup :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Text)
computeResourceUpdate_placementGroup = Lens.lens (\ComputeResourceUpdate' {placementGroup} -> placementGroup) (\s@ComputeResourceUpdate' {} a -> s {placementGroup = a} :: ComputeResourceUpdate)

-- | The Amazon EC2 security groups that are associated with instances
-- launched in the compute environment. This parameter is required for
-- Fargate compute resources, where it can contain up to 5 security groups.
-- For Fargate compute resources, providing an empty list is handled as if
-- this parameter wasn\'t specified and no change is made. For EC2 compute
-- resources, providing an empty list removes the security groups from the
-- compute resource.
--
-- When updating a compute environment, changing the EC2 security groups
-- requires an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
computeResourceUpdate_securityGroupIds :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Prelude.Text])
computeResourceUpdate_securityGroupIds = Lens.lens (\ComputeResourceUpdate' {securityGroupIds} -> securityGroupIds) (\s@ComputeResourceUpdate' {} a -> s {securityGroupIds = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The VPC subnets where the compute resources are launched. Fargate
-- compute resources can contain up to 16 subnets. For Fargate compute
-- resources, providing an empty list will be handled as if this parameter
-- wasn\'t specified and no change is made. For EC2 compute resources,
-- providing an empty list removes the VPC subnets from the compute
-- resource. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and subnets>
-- in the /Amazon VPC User Guide/.
--
-- When updating a compute environment, changing the VPC subnets requires
-- an infrastructure update of the compute environment. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
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
computeResourceUpdate_subnets :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Prelude.Text])
computeResourceUpdate_subnets = Lens.lens (\ComputeResourceUpdate' {subnets} -> subnets) (\s@ComputeResourceUpdate' {} a -> s {subnets = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Key-value pair tags to be applied to EC2 resources that are launched in
-- the compute environment. For Batch, these take the form of
-- @\"String1\": \"String2\"@, where @String1@ is the tag key and @String2@
-- is the tag value-for example,
-- @{ \"Name\": \"Batch Instance - C4OnDemand\" }@. This is helpful for
-- recognizing your Batch instances in the Amazon EC2 console. These tags
-- aren\'t seen when using the Batch @ListTagsForResource@ API operation.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t specify it.
computeResourceUpdate_tags :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
computeResourceUpdate_tags = Lens.lens (\ComputeResourceUpdate' {tags} -> tags) (\s@ComputeResourceUpdate' {} a -> s {tags = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The type of compute environment: @EC2@, @SPOT@, @FARGATE@, or
-- @FARGATE_SPOT@. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute environments>
-- in the /Batch User Guide/.
--
-- If you choose @SPOT@, you must also specify an Amazon EC2 Spot Fleet
-- role with the @spotIamFleetRole@ parameter. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 spot fleet role>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing the type of a compute
-- environment requires an infrastructure update of the compute
-- environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
computeResourceUpdate_type :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe CRType)
computeResourceUpdate_type = Lens.lens (\ComputeResourceUpdate' {type'} -> type') (\s@ComputeResourceUpdate' {} a -> s {type' = a} :: ComputeResourceUpdate)

-- | Specifies whether the AMI ID is updated to the latest one that\'s
-- supported by Batch when the compute environment has an infrastructure
-- update. The default value is @false@.
--
-- An AMI ID can either be specified in the @imageId@ or @imageIdOverride@
-- parameters or be determined by the launch template that\'s specified in
-- the @launchTemplate@ parameter. If an AMI ID is specified any of these
-- ways, this parameter is ignored. For more information about to update
-- AMI IDs during an infrastructure update, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html#updating-compute-environments-ami Updating the AMI ID>
-- in the /Batch User Guide/.
--
-- When updating a compute environment, changing this setting requires an
-- infrastructure update of the compute environment. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
computeResourceUpdate_updateToLatestImageVersion :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Bool)
computeResourceUpdate_updateToLatestImageVersion = Lens.lens (\ComputeResourceUpdate' {updateToLatestImageVersion} -> updateToLatestImageVersion) (\s@ComputeResourceUpdate' {} a -> s {updateToLatestImageVersion = a} :: ComputeResourceUpdate)

instance Prelude.Hashable ComputeResourceUpdate where
  hashWithSalt _salt ComputeResourceUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` bidPercentage
      `Prelude.hashWithSalt` desiredvCpus
      `Prelude.hashWithSalt` ec2Configuration
      `Prelude.hashWithSalt` ec2KeyPair
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` instanceRole
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` maxvCpus
      `Prelude.hashWithSalt` minvCpus
      `Prelude.hashWithSalt` placementGroup
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updateToLatestImageVersion

instance Prelude.NFData ComputeResourceUpdate where
  rnf ComputeResourceUpdate' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf bidPercentage
      `Prelude.seq` Prelude.rnf desiredvCpus
      `Prelude.seq` Prelude.rnf ec2Configuration
      `Prelude.seq` Prelude.rnf ec2KeyPair
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf instanceRole
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf maxvCpus
      `Prelude.seq` Prelude.rnf minvCpus
      `Prelude.seq` Prelude.rnf placementGroup
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf
        updateToLatestImageVersion

instance Data.ToJSON ComputeResourceUpdate where
  toJSON ComputeResourceUpdate' {..} =
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
            ("maxvCpus" Data..=) Prelude.<$> maxvCpus,
            ("minvCpus" Data..=) Prelude.<$> minvCpus,
            ("placementGroup" Data..=)
              Prelude.<$> placementGroup,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("subnets" Data..=) Prelude.<$> subnets,
            ("tags" Data..=) Prelude.<$> tags,
            ("type" Data..=) Prelude.<$> type',
            ("updateToLatestImageVersion" Data..=)
              Prelude.<$> updateToLatestImageVersion
          ]
      )
