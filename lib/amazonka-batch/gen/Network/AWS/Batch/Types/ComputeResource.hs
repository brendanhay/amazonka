{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ComputeResource
  ( ComputeResource (..)
  -- * Smart constructor
  , mkComputeResource
  -- * Lenses
  , crType
  , crMinvCpus
  , crMaxvCpus
  , crInstanceTypes
  , crSubnets
  , crInstanceRole
  , crAllocationStrategy
  , crBidPercentage
  , crDesiredvCpus
  , crEc2Configuration
  , crEc2KeyPair
  , crImageId
  , crLaunchTemplate
  , crPlacementGroup
  , crSecurityGroupIds
  , crSpotIamFleetRole
  , crTags
  ) where

import qualified Network.AWS.Batch.Types.CRAllocationStrategy as Types
import qualified Network.AWS.Batch.Types.CRType as Types
import qualified Network.AWS.Batch.Types.Ec2Configuration as Types
import qualified Network.AWS.Batch.Types.LaunchTemplateSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch compute resource.
--
-- /See:/ 'mkComputeResource' smart constructor.
data ComputeResource = ComputeResource'
  { type' :: Types.CRType
    -- ^ The type of compute environment: @EC2@ or @SPOT@ .
  , minvCpus :: Core.Int
    -- ^ The minimum number of Amazon EC2 vCPUs that an environment should maintain (even if the compute environment is @DISABLED@ ).
  , maxvCpus :: Core.Int
    -- ^ The maximum number of Amazon EC2 vCPUs that an environment can reach.
  , instanceTypes :: [Core.Text]
    -- ^ The instances types that may be launched. You can specify instance families to launch any instance type within those families (for example, @c5@ or @p3@ ), or you can specify specific sizes within a family (such as @c5.8xlarge@ ). You can also choose @optimal@ to pick instance types (from the C, M, and R instance families) on the fly that match the demand of your job queues.
  , subnets :: [Core.Text]
    -- ^ The VPC subnets into which the compute resources are launched. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> in the /Amazon VPC User Guide/ .
  , instanceRole :: Core.Text
    -- ^ The Amazon ECS instance profile applied to Amazon EC2 instances in a compute environment. You can specify the short name or full Amazon Resource Name (ARN) of an instance profile. For example, @/ecsInstanceRole/ @ or @arn:aws:iam::/<aws_account_id>/ :instance-profile//ecsInstanceRole/ @ . For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS Instance Role> in the /AWS Batch User Guide/ .
  , allocationStrategy :: Core.Maybe Types.CRAllocationStrategy
    -- ^ The allocation strategy to use for the compute resource in case not enough instances of the best fitting instance type can be allocated. This could be due to availability of the instance type in the region or <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits> . If this is not specified, the default is @BEST_FIT@ , which will use only the best fitting instance type, waiting for additional capacity if it's not available. This allocation strategy keeps costs lower but can limit scaling. If you are using Spot Fleets with @BEST_FIT@ then the Spot Fleet IAM Role must be specified. @BEST_FIT_PROGRESSIVE@ will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types with a lower cost per vCPU. @SPOT_CAPACITY_OPTIMIZED@ is only available for Spot Instance compute resources and will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types that are less likely to be interrupted. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation Strategies> in the /AWS Batch User Guide/ .
  , bidPercentage :: Core.Maybe Core.Int
    -- ^ The maximum percentage that a Spot Instance price can be when compared with the On-Demand price for that instance type before instances are launched. For example, if your maximum percentage is 20%, then the Spot price must be below 20% of the current On-Demand price for that Amazon EC2 instance. You always pay the lowest (market) price and never more than your maximum percentage. If you leave this field empty, the default value is 100% of the On-Demand price.
  , desiredvCpus :: Core.Maybe Core.Int
    -- ^ The desired number of Amazon EC2 vCPUS in the compute environment.
  , ec2Configuration :: Core.Maybe [Types.Ec2Configuration]
    -- ^ Provides additional details used to selecting the AMI to use for instances in a compute environment.
  , ec2KeyPair :: Core.Maybe Core.Text
    -- ^ The Amazon EC2 key pair that is used for instances launched in the compute environment.
  , imageId :: Core.Maybe Core.Text
    -- ^ The Amazon Machine Image (AMI) ID used for instances launched in the compute environment. This parameter is overridden by the @imageIdOverride@ member of the @Ec2Configuration@ structure.
  , launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification
    -- ^ The launch template to use for your compute resources. Any other compute resource parameters that you specify in a 'CreateComputeEnvironment' API operation override the same parameters in the launch template. You must specify either the launch template ID or launch template name in the request, but not both. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch Template Support> in the /AWS Batch User Guide/ .
  , placementGroup :: Core.Maybe Core.Text
    -- ^ The Amazon EC2 placement group to associate with your compute resources. If you intend to submit multi-node parallel jobs to your compute environment, you should consider creating a cluster placement group and associate it with your compute resources. This keeps your multi-node parallel job on a logical grouping of instances within a single Availability Zone with high network flow potential. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ The Amazon EC2 security groups associated with instances launched in the compute environment. One or more security groups must be specified, either in @securityGroupIds@ or using a launch template referenced in @launchTemplate@ . If security groups are specified using both @securityGroupIds@ and @launchTemplate@ , the values in @securityGroupIds@ will be used.
  , spotIamFleetRole :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role applied to a @SPOT@ compute environment. This role is required if the allocation strategy set to @BEST_FIT@ or if the allocation strategy is not specified. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet Role> in the /AWS Batch User Guide/ .
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Key-value pair tags to be applied to resources that are launched in the compute environment. For AWS Batch, these take the form of "String1": "String2", where String1 is the tag key and String2 is the tag value—for example, { "Name": "AWS Batch Instance - C4OnDemand" }. These tags can not be updated or removed after the compute environment has been created; any changes require creating a new compute environment and removing the old compute environment. These tags are not seen when using the AWS Batch ListTagsForResource API operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeResource' value with any optional fields omitted.
mkComputeResource
    :: Types.CRType -- ^ 'type\''
    -> Core.Int -- ^ 'minvCpus'
    -> Core.Int -- ^ 'maxvCpus'
    -> Core.Text -- ^ 'instanceRole'
    -> ComputeResource
mkComputeResource type' minvCpus maxvCpus instanceRole
  = ComputeResource'{type', minvCpus, maxvCpus,
                     instanceTypes = Core.mempty, subnets = Core.mempty, instanceRole,
                     allocationStrategy = Core.Nothing, bidPercentage = Core.Nothing,
                     desiredvCpus = Core.Nothing, ec2Configuration = Core.Nothing,
                     ec2KeyPair = Core.Nothing, imageId = Core.Nothing,
                     launchTemplate = Core.Nothing, placementGroup = Core.Nothing,
                     securityGroupIds = Core.Nothing, spotIamFleetRole = Core.Nothing,
                     tags = Core.Nothing}

-- | The type of compute environment: @EC2@ or @SPOT@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crType :: Lens.Lens' ComputeResource Types.CRType
crType = Lens.field @"type'"
{-# INLINEABLE crType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The minimum number of Amazon EC2 vCPUs that an environment should maintain (even if the compute environment is @DISABLED@ ).
--
-- /Note:/ Consider using 'minvCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMinvCpus :: Lens.Lens' ComputeResource Core.Int
crMinvCpus = Lens.field @"minvCpus"
{-# INLINEABLE crMinvCpus #-}
{-# DEPRECATED minvCpus "Use generic-lens or generic-optics with 'minvCpus' instead"  #-}

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- /Note:/ Consider using 'maxvCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaxvCpus :: Lens.Lens' ComputeResource Core.Int
crMaxvCpus = Lens.field @"maxvCpus"
{-# INLINEABLE crMaxvCpus #-}
{-# DEPRECATED maxvCpus "Use generic-lens or generic-optics with 'maxvCpus' instead"  #-}

-- | The instances types that may be launched. You can specify instance families to launch any instance type within those families (for example, @c5@ or @p3@ ), or you can specify specific sizes within a family (such as @c5.8xlarge@ ). You can also choose @optimal@ to pick instance types (from the C, M, and R instance families) on the fly that match the demand of your job queues.
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceTypes :: Lens.Lens' ComputeResource [Core.Text]
crInstanceTypes = Lens.field @"instanceTypes"
{-# INLINEABLE crInstanceTypes #-}
{-# DEPRECATED instanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead"  #-}

-- | The VPC subnets into which the compute resources are launched. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> in the /Amazon VPC User Guide/ .
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSubnets :: Lens.Lens' ComputeResource [Core.Text]
crSubnets = Lens.field @"subnets"
{-# INLINEABLE crSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The Amazon ECS instance profile applied to Amazon EC2 instances in a compute environment. You can specify the short name or full Amazon Resource Name (ARN) of an instance profile. For example, @/ecsInstanceRole/ @ or @arn:aws:iam::/<aws_account_id>/ :instance-profile//ecsInstanceRole/ @ . For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS Instance Role> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'instanceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceRole :: Lens.Lens' ComputeResource Core.Text
crInstanceRole = Lens.field @"instanceRole"
{-# INLINEABLE crInstanceRole #-}
{-# DEPRECATED instanceRole "Use generic-lens or generic-optics with 'instanceRole' instead"  #-}

-- | The allocation strategy to use for the compute resource in case not enough instances of the best fitting instance type can be allocated. This could be due to availability of the instance type in the region or <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits> . If this is not specified, the default is @BEST_FIT@ , which will use only the best fitting instance type, waiting for additional capacity if it's not available. This allocation strategy keeps costs lower but can limit scaling. If you are using Spot Fleets with @BEST_FIT@ then the Spot Fleet IAM Role must be specified. @BEST_FIT_PROGRESSIVE@ will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types with a lower cost per vCPU. @SPOT_CAPACITY_OPTIMIZED@ is only available for Spot Instance compute resources and will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types that are less likely to be interrupted. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation Strategies> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllocationStrategy :: Lens.Lens' ComputeResource (Core.Maybe Types.CRAllocationStrategy)
crAllocationStrategy = Lens.field @"allocationStrategy"
{-# INLINEABLE crAllocationStrategy #-}
{-# DEPRECATED allocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead"  #-}

-- | The maximum percentage that a Spot Instance price can be when compared with the On-Demand price for that instance type before instances are launched. For example, if your maximum percentage is 20%, then the Spot price must be below 20% of the current On-Demand price for that Amazon EC2 instance. You always pay the lowest (market) price and never more than your maximum percentage. If you leave this field empty, the default value is 100% of the On-Demand price.
--
-- /Note:/ Consider using 'bidPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crBidPercentage :: Lens.Lens' ComputeResource (Core.Maybe Core.Int)
crBidPercentage = Lens.field @"bidPercentage"
{-# INLINEABLE crBidPercentage #-}
{-# DEPRECATED bidPercentage "Use generic-lens or generic-optics with 'bidPercentage' instead"  #-}

-- | The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- /Note:/ Consider using 'desiredvCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDesiredvCpus :: Lens.Lens' ComputeResource (Core.Maybe Core.Int)
crDesiredvCpus = Lens.field @"desiredvCpus"
{-# INLINEABLE crDesiredvCpus #-}
{-# DEPRECATED desiredvCpus "Use generic-lens or generic-optics with 'desiredvCpus' instead"  #-}

-- | Provides additional details used to selecting the AMI to use for instances in a compute environment.
--
-- /Note:/ Consider using 'ec2Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEc2Configuration :: Lens.Lens' ComputeResource (Core.Maybe [Types.Ec2Configuration])
crEc2Configuration = Lens.field @"ec2Configuration"
{-# INLINEABLE crEc2Configuration #-}
{-# DEPRECATED ec2Configuration "Use generic-lens or generic-optics with 'ec2Configuration' instead"  #-}

-- | The Amazon EC2 key pair that is used for instances launched in the compute environment.
--
-- /Note:/ Consider using 'ec2KeyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEc2KeyPair :: Lens.Lens' ComputeResource (Core.Maybe Core.Text)
crEc2KeyPair = Lens.field @"ec2KeyPair"
{-# INLINEABLE crEc2KeyPair #-}
{-# DEPRECATED ec2KeyPair "Use generic-lens or generic-optics with 'ec2KeyPair' instead"  #-}

-- | The Amazon Machine Image (AMI) ID used for instances launched in the compute environment. This parameter is overridden by the @imageIdOverride@ member of the @Ec2Configuration@ structure.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crImageId :: Lens.Lens' ComputeResource (Core.Maybe Core.Text)
crImageId = Lens.field @"imageId"
{-# INLINEABLE crImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The launch template to use for your compute resources. Any other compute resource parameters that you specify in a 'CreateComputeEnvironment' API operation override the same parameters in the launch template. You must specify either the launch template ID or launch template name in the request, but not both. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch Template Support> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crLaunchTemplate :: Lens.Lens' ComputeResource (Core.Maybe Types.LaunchTemplateSpecification)
crLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE crLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | The Amazon EC2 placement group to associate with your compute resources. If you intend to submit multi-node parallel jobs to your compute environment, you should consider creating a cluster placement group and associate it with your compute resources. This keeps your multi-node parallel job on a logical grouping of instances within a single Availability Zone with high network flow potential. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPlacementGroup :: Lens.Lens' ComputeResource (Core.Maybe Core.Text)
crPlacementGroup = Lens.field @"placementGroup"
{-# INLINEABLE crPlacementGroup #-}
{-# DEPRECATED placementGroup "Use generic-lens or generic-optics with 'placementGroup' instead"  #-}

-- | The Amazon EC2 security groups associated with instances launched in the compute environment. One or more security groups must be specified, either in @securityGroupIds@ or using a launch template referenced in @launchTemplate@ . If security groups are specified using both @securityGroupIds@ and @launchTemplate@ , the values in @securityGroupIds@ will be used.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSecurityGroupIds :: Lens.Lens' ComputeResource (Core.Maybe [Core.Text])
crSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE crSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role applied to a @SPOT@ compute environment. This role is required if the allocation strategy set to @BEST_FIT@ or if the allocation strategy is not specified. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet Role> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'spotIamFleetRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSpotIamFleetRole :: Lens.Lens' ComputeResource (Core.Maybe Core.Text)
crSpotIamFleetRole = Lens.field @"spotIamFleetRole"
{-# INLINEABLE crSpotIamFleetRole #-}
{-# DEPRECATED spotIamFleetRole "Use generic-lens or generic-optics with 'spotIamFleetRole' instead"  #-}

-- | Key-value pair tags to be applied to resources that are launched in the compute environment. For AWS Batch, these take the form of "String1": "String2", where String1 is the tag key and String2 is the tag value—for example, { "Name": "AWS Batch Instance - C4OnDemand" }. These tags can not be updated or removed after the compute environment has been created; any changes require creating a new compute environment and removing the old compute environment. These tags are not seen when using the AWS Batch ListTagsForResource API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' ComputeResource (Core.Maybe (Core.HashMap Core.Text Core.Text))
crTags = Lens.field @"tags"
{-# INLINEABLE crTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON ComputeResource where
        toJSON ComputeResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  Core.Just ("minvCpus" Core..= minvCpus),
                  Core.Just ("maxvCpus" Core..= maxvCpus),
                  Core.Just ("instanceTypes" Core..= instanceTypes),
                  Core.Just ("subnets" Core..= subnets),
                  Core.Just ("instanceRole" Core..= instanceRole),
                  ("allocationStrategy" Core..=) Core.<$> allocationStrategy,
                  ("bidPercentage" Core..=) Core.<$> bidPercentage,
                  ("desiredvCpus" Core..=) Core.<$> desiredvCpus,
                  ("ec2Configuration" Core..=) Core.<$> ec2Configuration,
                  ("ec2KeyPair" Core..=) Core.<$> ec2KeyPair,
                  ("imageId" Core..=) Core.<$> imageId,
                  ("launchTemplate" Core..=) Core.<$> launchTemplate,
                  ("placementGroup" Core..=) Core.<$> placementGroup,
                  ("securityGroupIds" Core..=) Core.<$> securityGroupIds,
                  ("spotIamFleetRole" Core..=) Core.<$> spotIamFleetRole,
                  ("tags" Core..=) Core.<$> tags])

instance Core.FromJSON ComputeResource where
        parseJSON
          = Core.withObject "ComputeResource" Core.$
              \ x ->
                ComputeResource' Core.<$>
                  (x Core..: "type") Core.<*> x Core..: "minvCpus" Core.<*>
                    x Core..: "maxvCpus"
                    Core.<*> x Core..:? "instanceTypes" Core..!= Core.mempty
                    Core.<*> x Core..:? "subnets" Core..!= Core.mempty
                    Core.<*> x Core..: "instanceRole"
                    Core.<*> x Core..:? "allocationStrategy"
                    Core.<*> x Core..:? "bidPercentage"
                    Core.<*> x Core..:? "desiredvCpus"
                    Core.<*> x Core..:? "ec2Configuration"
                    Core.<*> x Core..:? "ec2KeyPair"
                    Core.<*> x Core..:? "imageId"
                    Core.<*> x Core..:? "launchTemplate"
                    Core.<*> x Core..:? "placementGroup"
                    Core.<*> x Core..:? "securityGroupIds"
                    Core.<*> x Core..:? "spotIamFleetRole"
                    Core.<*> x Core..:? "tags"
