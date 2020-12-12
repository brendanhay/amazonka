{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeResource
  ( ComputeResource (..),

    -- * Smart constructor
    mkComputeResource,

    -- * Lenses
    crSecurityGroupIds,
    crEc2KeyPair,
    crEc2Configuration,
    crBidPercentage,
    crSpotIAMFleetRole,
    crImageId,
    crLaunchTemplate,
    crDesiredvCPUs,
    crAllocationStrategy,
    crPlacementGroup,
    crTags,
    crType,
    crMinvCPUs,
    crMaxvCPUs,
    crInstanceTypes,
    crSubnets,
    crInstanceRole,
  )
where

import Network.AWS.Batch.Types.CRAllocationStrategy
import Network.AWS.Batch.Types.CRType
import Network.AWS.Batch.Types.EC2Configuration
import Network.AWS.Batch.Types.LaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an AWS Batch compute resource.
--
-- /See:/ 'mkComputeResource' smart constructor.
data ComputeResource = ComputeResource'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    ec2KeyPair :: Lude.Maybe Lude.Text,
    ec2Configuration :: Lude.Maybe [EC2Configuration],
    bidPercentage :: Lude.Maybe Lude.Int,
    spotIAMFleetRole :: Lude.Maybe Lude.Text,
    imageId :: Lude.Maybe Lude.Text,
    launchTemplate :: Lude.Maybe LaunchTemplateSpecification,
    desiredvCPUs :: Lude.Maybe Lude.Int,
    allocationStrategy :: Lude.Maybe CRAllocationStrategy,
    placementGroup :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    type' :: CRType,
    minvCPUs :: Lude.Int,
    maxvCPUs :: Lude.Int,
    instanceTypes :: [Lude.Text],
    subnets :: [Lude.Text],
    instanceRole :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeResource' with the minimum fields required to make a request.
--
-- * 'allocationStrategy' - The allocation strategy to use for the compute resource in case not enough instances of the best fitting instance type can be allocated. This could be due to availability of the instance type in the region or <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits> . If this is not specified, the default is @BEST_FIT@ , which will use only the best fitting instance type, waiting for additional capacity if it's not available. This allocation strategy keeps costs lower but can limit scaling. If you are using Spot Fleets with @BEST_FIT@ then the Spot Fleet IAM Role must be specified. @BEST_FIT_PROGRESSIVE@ will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types with a lower cost per vCPU. @SPOT_CAPACITY_OPTIMIZED@ is only available for Spot Instance compute resources and will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types that are less likely to be interrupted. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation Strategies> in the /AWS Batch User Guide/ .
-- * 'bidPercentage' - The maximum percentage that a Spot Instance price can be when compared with the On-Demand price for that instance type before instances are launched. For example, if your maximum percentage is 20%, then the Spot price must be below 20% of the current On-Demand price for that Amazon EC2 instance. You always pay the lowest (market) price and never more than your maximum percentage. If you leave this field empty, the default value is 100% of the On-Demand price.
-- * 'desiredvCPUs' - The desired number of Amazon EC2 vCPUS in the compute environment.
-- * 'ec2Configuration' - Provides additional details used to selecting the AMI to use for instances in a compute environment.
-- * 'ec2KeyPair' - The Amazon EC2 key pair that is used for instances launched in the compute environment.
-- * 'imageId' - The Amazon Machine Image (AMI) ID used for instances launched in the compute environment. This parameter is overridden by the @imageIdOverride@ member of the @Ec2Configuration@ structure.
-- * 'instanceRole' - The Amazon ECS instance profile applied to Amazon EC2 instances in a compute environment. You can specify the short name or full Amazon Resource Name (ARN) of an instance profile. For example, @/ecsInstanceRole/ @ or @arn:aws:iam::/<aws_account_id>/ :instance-profile//ecsInstanceRole/ @ . For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS Instance Role> in the /AWS Batch User Guide/ .
-- * 'instanceTypes' - The instances types that may be launched. You can specify instance families to launch any instance type within those families (for example, @c5@ or @p3@ ), or you can specify specific sizes within a family (such as @c5.8xlarge@ ). You can also choose @optimal@ to pick instance types (from the C, M, and R instance families) on the fly that match the demand of your job queues.
-- * 'launchTemplate' - The launch template to use for your compute resources. Any other compute resource parameters that you specify in a 'CreateComputeEnvironment' API operation override the same parameters in the launch template. You must specify either the launch template ID or launch template name in the request, but not both. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch Template Support> in the /AWS Batch User Guide/ .
-- * 'maxvCPUs' - The maximum number of Amazon EC2 vCPUs that an environment can reach.
-- * 'minvCPUs' - The minimum number of Amazon EC2 vCPUs that an environment should maintain (even if the compute environment is @DISABLED@ ).
-- * 'placementGroup' - The Amazon EC2 placement group to associate with your compute resources. If you intend to submit multi-node parallel jobs to your compute environment, you should consider creating a cluster placement group and associate it with your compute resources. This keeps your multi-node parallel job on a logical grouping of instances within a single Availability Zone with high network flow potential. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'securityGroupIds' - The Amazon EC2 security groups associated with instances launched in the compute environment. One or more security groups must be specified, either in @securityGroupIds@ or using a launch template referenced in @launchTemplate@ . If security groups are specified using both @securityGroupIds@ and @launchTemplate@ , the values in @securityGroupIds@ will be used.
-- * 'spotIAMFleetRole' - The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role applied to a @SPOT@ compute environment. This role is required if the allocation strategy set to @BEST_FIT@ or if the allocation strategy is not specified. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet Role> in the /AWS Batch User Guide/ .
-- * 'subnets' - The VPC subnets into which the compute resources are launched. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> in the /Amazon VPC User Guide/ .
-- * 'tags' - Key-value pair tags to be applied to resources that are launched in the compute environment. For AWS Batch, these take the form of "String1": "String2", where String1 is the tag key and String2 is the tag value—for example, { "Name": "AWS Batch Instance - C4OnDemand" }. These tags can not be updated or removed after the compute environment has been created; any changes require creating a new compute environment and removing the old compute environment. These tags are not seen when using the AWS Batch ListTagsForResource API operation.
-- * 'type'' - The type of compute environment: @EC2@ or @SPOT@ .
mkComputeResource ::
  -- | 'type''
  CRType ->
  -- | 'minvCPUs'
  Lude.Int ->
  -- | 'maxvCPUs'
  Lude.Int ->
  -- | 'instanceRole'
  Lude.Text ->
  ComputeResource
mkComputeResource pType_ pMinvCPUs_ pMaxvCPUs_ pInstanceRole_ =
  ComputeResource'
    { securityGroupIds = Lude.Nothing,
      ec2KeyPair = Lude.Nothing,
      ec2Configuration = Lude.Nothing,
      bidPercentage = Lude.Nothing,
      spotIAMFleetRole = Lude.Nothing,
      imageId = Lude.Nothing,
      launchTemplate = Lude.Nothing,
      desiredvCPUs = Lude.Nothing,
      allocationStrategy = Lude.Nothing,
      placementGroup = Lude.Nothing,
      tags = Lude.Nothing,
      type' = pType_,
      minvCPUs = pMinvCPUs_,
      maxvCPUs = pMaxvCPUs_,
      instanceTypes = Lude.mempty,
      subnets = Lude.mempty,
      instanceRole = pInstanceRole_
    }

-- | The Amazon EC2 security groups associated with instances launched in the compute environment. One or more security groups must be specified, either in @securityGroupIds@ or using a launch template referenced in @launchTemplate@ . If security groups are specified using both @securityGroupIds@ and @launchTemplate@ , the values in @securityGroupIds@ will be used.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSecurityGroupIds :: Lens.Lens' ComputeResource (Lude.Maybe [Lude.Text])
crSecurityGroupIds = Lens.lens (securityGroupIds :: ComputeResource -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ComputeResource)
{-# DEPRECATED crSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The Amazon EC2 key pair that is used for instances launched in the compute environment.
--
-- /Note:/ Consider using 'ec2KeyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEc2KeyPair :: Lens.Lens' ComputeResource (Lude.Maybe Lude.Text)
crEc2KeyPair = Lens.lens (ec2KeyPair :: ComputeResource -> Lude.Maybe Lude.Text) (\s a -> s {ec2KeyPair = a} :: ComputeResource)
{-# DEPRECATED crEc2KeyPair "Use generic-lens or generic-optics with 'ec2KeyPair' instead." #-}

-- | Provides additional details used to selecting the AMI to use for instances in a compute environment.
--
-- /Note:/ Consider using 'ec2Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEc2Configuration :: Lens.Lens' ComputeResource (Lude.Maybe [EC2Configuration])
crEc2Configuration = Lens.lens (ec2Configuration :: ComputeResource -> Lude.Maybe [EC2Configuration]) (\s a -> s {ec2Configuration = a} :: ComputeResource)
{-# DEPRECATED crEc2Configuration "Use generic-lens or generic-optics with 'ec2Configuration' instead." #-}

-- | The maximum percentage that a Spot Instance price can be when compared with the On-Demand price for that instance type before instances are launched. For example, if your maximum percentage is 20%, then the Spot price must be below 20% of the current On-Demand price for that Amazon EC2 instance. You always pay the lowest (market) price and never more than your maximum percentage. If you leave this field empty, the default value is 100% of the On-Demand price.
--
-- /Note:/ Consider using 'bidPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crBidPercentage :: Lens.Lens' ComputeResource (Lude.Maybe Lude.Int)
crBidPercentage = Lens.lens (bidPercentage :: ComputeResource -> Lude.Maybe Lude.Int) (\s a -> s {bidPercentage = a} :: ComputeResource)
{-# DEPRECATED crBidPercentage "Use generic-lens or generic-optics with 'bidPercentage' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon EC2 Spot Fleet IAM role applied to a @SPOT@ compute environment. This role is required if the allocation strategy set to @BEST_FIT@ or if the allocation strategy is not specified. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/spot_fleet_IAM_role.html Amazon EC2 Spot Fleet Role> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'spotIAMFleetRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSpotIAMFleetRole :: Lens.Lens' ComputeResource (Lude.Maybe Lude.Text)
crSpotIAMFleetRole = Lens.lens (spotIAMFleetRole :: ComputeResource -> Lude.Maybe Lude.Text) (\s a -> s {spotIAMFleetRole = a} :: ComputeResource)
{-# DEPRECATED crSpotIAMFleetRole "Use generic-lens or generic-optics with 'spotIAMFleetRole' instead." #-}

-- | The Amazon Machine Image (AMI) ID used for instances launched in the compute environment. This parameter is overridden by the @imageIdOverride@ member of the @Ec2Configuration@ structure.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crImageId :: Lens.Lens' ComputeResource (Lude.Maybe Lude.Text)
crImageId = Lens.lens (imageId :: ComputeResource -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: ComputeResource)
{-# DEPRECATED crImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The launch template to use for your compute resources. Any other compute resource parameters that you specify in a 'CreateComputeEnvironment' API operation override the same parameters in the launch template. You must specify either the launch template ID or launch template name in the request, but not both. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Launch Template Support> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crLaunchTemplate :: Lens.Lens' ComputeResource (Lude.Maybe LaunchTemplateSpecification)
crLaunchTemplate = Lens.lens (launchTemplate :: ComputeResource -> Lude.Maybe LaunchTemplateSpecification) (\s a -> s {launchTemplate = a} :: ComputeResource)
{-# DEPRECATED crLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- /Note:/ Consider using 'desiredvCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDesiredvCPUs :: Lens.Lens' ComputeResource (Lude.Maybe Lude.Int)
crDesiredvCPUs = Lens.lens (desiredvCPUs :: ComputeResource -> Lude.Maybe Lude.Int) (\s a -> s {desiredvCPUs = a} :: ComputeResource)
{-# DEPRECATED crDesiredvCPUs "Use generic-lens or generic-optics with 'desiredvCPUs' instead." #-}

-- | The allocation strategy to use for the compute resource in case not enough instances of the best fitting instance type can be allocated. This could be due to availability of the instance type in the region or <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 service limits> . If this is not specified, the default is @BEST_FIT@ , which will use only the best fitting instance type, waiting for additional capacity if it's not available. This allocation strategy keeps costs lower but can limit scaling. If you are using Spot Fleets with @BEST_FIT@ then the Spot Fleet IAM Role must be specified. @BEST_FIT_PROGRESSIVE@ will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types with a lower cost per vCPU. @SPOT_CAPACITY_OPTIMIZED@ is only available for Spot Instance compute resources and will select additional instance types that are large enough to meet the requirements of the jobs in the queue, with a preference for instance types that are less likely to be interrupted. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/allocation-strategies.html Allocation Strategies> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllocationStrategy :: Lens.Lens' ComputeResource (Lude.Maybe CRAllocationStrategy)
crAllocationStrategy = Lens.lens (allocationStrategy :: ComputeResource -> Lude.Maybe CRAllocationStrategy) (\s a -> s {allocationStrategy = a} :: ComputeResource)
{-# DEPRECATED crAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

-- | The Amazon EC2 placement group to associate with your compute resources. If you intend to submit multi-node parallel jobs to your compute environment, you should consider creating a cluster placement group and associate it with your compute resources. This keeps your multi-node parallel job on a logical grouping of instances within a single Availability Zone with high network flow potential. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPlacementGroup :: Lens.Lens' ComputeResource (Lude.Maybe Lude.Text)
crPlacementGroup = Lens.lens (placementGroup :: ComputeResource -> Lude.Maybe Lude.Text) (\s a -> s {placementGroup = a} :: ComputeResource)
{-# DEPRECATED crPlacementGroup "Use generic-lens or generic-optics with 'placementGroup' instead." #-}

-- | Key-value pair tags to be applied to resources that are launched in the compute environment. For AWS Batch, these take the form of "String1": "String2", where String1 is the tag key and String2 is the tag value—for example, { "Name": "AWS Batch Instance - C4OnDemand" }. These tags can not be updated or removed after the compute environment has been created; any changes require creating a new compute environment and removing the old compute environment. These tags are not seen when using the AWS Batch ListTagsForResource API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' ComputeResource (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
crTags = Lens.lens (tags :: ComputeResource -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ComputeResource)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The type of compute environment: @EC2@ or @SPOT@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crType :: Lens.Lens' ComputeResource CRType
crType = Lens.lens (type' :: ComputeResource -> CRType) (\s a -> s {type' = a} :: ComputeResource)
{-# DEPRECATED crType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The minimum number of Amazon EC2 vCPUs that an environment should maintain (even if the compute environment is @DISABLED@ ).
--
-- /Note:/ Consider using 'minvCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMinvCPUs :: Lens.Lens' ComputeResource Lude.Int
crMinvCPUs = Lens.lens (minvCPUs :: ComputeResource -> Lude.Int) (\s a -> s {minvCPUs = a} :: ComputeResource)
{-# DEPRECATED crMinvCPUs "Use generic-lens or generic-optics with 'minvCPUs' instead." #-}

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- /Note:/ Consider using 'maxvCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaxvCPUs :: Lens.Lens' ComputeResource Lude.Int
crMaxvCPUs = Lens.lens (maxvCPUs :: ComputeResource -> Lude.Int) (\s a -> s {maxvCPUs = a} :: ComputeResource)
{-# DEPRECATED crMaxvCPUs "Use generic-lens or generic-optics with 'maxvCPUs' instead." #-}

-- | The instances types that may be launched. You can specify instance families to launch any instance type within those families (for example, @c5@ or @p3@ ), or you can specify specific sizes within a family (such as @c5.8xlarge@ ). You can also choose @optimal@ to pick instance types (from the C, M, and R instance families) on the fly that match the demand of your job queues.
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceTypes :: Lens.Lens' ComputeResource [Lude.Text]
crInstanceTypes = Lens.lens (instanceTypes :: ComputeResource -> [Lude.Text]) (\s a -> s {instanceTypes = a} :: ComputeResource)
{-# DEPRECATED crInstanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead." #-}

-- | The VPC subnets into which the compute resources are launched. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> in the /Amazon VPC User Guide/ .
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSubnets :: Lens.Lens' ComputeResource [Lude.Text]
crSubnets = Lens.lens (subnets :: ComputeResource -> [Lude.Text]) (\s a -> s {subnets = a} :: ComputeResource)
{-# DEPRECATED crSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The Amazon ECS instance profile applied to Amazon EC2 instances in a compute environment. You can specify the short name or full Amazon Resource Name (ARN) of an instance profile. For example, @/ecsInstanceRole/ @ or @arn:aws:iam::/<aws_account_id>/ :instance-profile//ecsInstanceRole/ @ . For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/instance_IAM_role.html Amazon ECS Instance Role> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'instanceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceRole :: Lens.Lens' ComputeResource Lude.Text
crInstanceRole = Lens.lens (instanceRole :: ComputeResource -> Lude.Text) (\s a -> s {instanceRole = a} :: ComputeResource)
{-# DEPRECATED crInstanceRole "Use generic-lens or generic-optics with 'instanceRole' instead." #-}

instance Lude.FromJSON ComputeResource where
  parseJSON =
    Lude.withObject
      "ComputeResource"
      ( \x ->
          ComputeResource'
            Lude.<$> (x Lude..:? "securityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ec2KeyPair")
            Lude.<*> (x Lude..:? "ec2Configuration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "bidPercentage")
            Lude.<*> (x Lude..:? "spotIamFleetRole")
            Lude.<*> (x Lude..:? "imageId")
            Lude.<*> (x Lude..:? "launchTemplate")
            Lude.<*> (x Lude..:? "desiredvCpus")
            Lude.<*> (x Lude..:? "allocationStrategy")
            Lude.<*> (x Lude..:? "placementGroup")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..: "minvCpus")
            Lude.<*> (x Lude..: "maxvCpus")
            Lude.<*> (x Lude..:? "instanceTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "subnets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "instanceRole")
      )

instance Lude.ToJSON ComputeResource where
  toJSON ComputeResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("securityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("ec2KeyPair" Lude..=) Lude.<$> ec2KeyPair,
            ("ec2Configuration" Lude..=) Lude.<$> ec2Configuration,
            ("bidPercentage" Lude..=) Lude.<$> bidPercentage,
            ("spotIamFleetRole" Lude..=) Lude.<$> spotIAMFleetRole,
            ("imageId" Lude..=) Lude.<$> imageId,
            ("launchTemplate" Lude..=) Lude.<$> launchTemplate,
            ("desiredvCpus" Lude..=) Lude.<$> desiredvCPUs,
            ("allocationStrategy" Lude..=) Lude.<$> allocationStrategy,
            ("placementGroup" Lude..=) Lude.<$> placementGroup,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("minvCpus" Lude..= minvCPUs),
            Lude.Just ("maxvCpus" Lude..= maxvCPUs),
            Lude.Just ("instanceTypes" Lude..= instanceTypes),
            Lude.Just ("subnets" Lude..= subnets),
            Lude.Just ("instanceRole" Lude..= instanceRole)
          ]
      )
