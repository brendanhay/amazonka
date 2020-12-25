{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Layer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Layer
  ( Layer (..),

    -- * Smart constructor
    mkLayer,

    -- * Lenses
    lArn,
    lAttributes,
    lAutoAssignElasticIps,
    lAutoAssignPublicIps,
    lCloudWatchLogsConfiguration,
    lCreatedAt,
    lCustomInstanceProfileArn,
    lCustomJson,
    lCustomRecipes,
    lCustomSecurityGroupIds,
    lDefaultRecipes,
    lDefaultSecurityGroupNames,
    lEnableAutoHealing,
    lInstallUpdatesOnBoot,
    lLayerId,
    lLifecycleEventConfiguration,
    lName,
    lPackages,
    lShortname,
    lStackId,
    lType,
    lUseEbsOptimizedInstances,
    lVolumeConfigurations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration as Types
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.OpsWorks.Types.LayerAttributesKeys as Types
import qualified Network.AWS.OpsWorks.Types.LayerType as Types
import qualified Network.AWS.OpsWorks.Types.LifecycleEventConfiguration as Types
import qualified Network.AWS.OpsWorks.Types.Recipes as Types
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.OpsWorks.Types.VolumeConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a layer.
--
-- /See:/ 'mkLayer' smart constructor.
data Layer = Layer'
  { -- | The Amazon Resource Number (ARN) of a layer.
    arn :: Core.Maybe Types.String,
    -- | The layer attributes.
    --
    -- For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value
    -- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
    attributes :: Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text),
    -- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignElasticIps :: Core.Maybe Core.Bool,
    -- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignPublicIps :: Core.Maybe Core.Bool,
    -- | The Amazon CloudWatch Logs configuration settings for the layer.
    cloudWatchLogsConfiguration :: Core.Maybe Types.CloudWatchLogsConfiguration,
    -- | Date when the layer was created.
    createdAt :: Core.Maybe Types.CreatedAt,
    -- | The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    customInstanceProfileArn :: Core.Maybe Types.String,
    -- | A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
    customJson :: Core.Maybe Types.String,
    -- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
    customRecipes :: Core.Maybe Types.Recipes,
    -- | An array containing the layer's custom security group IDs.
    customSecurityGroupIds :: Core.Maybe [Types.String],
    -- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. You can also provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
    --
    -- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the @.rb@ extension. For example: @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the repository's @phpapp2@ folder.
    defaultRecipes :: Core.Maybe Types.Recipes,
    -- | An array containing the layer's security group names.
    defaultSecurityGroupNames :: Core.Maybe [Types.String],
    -- | Whether auto healing is disabled for the layer.
    enableAutoHealing :: Core.Maybe Core.Bool,
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | The layer ID.
    layerId :: Core.Maybe Types.String,
    -- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
    lifecycleEventConfiguration :: Core.Maybe Types.LifecycleEventConfiguration,
    -- | The layer name.
    name :: Core.Maybe Types.String,
    -- | An array of @Package@ objects that describe the layer's packages.
    packages :: Core.Maybe [Types.String],
    -- | The layer short name.
    shortname :: Core.Maybe Types.String,
    -- | The layer stack ID.
    stackId :: Core.Maybe Types.String,
    -- | The layer type.
    type' :: Core.Maybe Types.LayerType,
    -- | Whether the layer uses Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Core.Maybe Core.Bool,
    -- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
    volumeConfigurations :: Core.Maybe [Types.VolumeConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Layer' value with any optional fields omitted.
mkLayer ::
  Layer
mkLayer =
  Layer'
    { arn = Core.Nothing,
      attributes = Core.Nothing,
      autoAssignElasticIps = Core.Nothing,
      autoAssignPublicIps = Core.Nothing,
      cloudWatchLogsConfiguration = Core.Nothing,
      createdAt = Core.Nothing,
      customInstanceProfileArn = Core.Nothing,
      customJson = Core.Nothing,
      customRecipes = Core.Nothing,
      customSecurityGroupIds = Core.Nothing,
      defaultRecipes = Core.Nothing,
      defaultSecurityGroupNames = Core.Nothing,
      enableAutoHealing = Core.Nothing,
      installUpdatesOnBoot = Core.Nothing,
      layerId = Core.Nothing,
      lifecycleEventConfiguration = Core.Nothing,
      name = Core.Nothing,
      packages = Core.Nothing,
      shortname = Core.Nothing,
      stackId = Core.Nothing,
      type' = Core.Nothing,
      useEbsOptimizedInstances = Core.Nothing,
      volumeConfigurations = Core.Nothing
    }

-- | The Amazon Resource Number (ARN) of a layer.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lArn :: Lens.Lens' Layer (Core.Maybe Types.String)
lArn = Lens.field @"arn"
{-# DEPRECATED lArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The layer attributes.
--
-- For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value
-- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAttributes :: Lens.Lens' Layer (Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text))
lAttributes = Lens.field @"attributes"
{-# DEPRECATED lAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAutoAssignElasticIps :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lAutoAssignElasticIps = Lens.field @"autoAssignElasticIps"
{-# DEPRECATED lAutoAssignElasticIps "Use generic-lens or generic-optics with 'autoAssignElasticIps' instead." #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAutoAssignPublicIps :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lAutoAssignPublicIps = Lens.field @"autoAssignPublicIps"
{-# DEPRECATED lAutoAssignPublicIps "Use generic-lens or generic-optics with 'autoAssignPublicIps' instead." #-}

-- | The Amazon CloudWatch Logs configuration settings for the layer.
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCloudWatchLogsConfiguration :: Lens.Lens' Layer (Core.Maybe Types.CloudWatchLogsConfiguration)
lCloudWatchLogsConfiguration = Lens.field @"cloudWatchLogsConfiguration"
{-# DEPRECATED lCloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead." #-}

-- | Date when the layer was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCreatedAt :: Lens.Lens' Layer (Core.Maybe Types.CreatedAt)
lCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED lCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomInstanceProfileArn :: Lens.Lens' Layer (Core.Maybe Types.String)
lCustomInstanceProfileArn = Lens.field @"customInstanceProfileArn"
{-# DEPRECATED lCustomInstanceProfileArn "Use generic-lens or generic-optics with 'customInstanceProfileArn' instead." #-}

-- | A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomJson :: Lens.Lens' Layer (Core.Maybe Types.String)
lCustomJson = Lens.field @"customJson"
{-# DEPRECATED lCustomJson "Use generic-lens or generic-optics with 'customJson' instead." #-}

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomRecipes :: Lens.Lens' Layer (Core.Maybe Types.Recipes)
lCustomRecipes = Lens.field @"customRecipes"
{-# DEPRECATED lCustomRecipes "Use generic-lens or generic-optics with 'customRecipes' instead." #-}

-- | An array containing the layer's custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomSecurityGroupIds :: Lens.Lens' Layer (Core.Maybe [Types.String])
lCustomSecurityGroupIds = Lens.field @"customSecurityGroupIds"
{-# DEPRECATED lCustomSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead." #-}

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. You can also provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the @.rb@ extension. For example: @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the repository's @phpapp2@ folder.
--
-- /Note:/ Consider using 'defaultRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultRecipes :: Lens.Lens' Layer (Core.Maybe Types.Recipes)
lDefaultRecipes = Lens.field @"defaultRecipes"
{-# DEPRECATED lDefaultRecipes "Use generic-lens or generic-optics with 'defaultRecipes' instead." #-}

-- | An array containing the layer's security group names.
--
-- /Note:/ Consider using 'defaultSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultSecurityGroupNames :: Lens.Lens' Layer (Core.Maybe [Types.String])
lDefaultSecurityGroupNames = Lens.field @"defaultSecurityGroupNames"
{-# DEPRECATED lDefaultSecurityGroupNames "Use generic-lens or generic-optics with 'defaultSecurityGroupNames' instead." #-}

-- | Whether auto healing is disabled for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lEnableAutoHealing :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lEnableAutoHealing = Lens.field @"enableAutoHealing"
{-# DEPRECATED lEnableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lInstallUpdatesOnBoot :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# DEPRECATED lInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerId :: Lens.Lens' Layer (Core.Maybe Types.String)
lLayerId = Lens.field @"layerId"
{-# DEPRECATED lLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLifecycleEventConfiguration :: Lens.Lens' Layer (Core.Maybe Types.LifecycleEventConfiguration)
lLifecycleEventConfiguration = Lens.field @"lifecycleEventConfiguration"
{-# DEPRECATED lLifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead." #-}

-- | The layer name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Layer (Core.Maybe Types.String)
lName = Lens.field @"name"
{-# DEPRECATED lName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An array of @Package@ objects that describe the layer's packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPackages :: Lens.Lens' Layer (Core.Maybe [Types.String])
lPackages = Lens.field @"packages"
{-# DEPRECATED lPackages "Use generic-lens or generic-optics with 'packages' instead." #-}

-- | The layer short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lShortname :: Lens.Lens' Layer (Core.Maybe Types.String)
lShortname = Lens.field @"shortname"
{-# DEPRECATED lShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | The layer stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lStackId :: Lens.Lens' Layer (Core.Maybe Types.String)
lStackId = Lens.field @"stackId"
{-# DEPRECATED lStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The layer type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' Layer (Core.Maybe Types.LayerType)
lType = Lens.field @"type'"
{-# DEPRECATED lType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Whether the layer uses Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEbsOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lUseEbsOptimizedInstances :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lUseEbsOptimizedInstances = Lens.field @"useEbsOptimizedInstances"
{-# DEPRECATED lUseEbsOptimizedInstances "Use generic-lens or generic-optics with 'useEbsOptimizedInstances' instead." #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lVolumeConfigurations :: Lens.Lens' Layer (Core.Maybe [Types.VolumeConfiguration])
lVolumeConfigurations = Lens.field @"volumeConfigurations"
{-# DEPRECATED lVolumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead." #-}

instance Core.FromJSON Layer where
  parseJSON =
    Core.withObject "Layer" Core.$
      \x ->
        Layer'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Attributes")
          Core.<*> (x Core..:? "AutoAssignElasticIps")
          Core.<*> (x Core..:? "AutoAssignPublicIps")
          Core.<*> (x Core..:? "CloudWatchLogsConfiguration")
          Core.<*> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "CustomInstanceProfileArn")
          Core.<*> (x Core..:? "CustomJson")
          Core.<*> (x Core..:? "CustomRecipes")
          Core.<*> (x Core..:? "CustomSecurityGroupIds")
          Core.<*> (x Core..:? "DefaultRecipes")
          Core.<*> (x Core..:? "DefaultSecurityGroupNames")
          Core.<*> (x Core..:? "EnableAutoHealing")
          Core.<*> (x Core..:? "InstallUpdatesOnBoot")
          Core.<*> (x Core..:? "LayerId")
          Core.<*> (x Core..:? "LifecycleEventConfiguration")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Packages")
          Core.<*> (x Core..:? "Shortname")
          Core.<*> (x Core..:? "StackId")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "UseEbsOptimizedInstances")
          Core.<*> (x Core..:? "VolumeConfigurations")
