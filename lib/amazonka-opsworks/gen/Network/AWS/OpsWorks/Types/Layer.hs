{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Layer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Layer
  ( Layer (..)
  -- * Smart constructor
  , mkLayer
  -- * Lenses
  , lArn
  , lAttributes
  , lAutoAssignElasticIps
  , lAutoAssignPublicIps
  , lCloudWatchLogsConfiguration
  , lCreatedAt
  , lCustomInstanceProfileArn
  , lCustomJson
  , lCustomRecipes
  , lCustomSecurityGroupIds
  , lDefaultRecipes
  , lDefaultSecurityGroupNames
  , lEnableAutoHealing
  , lInstallUpdatesOnBoot
  , lLayerId
  , lLifecycleEventConfiguration
  , lName
  , lPackages
  , lShortname
  , lStackId
  , lType
  , lUseEbsOptimizedInstances
  , lVolumeConfigurations
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration as Types
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.OpsWorks.Types.LayerAttributesKeys as Types
import qualified Network.AWS.OpsWorks.Types.LayerType as Types
import qualified Network.AWS.OpsWorks.Types.LifecycleEventConfiguration as Types
import qualified Network.AWS.OpsWorks.Types.Recipes as Types
import qualified Network.AWS.OpsWorks.Types.VolumeConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a layer.
--
-- /See:/ 'mkLayer' smart constructor.
data Layer = Layer'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Number (ARN) of a layer.
  , attributes :: Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text)
    -- ^ The layer attributes.
--
-- For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value
-- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
  , autoAssignElasticIps :: Core.Maybe Core.Bool
    -- ^ Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
  , autoAssignPublicIps :: Core.Maybe Core.Bool
    -- ^ For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
  , cloudWatchLogsConfiguration :: Core.Maybe Types.CloudWatchLogsConfiguration
    -- ^ The Amazon CloudWatch Logs configuration settings for the layer.
  , createdAt :: Core.Maybe Types.CreatedAt
    -- ^ Date when the layer was created.
  , customInstanceProfileArn :: Core.Maybe Core.Text
    -- ^ The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
  , customJson :: Core.Maybe Core.Text
    -- ^ A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
  , customRecipes :: Core.Maybe Types.Recipes
    -- ^ A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
  , customSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ An array containing the layer's custom security group IDs.
  , defaultRecipes :: Core.Maybe Types.Recipes
    -- ^ AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. You can also provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the @.rb@ extension. For example: @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the repository's @phpapp2@ folder.
  , defaultSecurityGroupNames :: Core.Maybe [Core.Text]
    -- ^ An array containing the layer's security group names.
  , enableAutoHealing :: Core.Maybe Core.Bool
    -- ^ Whether auto healing is disabled for the layer.
  , installUpdatesOnBoot :: Core.Maybe Core.Bool
    -- ^ Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances. 
  , layerId :: Core.Maybe Core.Text
    -- ^ The layer ID.
  , lifecycleEventConfiguration :: Core.Maybe Types.LifecycleEventConfiguration
    -- ^ A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
  , name :: Core.Maybe Core.Text
    -- ^ The layer name.
  , packages :: Core.Maybe [Core.Text]
    -- ^ An array of @Package@ objects that describe the layer's packages.
  , shortname :: Core.Maybe Core.Text
    -- ^ The layer short name.
  , stackId :: Core.Maybe Core.Text
    -- ^ The layer stack ID.
  , type' :: Core.Maybe Types.LayerType
    -- ^ The layer type.
  , useEbsOptimizedInstances :: Core.Maybe Core.Bool
    -- ^ Whether the layer uses Amazon EBS-optimized instances.
  , volumeConfigurations :: Core.Maybe [Types.VolumeConfiguration]
    -- ^ A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Layer' value with any optional fields omitted.
mkLayer
    :: Layer
mkLayer
  = Layer'{arn = Core.Nothing, attributes = Core.Nothing,
           autoAssignElasticIps = Core.Nothing,
           autoAssignPublicIps = Core.Nothing,
           cloudWatchLogsConfiguration = Core.Nothing,
           createdAt = Core.Nothing, customInstanceProfileArn = Core.Nothing,
           customJson = Core.Nothing, customRecipes = Core.Nothing,
           customSecurityGroupIds = Core.Nothing,
           defaultRecipes = Core.Nothing,
           defaultSecurityGroupNames = Core.Nothing,
           enableAutoHealing = Core.Nothing,
           installUpdatesOnBoot = Core.Nothing, layerId = Core.Nothing,
           lifecycleEventConfiguration = Core.Nothing, name = Core.Nothing,
           packages = Core.Nothing, shortname = Core.Nothing,
           stackId = Core.Nothing, type' = Core.Nothing,
           useEbsOptimizedInstances = Core.Nothing,
           volumeConfigurations = Core.Nothing}

-- | The Amazon Resource Number (ARN) of a layer.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lArn :: Lens.Lens' Layer (Core.Maybe Core.Text)
lArn = Lens.field @"arn"
{-# INLINEABLE lArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The layer attributes.
--
-- For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value
-- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAttributes :: Lens.Lens' Layer (Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text))
lAttributes = Lens.field @"attributes"
{-# INLINEABLE lAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAutoAssignElasticIps :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lAutoAssignElasticIps = Lens.field @"autoAssignElasticIps"
{-# INLINEABLE lAutoAssignElasticIps #-}
{-# DEPRECATED autoAssignElasticIps "Use generic-lens or generic-optics with 'autoAssignElasticIps' instead"  #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAutoAssignPublicIps :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lAutoAssignPublicIps = Lens.field @"autoAssignPublicIps"
{-# INLINEABLE lAutoAssignPublicIps #-}
{-# DEPRECATED autoAssignPublicIps "Use generic-lens or generic-optics with 'autoAssignPublicIps' instead"  #-}

-- | The Amazon CloudWatch Logs configuration settings for the layer.
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCloudWatchLogsConfiguration :: Lens.Lens' Layer (Core.Maybe Types.CloudWatchLogsConfiguration)
lCloudWatchLogsConfiguration = Lens.field @"cloudWatchLogsConfiguration"
{-# INLINEABLE lCloudWatchLogsConfiguration #-}
{-# DEPRECATED cloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead"  #-}

-- | Date when the layer was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCreatedAt :: Lens.Lens' Layer (Core.Maybe Types.CreatedAt)
lCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE lCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomInstanceProfileArn :: Lens.Lens' Layer (Core.Maybe Core.Text)
lCustomInstanceProfileArn = Lens.field @"customInstanceProfileArn"
{-# INLINEABLE lCustomInstanceProfileArn #-}
{-# DEPRECATED customInstanceProfileArn "Use generic-lens or generic-optics with 'customInstanceProfileArn' instead"  #-}

-- | A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomJson :: Lens.Lens' Layer (Core.Maybe Core.Text)
lCustomJson = Lens.field @"customJson"
{-# INLINEABLE lCustomJson #-}
{-# DEPRECATED customJson "Use generic-lens or generic-optics with 'customJson' instead"  #-}

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomRecipes :: Lens.Lens' Layer (Core.Maybe Types.Recipes)
lCustomRecipes = Lens.field @"customRecipes"
{-# INLINEABLE lCustomRecipes #-}
{-# DEPRECATED customRecipes "Use generic-lens or generic-optics with 'customRecipes' instead"  #-}

-- | An array containing the layer's custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomSecurityGroupIds :: Lens.Lens' Layer (Core.Maybe [Core.Text])
lCustomSecurityGroupIds = Lens.field @"customSecurityGroupIds"
{-# INLINEABLE lCustomSecurityGroupIds #-}
{-# DEPRECATED customSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead"  #-}

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. You can also provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the @.rb@ extension. For example: @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the repository's @phpapp2@ folder.
--
-- /Note:/ Consider using 'defaultRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultRecipes :: Lens.Lens' Layer (Core.Maybe Types.Recipes)
lDefaultRecipes = Lens.field @"defaultRecipes"
{-# INLINEABLE lDefaultRecipes #-}
{-# DEPRECATED defaultRecipes "Use generic-lens or generic-optics with 'defaultRecipes' instead"  #-}

-- | An array containing the layer's security group names.
--
-- /Note:/ Consider using 'defaultSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultSecurityGroupNames :: Lens.Lens' Layer (Core.Maybe [Core.Text])
lDefaultSecurityGroupNames = Lens.field @"defaultSecurityGroupNames"
{-# INLINEABLE lDefaultSecurityGroupNames #-}
{-# DEPRECATED defaultSecurityGroupNames "Use generic-lens or generic-optics with 'defaultSecurityGroupNames' instead"  #-}

-- | Whether auto healing is disabled for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lEnableAutoHealing :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lEnableAutoHealing = Lens.field @"enableAutoHealing"
{-# INLINEABLE lEnableAutoHealing #-}
{-# DEPRECATED enableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead"  #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances. 
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lInstallUpdatesOnBoot :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# INLINEABLE lInstallUpdatesOnBoot #-}
{-# DEPRECATED installUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead"  #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerId :: Lens.Lens' Layer (Core.Maybe Core.Text)
lLayerId = Lens.field @"layerId"
{-# INLINEABLE lLayerId #-}
{-# DEPRECATED layerId "Use generic-lens or generic-optics with 'layerId' instead"  #-}

-- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLifecycleEventConfiguration :: Lens.Lens' Layer (Core.Maybe Types.LifecycleEventConfiguration)
lLifecycleEventConfiguration = Lens.field @"lifecycleEventConfiguration"
{-# INLINEABLE lLifecycleEventConfiguration #-}
{-# DEPRECATED lifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead"  #-}

-- | The layer name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Layer (Core.Maybe Core.Text)
lName = Lens.field @"name"
{-# INLINEABLE lName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An array of @Package@ objects that describe the layer's packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPackages :: Lens.Lens' Layer (Core.Maybe [Core.Text])
lPackages = Lens.field @"packages"
{-# INLINEABLE lPackages #-}
{-# DEPRECATED packages "Use generic-lens or generic-optics with 'packages' instead"  #-}

-- | The layer short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lShortname :: Lens.Lens' Layer (Core.Maybe Core.Text)
lShortname = Lens.field @"shortname"
{-# INLINEABLE lShortname #-}
{-# DEPRECATED shortname "Use generic-lens or generic-optics with 'shortname' instead"  #-}

-- | The layer stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lStackId :: Lens.Lens' Layer (Core.Maybe Core.Text)
lStackId = Lens.field @"stackId"
{-# INLINEABLE lStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The layer type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' Layer (Core.Maybe Types.LayerType)
lType = Lens.field @"type'"
{-# INLINEABLE lType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Whether the layer uses Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEbsOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lUseEbsOptimizedInstances :: Lens.Lens' Layer (Core.Maybe Core.Bool)
lUseEbsOptimizedInstances = Lens.field @"useEbsOptimizedInstances"
{-# INLINEABLE lUseEbsOptimizedInstances #-}
{-# DEPRECATED useEbsOptimizedInstances "Use generic-lens or generic-optics with 'useEbsOptimizedInstances' instead"  #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lVolumeConfigurations :: Lens.Lens' Layer (Core.Maybe [Types.VolumeConfiguration])
lVolumeConfigurations = Lens.field @"volumeConfigurations"
{-# INLINEABLE lVolumeConfigurations #-}
{-# DEPRECATED volumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead"  #-}

instance Core.FromJSON Layer where
        parseJSON
          = Core.withObject "Layer" Core.$
              \ x ->
                Layer' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Attributes" Core.<*>
                    x Core..:? "AutoAssignElasticIps"
                    Core.<*> x Core..:? "AutoAssignPublicIps"
                    Core.<*> x Core..:? "CloudWatchLogsConfiguration"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "CustomInstanceProfileArn"
                    Core.<*> x Core..:? "CustomJson"
                    Core.<*> x Core..:? "CustomRecipes"
                    Core.<*> x Core..:? "CustomSecurityGroupIds"
                    Core.<*> x Core..:? "DefaultRecipes"
                    Core.<*> x Core..:? "DefaultSecurityGroupNames"
                    Core.<*> x Core..:? "EnableAutoHealing"
                    Core.<*> x Core..:? "InstallUpdatesOnBoot"
                    Core.<*> x Core..:? "LayerId"
                    Core.<*> x Core..:? "LifecycleEventConfiguration"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Packages"
                    Core.<*> x Core..:? "Shortname"
                    Core.<*> x Core..:? "StackId"
                    Core.<*> x Core..:? "Type"
                    Core.<*> x Core..:? "UseEbsOptimizedInstances"
                    Core.<*> x Core..:? "VolumeConfigurations"
