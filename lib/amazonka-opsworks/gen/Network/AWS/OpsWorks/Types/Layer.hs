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
    lCustomInstanceProfileARN,
    lCustomSecurityGroupIds,
    lInstallUpdatesOnBoot,
    lCloudWatchLogsConfiguration,
    lLifecycleEventConfiguration,
    lARN,
    lCreatedAt,
    lShortname,
    lDefaultRecipes,
    lCustomRecipes,
    lCustomJSON,
    lVolumeConfigurations,
    lEnableAutoHealing,
    lPackages,
    lAttributes,
    lName,
    lAutoAssignPublicIPs,
    lType,
    lUseEBSOptimizedInstances,
    lStackId,
    lLayerId,
    lDefaultSecurityGroupNames,
    lAutoAssignElasticIPs,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
import Network.AWS.OpsWorks.Types.LayerAttributesKeys
import Network.AWS.OpsWorks.Types.LayerType
import Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
import Network.AWS.OpsWorks.Types.Recipes
import Network.AWS.OpsWorks.Types.VolumeConfiguration
import qualified Network.AWS.Prelude as Lude

-- | Describes a layer.
--
-- /See:/ 'mkLayer' smart constructor.
data Layer = Layer'
  { customInstanceProfileARN ::
      Lude.Maybe Lude.Text,
    customSecurityGroupIds :: Lude.Maybe [Lude.Text],
    installUpdatesOnBoot :: Lude.Maybe Lude.Bool,
    cloudWatchLogsConfiguration ::
      Lude.Maybe CloudWatchLogsConfiguration,
    lifecycleEventConfiguration ::
      Lude.Maybe LifecycleEventConfiguration,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    shortname :: Lude.Maybe Lude.Text,
    defaultRecipes :: Lude.Maybe Recipes,
    customRecipes :: Lude.Maybe Recipes,
    customJSON :: Lude.Maybe Lude.Text,
    volumeConfigurations :: Lude.Maybe [VolumeConfiguration],
    enableAutoHealing :: Lude.Maybe Lude.Bool,
    packages :: Lude.Maybe [Lude.Text],
    attributes ::
      Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text)),
    name :: Lude.Maybe Lude.Text,
    autoAssignPublicIPs :: Lude.Maybe Lude.Bool,
    type' :: Lude.Maybe LayerType,
    useEBSOptimizedInstances :: Lude.Maybe Lude.Bool,
    stackId :: Lude.Maybe Lude.Text,
    layerId :: Lude.Maybe Lude.Text,
    defaultSecurityGroupNames :: Lude.Maybe [Lude.Text],
    autoAssignElasticIPs :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Number (ARN) of a layer.
-- * 'attributes' - The layer attributes.
--
-- For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value
-- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
-- * 'autoAssignElasticIPs' - Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
-- * 'autoAssignPublicIPs' - For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
-- * 'cloudWatchLogsConfiguration' - The Amazon CloudWatch Logs configuration settings for the layer.
-- * 'createdAt' - Date when the layer was created.
-- * 'customInstanceProfileARN' - The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'customJSON' - A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
-- * 'customRecipes' - A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
-- * 'customSecurityGroupIds' - An array containing the layer's custom security group IDs.
-- * 'defaultRecipes' - AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. You can also provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the @.rb@ extension. For example: @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the repository's @phpapp2@ folder.
-- * 'defaultSecurityGroupNames' - An array containing the layer's security group names.
-- * 'enableAutoHealing' - Whether auto healing is disabled for the layer.
-- * 'installUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
-- * 'layerId' - The layer ID.
-- * 'lifecycleEventConfiguration' - A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
-- * 'name' - The layer name.
-- * 'packages' - An array of @Package@ objects that describe the layer's packages.
-- * 'shortname' - The layer short name.
-- * 'stackId' - The layer stack ID.
-- * 'type'' - The layer type.
-- * 'useEBSOptimizedInstances' - Whether the layer uses Amazon EBS-optimized instances.
-- * 'volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
mkLayer ::
  Layer
mkLayer =
  Layer'
    { customInstanceProfileARN = Lude.Nothing,
      customSecurityGroupIds = Lude.Nothing,
      installUpdatesOnBoot = Lude.Nothing,
      cloudWatchLogsConfiguration = Lude.Nothing,
      lifecycleEventConfiguration = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      shortname = Lude.Nothing,
      defaultRecipes = Lude.Nothing,
      customRecipes = Lude.Nothing,
      customJSON = Lude.Nothing,
      volumeConfigurations = Lude.Nothing,
      enableAutoHealing = Lude.Nothing,
      packages = Lude.Nothing,
      attributes = Lude.Nothing,
      name = Lude.Nothing,
      autoAssignPublicIPs = Lude.Nothing,
      type' = Lude.Nothing,
      useEBSOptimizedInstances = Lude.Nothing,
      stackId = Lude.Nothing,
      layerId = Lude.Nothing,
      defaultSecurityGroupNames = Lude.Nothing,
      autoAssignElasticIPs = Lude.Nothing
    }

-- | The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomInstanceProfileARN :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lCustomInstanceProfileARN = Lens.lens (customInstanceProfileARN :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {customInstanceProfileARN = a} :: Layer)
{-# DEPRECATED lCustomInstanceProfileARN "Use generic-lens or generic-optics with 'customInstanceProfileARN' instead." #-}

-- | An array containing the layer's custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomSecurityGroupIds :: Lens.Lens' Layer (Lude.Maybe [Lude.Text])
lCustomSecurityGroupIds = Lens.lens (customSecurityGroupIds :: Layer -> Lude.Maybe [Lude.Text]) (\s a -> s {customSecurityGroupIds = a} :: Layer)
{-# DEPRECATED lCustomSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lInstallUpdatesOnBoot :: Lens.Lens' Layer (Lude.Maybe Lude.Bool)
lInstallUpdatesOnBoot = Lens.lens (installUpdatesOnBoot :: Layer -> Lude.Maybe Lude.Bool) (\s a -> s {installUpdatesOnBoot = a} :: Layer)
{-# DEPRECATED lInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | The Amazon CloudWatch Logs configuration settings for the layer.
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCloudWatchLogsConfiguration :: Lens.Lens' Layer (Lude.Maybe CloudWatchLogsConfiguration)
lCloudWatchLogsConfiguration = Lens.lens (cloudWatchLogsConfiguration :: Layer -> Lude.Maybe CloudWatchLogsConfiguration) (\s a -> s {cloudWatchLogsConfiguration = a} :: Layer)
{-# DEPRECATED lCloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead." #-}

-- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLifecycleEventConfiguration :: Lens.Lens' Layer (Lude.Maybe LifecycleEventConfiguration)
lLifecycleEventConfiguration = Lens.lens (lifecycleEventConfiguration :: Layer -> Lude.Maybe LifecycleEventConfiguration) (\s a -> s {lifecycleEventConfiguration = a} :: Layer)
{-# DEPRECATED lLifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead." #-}

-- | The Amazon Resource Number (ARN) of a layer.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lARN :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lARN = Lens.lens (arn :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Layer)
{-# DEPRECATED lARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Date when the layer was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCreatedAt :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lCreatedAt = Lens.lens (createdAt :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: Layer)
{-# DEPRECATED lCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The layer short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lShortname :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lShortname = Lens.lens (shortname :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {shortname = a} :: Layer)
{-# DEPRECATED lShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. You can also provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the @.rb@ extension. For example: @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the repository's @phpapp2@ folder.
--
-- /Note:/ Consider using 'defaultRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultRecipes :: Lens.Lens' Layer (Lude.Maybe Recipes)
lDefaultRecipes = Lens.lens (defaultRecipes :: Layer -> Lude.Maybe Recipes) (\s a -> s {defaultRecipes = a} :: Layer)
{-# DEPRECATED lDefaultRecipes "Use generic-lens or generic-optics with 'defaultRecipes' instead." #-}

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomRecipes :: Lens.Lens' Layer (Lude.Maybe Recipes)
lCustomRecipes = Lens.lens (customRecipes :: Layer -> Lude.Maybe Recipes) (\s a -> s {customRecipes = a} :: Layer)
{-# DEPRECATED lCustomRecipes "Use generic-lens or generic-optics with 'customRecipes' instead." #-}

-- | A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCustomJSON :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lCustomJSON = Lens.lens (customJSON :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: Layer)
{-# DEPRECATED lCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lVolumeConfigurations :: Lens.Lens' Layer (Lude.Maybe [VolumeConfiguration])
lVolumeConfigurations = Lens.lens (volumeConfigurations :: Layer -> Lude.Maybe [VolumeConfiguration]) (\s a -> s {volumeConfigurations = a} :: Layer)
{-# DEPRECATED lVolumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead." #-}

-- | Whether auto healing is disabled for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lEnableAutoHealing :: Lens.Lens' Layer (Lude.Maybe Lude.Bool)
lEnableAutoHealing = Lens.lens (enableAutoHealing :: Layer -> Lude.Maybe Lude.Bool) (\s a -> s {enableAutoHealing = a} :: Layer)
{-# DEPRECATED lEnableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead." #-}

-- | An array of @Package@ objects that describe the layer's packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPackages :: Lens.Lens' Layer (Lude.Maybe [Lude.Text])
lPackages = Lens.lens (packages :: Layer -> Lude.Maybe [Lude.Text]) (\s a -> s {packages = a} :: Layer)
{-# DEPRECATED lPackages "Use generic-lens or generic-optics with 'packages' instead." #-}

-- | The layer attributes.
--
-- For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value
-- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAttributes :: Lens.Lens' Layer (Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text)))
lAttributes = Lens.lens (attributes :: Layer -> Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: Layer)
{-# DEPRECATED lAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The layer name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lName = Lens.lens (name :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Layer)
{-# DEPRECATED lName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAutoAssignPublicIPs :: Lens.Lens' Layer (Lude.Maybe Lude.Bool)
lAutoAssignPublicIPs = Lens.lens (autoAssignPublicIPs :: Layer -> Lude.Maybe Lude.Bool) (\s a -> s {autoAssignPublicIPs = a} :: Layer)
{-# DEPRECATED lAutoAssignPublicIPs "Use generic-lens or generic-optics with 'autoAssignPublicIPs' instead." #-}

-- | The layer type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' Layer (Lude.Maybe LayerType)
lType = Lens.lens (type' :: Layer -> Lude.Maybe LayerType) (\s a -> s {type' = a} :: Layer)
{-# DEPRECATED lType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Whether the layer uses Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEBSOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lUseEBSOptimizedInstances :: Lens.Lens' Layer (Lude.Maybe Lude.Bool)
lUseEBSOptimizedInstances = Lens.lens (useEBSOptimizedInstances :: Layer -> Lude.Maybe Lude.Bool) (\s a -> s {useEBSOptimizedInstances = a} :: Layer)
{-# DEPRECATED lUseEBSOptimizedInstances "Use generic-lens or generic-optics with 'useEBSOptimizedInstances' instead." #-}

-- | The layer stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lStackId :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lStackId = Lens.lens (stackId :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Layer)
{-# DEPRECATED lStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLayerId :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lLayerId = Lens.lens (layerId :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {layerId = a} :: Layer)
{-# DEPRECATED lLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | An array containing the layer's security group names.
--
-- /Note:/ Consider using 'defaultSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDefaultSecurityGroupNames :: Lens.Lens' Layer (Lude.Maybe [Lude.Text])
lDefaultSecurityGroupNames = Lens.lens (defaultSecurityGroupNames :: Layer -> Lude.Maybe [Lude.Text]) (\s a -> s {defaultSecurityGroupNames = a} :: Layer)
{-# DEPRECATED lDefaultSecurityGroupNames "Use generic-lens or generic-optics with 'defaultSecurityGroupNames' instead." #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAutoAssignElasticIPs :: Lens.Lens' Layer (Lude.Maybe Lude.Bool)
lAutoAssignElasticIPs = Lens.lens (autoAssignElasticIPs :: Layer -> Lude.Maybe Lude.Bool) (\s a -> s {autoAssignElasticIPs = a} :: Layer)
{-# DEPRECATED lAutoAssignElasticIPs "Use generic-lens or generic-optics with 'autoAssignElasticIPs' instead." #-}

instance Lude.FromJSON Layer where
  parseJSON =
    Lude.withObject
      "Layer"
      ( \x ->
          Layer'
            Lude.<$> (x Lude..:? "CustomInstanceProfileArn")
            Lude.<*> (x Lude..:? "CustomSecurityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstallUpdatesOnBoot")
            Lude.<*> (x Lude..:? "CloudWatchLogsConfiguration")
            Lude.<*> (x Lude..:? "LifecycleEventConfiguration")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "Shortname")
            Lude.<*> (x Lude..:? "DefaultRecipes")
            Lude.<*> (x Lude..:? "CustomRecipes")
            Lude.<*> (x Lude..:? "CustomJson")
            Lude.<*> (x Lude..:? "VolumeConfigurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EnableAutoHealing")
            Lude.<*> (x Lude..:? "Packages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "AutoAssignPublicIps")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "UseEbsOptimizedInstances")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "LayerId")
            Lude.<*> (x Lude..:? "DefaultSecurityGroupNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AutoAssignElasticIps")
      )
