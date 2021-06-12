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
-- Module      : Network.AWS.OpsWorks.Types.Layer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Layer where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
import Network.AWS.OpsWorks.Types.LayerAttributesKeys
import Network.AWS.OpsWorks.Types.LayerType
import Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
import Network.AWS.OpsWorks.Types.Recipes
import Network.AWS.OpsWorks.Types.VolumeConfiguration

-- | Describes a layer.
--
-- /See:/ 'newLayer' smart constructor.
data Layer = Layer'
  { -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. If this value is set to
    -- @false@, you must then update your instances manually by using
    -- CreateDeployment to run the @update_dependencies@ stack command or
    -- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
    -- instances.
    --
    -- We strongly recommend using the default value of @true@, to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | The ARN of the default IAM profile to be used for the layer\'s EC2
    -- instances. For more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    customInstanceProfileArn :: Core.Maybe Core.Text,
    -- | An array containing the layer\'s custom security group IDs.
    customSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | An array of @Package@ objects that describe the layer\'s packages.
    packages :: Core.Maybe [Core.Text],
    -- | Whether auto healing is disabled for the layer.
    enableAutoHealing :: Core.Maybe Core.Bool,
    -- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
    -- volumes.
    volumeConfigurations :: Core.Maybe [VolumeConfiguration],
    -- | The layer stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | A JSON formatted string containing the layer\'s custom stack
    -- configuration and deployment attributes.
    customJson :: Core.Maybe Core.Text,
    -- | AWS OpsWorks Stacks supports five lifecycle events: __setup__,
    -- __configuration__, __deploy__, __undeploy__, and __shutdown__. For each
    -- layer, AWS OpsWorks Stacks runs a set of standard recipes for each
    -- event. You can also provide custom recipes for any or all layers and
    -- events. AWS OpsWorks Stacks runs custom event recipes after the standard
    -- recipes. @LayerCustomRecipes@ specifies the custom recipes for a
    -- particular layer to be run in response to each of the five events.
    --
    -- To specify a recipe, use the cookbook\'s directory name in the
    -- repository followed by two colons and the recipe name, which is the
    -- recipe\'s file name without the @.rb@ extension. For example:
    -- @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the
    -- repository\'s @phpapp2@ folder.
    defaultRecipes :: Core.Maybe Recipes,
    -- | The Amazon Resource Number (ARN) of a layer.
    arn :: Core.Maybe Core.Text,
    -- | The layer short name.
    shortname :: Core.Maybe Core.Text,
    -- | Date when the layer was created.
    createdAt :: Core.Maybe Core.Text,
    -- | The layer attributes.
    --
    -- For the @HaproxyStatsPassword@, @MysqlRootPassword@, and
    -- @GangliaPassword@ attributes, AWS OpsWorks Stacks returns
    -- @*****FILTERED*****@ instead of the actual value
    --
    -- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@
    -- attribute is set to the cluster\'s ARN.
    attributes :: Core.Maybe (Core.HashMap LayerAttributesKeys (Core.Maybe Core.Text)),
    -- | The layer name.
    name :: Core.Maybe Core.Text,
    -- | The Amazon CloudWatch Logs configuration settings for the layer.
    cloudWatchLogsConfiguration :: Core.Maybe CloudWatchLogsConfiguration,
    -- | Whether to automatically assign an
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignElasticIps :: Core.Maybe Core.Bool,
    -- | The layer ID.
    layerId :: Core.Maybe Core.Text,
    -- | An array containing the layer\'s security group names.
    defaultSecurityGroupNames :: Core.Maybe [Core.Text],
    -- | The layer type.
    type' :: Core.Maybe LayerType,
    -- | Whether the layer uses Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Core.Maybe Core.Bool,
    -- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
    -- recipes.
    customRecipes :: Core.Maybe Recipes,
    -- | For stacks that are running in a VPC, whether to automatically assign a
    -- public IP address to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignPublicIps :: Core.Maybe Core.Bool,
    -- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event
    -- configuration.
    lifecycleEventConfiguration :: Core.Maybe LifecycleEventConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Layer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'installUpdatesOnBoot', 'layer_installUpdatesOnBoot' - Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
--
-- 'customInstanceProfileArn', 'layer_customInstanceProfileArn' - The ARN of the default IAM profile to be used for the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'customSecurityGroupIds', 'layer_customSecurityGroupIds' - An array containing the layer\'s custom security group IDs.
--
-- 'packages', 'layer_packages' - An array of @Package@ objects that describe the layer\'s packages.
--
-- 'enableAutoHealing', 'layer_enableAutoHealing' - Whether auto healing is disabled for the layer.
--
-- 'volumeConfigurations', 'layer_volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
--
-- 'stackId', 'layer_stackId' - The layer stack ID.
--
-- 'customJson', 'layer_customJson' - A JSON formatted string containing the layer\'s custom stack
-- configuration and deployment attributes.
--
-- 'defaultRecipes', 'layer_defaultRecipes' - AWS OpsWorks Stacks supports five lifecycle events: __setup__,
-- __configuration__, __deploy__, __undeploy__, and __shutdown__. For each
-- layer, AWS OpsWorks Stacks runs a set of standard recipes for each
-- event. You can also provide custom recipes for any or all layers and
-- events. AWS OpsWorks Stacks runs custom event recipes after the standard
-- recipes. @LayerCustomRecipes@ specifies the custom recipes for a
-- particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook\'s directory name in the
-- repository followed by two colons and the recipe name, which is the
-- recipe\'s file name without the @.rb@ extension. For example:
-- @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the
-- repository\'s @phpapp2@ folder.
--
-- 'arn', 'layer_arn' - The Amazon Resource Number (ARN) of a layer.
--
-- 'shortname', 'layer_shortname' - The layer short name.
--
-- 'createdAt', 'layer_createdAt' - Date when the layer was created.
--
-- 'attributes', 'layer_attributes' - The layer attributes.
--
-- For the @HaproxyStatsPassword@, @MysqlRootPassword@, and
-- @GangliaPassword@ attributes, AWS OpsWorks Stacks returns
-- @*****FILTERED*****@ instead of the actual value
--
-- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@
-- attribute is set to the cluster\'s ARN.
--
-- 'name', 'layer_name' - The layer name.
--
-- 'cloudWatchLogsConfiguration', 'layer_cloudWatchLogsConfiguration' - The Amazon CloudWatch Logs configuration settings for the layer.
--
-- 'autoAssignElasticIps', 'layer_autoAssignElasticIps' - Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'layerId', 'layer_layerId' - The layer ID.
--
-- 'defaultSecurityGroupNames', 'layer_defaultSecurityGroupNames' - An array containing the layer\'s security group names.
--
-- 'type'', 'layer_type' - The layer type.
--
-- 'useEbsOptimizedInstances', 'layer_useEbsOptimizedInstances' - Whether the layer uses Amazon EBS-optimized instances.
--
-- 'customRecipes', 'layer_customRecipes' - A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
--
-- 'autoAssignPublicIps', 'layer_autoAssignPublicIps' - For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'lifecycleEventConfiguration', 'layer_lifecycleEventConfiguration' - A @LifeCycleEventConfiguration@ object that specifies the Shutdown event
-- configuration.
newLayer ::
  Layer
newLayer =
  Layer'
    { installUpdatesOnBoot = Core.Nothing,
      customInstanceProfileArn = Core.Nothing,
      customSecurityGroupIds = Core.Nothing,
      packages = Core.Nothing,
      enableAutoHealing = Core.Nothing,
      volumeConfigurations = Core.Nothing,
      stackId = Core.Nothing,
      customJson = Core.Nothing,
      defaultRecipes = Core.Nothing,
      arn = Core.Nothing,
      shortname = Core.Nothing,
      createdAt = Core.Nothing,
      attributes = Core.Nothing,
      name = Core.Nothing,
      cloudWatchLogsConfiguration = Core.Nothing,
      autoAssignElasticIps = Core.Nothing,
      layerId = Core.Nothing,
      defaultSecurityGroupNames = Core.Nothing,
      type' = Core.Nothing,
      useEbsOptimizedInstances = Core.Nothing,
      customRecipes = Core.Nothing,
      autoAssignPublicIps = Core.Nothing,
      lifecycleEventConfiguration = Core.Nothing
    }

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
layer_installUpdatesOnBoot :: Lens.Lens' Layer (Core.Maybe Core.Bool)
layer_installUpdatesOnBoot = Lens.lens (\Layer' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@Layer' {} a -> s {installUpdatesOnBoot = a} :: Layer)

-- | The ARN of the default IAM profile to be used for the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
layer_customInstanceProfileArn :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_customInstanceProfileArn = Lens.lens (\Layer' {customInstanceProfileArn} -> customInstanceProfileArn) (\s@Layer' {} a -> s {customInstanceProfileArn = a} :: Layer)

-- | An array containing the layer\'s custom security group IDs.
layer_customSecurityGroupIds :: Lens.Lens' Layer (Core.Maybe [Core.Text])
layer_customSecurityGroupIds = Lens.lens (\Layer' {customSecurityGroupIds} -> customSecurityGroupIds) (\s@Layer' {} a -> s {customSecurityGroupIds = a} :: Layer) Core.. Lens.mapping Lens._Coerce

-- | An array of @Package@ objects that describe the layer\'s packages.
layer_packages :: Lens.Lens' Layer (Core.Maybe [Core.Text])
layer_packages = Lens.lens (\Layer' {packages} -> packages) (\s@Layer' {} a -> s {packages = a} :: Layer) Core.. Lens.mapping Lens._Coerce

-- | Whether auto healing is disabled for the layer.
layer_enableAutoHealing :: Lens.Lens' Layer (Core.Maybe Core.Bool)
layer_enableAutoHealing = Lens.lens (\Layer' {enableAutoHealing} -> enableAutoHealing) (\s@Layer' {} a -> s {enableAutoHealing = a} :: Layer)

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
layer_volumeConfigurations :: Lens.Lens' Layer (Core.Maybe [VolumeConfiguration])
layer_volumeConfigurations = Lens.lens (\Layer' {volumeConfigurations} -> volumeConfigurations) (\s@Layer' {} a -> s {volumeConfigurations = a} :: Layer) Core.. Lens.mapping Lens._Coerce

-- | The layer stack ID.
layer_stackId :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_stackId = Lens.lens (\Layer' {stackId} -> stackId) (\s@Layer' {} a -> s {stackId = a} :: Layer)

-- | A JSON formatted string containing the layer\'s custom stack
-- configuration and deployment attributes.
layer_customJson :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_customJson = Lens.lens (\Layer' {customJson} -> customJson) (\s@Layer' {} a -> s {customJson = a} :: Layer)

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__,
-- __configuration__, __deploy__, __undeploy__, and __shutdown__. For each
-- layer, AWS OpsWorks Stacks runs a set of standard recipes for each
-- event. You can also provide custom recipes for any or all layers and
-- events. AWS OpsWorks Stacks runs custom event recipes after the standard
-- recipes. @LayerCustomRecipes@ specifies the custom recipes for a
-- particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook\'s directory name in the
-- repository followed by two colons and the recipe name, which is the
-- recipe\'s file name without the @.rb@ extension. For example:
-- @phpapp2::dbsetup@ specifies the @dbsetup.rb@ recipe in the
-- repository\'s @phpapp2@ folder.
layer_defaultRecipes :: Lens.Lens' Layer (Core.Maybe Recipes)
layer_defaultRecipes = Lens.lens (\Layer' {defaultRecipes} -> defaultRecipes) (\s@Layer' {} a -> s {defaultRecipes = a} :: Layer)

-- | The Amazon Resource Number (ARN) of a layer.
layer_arn :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_arn = Lens.lens (\Layer' {arn} -> arn) (\s@Layer' {} a -> s {arn = a} :: Layer)

-- | The layer short name.
layer_shortname :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_shortname = Lens.lens (\Layer' {shortname} -> shortname) (\s@Layer' {} a -> s {shortname = a} :: Layer)

-- | Date when the layer was created.
layer_createdAt :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_createdAt = Lens.lens (\Layer' {createdAt} -> createdAt) (\s@Layer' {} a -> s {createdAt = a} :: Layer)

-- | The layer attributes.
--
-- For the @HaproxyStatsPassword@, @MysqlRootPassword@, and
-- @GangliaPassword@ attributes, AWS OpsWorks Stacks returns
-- @*****FILTERED*****@ instead of the actual value
--
-- For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@
-- attribute is set to the cluster\'s ARN.
layer_attributes :: Lens.Lens' Layer (Core.Maybe (Core.HashMap LayerAttributesKeys (Core.Maybe Core.Text)))
layer_attributes = Lens.lens (\Layer' {attributes} -> attributes) (\s@Layer' {} a -> s {attributes = a} :: Layer) Core.. Lens.mapping Lens._Coerce

-- | The layer name.
layer_name :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_name = Lens.lens (\Layer' {name} -> name) (\s@Layer' {} a -> s {name = a} :: Layer)

-- | The Amazon CloudWatch Logs configuration settings for the layer.
layer_cloudWatchLogsConfiguration :: Lens.Lens' Layer (Core.Maybe CloudWatchLogsConfiguration)
layer_cloudWatchLogsConfiguration = Lens.lens (\Layer' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@Layer' {} a -> s {cloudWatchLogsConfiguration = a} :: Layer)

-- | Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
layer_autoAssignElasticIps :: Lens.Lens' Layer (Core.Maybe Core.Bool)
layer_autoAssignElasticIps = Lens.lens (\Layer' {autoAssignElasticIps} -> autoAssignElasticIps) (\s@Layer' {} a -> s {autoAssignElasticIps = a} :: Layer)

-- | The layer ID.
layer_layerId :: Lens.Lens' Layer (Core.Maybe Core.Text)
layer_layerId = Lens.lens (\Layer' {layerId} -> layerId) (\s@Layer' {} a -> s {layerId = a} :: Layer)

-- | An array containing the layer\'s security group names.
layer_defaultSecurityGroupNames :: Lens.Lens' Layer (Core.Maybe [Core.Text])
layer_defaultSecurityGroupNames = Lens.lens (\Layer' {defaultSecurityGroupNames} -> defaultSecurityGroupNames) (\s@Layer' {} a -> s {defaultSecurityGroupNames = a} :: Layer) Core.. Lens.mapping Lens._Coerce

-- | The layer type.
layer_type :: Lens.Lens' Layer (Core.Maybe LayerType)
layer_type = Lens.lens (\Layer' {type'} -> type') (\s@Layer' {} a -> s {type' = a} :: Layer)

-- | Whether the layer uses Amazon EBS-optimized instances.
layer_useEbsOptimizedInstances :: Lens.Lens' Layer (Core.Maybe Core.Bool)
layer_useEbsOptimizedInstances = Lens.lens (\Layer' {useEbsOptimizedInstances} -> useEbsOptimizedInstances) (\s@Layer' {} a -> s {useEbsOptimizedInstances = a} :: Layer)

-- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
layer_customRecipes :: Lens.Lens' Layer (Core.Maybe Recipes)
layer_customRecipes = Lens.lens (\Layer' {customRecipes} -> customRecipes) (\s@Layer' {} a -> s {customRecipes = a} :: Layer)

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
layer_autoAssignPublicIps :: Lens.Lens' Layer (Core.Maybe Core.Bool)
layer_autoAssignPublicIps = Lens.lens (\Layer' {autoAssignPublicIps} -> autoAssignPublicIps) (\s@Layer' {} a -> s {autoAssignPublicIps = a} :: Layer)

-- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event
-- configuration.
layer_lifecycleEventConfiguration :: Lens.Lens' Layer (Core.Maybe LifecycleEventConfiguration)
layer_lifecycleEventConfiguration = Lens.lens (\Layer' {lifecycleEventConfiguration} -> lifecycleEventConfiguration) (\s@Layer' {} a -> s {lifecycleEventConfiguration = a} :: Layer)

instance Core.FromJSON Layer where
  parseJSON =
    Core.withObject
      "Layer"
      ( \x ->
          Layer'
            Core.<$> (x Core..:? "InstallUpdatesOnBoot")
            Core.<*> (x Core..:? "CustomInstanceProfileArn")
            Core.<*> ( x Core..:? "CustomSecurityGroupIds"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Packages" Core..!= Core.mempty)
            Core.<*> (x Core..:? "EnableAutoHealing")
            Core.<*> ( x Core..:? "VolumeConfigurations"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "StackId")
            Core.<*> (x Core..:? "CustomJson")
            Core.<*> (x Core..:? "DefaultRecipes")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Shortname")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "CloudWatchLogsConfiguration")
            Core.<*> (x Core..:? "AutoAssignElasticIps")
            Core.<*> (x Core..:? "LayerId")
            Core.<*> ( x Core..:? "DefaultSecurityGroupNames"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "UseEbsOptimizedInstances")
            Core.<*> (x Core..:? "CustomRecipes")
            Core.<*> (x Core..:? "AutoAssignPublicIps")
            Core.<*> (x Core..:? "LifecycleEventConfiguration")
      )

instance Core.Hashable Layer

instance Core.NFData Layer
