{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified layer.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateLayer
  ( -- * Creating a request
    UpdateLayer (..),
    mkUpdateLayer,

    -- ** Request lenses
    ulLayerId,
    ulAttributes,
    ulAutoAssignElasticIps,
    ulAutoAssignPublicIps,
    ulCloudWatchLogsConfiguration,
    ulCustomInstanceProfileArn,
    ulCustomJson,
    ulCustomRecipes,
    ulCustomSecurityGroupIds,
    ulEnableAutoHealing,
    ulInstallUpdatesOnBoot,
    ulLifecycleEventConfiguration,
    ulName,
    ulPackages,
    ulShortname,
    ulUseEbsOptimizedInstances,
    ulVolumeConfigurations,

    -- * Destructuring the response
    UpdateLayerResponse (..),
    mkUpdateLayerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateLayer' smart constructor.
data UpdateLayer = UpdateLayer'
  { -- | The layer ID.
    layerId :: Types.String,
    -- | One or more user-defined key/value pairs to be added to the stack attributes.
    attributes :: Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text),
    -- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignElasticIps :: Core.Maybe Core.Bool,
    -- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignPublicIps :: Core.Maybe Core.Bool,
    -- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
    cloudWatchLogsConfiguration :: Core.Maybe Types.CloudWatchLogsConfiguration,
    -- | The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    customInstanceProfileArn :: Core.Maybe Types.String,
    -- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> .
    customJson :: Core.Maybe Types.String,
    -- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
    customRecipes :: Core.Maybe Types.Recipes,
    -- | An array containing the layer's custom security group IDs.
    customSecurityGroupIds :: Core.Maybe [Types.String],
    -- | Whether to disable auto healing for the layer.
    enableAutoHealing :: Core.Maybe Core.Bool,
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- |
    lifecycleEventConfiguration :: Core.Maybe Types.LifecycleEventConfiguration,
    -- | The layer name, which is used by the console.
    name :: Core.Maybe Types.String,
    -- | An array of @Package@ objects that describe the layer's packages.
    packages :: Core.Maybe [Types.String],
    -- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
    --
    -- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
    shortname :: Core.Maybe Types.String,
    -- | Whether to use Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Core.Maybe Core.Bool,
    -- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
    volumeConfigurations :: Core.Maybe [Types.VolumeConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLayer' value with any optional fields omitted.
mkUpdateLayer ::
  -- | 'layerId'
  Types.String ->
  UpdateLayer
mkUpdateLayer layerId =
  UpdateLayer'
    { layerId,
      attributes = Core.Nothing,
      autoAssignElasticIps = Core.Nothing,
      autoAssignPublicIps = Core.Nothing,
      cloudWatchLogsConfiguration = Core.Nothing,
      customInstanceProfileArn = Core.Nothing,
      customJson = Core.Nothing,
      customRecipes = Core.Nothing,
      customSecurityGroupIds = Core.Nothing,
      enableAutoHealing = Core.Nothing,
      installUpdatesOnBoot = Core.Nothing,
      lifecycleEventConfiguration = Core.Nothing,
      name = Core.Nothing,
      packages = Core.Nothing,
      shortname = Core.Nothing,
      useEbsOptimizedInstances = Core.Nothing,
      volumeConfigurations = Core.Nothing
    }

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLayerId :: Lens.Lens' UpdateLayer Types.String
ulLayerId = Lens.field @"layerId"
{-# DEPRECATED ulLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAttributes :: Lens.Lens' UpdateLayer (Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text))
ulAttributes = Lens.field @"attributes"
{-# DEPRECATED ulAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAutoAssignElasticIps :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulAutoAssignElasticIps = Lens.field @"autoAssignElasticIps"
{-# DEPRECATED ulAutoAssignElasticIps "Use generic-lens or generic-optics with 'autoAssignElasticIps' instead." #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAutoAssignPublicIps :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulAutoAssignPublicIps = Lens.field @"autoAssignPublicIps"
{-# DEPRECATED ulAutoAssignPublicIps "Use generic-lens or generic-optics with 'autoAssignPublicIps' instead." #-}

-- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCloudWatchLogsConfiguration :: Lens.Lens' UpdateLayer (Core.Maybe Types.CloudWatchLogsConfiguration)
ulCloudWatchLogsConfiguration = Lens.field @"cloudWatchLogsConfiguration"
{-# DEPRECATED ulCloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead." #-}

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomInstanceProfileArn :: Lens.Lens' UpdateLayer (Core.Maybe Types.String)
ulCustomInstanceProfileArn = Lens.field @"customInstanceProfileArn"
{-# DEPRECATED ulCustomInstanceProfileArn "Use generic-lens or generic-optics with 'customInstanceProfileArn' instead." #-}

-- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> .
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomJson :: Lens.Lens' UpdateLayer (Core.Maybe Types.String)
ulCustomJson = Lens.field @"customJson"
{-# DEPRECATED ulCustomJson "Use generic-lens or generic-optics with 'customJson' instead." #-}

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomRecipes :: Lens.Lens' UpdateLayer (Core.Maybe Types.Recipes)
ulCustomRecipes = Lens.field @"customRecipes"
{-# DEPRECATED ulCustomRecipes "Use generic-lens or generic-optics with 'customRecipes' instead." #-}

-- | An array containing the layer's custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomSecurityGroupIds :: Lens.Lens' UpdateLayer (Core.Maybe [Types.String])
ulCustomSecurityGroupIds = Lens.field @"customSecurityGroupIds"
{-# DEPRECATED ulCustomSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead." #-}

-- | Whether to disable auto healing for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulEnableAutoHealing :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulEnableAutoHealing = Lens.field @"enableAutoHealing"
{-# DEPRECATED ulEnableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulInstallUpdatesOnBoot :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# DEPRECATED ulInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- |
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLifecycleEventConfiguration :: Lens.Lens' UpdateLayer (Core.Maybe Types.LifecycleEventConfiguration)
ulLifecycleEventConfiguration = Lens.field @"lifecycleEventConfiguration"
{-# DEPRECATED ulLifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead." #-}

-- | The layer name, which is used by the console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulName :: Lens.Lens' UpdateLayer (Core.Maybe Types.String)
ulName = Lens.field @"name"
{-# DEPRECATED ulName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An array of @Package@ objects that describe the layer's packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulPackages :: Lens.Lens' UpdateLayer (Core.Maybe [Types.String])
ulPackages = Lens.field @"packages"
{-# DEPRECATED ulPackages "Use generic-lens or generic-optics with 'packages' instead." #-}

-- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulShortname :: Lens.Lens' UpdateLayer (Core.Maybe Types.String)
ulShortname = Lens.field @"shortname"
{-# DEPRECATED ulShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | Whether to use Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEbsOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulUseEbsOptimizedInstances :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulUseEbsOptimizedInstances = Lens.field @"useEbsOptimizedInstances"
{-# DEPRECATED ulUseEbsOptimizedInstances "Use generic-lens or generic-optics with 'useEbsOptimizedInstances' instead." #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulVolumeConfigurations :: Lens.Lens' UpdateLayer (Core.Maybe [Types.VolumeConfiguration])
ulVolumeConfigurations = Lens.field @"volumeConfigurations"
{-# DEPRECATED ulVolumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead." #-}

instance Core.FromJSON UpdateLayer where
  toJSON UpdateLayer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("LayerId" Core..= layerId),
            ("Attributes" Core..=) Core.<$> attributes,
            ("AutoAssignElasticIps" Core..=) Core.<$> autoAssignElasticIps,
            ("AutoAssignPublicIps" Core..=) Core.<$> autoAssignPublicIps,
            ("CloudWatchLogsConfiguration" Core..=)
              Core.<$> cloudWatchLogsConfiguration,
            ("CustomInstanceProfileArn" Core..=)
              Core.<$> customInstanceProfileArn,
            ("CustomJson" Core..=) Core.<$> customJson,
            ("CustomRecipes" Core..=) Core.<$> customRecipes,
            ("CustomSecurityGroupIds" Core..=) Core.<$> customSecurityGroupIds,
            ("EnableAutoHealing" Core..=) Core.<$> enableAutoHealing,
            ("InstallUpdatesOnBoot" Core..=) Core.<$> installUpdatesOnBoot,
            ("LifecycleEventConfiguration" Core..=)
              Core.<$> lifecycleEventConfiguration,
            ("Name" Core..=) Core.<$> name,
            ("Packages" Core..=) Core.<$> packages,
            ("Shortname" Core..=) Core.<$> shortname,
            ("UseEbsOptimizedInstances" Core..=)
              Core.<$> useEbsOptimizedInstances,
            ("VolumeConfigurations" Core..=) Core.<$> volumeConfigurations
          ]
      )

instance Core.AWSRequest UpdateLayer where
  type Rs UpdateLayer = UpdateLayerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateLayer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateLayerResponse'

-- | /See:/ 'mkUpdateLayerResponse' smart constructor.
data UpdateLayerResponse = UpdateLayerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLayerResponse' value with any optional fields omitted.
mkUpdateLayerResponse ::
  UpdateLayerResponse
mkUpdateLayerResponse = UpdateLayerResponse'
