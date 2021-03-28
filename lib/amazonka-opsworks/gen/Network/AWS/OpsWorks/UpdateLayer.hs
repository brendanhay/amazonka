{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateLayer (..)
    , mkUpdateLayer
    -- ** Request lenses
    , ulLayerId
    , ulAttributes
    , ulAutoAssignElasticIps
    , ulAutoAssignPublicIps
    , ulCloudWatchLogsConfiguration
    , ulCustomInstanceProfileArn
    , ulCustomJson
    , ulCustomRecipes
    , ulCustomSecurityGroupIds
    , ulEnableAutoHealing
    , ulInstallUpdatesOnBoot
    , ulLifecycleEventConfiguration
    , ulName
    , ulPackages
    , ulShortname
    , ulUseEbsOptimizedInstances
    , ulVolumeConfigurations

    -- * Destructuring the response
    , UpdateLayerResponse (..)
    , mkUpdateLayerResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateLayer' smart constructor.
data UpdateLayer = UpdateLayer'
  { layerId :: Core.Text
    -- ^ The layer ID.
  , attributes :: Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text)
    -- ^ One or more user-defined key/value pairs to be added to the stack attributes.
  , autoAssignElasticIps :: Core.Maybe Core.Bool
    -- ^ Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
  , autoAssignPublicIps :: Core.Maybe Core.Bool
    -- ^ For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
  , cloudWatchLogsConfiguration :: Core.Maybe Types.CloudWatchLogsConfiguration
    -- ^ Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
  , customInstanceProfileArn :: Core.Maybe Core.Text
    -- ^ The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
  , customJson :: Core.Maybe Core.Text
    -- ^ A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> . 
  , customRecipes :: Core.Maybe Types.Recipes
    -- ^ A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
  , customSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ An array containing the layer's custom security group IDs.
  , enableAutoHealing :: Core.Maybe Core.Bool
    -- ^ Whether to disable auto healing for the layer.
  , installUpdatesOnBoot :: Core.Maybe Core.Bool
    -- ^ Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances. 
  , lifecycleEventConfiguration :: Core.Maybe Types.LifecycleEventConfiguration
    -- ^ 
  , name :: Core.Maybe Core.Text
    -- ^ The layer name, which is used by the console.
  , packages :: Core.Maybe [Core.Text]
    -- ^ An array of @Package@ objects that describe the layer's packages.
  , shortname :: Core.Maybe Core.Text
    -- ^ For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference> 
  , useEbsOptimizedInstances :: Core.Maybe Core.Bool
    -- ^ Whether to use Amazon EBS-optimized instances.
  , volumeConfigurations :: Core.Maybe [Types.VolumeConfiguration]
    -- ^ A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLayer' value with any optional fields omitted.
mkUpdateLayer
    :: Core.Text -- ^ 'layerId'
    -> UpdateLayer
mkUpdateLayer layerId
  = UpdateLayer'{layerId, attributes = Core.Nothing,
                 autoAssignElasticIps = Core.Nothing,
                 autoAssignPublicIps = Core.Nothing,
                 cloudWatchLogsConfiguration = Core.Nothing,
                 customInstanceProfileArn = Core.Nothing, customJson = Core.Nothing,
                 customRecipes = Core.Nothing,
                 customSecurityGroupIds = Core.Nothing,
                 enableAutoHealing = Core.Nothing,
                 installUpdatesOnBoot = Core.Nothing,
                 lifecycleEventConfiguration = Core.Nothing, name = Core.Nothing,
                 packages = Core.Nothing, shortname = Core.Nothing,
                 useEbsOptimizedInstances = Core.Nothing,
                 volumeConfigurations = Core.Nothing}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLayerId :: Lens.Lens' UpdateLayer Core.Text
ulLayerId = Lens.field @"layerId"
{-# INLINEABLE ulLayerId #-}
{-# DEPRECATED layerId "Use generic-lens or generic-optics with 'layerId' instead"  #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAttributes :: Lens.Lens' UpdateLayer (Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text))
ulAttributes = Lens.field @"attributes"
{-# INLINEABLE ulAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAutoAssignElasticIps :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulAutoAssignElasticIps = Lens.field @"autoAssignElasticIps"
{-# INLINEABLE ulAutoAssignElasticIps #-}
{-# DEPRECATED autoAssignElasticIps "Use generic-lens or generic-optics with 'autoAssignElasticIps' instead"  #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAutoAssignPublicIps :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulAutoAssignPublicIps = Lens.field @"autoAssignPublicIps"
{-# INLINEABLE ulAutoAssignPublicIps #-}
{-# DEPRECATED autoAssignPublicIps "Use generic-lens or generic-optics with 'autoAssignPublicIps' instead"  #-}

-- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCloudWatchLogsConfiguration :: Lens.Lens' UpdateLayer (Core.Maybe Types.CloudWatchLogsConfiguration)
ulCloudWatchLogsConfiguration = Lens.field @"cloudWatchLogsConfiguration"
{-# INLINEABLE ulCloudWatchLogsConfiguration #-}
{-# DEPRECATED cloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead"  #-}

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomInstanceProfileArn :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
ulCustomInstanceProfileArn = Lens.field @"customInstanceProfileArn"
{-# INLINEABLE ulCustomInstanceProfileArn #-}
{-# DEPRECATED customInstanceProfileArn "Use generic-lens or generic-optics with 'customInstanceProfileArn' instead"  #-}

-- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> . 
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomJson :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
ulCustomJson = Lens.field @"customJson"
{-# INLINEABLE ulCustomJson #-}
{-# DEPRECATED customJson "Use generic-lens or generic-optics with 'customJson' instead"  #-}

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomRecipes :: Lens.Lens' UpdateLayer (Core.Maybe Types.Recipes)
ulCustomRecipes = Lens.field @"customRecipes"
{-# INLINEABLE ulCustomRecipes #-}
{-# DEPRECATED customRecipes "Use generic-lens or generic-optics with 'customRecipes' instead"  #-}

-- | An array containing the layer's custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomSecurityGroupIds :: Lens.Lens' UpdateLayer (Core.Maybe [Core.Text])
ulCustomSecurityGroupIds = Lens.field @"customSecurityGroupIds"
{-# INLINEABLE ulCustomSecurityGroupIds #-}
{-# DEPRECATED customSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead"  #-}

-- | Whether to disable auto healing for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulEnableAutoHealing :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulEnableAutoHealing = Lens.field @"enableAutoHealing"
{-# INLINEABLE ulEnableAutoHealing #-}
{-# DEPRECATED enableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead"  #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances. 
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulInstallUpdatesOnBoot :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# INLINEABLE ulInstallUpdatesOnBoot #-}
{-# DEPRECATED installUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLifecycleEventConfiguration :: Lens.Lens' UpdateLayer (Core.Maybe Types.LifecycleEventConfiguration)
ulLifecycleEventConfiguration = Lens.field @"lifecycleEventConfiguration"
{-# INLINEABLE ulLifecycleEventConfiguration #-}
{-# DEPRECATED lifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead"  #-}

-- | The layer name, which is used by the console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulName :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
ulName = Lens.field @"name"
{-# INLINEABLE ulName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An array of @Package@ objects that describe the layer's packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulPackages :: Lens.Lens' UpdateLayer (Core.Maybe [Core.Text])
ulPackages = Lens.field @"packages"
{-# INLINEABLE ulPackages #-}
{-# DEPRECATED packages "Use generic-lens or generic-optics with 'packages' instead"  #-}

-- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference> 
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulShortname :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
ulShortname = Lens.field @"shortname"
{-# INLINEABLE ulShortname #-}
{-# DEPRECATED shortname "Use generic-lens or generic-optics with 'shortname' instead"  #-}

-- | Whether to use Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEbsOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulUseEbsOptimizedInstances :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
ulUseEbsOptimizedInstances = Lens.field @"useEbsOptimizedInstances"
{-# INLINEABLE ulUseEbsOptimizedInstances #-}
{-# DEPRECATED useEbsOptimizedInstances "Use generic-lens or generic-optics with 'useEbsOptimizedInstances' instead"  #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulVolumeConfigurations :: Lens.Lens' UpdateLayer (Core.Maybe [Types.VolumeConfiguration])
ulVolumeConfigurations = Lens.field @"volumeConfigurations"
{-# INLINEABLE ulVolumeConfigurations #-}
{-# DEPRECATED volumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead"  #-}

instance Core.ToQuery UpdateLayer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateLayer where
        toHeaders UpdateLayer{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateLayer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateLayer where
        toJSON UpdateLayer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LayerId" Core..= layerId),
                  ("Attributes" Core..=) Core.<$> attributes,
                  ("AutoAssignElasticIps" Core..=) Core.<$> autoAssignElasticIps,
                  ("AutoAssignPublicIps" Core..=) Core.<$> autoAssignPublicIps,
                  ("CloudWatchLogsConfiguration" Core..=) Core.<$>
                    cloudWatchLogsConfiguration,
                  ("CustomInstanceProfileArn" Core..=) Core.<$>
                    customInstanceProfileArn,
                  ("CustomJson" Core..=) Core.<$> customJson,
                  ("CustomRecipes" Core..=) Core.<$> customRecipes,
                  ("CustomSecurityGroupIds" Core..=) Core.<$> customSecurityGroupIds,
                  ("EnableAutoHealing" Core..=) Core.<$> enableAutoHealing,
                  ("InstallUpdatesOnBoot" Core..=) Core.<$> installUpdatesOnBoot,
                  ("LifecycleEventConfiguration" Core..=) Core.<$>
                    lifecycleEventConfiguration,
                  ("Name" Core..=) Core.<$> name,
                  ("Packages" Core..=) Core.<$> packages,
                  ("Shortname" Core..=) Core.<$> shortname,
                  ("UseEbsOptimizedInstances" Core..=) Core.<$>
                    useEbsOptimizedInstances,
                  ("VolumeConfigurations" Core..=) Core.<$> volumeConfigurations])

instance Core.AWSRequest UpdateLayer where
        type Rs UpdateLayer = UpdateLayerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateLayerResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateLayerResponse' smart constructor.
data UpdateLayerResponse = UpdateLayerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLayerResponse' value with any optional fields omitted.
mkUpdateLayerResponse
    :: UpdateLayerResponse
mkUpdateLayerResponse = UpdateLayerResponse'
