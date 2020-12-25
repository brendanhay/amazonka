{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateLayer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a layer. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-create.html How to Create a Layer> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateLayer
  ( -- * Creating a request
    CreateLayer (..),
    mkCreateLayer,

    -- ** Request lenses
    clStackId,
    clType,
    clName,
    clShortname,
    clAttributes,
    clAutoAssignElasticIps,
    clAutoAssignPublicIps,
    clCloudWatchLogsConfiguration,
    clCustomInstanceProfileArn,
    clCustomJson,
    clCustomRecipes,
    clCustomSecurityGroupIds,
    clEnableAutoHealing,
    clInstallUpdatesOnBoot,
    clLifecycleEventConfiguration,
    clPackages,
    clUseEbsOptimizedInstances,
    clVolumeConfigurations,

    -- * Destructuring the response
    CreateLayerResponse (..),
    mkCreateLayerResponse,

    -- ** Response lenses
    clrrsLayerId,
    clrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLayer' smart constructor.
data CreateLayer = CreateLayer'
  { -- | The layer stack ID.
    stackId :: Types.String,
    -- | The layer type. A stack cannot have more than one built-in layer of the same type. It can have any number of custom layers. Built-in layers are not available in Chef 12 stacks.
    type' :: Types.LayerType,
    -- | The layer name, which is used by the console.
    name :: Types.String,
    -- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef recipes. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters, which are limited to the alphanumeric characters, '-', '_', and '.'.
    --
    -- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference> .
    shortname :: Types.String,
    -- | One or more user-defined key-value pairs to be added to the stack attributes.
    --
    -- To create a cluster layer, set the @EcsClusterArn@ attribute to the cluster's ARN.
    attributes :: Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text),
    -- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignElasticIps :: Core.Maybe Core.Bool,
    -- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignPublicIps :: Core.Maybe Core.Bool,
    -- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
    cloudWatchLogsConfiguration :: Core.Maybe Types.CloudWatchLogsConfiguration,
    -- | The ARN of an IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    customInstanceProfileArn :: Core.Maybe Types.String,
    -- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> . This feature is supported as of version 1.7.42 of the AWS CLI.
    customJson :: Core.Maybe Types.String,
    -- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
    customRecipes :: Core.Maybe Types.Recipes,
    -- | An array containing the layer custom security group IDs.
    customSecurityGroupIds :: Core.Maybe [Types.String],
    -- | Whether to disable auto healing for the layer.
    enableAutoHealing :: Core.Maybe Core.Bool,
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | A @LifeCycleEventConfiguration@ object that you can use to configure the Shutdown event to specify an execution timeout and enable or disable Elastic Load Balancer connection draining.
    lifecycleEventConfiguration :: Core.Maybe Types.LifecycleEventConfiguration,
    -- | An array of @Package@ objects that describes the layer packages.
    packages :: Core.Maybe [Types.String],
    -- | Whether to use Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Core.Maybe Core.Bool,
    -- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
    volumeConfigurations :: Core.Maybe [Types.VolumeConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLayer' value with any optional fields omitted.
mkCreateLayer ::
  -- | 'stackId'
  Types.String ->
  -- | 'type\''
  Types.LayerType ->
  -- | 'name'
  Types.String ->
  -- | 'shortname'
  Types.String ->
  CreateLayer
mkCreateLayer stackId type' name shortname =
  CreateLayer'
    { stackId,
      type',
      name,
      shortname,
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
      packages = Core.Nothing,
      useEbsOptimizedInstances = Core.Nothing,
      volumeConfigurations = Core.Nothing
    }

-- | The layer stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clStackId :: Lens.Lens' CreateLayer Types.String
clStackId = Lens.field @"stackId"
{-# DEPRECATED clStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The layer type. A stack cannot have more than one built-in layer of the same type. It can have any number of custom layers. Built-in layers are not available in Chef 12 stacks.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clType :: Lens.Lens' CreateLayer Types.LayerType
clType = Lens.field @"type'"
{-# DEPRECATED clType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The layer name, which is used by the console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clName :: Lens.Lens' CreateLayer Types.String
clName = Lens.field @"name"
{-# DEPRECATED clName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef recipes. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters, which are limited to the alphanumeric characters, '-', '_', and '.'.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference> .
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clShortname :: Lens.Lens' CreateLayer Types.String
clShortname = Lens.field @"shortname"
{-# DEPRECATED clShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | One or more user-defined key-value pairs to be added to the stack attributes.
--
-- To create a cluster layer, set the @EcsClusterArn@ attribute to the cluster's ARN.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAttributes :: Lens.Lens' CreateLayer (Core.Maybe (Core.HashMap Types.LayerAttributesKeys Types.Maybe Text))
clAttributes = Lens.field @"attributes"
{-# DEPRECATED clAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAutoAssignElasticIps :: Lens.Lens' CreateLayer (Core.Maybe Core.Bool)
clAutoAssignElasticIps = Lens.field @"autoAssignElasticIps"
{-# DEPRECATED clAutoAssignElasticIps "Use generic-lens or generic-optics with 'autoAssignElasticIps' instead." #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAutoAssignPublicIps :: Lens.Lens' CreateLayer (Core.Maybe Core.Bool)
clAutoAssignPublicIps = Lens.field @"autoAssignPublicIps"
{-# DEPRECATED clAutoAssignPublicIps "Use generic-lens or generic-optics with 'autoAssignPublicIps' instead." #-}

-- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCloudWatchLogsConfiguration :: Lens.Lens' CreateLayer (Core.Maybe Types.CloudWatchLogsConfiguration)
clCloudWatchLogsConfiguration = Lens.field @"cloudWatchLogsConfiguration"
{-# DEPRECATED clCloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead." #-}

-- | The ARN of an IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomInstanceProfileArn :: Lens.Lens' CreateLayer (Core.Maybe Types.String)
clCustomInstanceProfileArn = Lens.field @"customInstanceProfileArn"
{-# DEPRECATED clCustomInstanceProfileArn "Use generic-lens or generic-optics with 'customInstanceProfileArn' instead." #-}

-- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> . This feature is supported as of version 1.7.42 of the AWS CLI.
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomJson :: Lens.Lens' CreateLayer (Core.Maybe Types.String)
clCustomJson = Lens.field @"customJson"
{-# DEPRECATED clCustomJson "Use generic-lens or generic-optics with 'customJson' instead." #-}

-- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomRecipes :: Lens.Lens' CreateLayer (Core.Maybe Types.Recipes)
clCustomRecipes = Lens.field @"customRecipes"
{-# DEPRECATED clCustomRecipes "Use generic-lens or generic-optics with 'customRecipes' instead." #-}

-- | An array containing the layer custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomSecurityGroupIds :: Lens.Lens' CreateLayer (Core.Maybe [Types.String])
clCustomSecurityGroupIds = Lens.field @"customSecurityGroupIds"
{-# DEPRECATED clCustomSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead." #-}

-- | Whether to disable auto healing for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clEnableAutoHealing :: Lens.Lens' CreateLayer (Core.Maybe Core.Bool)
clEnableAutoHealing = Lens.field @"enableAutoHealing"
{-# DEPRECATED clEnableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clInstallUpdatesOnBoot :: Lens.Lens' CreateLayer (Core.Maybe Core.Bool)
clInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# DEPRECATED clInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | A @LifeCycleEventConfiguration@ object that you can use to configure the Shutdown event to specify an execution timeout and enable or disable Elastic Load Balancer connection draining.
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLifecycleEventConfiguration :: Lens.Lens' CreateLayer (Core.Maybe Types.LifecycleEventConfiguration)
clLifecycleEventConfiguration = Lens.field @"lifecycleEventConfiguration"
{-# DEPRECATED clLifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead." #-}

-- | An array of @Package@ objects that describes the layer packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clPackages :: Lens.Lens' CreateLayer (Core.Maybe [Types.String])
clPackages = Lens.field @"packages"
{-# DEPRECATED clPackages "Use generic-lens or generic-optics with 'packages' instead." #-}

-- | Whether to use Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEbsOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clUseEbsOptimizedInstances :: Lens.Lens' CreateLayer (Core.Maybe Core.Bool)
clUseEbsOptimizedInstances = Lens.field @"useEbsOptimizedInstances"
{-# DEPRECATED clUseEbsOptimizedInstances "Use generic-lens or generic-optics with 'useEbsOptimizedInstances' instead." #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clVolumeConfigurations :: Lens.Lens' CreateLayer (Core.Maybe [Types.VolumeConfiguration])
clVolumeConfigurations = Lens.field @"volumeConfigurations"
{-# DEPRECATED clVolumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead." #-}

instance Core.FromJSON CreateLayer where
  toJSON CreateLayer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackId" Core..= stackId),
            Core.Just ("Type" Core..= type'),
            Core.Just ("Name" Core..= name),
            Core.Just ("Shortname" Core..= shortname),
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
            ("Packages" Core..=) Core.<$> packages,
            ("UseEbsOptimizedInstances" Core..=)
              Core.<$> useEbsOptimizedInstances,
            ("VolumeConfigurations" Core..=) Core.<$> volumeConfigurations
          ]
      )

instance Core.AWSRequest CreateLayer where
  type Rs CreateLayer = CreateLayerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.CreateLayer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLayerResponse'
            Core.<$> (x Core..:? "LayerId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @CreateLayer@ request.
--
-- /See:/ 'mkCreateLayerResponse' smart constructor.
data CreateLayerResponse = CreateLayerResponse'
  { -- | The layer ID.
    layerId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLayerResponse' value with any optional fields omitted.
mkCreateLayerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLayerResponse
mkCreateLayerResponse responseStatus =
  CreateLayerResponse' {layerId = Core.Nothing, responseStatus}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsLayerId :: Lens.Lens' CreateLayerResponse (Core.Maybe Types.String)
clrrsLayerId = Lens.field @"layerId"
{-# DEPRECATED clrrsLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsResponseStatus :: Lens.Lens' CreateLayerResponse Core.Int
clrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
