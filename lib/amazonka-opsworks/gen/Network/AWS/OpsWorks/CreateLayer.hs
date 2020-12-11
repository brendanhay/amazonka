{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    clCustomInstanceProfileARN,
    clCustomSecurityGroupIds,
    clInstallUpdatesOnBoot,
    clCloudWatchLogsConfiguration,
    clLifecycleEventConfiguration,
    clCustomRecipes,
    clCustomJSON,
    clVolumeConfigurations,
    clEnableAutoHealing,
    clPackages,
    clAttributes,
    clAutoAssignPublicIPs,
    clUseEBSOptimizedInstances,
    clAutoAssignElasticIPs,
    clStackId,
    clType,
    clName,
    clShortname,

    -- * Destructuring the response
    CreateLayerResponse (..),
    mkCreateLayerResponse,

    -- ** Response lenses
    clrsLayerId,
    clrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLayer' smart constructor.
data CreateLayer = CreateLayer'
  { customInstanceProfileARN ::
      Lude.Maybe Lude.Text,
    customSecurityGroupIds :: Lude.Maybe [Lude.Text],
    installUpdatesOnBoot :: Lude.Maybe Lude.Bool,
    cloudWatchLogsConfiguration ::
      Lude.Maybe CloudWatchLogsConfiguration,
    lifecycleEventConfiguration ::
      Lude.Maybe LifecycleEventConfiguration,
    customRecipes :: Lude.Maybe Recipes,
    customJSON :: Lude.Maybe Lude.Text,
    volumeConfigurations :: Lude.Maybe [VolumeConfiguration],
    enableAutoHealing :: Lude.Maybe Lude.Bool,
    packages :: Lude.Maybe [Lude.Text],
    attributes ::
      Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text)),
    autoAssignPublicIPs :: Lude.Maybe Lude.Bool,
    useEBSOptimizedInstances :: Lude.Maybe Lude.Bool,
    autoAssignElasticIPs :: Lude.Maybe Lude.Bool,
    stackId :: Lude.Text,
    type' :: LayerType,
    name :: Lude.Text,
    shortname :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLayer' with the minimum fields required to make a request.
--
-- * 'attributes' - One or more user-defined key-value pairs to be added to the stack attributes.
--
-- To create a cluster layer, set the @EcsClusterArn@ attribute to the cluster's ARN.
-- * 'autoAssignElasticIPs' - Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
-- * 'autoAssignPublicIPs' - For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
-- * 'cloudWatchLogsConfiguration' - Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
-- * 'customInstanceProfileARN' - The ARN of an IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'customJSON' - A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> . This feature is supported as of version 1.7.42 of the AWS CLI.
-- * 'customRecipes' - A @LayerCustomRecipes@ object that specifies the layer custom recipes.
-- * 'customSecurityGroupIds' - An array containing the layer custom security group IDs.
-- * 'enableAutoHealing' - Whether to disable auto healing for the layer.
-- * 'installUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
-- * 'lifecycleEventConfiguration' - A @LifeCycleEventConfiguration@ object that you can use to configure the Shutdown event to specify an execution timeout and enable or disable Elastic Load Balancer connection draining.
-- * 'name' - The layer name, which is used by the console.
-- * 'packages' - An array of @Package@ objects that describes the layer packages.
-- * 'shortname' - For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef recipes. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters, which are limited to the alphanumeric characters, '-', '_', and '.'.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference> .
-- * 'stackId' - The layer stack ID.
-- * 'type'' - The layer type. A stack cannot have more than one built-in layer of the same type. It can have any number of custom layers. Built-in layers are not available in Chef 12 stacks.
-- * 'useEBSOptimizedInstances' - Whether to use Amazon EBS-optimized instances.
-- * 'volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
mkCreateLayer ::
  -- | 'stackId'
  Lude.Text ->
  -- | 'type''
  LayerType ->
  -- | 'name'
  Lude.Text ->
  -- | 'shortname'
  Lude.Text ->
  CreateLayer
mkCreateLayer pStackId_ pType_ pName_ pShortname_ =
  CreateLayer'
    { customInstanceProfileARN = Lude.Nothing,
      customSecurityGroupIds = Lude.Nothing,
      installUpdatesOnBoot = Lude.Nothing,
      cloudWatchLogsConfiguration = Lude.Nothing,
      lifecycleEventConfiguration = Lude.Nothing,
      customRecipes = Lude.Nothing,
      customJSON = Lude.Nothing,
      volumeConfigurations = Lude.Nothing,
      enableAutoHealing = Lude.Nothing,
      packages = Lude.Nothing,
      attributes = Lude.Nothing,
      autoAssignPublicIPs = Lude.Nothing,
      useEBSOptimizedInstances = Lude.Nothing,
      autoAssignElasticIPs = Lude.Nothing,
      stackId = pStackId_,
      type' = pType_,
      name = pName_,
      shortname = pShortname_
    }

-- | The ARN of an IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomInstanceProfileARN :: Lens.Lens' CreateLayer (Lude.Maybe Lude.Text)
clCustomInstanceProfileARN = Lens.lens (customInstanceProfileARN :: CreateLayer -> Lude.Maybe Lude.Text) (\s a -> s {customInstanceProfileARN = a} :: CreateLayer)
{-# DEPRECATED clCustomInstanceProfileARN "Use generic-lens or generic-optics with 'customInstanceProfileARN' instead." #-}

-- | An array containing the layer custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomSecurityGroupIds :: Lens.Lens' CreateLayer (Lude.Maybe [Lude.Text])
clCustomSecurityGroupIds = Lens.lens (customSecurityGroupIds :: CreateLayer -> Lude.Maybe [Lude.Text]) (\s a -> s {customSecurityGroupIds = a} :: CreateLayer)
{-# DEPRECATED clCustomSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clInstallUpdatesOnBoot :: Lens.Lens' CreateLayer (Lude.Maybe Lude.Bool)
clInstallUpdatesOnBoot = Lens.lens (installUpdatesOnBoot :: CreateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {installUpdatesOnBoot = a} :: CreateLayer)
{-# DEPRECATED clInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCloudWatchLogsConfiguration :: Lens.Lens' CreateLayer (Lude.Maybe CloudWatchLogsConfiguration)
clCloudWatchLogsConfiguration = Lens.lens (cloudWatchLogsConfiguration :: CreateLayer -> Lude.Maybe CloudWatchLogsConfiguration) (\s a -> s {cloudWatchLogsConfiguration = a} :: CreateLayer)
{-# DEPRECATED clCloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead." #-}

-- | A @LifeCycleEventConfiguration@ object that you can use to configure the Shutdown event to specify an execution timeout and enable or disable Elastic Load Balancer connection draining.
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLifecycleEventConfiguration :: Lens.Lens' CreateLayer (Lude.Maybe LifecycleEventConfiguration)
clLifecycleEventConfiguration = Lens.lens (lifecycleEventConfiguration :: CreateLayer -> Lude.Maybe LifecycleEventConfiguration) (\s a -> s {lifecycleEventConfiguration = a} :: CreateLayer)
{-# DEPRECATED clLifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead." #-}

-- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomRecipes :: Lens.Lens' CreateLayer (Lude.Maybe Recipes)
clCustomRecipes = Lens.lens (customRecipes :: CreateLayer -> Lude.Maybe Recipes) (\s a -> s {customRecipes = a} :: CreateLayer)
{-# DEPRECATED clCustomRecipes "Use generic-lens or generic-optics with 'customRecipes' instead." #-}

-- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> . This feature is supported as of version 1.7.42 of the AWS CLI.
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clCustomJSON :: Lens.Lens' CreateLayer (Lude.Maybe Lude.Text)
clCustomJSON = Lens.lens (customJSON :: CreateLayer -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: CreateLayer)
{-# DEPRECATED clCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clVolumeConfigurations :: Lens.Lens' CreateLayer (Lude.Maybe [VolumeConfiguration])
clVolumeConfigurations = Lens.lens (volumeConfigurations :: CreateLayer -> Lude.Maybe [VolumeConfiguration]) (\s a -> s {volumeConfigurations = a} :: CreateLayer)
{-# DEPRECATED clVolumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead." #-}

-- | Whether to disable auto healing for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clEnableAutoHealing :: Lens.Lens' CreateLayer (Lude.Maybe Lude.Bool)
clEnableAutoHealing = Lens.lens (enableAutoHealing :: CreateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {enableAutoHealing = a} :: CreateLayer)
{-# DEPRECATED clEnableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead." #-}

-- | An array of @Package@ objects that describes the layer packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clPackages :: Lens.Lens' CreateLayer (Lude.Maybe [Lude.Text])
clPackages = Lens.lens (packages :: CreateLayer -> Lude.Maybe [Lude.Text]) (\s a -> s {packages = a} :: CreateLayer)
{-# DEPRECATED clPackages "Use generic-lens or generic-optics with 'packages' instead." #-}

-- | One or more user-defined key-value pairs to be added to the stack attributes.
--
-- To create a cluster layer, set the @EcsClusterArn@ attribute to the cluster's ARN.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAttributes :: Lens.Lens' CreateLayer (Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text)))
clAttributes = Lens.lens (attributes :: CreateLayer -> Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: CreateLayer)
{-# DEPRECATED clAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAutoAssignPublicIPs :: Lens.Lens' CreateLayer (Lude.Maybe Lude.Bool)
clAutoAssignPublicIPs = Lens.lens (autoAssignPublicIPs :: CreateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {autoAssignPublicIPs = a} :: CreateLayer)
{-# DEPRECATED clAutoAssignPublicIPs "Use generic-lens or generic-optics with 'autoAssignPublicIPs' instead." #-}

-- | Whether to use Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEBSOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clUseEBSOptimizedInstances :: Lens.Lens' CreateLayer (Lude.Maybe Lude.Bool)
clUseEBSOptimizedInstances = Lens.lens (useEBSOptimizedInstances :: CreateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {useEBSOptimizedInstances = a} :: CreateLayer)
{-# DEPRECATED clUseEBSOptimizedInstances "Use generic-lens or generic-optics with 'useEBSOptimizedInstances' instead." #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAutoAssignElasticIPs :: Lens.Lens' CreateLayer (Lude.Maybe Lude.Bool)
clAutoAssignElasticIPs = Lens.lens (autoAssignElasticIPs :: CreateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {autoAssignElasticIPs = a} :: CreateLayer)
{-# DEPRECATED clAutoAssignElasticIPs "Use generic-lens or generic-optics with 'autoAssignElasticIPs' instead." #-}

-- | The layer stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clStackId :: Lens.Lens' CreateLayer Lude.Text
clStackId = Lens.lens (stackId :: CreateLayer -> Lude.Text) (\s a -> s {stackId = a} :: CreateLayer)
{-# DEPRECATED clStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The layer type. A stack cannot have more than one built-in layer of the same type. It can have any number of custom layers. Built-in layers are not available in Chef 12 stacks.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clType :: Lens.Lens' CreateLayer LayerType
clType = Lens.lens (type' :: CreateLayer -> LayerType) (\s a -> s {type' = a} :: CreateLayer)
{-# DEPRECATED clType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The layer name, which is used by the console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clName :: Lens.Lens' CreateLayer Lude.Text
clName = Lens.lens (name :: CreateLayer -> Lude.Text) (\s a -> s {name = a} :: CreateLayer)
{-# DEPRECATED clName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef recipes. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters, which are limited to the alphanumeric characters, '-', '_', and '.'.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference> .
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clShortname :: Lens.Lens' CreateLayer Lude.Text
clShortname = Lens.lens (shortname :: CreateLayer -> Lude.Text) (\s a -> s {shortname = a} :: CreateLayer)
{-# DEPRECATED clShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

instance Lude.AWSRequest CreateLayer where
  type Rs CreateLayer = CreateLayerResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLayerResponse'
            Lude.<$> (x Lude..?> "LayerId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLayer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.CreateLayer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLayer where
  toJSON CreateLayer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CustomInstanceProfileArn" Lude..=)
              Lude.<$> customInstanceProfileARN,
            ("CustomSecurityGroupIds" Lude..=) Lude.<$> customSecurityGroupIds,
            ("InstallUpdatesOnBoot" Lude..=) Lude.<$> installUpdatesOnBoot,
            ("CloudWatchLogsConfiguration" Lude..=)
              Lude.<$> cloudWatchLogsConfiguration,
            ("LifecycleEventConfiguration" Lude..=)
              Lude.<$> lifecycleEventConfiguration,
            ("CustomRecipes" Lude..=) Lude.<$> customRecipes,
            ("CustomJson" Lude..=) Lude.<$> customJSON,
            ("VolumeConfigurations" Lude..=) Lude.<$> volumeConfigurations,
            ("EnableAutoHealing" Lude..=) Lude.<$> enableAutoHealing,
            ("Packages" Lude..=) Lude.<$> packages,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("AutoAssignPublicIps" Lude..=) Lude.<$> autoAssignPublicIPs,
            ("UseEbsOptimizedInstances" Lude..=)
              Lude.<$> useEBSOptimizedInstances,
            ("AutoAssignElasticIps" Lude..=) Lude.<$> autoAssignElasticIPs,
            Lude.Just ("StackId" Lude..= stackId),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Shortname" Lude..= shortname)
          ]
      )

instance Lude.ToPath CreateLayer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLayer where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @CreateLayer@ request.
--
-- /See:/ 'mkCreateLayerResponse' smart constructor.
data CreateLayerResponse = CreateLayerResponse'
  { layerId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLayerResponse' with the minimum fields required to make a request.
--
-- * 'layerId' - The layer ID.
-- * 'responseStatus' - The response status code.
mkCreateLayerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLayerResponse
mkCreateLayerResponse pResponseStatus_ =
  CreateLayerResponse'
    { layerId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsLayerId :: Lens.Lens' CreateLayerResponse (Lude.Maybe Lude.Text)
clrsLayerId = Lens.lens (layerId :: CreateLayerResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerId = a} :: CreateLayerResponse)
{-# DEPRECATED clrsLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsResponseStatus :: Lens.Lens' CreateLayerResponse Lude.Int
clrsResponseStatus = Lens.lens (responseStatus :: CreateLayerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLayerResponse)
{-# DEPRECATED clrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
