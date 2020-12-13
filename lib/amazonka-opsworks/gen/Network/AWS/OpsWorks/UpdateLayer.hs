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
    ulCustomInstanceProfileARN,
    ulCustomSecurityGroupIds,
    ulInstallUpdatesOnBoot,
    ulCloudWatchLogsConfiguration,
    ulLifecycleEventConfiguration,
    ulShortname,
    ulCustomRecipes,
    ulCustomJSON,
    ulVolumeConfigurations,
    ulEnableAutoHealing,
    ulPackages,
    ulAttributes,
    ulName,
    ulAutoAssignPublicIPs,
    ulUseEBSOptimizedInstances,
    ulLayerId,
    ulAutoAssignElasticIPs,

    -- * Destructuring the response
    UpdateLayerResponse (..),
    mkUpdateLayerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateLayer' smart constructor.
data UpdateLayer = UpdateLayer'
  { -- | The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    customInstanceProfileARN :: Lude.Maybe Lude.Text,
    -- | An array containing the layer's custom security group IDs.
    customSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Lude.Maybe Lude.Bool,
    -- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
    cloudWatchLogsConfiguration :: Lude.Maybe CloudWatchLogsConfiguration,
    -- |
    lifecycleEventConfiguration :: Lude.Maybe LifecycleEventConfiguration,
    -- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
    --
    -- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
    shortname :: Lude.Maybe Lude.Text,
    -- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
    customRecipes :: Lude.Maybe Recipes,
    -- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> .
    customJSON :: Lude.Maybe Lude.Text,
    -- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
    volumeConfigurations :: Lude.Maybe [VolumeConfiguration],
    -- | Whether to disable auto healing for the layer.
    enableAutoHealing :: Lude.Maybe Lude.Bool,
    -- | An array of @Package@ objects that describe the layer's packages.
    packages :: Lude.Maybe [Lude.Text],
    -- | One or more user-defined key/value pairs to be added to the stack attributes.
    attributes :: Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text)),
    -- | The layer name, which is used by the console.
    name :: Lude.Maybe Lude.Text,
    -- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignPublicIPs :: Lude.Maybe Lude.Bool,
    -- | Whether to use Amazon EBS-optimized instances.
    useEBSOptimizedInstances :: Lude.Maybe Lude.Bool,
    -- | The layer ID.
    layerId :: Lude.Text,
    -- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
    autoAssignElasticIPs :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLayer' with the minimum fields required to make a request.
--
-- * 'customInstanceProfileARN' - The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'customSecurityGroupIds' - An array containing the layer's custom security group IDs.
-- * 'installUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
-- * 'cloudWatchLogsConfiguration' - Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
-- * 'lifecycleEventConfiguration' -
-- * 'shortname' - For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
-- * 'customRecipes' - A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
-- * 'customJSON' - A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> .
-- * 'volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
-- * 'enableAutoHealing' - Whether to disable auto healing for the layer.
-- * 'packages' - An array of @Package@ objects that describe the layer's packages.
-- * 'attributes' - One or more user-defined key/value pairs to be added to the stack attributes.
-- * 'name' - The layer name, which is used by the console.
-- * 'autoAssignPublicIPs' - For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
-- * 'useEBSOptimizedInstances' - Whether to use Amazon EBS-optimized instances.
-- * 'layerId' - The layer ID.
-- * 'autoAssignElasticIPs' - Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
mkUpdateLayer ::
  -- | 'layerId'
  Lude.Text ->
  UpdateLayer
mkUpdateLayer pLayerId_ =
  UpdateLayer'
    { customInstanceProfileARN = Lude.Nothing,
      customSecurityGroupIds = Lude.Nothing,
      installUpdatesOnBoot = Lude.Nothing,
      cloudWatchLogsConfiguration = Lude.Nothing,
      lifecycleEventConfiguration = Lude.Nothing,
      shortname = Lude.Nothing,
      customRecipes = Lude.Nothing,
      customJSON = Lude.Nothing,
      volumeConfigurations = Lude.Nothing,
      enableAutoHealing = Lude.Nothing,
      packages = Lude.Nothing,
      attributes = Lude.Nothing,
      name = Lude.Nothing,
      autoAssignPublicIPs = Lude.Nothing,
      useEBSOptimizedInstances = Lude.Nothing,
      layerId = pLayerId_,
      autoAssignElasticIPs = Lude.Nothing
    }

-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'customInstanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomInstanceProfileARN :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Text)
ulCustomInstanceProfileARN = Lens.lens (customInstanceProfileARN :: UpdateLayer -> Lude.Maybe Lude.Text) (\s a -> s {customInstanceProfileARN = a} :: UpdateLayer)
{-# DEPRECATED ulCustomInstanceProfileARN "Use generic-lens or generic-optics with 'customInstanceProfileARN' instead." #-}

-- | An array containing the layer's custom security group IDs.
--
-- /Note:/ Consider using 'customSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomSecurityGroupIds :: Lens.Lens' UpdateLayer (Lude.Maybe [Lude.Text])
ulCustomSecurityGroupIds = Lens.lens (customSecurityGroupIds :: UpdateLayer -> Lude.Maybe [Lude.Text]) (\s a -> s {customSecurityGroupIds = a} :: UpdateLayer)
{-# DEPRECATED ulCustomSecurityGroupIds "Use generic-lens or generic-optics with 'customSecurityGroupIds' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulInstallUpdatesOnBoot :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Bool)
ulInstallUpdatesOnBoot = Lens.lens (installUpdatesOnBoot :: UpdateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {installUpdatesOnBoot = a} :: UpdateLayer)
{-# DEPRECATED ulInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
--
-- /Note:/ Consider using 'cloudWatchLogsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCloudWatchLogsConfiguration :: Lens.Lens' UpdateLayer (Lude.Maybe CloudWatchLogsConfiguration)
ulCloudWatchLogsConfiguration = Lens.lens (cloudWatchLogsConfiguration :: UpdateLayer -> Lude.Maybe CloudWatchLogsConfiguration) (\s a -> s {cloudWatchLogsConfiguration = a} :: UpdateLayer)
{-# DEPRECATED ulCloudWatchLogsConfiguration "Use generic-lens or generic-optics with 'cloudWatchLogsConfiguration' instead." #-}

-- |
--
-- /Note:/ Consider using 'lifecycleEventConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLifecycleEventConfiguration :: Lens.Lens' UpdateLayer (Lude.Maybe LifecycleEventConfiguration)
ulLifecycleEventConfiguration = Lens.lens (lifecycleEventConfiguration :: UpdateLayer -> Lude.Maybe LifecycleEventConfiguration) (\s a -> s {lifecycleEventConfiguration = a} :: UpdateLayer)
{-# DEPRECATED ulLifecycleEventConfiguration "Use generic-lens or generic-optics with 'lifecycleEventConfiguration' instead." #-}

-- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/.
--
-- The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulShortname :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Text)
ulShortname = Lens.lens (shortname :: UpdateLayer -> Lude.Maybe Lude.Text) (\s a -> s {shortname = a} :: UpdateLayer)
{-# DEPRECATED ulShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- /Note:/ Consider using 'customRecipes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomRecipes :: Lens.Lens' UpdateLayer (Lude.Maybe Recipes)
ulCustomRecipes = Lens.lens (customRecipes :: UpdateLayer -> Lude.Maybe Recipes) (\s a -> s {customRecipes = a} :: UpdateLayer)
{-# DEPRECATED ulCustomRecipes "Use generic-lens or generic-optics with 'customRecipes' instead." #-}

-- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> .
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulCustomJSON :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Text)
ulCustomJSON = Lens.lens (customJSON :: UpdateLayer -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: UpdateLayer)
{-# DEPRECATED ulCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- /Note:/ Consider using 'volumeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulVolumeConfigurations :: Lens.Lens' UpdateLayer (Lude.Maybe [VolumeConfiguration])
ulVolumeConfigurations = Lens.lens (volumeConfigurations :: UpdateLayer -> Lude.Maybe [VolumeConfiguration]) (\s a -> s {volumeConfigurations = a} :: UpdateLayer)
{-# DEPRECATED ulVolumeConfigurations "Use generic-lens or generic-optics with 'volumeConfigurations' instead." #-}

-- | Whether to disable auto healing for the layer.
--
-- /Note:/ Consider using 'enableAutoHealing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulEnableAutoHealing :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Bool)
ulEnableAutoHealing = Lens.lens (enableAutoHealing :: UpdateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {enableAutoHealing = a} :: UpdateLayer)
{-# DEPRECATED ulEnableAutoHealing "Use generic-lens or generic-optics with 'enableAutoHealing' instead." #-}

-- | An array of @Package@ objects that describe the layer's packages.
--
-- /Note:/ Consider using 'packages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulPackages :: Lens.Lens' UpdateLayer (Lude.Maybe [Lude.Text])
ulPackages = Lens.lens (packages :: UpdateLayer -> Lude.Maybe [Lude.Text]) (\s a -> s {packages = a} :: UpdateLayer)
{-# DEPRECATED ulPackages "Use generic-lens or generic-optics with 'packages' instead." #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAttributes :: Lens.Lens' UpdateLayer (Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text)))
ulAttributes = Lens.lens (attributes :: UpdateLayer -> Lude.Maybe (Lude.HashMap LayerAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: UpdateLayer)
{-# DEPRECATED ulAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The layer name, which is used by the console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulName :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Text)
ulName = Lens.lens (name :: UpdateLayer -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateLayer)
{-# DEPRECATED ulName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignPublicIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAutoAssignPublicIPs :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Bool)
ulAutoAssignPublicIPs = Lens.lens (autoAssignPublicIPs :: UpdateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {autoAssignPublicIPs = a} :: UpdateLayer)
{-# DEPRECATED ulAutoAssignPublicIPs "Use generic-lens or generic-optics with 'autoAssignPublicIPs' instead." #-}

-- | Whether to use Amazon EBS-optimized instances.
--
-- /Note:/ Consider using 'useEBSOptimizedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulUseEBSOptimizedInstances :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Bool)
ulUseEBSOptimizedInstances = Lens.lens (useEBSOptimizedInstances :: UpdateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {useEBSOptimizedInstances = a} :: UpdateLayer)
{-# DEPRECATED ulUseEBSOptimizedInstances "Use generic-lens or generic-optics with 'useEBSOptimizedInstances' instead." #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLayerId :: Lens.Lens' UpdateLayer Lude.Text
ulLayerId = Lens.lens (layerId :: UpdateLayer -> Lude.Text) (\s a -> s {layerId = a} :: UpdateLayer)
{-# DEPRECATED ulLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | Whether to automatically assign an <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- /Note:/ Consider using 'autoAssignElasticIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAutoAssignElasticIPs :: Lens.Lens' UpdateLayer (Lude.Maybe Lude.Bool)
ulAutoAssignElasticIPs = Lens.lens (autoAssignElasticIPs :: UpdateLayer -> Lude.Maybe Lude.Bool) (\s a -> s {autoAssignElasticIPs = a} :: UpdateLayer)
{-# DEPRECATED ulAutoAssignElasticIPs "Use generic-lens or generic-optics with 'autoAssignElasticIPs' instead." #-}

instance Lude.AWSRequest UpdateLayer where
  type Rs UpdateLayer = UpdateLayerResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateLayerResponse'

instance Lude.ToHeaders UpdateLayer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateLayer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateLayer where
  toJSON UpdateLayer' {..} =
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
            ("Shortname" Lude..=) Lude.<$> shortname,
            ("CustomRecipes" Lude..=) Lude.<$> customRecipes,
            ("CustomJson" Lude..=) Lude.<$> customJSON,
            ("VolumeConfigurations" Lude..=) Lude.<$> volumeConfigurations,
            ("EnableAutoHealing" Lude..=) Lude.<$> enableAutoHealing,
            ("Packages" Lude..=) Lude.<$> packages,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("Name" Lude..=) Lude.<$> name,
            ("AutoAssignPublicIps" Lude..=) Lude.<$> autoAssignPublicIPs,
            ("UseEbsOptimizedInstances" Lude..=)
              Lude.<$> useEBSOptimizedInstances,
            Lude.Just ("LayerId" Lude..= layerId),
            ("AutoAssignElasticIps" Lude..=) Lude.<$> autoAssignElasticIPs
          ]
      )

instance Lude.ToPath UpdateLayer where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateLayer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateLayerResponse' smart constructor.
data UpdateLayerResponse = UpdateLayerResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLayerResponse' with the minimum fields required to make a request.
mkUpdateLayerResponse ::
  UpdateLayerResponse
mkUpdateLayerResponse = UpdateLayerResponse'
