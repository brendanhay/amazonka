{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified layer.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UpdateLayer
  ( -- * Creating a Request
    UpdateLayer (..),
    newUpdateLayer,

    -- * Request Lenses
    updateLayer_installUpdatesOnBoot,
    updateLayer_customInstanceProfileArn,
    updateLayer_customSecurityGroupIds,
    updateLayer_packages,
    updateLayer_enableAutoHealing,
    updateLayer_volumeConfigurations,
    updateLayer_customJson,
    updateLayer_shortname,
    updateLayer_attributes,
    updateLayer_name,
    updateLayer_cloudWatchLogsConfiguration,
    updateLayer_autoAssignElasticIps,
    updateLayer_useEbsOptimizedInstances,
    updateLayer_customRecipes,
    updateLayer_autoAssignPublicIps,
    updateLayer_lifecycleEventConfiguration,
    updateLayer_layerId,

    -- * Destructuring the Response
    UpdateLayerResponse (..),
    newUpdateLayerResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLayer' smart constructor.
data UpdateLayer = UpdateLayer'
  { -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. To control when updates are
    -- installed, set this value to @false@. You must then update your
    -- instances manually by using CreateDeployment to run the
    -- @update_dependencies@ stack command or manually running @yum@ (Amazon
    -- Linux) or @apt-get@ (Ubuntu) on the instances.
    --
    -- We strongly recommend using the default value of @true@, to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | The ARN of an IAM profile to be used for all of the layer\'s EC2
    -- instances. For more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    customInstanceProfileArn :: Core.Maybe Core.Text,
    -- | An array containing the layer\'s custom security group IDs.
    customSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | An array of @Package@ objects that describe the layer\'s packages.
    packages :: Core.Maybe [Core.Text],
    -- | Whether to disable auto healing for the layer.
    enableAutoHealing :: Core.Maybe Core.Bool,
    -- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
    -- volumes.
    volumeConfigurations :: Core.Maybe [VolumeConfiguration],
    -- | A JSON-formatted string containing custom stack configuration and
    -- deployment attributes to be installed on the layer\'s instances. For
    -- more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
    customJson :: Core.Maybe Core.Text,
    -- | For custom layers only, use this parameter to specify the layer\'s short
    -- name, which is used internally by AWS OpsWorks Stacks and by Chef. The
    -- short name is also used as the name for the directory where your app
    -- files are installed. It can have a maximum of 200 characters and must be
    -- in the following format: \/\\A[a-z0-9\\-\\_\\.]+\\Z\/.
    --
    -- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
    -- For more information, see the
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
    shortname :: Core.Maybe Core.Text,
    -- | One or more user-defined key\/value pairs to be added to the stack
    -- attributes.
    attributes :: Core.Maybe (Core.HashMap LayerAttributesKeys (Core.Maybe Core.Text)),
    -- | The layer name, which is used by the console.
    name :: Core.Maybe Core.Text,
    -- | Specifies CloudWatch Logs configuration options for the layer. For more
    -- information, see CloudWatchLogsLogStream.
    cloudWatchLogsConfiguration :: Core.Maybe CloudWatchLogsConfiguration,
    -- | Whether to automatically assign an
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignElasticIps :: Core.Maybe Core.Bool,
    -- | Whether to use Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Core.Maybe Core.Bool,
    -- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
    -- recipes.
    customRecipes :: Core.Maybe Recipes,
    -- | For stacks that are running in a VPC, whether to automatically assign a
    -- public IP address to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignPublicIps :: Core.Maybe Core.Bool,
    lifecycleEventConfiguration :: Core.Maybe LifecycleEventConfiguration,
    -- | The layer ID.
    layerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateLayer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'installUpdatesOnBoot', 'updateLayer_installUpdatesOnBoot' - Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
--
-- 'customInstanceProfileArn', 'updateLayer_customInstanceProfileArn' - The ARN of an IAM profile to be used for all of the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'customSecurityGroupIds', 'updateLayer_customSecurityGroupIds' - An array containing the layer\'s custom security group IDs.
--
-- 'packages', 'updateLayer_packages' - An array of @Package@ objects that describe the layer\'s packages.
--
-- 'enableAutoHealing', 'updateLayer_enableAutoHealing' - Whether to disable auto healing for the layer.
--
-- 'volumeConfigurations', 'updateLayer_volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
--
-- 'customJson', 'updateLayer_customJson' - A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
--
-- 'shortname', 'updateLayer_shortname' - For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorks Stacks and by Chef. The
-- short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be
-- in the following format: \/\\A[a-z0-9\\-\\_\\.]+\\Z\/.
--
-- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
-- For more information, see the
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
--
-- 'attributes', 'updateLayer_attributes' - One or more user-defined key\/value pairs to be added to the stack
-- attributes.
--
-- 'name', 'updateLayer_name' - The layer name, which is used by the console.
--
-- 'cloudWatchLogsConfiguration', 'updateLayer_cloudWatchLogsConfiguration' - Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
--
-- 'autoAssignElasticIps', 'updateLayer_autoAssignElasticIps' - Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'useEbsOptimizedInstances', 'updateLayer_useEbsOptimizedInstances' - Whether to use Amazon EBS-optimized instances.
--
-- 'customRecipes', 'updateLayer_customRecipes' - A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
--
-- 'autoAssignPublicIps', 'updateLayer_autoAssignPublicIps' - For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'lifecycleEventConfiguration', 'updateLayer_lifecycleEventConfiguration' -
--
-- 'layerId', 'updateLayer_layerId' - The layer ID.
newUpdateLayer ::
  -- | 'layerId'
  Core.Text ->
  UpdateLayer
newUpdateLayer pLayerId_ =
  UpdateLayer'
    { installUpdatesOnBoot = Core.Nothing,
      customInstanceProfileArn = Core.Nothing,
      customSecurityGroupIds = Core.Nothing,
      packages = Core.Nothing,
      enableAutoHealing = Core.Nothing,
      volumeConfigurations = Core.Nothing,
      customJson = Core.Nothing,
      shortname = Core.Nothing,
      attributes = Core.Nothing,
      name = Core.Nothing,
      cloudWatchLogsConfiguration = Core.Nothing,
      autoAssignElasticIps = Core.Nothing,
      useEbsOptimizedInstances = Core.Nothing,
      customRecipes = Core.Nothing,
      autoAssignPublicIps = Core.Nothing,
      lifecycleEventConfiguration = Core.Nothing,
      layerId = pLayerId_
    }

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
updateLayer_installUpdatesOnBoot :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
updateLayer_installUpdatesOnBoot = Lens.lens (\UpdateLayer' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@UpdateLayer' {} a -> s {installUpdatesOnBoot = a} :: UpdateLayer)

-- | The ARN of an IAM profile to be used for all of the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
updateLayer_customInstanceProfileArn :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
updateLayer_customInstanceProfileArn = Lens.lens (\UpdateLayer' {customInstanceProfileArn} -> customInstanceProfileArn) (\s@UpdateLayer' {} a -> s {customInstanceProfileArn = a} :: UpdateLayer)

-- | An array containing the layer\'s custom security group IDs.
updateLayer_customSecurityGroupIds :: Lens.Lens' UpdateLayer (Core.Maybe [Core.Text])
updateLayer_customSecurityGroupIds = Lens.lens (\UpdateLayer' {customSecurityGroupIds} -> customSecurityGroupIds) (\s@UpdateLayer' {} a -> s {customSecurityGroupIds = a} :: UpdateLayer) Core.. Lens.mapping Lens._Coerce

-- | An array of @Package@ objects that describe the layer\'s packages.
updateLayer_packages :: Lens.Lens' UpdateLayer (Core.Maybe [Core.Text])
updateLayer_packages = Lens.lens (\UpdateLayer' {packages} -> packages) (\s@UpdateLayer' {} a -> s {packages = a} :: UpdateLayer) Core.. Lens.mapping Lens._Coerce

-- | Whether to disable auto healing for the layer.
updateLayer_enableAutoHealing :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
updateLayer_enableAutoHealing = Lens.lens (\UpdateLayer' {enableAutoHealing} -> enableAutoHealing) (\s@UpdateLayer' {} a -> s {enableAutoHealing = a} :: UpdateLayer)

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
updateLayer_volumeConfigurations :: Lens.Lens' UpdateLayer (Core.Maybe [VolumeConfiguration])
updateLayer_volumeConfigurations = Lens.lens (\UpdateLayer' {volumeConfigurations} -> volumeConfigurations) (\s@UpdateLayer' {} a -> s {volumeConfigurations = a} :: UpdateLayer) Core.. Lens.mapping Lens._Coerce

-- | A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
updateLayer_customJson :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
updateLayer_customJson = Lens.lens (\UpdateLayer' {customJson} -> customJson) (\s@UpdateLayer' {} a -> s {customJson = a} :: UpdateLayer)

-- | For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorks Stacks and by Chef. The
-- short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be
-- in the following format: \/\\A[a-z0-9\\-\\_\\.]+\\Z\/.
--
-- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
-- For more information, see the
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
updateLayer_shortname :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
updateLayer_shortname = Lens.lens (\UpdateLayer' {shortname} -> shortname) (\s@UpdateLayer' {} a -> s {shortname = a} :: UpdateLayer)

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
updateLayer_attributes :: Lens.Lens' UpdateLayer (Core.Maybe (Core.HashMap LayerAttributesKeys (Core.Maybe Core.Text)))
updateLayer_attributes = Lens.lens (\UpdateLayer' {attributes} -> attributes) (\s@UpdateLayer' {} a -> s {attributes = a} :: UpdateLayer) Core.. Lens.mapping Lens._Coerce

-- | The layer name, which is used by the console.
updateLayer_name :: Lens.Lens' UpdateLayer (Core.Maybe Core.Text)
updateLayer_name = Lens.lens (\UpdateLayer' {name} -> name) (\s@UpdateLayer' {} a -> s {name = a} :: UpdateLayer)

-- | Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
updateLayer_cloudWatchLogsConfiguration :: Lens.Lens' UpdateLayer (Core.Maybe CloudWatchLogsConfiguration)
updateLayer_cloudWatchLogsConfiguration = Lens.lens (\UpdateLayer' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@UpdateLayer' {} a -> s {cloudWatchLogsConfiguration = a} :: UpdateLayer)

-- | Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
updateLayer_autoAssignElasticIps :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
updateLayer_autoAssignElasticIps = Lens.lens (\UpdateLayer' {autoAssignElasticIps} -> autoAssignElasticIps) (\s@UpdateLayer' {} a -> s {autoAssignElasticIps = a} :: UpdateLayer)

-- | Whether to use Amazon EBS-optimized instances.
updateLayer_useEbsOptimizedInstances :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
updateLayer_useEbsOptimizedInstances = Lens.lens (\UpdateLayer' {useEbsOptimizedInstances} -> useEbsOptimizedInstances) (\s@UpdateLayer' {} a -> s {useEbsOptimizedInstances = a} :: UpdateLayer)

-- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
updateLayer_customRecipes :: Lens.Lens' UpdateLayer (Core.Maybe Recipes)
updateLayer_customRecipes = Lens.lens (\UpdateLayer' {customRecipes} -> customRecipes) (\s@UpdateLayer' {} a -> s {customRecipes = a} :: UpdateLayer)

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
updateLayer_autoAssignPublicIps :: Lens.Lens' UpdateLayer (Core.Maybe Core.Bool)
updateLayer_autoAssignPublicIps = Lens.lens (\UpdateLayer' {autoAssignPublicIps} -> autoAssignPublicIps) (\s@UpdateLayer' {} a -> s {autoAssignPublicIps = a} :: UpdateLayer)

-- |
updateLayer_lifecycleEventConfiguration :: Lens.Lens' UpdateLayer (Core.Maybe LifecycleEventConfiguration)
updateLayer_lifecycleEventConfiguration = Lens.lens (\UpdateLayer' {lifecycleEventConfiguration} -> lifecycleEventConfiguration) (\s@UpdateLayer' {} a -> s {lifecycleEventConfiguration = a} :: UpdateLayer)

-- | The layer ID.
updateLayer_layerId :: Lens.Lens' UpdateLayer Core.Text
updateLayer_layerId = Lens.lens (\UpdateLayer' {layerId} -> layerId) (\s@UpdateLayer' {} a -> s {layerId = a} :: UpdateLayer)

instance Core.AWSRequest UpdateLayer where
  type AWSResponse UpdateLayer = UpdateLayerResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull UpdateLayerResponse'

instance Core.Hashable UpdateLayer

instance Core.NFData UpdateLayer

instance Core.ToHeaders UpdateLayer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OpsWorks_20130218.UpdateLayer" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateLayer where
  toJSON UpdateLayer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstallUpdatesOnBoot" Core..=)
              Core.<$> installUpdatesOnBoot,
            ("CustomInstanceProfileArn" Core..=)
              Core.<$> customInstanceProfileArn,
            ("CustomSecurityGroupIds" Core..=)
              Core.<$> customSecurityGroupIds,
            ("Packages" Core..=) Core.<$> packages,
            ("EnableAutoHealing" Core..=)
              Core.<$> enableAutoHealing,
            ("VolumeConfigurations" Core..=)
              Core.<$> volumeConfigurations,
            ("CustomJson" Core..=) Core.<$> customJson,
            ("Shortname" Core..=) Core.<$> shortname,
            ("Attributes" Core..=) Core.<$> attributes,
            ("Name" Core..=) Core.<$> name,
            ("CloudWatchLogsConfiguration" Core..=)
              Core.<$> cloudWatchLogsConfiguration,
            ("AutoAssignElasticIps" Core..=)
              Core.<$> autoAssignElasticIps,
            ("UseEbsOptimizedInstances" Core..=)
              Core.<$> useEbsOptimizedInstances,
            ("CustomRecipes" Core..=) Core.<$> customRecipes,
            ("AutoAssignPublicIps" Core..=)
              Core.<$> autoAssignPublicIps,
            ("LifecycleEventConfiguration" Core..=)
              Core.<$> lifecycleEventConfiguration,
            Core.Just ("LayerId" Core..= layerId)
          ]
      )

instance Core.ToPath UpdateLayer where
  toPath = Core.const "/"

instance Core.ToQuery UpdateLayer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateLayerResponse' smart constructor.
data UpdateLayerResponse = UpdateLayerResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateLayerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateLayerResponse ::
  UpdateLayerResponse
newUpdateLayerResponse = UpdateLayerResponse'

instance Core.NFData UpdateLayerResponse
