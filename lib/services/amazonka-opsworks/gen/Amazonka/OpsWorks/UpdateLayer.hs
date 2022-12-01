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
-- Module      : Amazonka.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorks.UpdateLayer
  ( -- * Creating a Request
    UpdateLayer (..),
    newUpdateLayer,

    -- * Request Lenses
    updateLayer_name,
    updateLayer_customRecipes,
    updateLayer_autoAssignPublicIps,
    updateLayer_customJson,
    updateLayer_packages,
    updateLayer_volumeConfigurations,
    updateLayer_enableAutoHealing,
    updateLayer_customSecurityGroupIds,
    updateLayer_autoAssignElasticIps,
    updateLayer_cloudWatchLogsConfiguration,
    updateLayer_lifecycleEventConfiguration,
    updateLayer_customInstanceProfileArn,
    updateLayer_attributes,
    updateLayer_shortname,
    updateLayer_useEbsOptimizedInstances,
    updateLayer_installUpdatesOnBoot,
    updateLayer_layerId,

    -- * Destructuring the Response
    UpdateLayerResponse (..),
    newUpdateLayerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLayer' smart constructor.
data UpdateLayer = UpdateLayer'
  { -- | The layer name, which is used by the console.
    name :: Prelude.Maybe Prelude.Text,
    -- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
    -- recipes.
    customRecipes :: Prelude.Maybe Recipes,
    -- | For stacks that are running in a VPC, whether to automatically assign a
    -- public IP address to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignPublicIps :: Prelude.Maybe Prelude.Bool,
    -- | A JSON-formatted string containing custom stack configuration and
    -- deployment attributes to be installed on the layer\'s instances. For
    -- more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
    customJson :: Prelude.Maybe Prelude.Text,
    -- | An array of @Package@ objects that describe the layer\'s packages.
    packages :: Prelude.Maybe [Prelude.Text],
    -- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
    -- volumes.
    volumeConfigurations :: Prelude.Maybe [VolumeConfiguration],
    -- | Whether to disable auto healing for the layer.
    enableAutoHealing :: Prelude.Maybe Prelude.Bool,
    -- | An array containing the layer\'s custom security group IDs.
    customSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Whether to automatically assign an
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignElasticIps :: Prelude.Maybe Prelude.Bool,
    -- | Specifies CloudWatch Logs configuration options for the layer. For more
    -- information, see CloudWatchLogsLogStream.
    cloudWatchLogsConfiguration :: Prelude.Maybe CloudWatchLogsConfiguration,
    lifecycleEventConfiguration :: Prelude.Maybe LifecycleEventConfiguration,
    -- | The ARN of an IAM profile to be used for all of the layer\'s EC2
    -- instances. For more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    customInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | One or more user-defined key\/value pairs to be added to the stack
    -- attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap LayerAttributesKeys (Prelude.Maybe Prelude.Text)),
    -- | For custom layers only, use this parameter to specify the layer\'s short
    -- name, which is used internally by AWS OpsWorks Stacks and by Chef. The
    -- short name is also used as the name for the directory where your app
    -- files are installed. It can have a maximum of 200 characters and must be
    -- in the following format: \/\\A[a-z0-9\\-\\_\\.]+\\Z\/.
    --
    -- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
    -- For more information, see the
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
    shortname :: Prelude.Maybe Prelude.Text,
    -- | Whether to use Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Prelude.Maybe Prelude.Bool,
    -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. To control when updates are
    -- installed, set this value to @false@. You must then update your
    -- instances manually by using CreateDeployment to run the
    -- @update_dependencies@ stack command or manually running @yum@ (Amazon
    -- Linux) or @apt-get@ (Ubuntu) on the instances.
    --
    -- We strongly recommend using the default value of @true@, to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Prelude.Maybe Prelude.Bool,
    -- | The layer ID.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLayer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateLayer_name' - The layer name, which is used by the console.
--
-- 'customRecipes', 'updateLayer_customRecipes' - A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
--
-- 'autoAssignPublicIps', 'updateLayer_autoAssignPublicIps' - For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'customJson', 'updateLayer_customJson' - A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
--
-- 'packages', 'updateLayer_packages' - An array of @Package@ objects that describe the layer\'s packages.
--
-- 'volumeConfigurations', 'updateLayer_volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
--
-- 'enableAutoHealing', 'updateLayer_enableAutoHealing' - Whether to disable auto healing for the layer.
--
-- 'customSecurityGroupIds', 'updateLayer_customSecurityGroupIds' - An array containing the layer\'s custom security group IDs.
--
-- 'autoAssignElasticIps', 'updateLayer_autoAssignElasticIps' - Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'cloudWatchLogsConfiguration', 'updateLayer_cloudWatchLogsConfiguration' - Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
--
-- 'lifecycleEventConfiguration', 'updateLayer_lifecycleEventConfiguration' -
--
-- 'customInstanceProfileArn', 'updateLayer_customInstanceProfileArn' - The ARN of an IAM profile to be used for all of the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'attributes', 'updateLayer_attributes' - One or more user-defined key\/value pairs to be added to the stack
-- attributes.
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
-- 'useEbsOptimizedInstances', 'updateLayer_useEbsOptimizedInstances' - Whether to use Amazon EBS-optimized instances.
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
-- 'layerId', 'updateLayer_layerId' - The layer ID.
newUpdateLayer ::
  -- | 'layerId'
  Prelude.Text ->
  UpdateLayer
newUpdateLayer pLayerId_ =
  UpdateLayer'
    { name = Prelude.Nothing,
      customRecipes = Prelude.Nothing,
      autoAssignPublicIps = Prelude.Nothing,
      customJson = Prelude.Nothing,
      packages = Prelude.Nothing,
      volumeConfigurations = Prelude.Nothing,
      enableAutoHealing = Prelude.Nothing,
      customSecurityGroupIds = Prelude.Nothing,
      autoAssignElasticIps = Prelude.Nothing,
      cloudWatchLogsConfiguration = Prelude.Nothing,
      lifecycleEventConfiguration = Prelude.Nothing,
      customInstanceProfileArn = Prelude.Nothing,
      attributes = Prelude.Nothing,
      shortname = Prelude.Nothing,
      useEbsOptimizedInstances = Prelude.Nothing,
      installUpdatesOnBoot = Prelude.Nothing,
      layerId = pLayerId_
    }

-- | The layer name, which is used by the console.
updateLayer_name :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Text)
updateLayer_name = Lens.lens (\UpdateLayer' {name} -> name) (\s@UpdateLayer' {} a -> s {name = a} :: UpdateLayer)

-- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
updateLayer_customRecipes :: Lens.Lens' UpdateLayer (Prelude.Maybe Recipes)
updateLayer_customRecipes = Lens.lens (\UpdateLayer' {customRecipes} -> customRecipes) (\s@UpdateLayer' {} a -> s {customRecipes = a} :: UpdateLayer)

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
updateLayer_autoAssignPublicIps :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Bool)
updateLayer_autoAssignPublicIps = Lens.lens (\UpdateLayer' {autoAssignPublicIps} -> autoAssignPublicIps) (\s@UpdateLayer' {} a -> s {autoAssignPublicIps = a} :: UpdateLayer)

-- | A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
updateLayer_customJson :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Text)
updateLayer_customJson = Lens.lens (\UpdateLayer' {customJson} -> customJson) (\s@UpdateLayer' {} a -> s {customJson = a} :: UpdateLayer)

-- | An array of @Package@ objects that describe the layer\'s packages.
updateLayer_packages :: Lens.Lens' UpdateLayer (Prelude.Maybe [Prelude.Text])
updateLayer_packages = Lens.lens (\UpdateLayer' {packages} -> packages) (\s@UpdateLayer' {} a -> s {packages = a} :: UpdateLayer) Prelude.. Lens.mapping Lens.coerced

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
updateLayer_volumeConfigurations :: Lens.Lens' UpdateLayer (Prelude.Maybe [VolumeConfiguration])
updateLayer_volumeConfigurations = Lens.lens (\UpdateLayer' {volumeConfigurations} -> volumeConfigurations) (\s@UpdateLayer' {} a -> s {volumeConfigurations = a} :: UpdateLayer) Prelude.. Lens.mapping Lens.coerced

-- | Whether to disable auto healing for the layer.
updateLayer_enableAutoHealing :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Bool)
updateLayer_enableAutoHealing = Lens.lens (\UpdateLayer' {enableAutoHealing} -> enableAutoHealing) (\s@UpdateLayer' {} a -> s {enableAutoHealing = a} :: UpdateLayer)

-- | An array containing the layer\'s custom security group IDs.
updateLayer_customSecurityGroupIds :: Lens.Lens' UpdateLayer (Prelude.Maybe [Prelude.Text])
updateLayer_customSecurityGroupIds = Lens.lens (\UpdateLayer' {customSecurityGroupIds} -> customSecurityGroupIds) (\s@UpdateLayer' {} a -> s {customSecurityGroupIds = a} :: UpdateLayer) Prelude.. Lens.mapping Lens.coerced

-- | Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
updateLayer_autoAssignElasticIps :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Bool)
updateLayer_autoAssignElasticIps = Lens.lens (\UpdateLayer' {autoAssignElasticIps} -> autoAssignElasticIps) (\s@UpdateLayer' {} a -> s {autoAssignElasticIps = a} :: UpdateLayer)

-- | Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
updateLayer_cloudWatchLogsConfiguration :: Lens.Lens' UpdateLayer (Prelude.Maybe CloudWatchLogsConfiguration)
updateLayer_cloudWatchLogsConfiguration = Lens.lens (\UpdateLayer' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@UpdateLayer' {} a -> s {cloudWatchLogsConfiguration = a} :: UpdateLayer)

-- |
updateLayer_lifecycleEventConfiguration :: Lens.Lens' UpdateLayer (Prelude.Maybe LifecycleEventConfiguration)
updateLayer_lifecycleEventConfiguration = Lens.lens (\UpdateLayer' {lifecycleEventConfiguration} -> lifecycleEventConfiguration) (\s@UpdateLayer' {} a -> s {lifecycleEventConfiguration = a} :: UpdateLayer)

-- | The ARN of an IAM profile to be used for all of the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
updateLayer_customInstanceProfileArn :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Text)
updateLayer_customInstanceProfileArn = Lens.lens (\UpdateLayer' {customInstanceProfileArn} -> customInstanceProfileArn) (\s@UpdateLayer' {} a -> s {customInstanceProfileArn = a} :: UpdateLayer)

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
updateLayer_attributes :: Lens.Lens' UpdateLayer (Prelude.Maybe (Prelude.HashMap LayerAttributesKeys (Prelude.Maybe Prelude.Text)))
updateLayer_attributes = Lens.lens (\UpdateLayer' {attributes} -> attributes) (\s@UpdateLayer' {} a -> s {attributes = a} :: UpdateLayer) Prelude.. Lens.mapping Lens.coerced

-- | For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorks Stacks and by Chef. The
-- short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters and must be
-- in the following format: \/\\A[a-z0-9\\-\\_\\.]+\\Z\/.
--
-- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
-- For more information, see the
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
updateLayer_shortname :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Text)
updateLayer_shortname = Lens.lens (\UpdateLayer' {shortname} -> shortname) (\s@UpdateLayer' {} a -> s {shortname = a} :: UpdateLayer)

-- | Whether to use Amazon EBS-optimized instances.
updateLayer_useEbsOptimizedInstances :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Bool)
updateLayer_useEbsOptimizedInstances = Lens.lens (\UpdateLayer' {useEbsOptimizedInstances} -> useEbsOptimizedInstances) (\s@UpdateLayer' {} a -> s {useEbsOptimizedInstances = a} :: UpdateLayer)

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
updateLayer_installUpdatesOnBoot :: Lens.Lens' UpdateLayer (Prelude.Maybe Prelude.Bool)
updateLayer_installUpdatesOnBoot = Lens.lens (\UpdateLayer' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@UpdateLayer' {} a -> s {installUpdatesOnBoot = a} :: UpdateLayer)

-- | The layer ID.
updateLayer_layerId :: Lens.Lens' UpdateLayer Prelude.Text
updateLayer_layerId = Lens.lens (\UpdateLayer' {layerId} -> layerId) (\s@UpdateLayer' {} a -> s {layerId = a} :: UpdateLayer)

instance Core.AWSRequest UpdateLayer where
  type AWSResponse UpdateLayer = UpdateLayerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull UpdateLayerResponse'

instance Prelude.Hashable UpdateLayer where
  hashWithSalt _salt UpdateLayer' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` customRecipes
      `Prelude.hashWithSalt` autoAssignPublicIps
      `Prelude.hashWithSalt` customJson
      `Prelude.hashWithSalt` packages
      `Prelude.hashWithSalt` volumeConfigurations
      `Prelude.hashWithSalt` enableAutoHealing
      `Prelude.hashWithSalt` customSecurityGroupIds
      `Prelude.hashWithSalt` autoAssignElasticIps
      `Prelude.hashWithSalt` cloudWatchLogsConfiguration
      `Prelude.hashWithSalt` lifecycleEventConfiguration
      `Prelude.hashWithSalt` customInstanceProfileArn
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` shortname
      `Prelude.hashWithSalt` useEbsOptimizedInstances
      `Prelude.hashWithSalt` installUpdatesOnBoot
      `Prelude.hashWithSalt` layerId

instance Prelude.NFData UpdateLayer where
  rnf UpdateLayer' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf customRecipes
      `Prelude.seq` Prelude.rnf autoAssignPublicIps
      `Prelude.seq` Prelude.rnf customJson
      `Prelude.seq` Prelude.rnf packages
      `Prelude.seq` Prelude.rnf volumeConfigurations
      `Prelude.seq` Prelude.rnf enableAutoHealing
      `Prelude.seq` Prelude.rnf customSecurityGroupIds
      `Prelude.seq` Prelude.rnf autoAssignElasticIps
      `Prelude.seq` Prelude.rnf cloudWatchLogsConfiguration
      `Prelude.seq` Prelude.rnf lifecycleEventConfiguration
      `Prelude.seq` Prelude.rnf customInstanceProfileArn
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf shortname
      `Prelude.seq` Prelude.rnf useEbsOptimizedInstances
      `Prelude.seq` Prelude.rnf installUpdatesOnBoot
      `Prelude.seq` Prelude.rnf layerId

instance Core.ToHeaders UpdateLayer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UpdateLayer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLayer where
  toJSON UpdateLayer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("CustomRecipes" Core..=) Prelude.<$> customRecipes,
            ("AutoAssignPublicIps" Core..=)
              Prelude.<$> autoAssignPublicIps,
            ("CustomJson" Core..=) Prelude.<$> customJson,
            ("Packages" Core..=) Prelude.<$> packages,
            ("VolumeConfigurations" Core..=)
              Prelude.<$> volumeConfigurations,
            ("EnableAutoHealing" Core..=)
              Prelude.<$> enableAutoHealing,
            ("CustomSecurityGroupIds" Core..=)
              Prelude.<$> customSecurityGroupIds,
            ("AutoAssignElasticIps" Core..=)
              Prelude.<$> autoAssignElasticIps,
            ("CloudWatchLogsConfiguration" Core..=)
              Prelude.<$> cloudWatchLogsConfiguration,
            ("LifecycleEventConfiguration" Core..=)
              Prelude.<$> lifecycleEventConfiguration,
            ("CustomInstanceProfileArn" Core..=)
              Prelude.<$> customInstanceProfileArn,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("Shortname" Core..=) Prelude.<$> shortname,
            ("UseEbsOptimizedInstances" Core..=)
              Prelude.<$> useEbsOptimizedInstances,
            ("InstallUpdatesOnBoot" Core..=)
              Prelude.<$> installUpdatesOnBoot,
            Prelude.Just ("LayerId" Core..= layerId)
          ]
      )

instance Core.ToPath UpdateLayer where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateLayer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLayerResponse' smart constructor.
data UpdateLayerResponse = UpdateLayerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLayerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateLayerResponse ::
  UpdateLayerResponse
newUpdateLayerResponse = UpdateLayerResponse'

instance Prelude.NFData UpdateLayerResponse where
  rnf _ = ()
