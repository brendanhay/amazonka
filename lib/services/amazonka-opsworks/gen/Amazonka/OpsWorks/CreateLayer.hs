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
-- Module      : Amazonka.OpsWorks.CreateLayer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a layer. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-create.html How to Create a Layer>.
--
-- You should use __CreateLayer__ for noncustom layer types such as PHP App
-- Server only if the stack does not have an existing layer of that type. A
-- stack can have at most one instance of each noncustom layer; if you
-- attempt to create a second instance, __CreateLayer__ fails. A stack can
-- have an arbitrary number of custom layers, so you can call
-- __CreateLayer__ as many times as you like for that layer type.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.CreateLayer
  ( -- * Creating a Request
    CreateLayer (..),
    newCreateLayer,

    -- * Request Lenses
    createLayer_customInstanceProfileArn,
    createLayer_customSecurityGroupIds,
    createLayer_installUpdatesOnBoot,
    createLayer_cloudWatchLogsConfiguration,
    createLayer_lifecycleEventConfiguration,
    createLayer_customRecipes,
    createLayer_customJson,
    createLayer_volumeConfigurations,
    createLayer_enableAutoHealing,
    createLayer_packages,
    createLayer_attributes,
    createLayer_autoAssignPublicIps,
    createLayer_useEbsOptimizedInstances,
    createLayer_autoAssignElasticIps,
    createLayer_stackId,
    createLayer_type,
    createLayer_name,
    createLayer_shortname,

    -- * Destructuring the Response
    CreateLayerResponse (..),
    newCreateLayerResponse,

    -- * Response Lenses
    createLayerResponse_layerId,
    createLayerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLayer' smart constructor.
data CreateLayer = CreateLayer'
  { -- | The ARN of an IAM profile to be used for the layer\'s EC2 instances. For
    -- more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    customInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | An array containing the layer custom security group IDs.
    customSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. To control when updates are
    -- installed, set this value to @false@. You must then update your
    -- instances manually by using CreateDeployment to run the
    -- @update_dependencies@ stack command or by manually running @yum@ (Amazon
    -- Linux) or @apt-get@ (Ubuntu) on the instances.
    --
    -- To ensure that your instances have the latest security updates, we
    -- strongly recommend using the default value of @true@.
    installUpdatesOnBoot :: Prelude.Maybe Prelude.Bool,
    -- | Specifies CloudWatch Logs configuration options for the layer. For more
    -- information, see CloudWatchLogsLogStream.
    cloudWatchLogsConfiguration :: Prelude.Maybe CloudWatchLogsConfiguration,
    -- | A @LifeCycleEventConfiguration@ object that you can use to configure the
    -- Shutdown event to specify an execution timeout and enable or disable
    -- Elastic Load Balancer connection draining.
    lifecycleEventConfiguration :: Prelude.Maybe LifecycleEventConfiguration,
    -- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
    customRecipes :: Prelude.Maybe Recipes,
    -- | A JSON-formatted string containing custom stack configuration and
    -- deployment attributes to be installed on the layer\'s instances. For
    -- more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
    -- This feature is supported as of version 1.7.42 of the AWS CLI.
    customJson :: Prelude.Maybe Prelude.Text,
    -- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
    -- volumes.
    volumeConfigurations :: Prelude.Maybe [VolumeConfiguration],
    -- | Whether to disable auto healing for the layer.
    enableAutoHealing :: Prelude.Maybe Prelude.Bool,
    -- | An array of @Package@ objects that describes the layer packages.
    packages :: Prelude.Maybe [Prelude.Text],
    -- | One or more user-defined key-value pairs to be added to the stack
    -- attributes.
    --
    -- To create a cluster layer, set the @EcsClusterArn@ attribute to the
    -- cluster\'s ARN.
    attributes :: Prelude.Maybe (Prelude.HashMap LayerAttributesKeys (Prelude.Maybe Prelude.Text)),
    -- | For stacks that are running in a VPC, whether to automatically assign a
    -- public IP address to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignPublicIps :: Prelude.Maybe Prelude.Bool,
    -- | Whether to use Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Prelude.Maybe Prelude.Bool,
    -- | Whether to automatically assign an
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignElasticIps :: Prelude.Maybe Prelude.Bool,
    -- | The layer stack ID.
    stackId :: Prelude.Text,
    -- | The layer type. A stack cannot have more than one built-in layer of the
    -- same type. It can have any number of custom layers. Built-in layers are
    -- not available in Chef 12 stacks.
    type' :: LayerType,
    -- | The layer name, which is used by the console.
    name :: Prelude.Text,
    -- | For custom layers only, use this parameter to specify the layer\'s short
    -- name, which is used internally by AWS OpsWorks Stacks and by Chef
    -- recipes. The short name is also used as the name for the directory where
    -- your app files are installed. It can have a maximum of 200 characters,
    -- which are limited to the alphanumeric characters, \'-\', \'_\', and
    -- \'.\'.
    --
    -- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
    -- For more information, see the
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>.
    shortname :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLayer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customInstanceProfileArn', 'createLayer_customInstanceProfileArn' - The ARN of an IAM profile to be used for the layer\'s EC2 instances. For
-- more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'customSecurityGroupIds', 'createLayer_customSecurityGroupIds' - An array containing the layer custom security group IDs.
--
-- 'installUpdatesOnBoot', 'createLayer_installUpdatesOnBoot' - Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- To ensure that your instances have the latest security updates, we
-- strongly recommend using the default value of @true@.
--
-- 'cloudWatchLogsConfiguration', 'createLayer_cloudWatchLogsConfiguration' - Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
--
-- 'lifecycleEventConfiguration', 'createLayer_lifecycleEventConfiguration' - A @LifeCycleEventConfiguration@ object that you can use to configure the
-- Shutdown event to specify an execution timeout and enable or disable
-- Elastic Load Balancer connection draining.
--
-- 'customRecipes', 'createLayer_customRecipes' - A @LayerCustomRecipes@ object that specifies the layer custom recipes.
--
-- 'customJson', 'createLayer_customJson' - A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
-- This feature is supported as of version 1.7.42 of the AWS CLI.
--
-- 'volumeConfigurations', 'createLayer_volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
--
-- 'enableAutoHealing', 'createLayer_enableAutoHealing' - Whether to disable auto healing for the layer.
--
-- 'packages', 'createLayer_packages' - An array of @Package@ objects that describes the layer packages.
--
-- 'attributes', 'createLayer_attributes' - One or more user-defined key-value pairs to be added to the stack
-- attributes.
--
-- To create a cluster layer, set the @EcsClusterArn@ attribute to the
-- cluster\'s ARN.
--
-- 'autoAssignPublicIps', 'createLayer_autoAssignPublicIps' - For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'useEbsOptimizedInstances', 'createLayer_useEbsOptimizedInstances' - Whether to use Amazon EBS-optimized instances.
--
-- 'autoAssignElasticIps', 'createLayer_autoAssignElasticIps' - Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'stackId', 'createLayer_stackId' - The layer stack ID.
--
-- 'type'', 'createLayer_type' - The layer type. A stack cannot have more than one built-in layer of the
-- same type. It can have any number of custom layers. Built-in layers are
-- not available in Chef 12 stacks.
--
-- 'name', 'createLayer_name' - The layer name, which is used by the console.
--
-- 'shortname', 'createLayer_shortname' - For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorks Stacks and by Chef
-- recipes. The short name is also used as the name for the directory where
-- your app files are installed. It can have a maximum of 200 characters,
-- which are limited to the alphanumeric characters, \'-\', \'_\', and
-- \'.\'.
--
-- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
-- For more information, see the
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>.
newCreateLayer ::
  -- | 'stackId'
  Prelude.Text ->
  -- | 'type''
  LayerType ->
  -- | 'name'
  Prelude.Text ->
  -- | 'shortname'
  Prelude.Text ->
  CreateLayer
newCreateLayer pStackId_ pType_ pName_ pShortname_ =
  CreateLayer'
    { customInstanceProfileArn =
        Prelude.Nothing,
      customSecurityGroupIds = Prelude.Nothing,
      installUpdatesOnBoot = Prelude.Nothing,
      cloudWatchLogsConfiguration = Prelude.Nothing,
      lifecycleEventConfiguration = Prelude.Nothing,
      customRecipes = Prelude.Nothing,
      customJson = Prelude.Nothing,
      volumeConfigurations = Prelude.Nothing,
      enableAutoHealing = Prelude.Nothing,
      packages = Prelude.Nothing,
      attributes = Prelude.Nothing,
      autoAssignPublicIps = Prelude.Nothing,
      useEbsOptimizedInstances = Prelude.Nothing,
      autoAssignElasticIps = Prelude.Nothing,
      stackId = pStackId_,
      type' = pType_,
      name = pName_,
      shortname = pShortname_
    }

-- | The ARN of an IAM profile to be used for the layer\'s EC2 instances. For
-- more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
createLayer_customInstanceProfileArn :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Text)
createLayer_customInstanceProfileArn = Lens.lens (\CreateLayer' {customInstanceProfileArn} -> customInstanceProfileArn) (\s@CreateLayer' {} a -> s {customInstanceProfileArn = a} :: CreateLayer)

-- | An array containing the layer custom security group IDs.
createLayer_customSecurityGroupIds :: Lens.Lens' CreateLayer (Prelude.Maybe [Prelude.Text])
createLayer_customSecurityGroupIds = Lens.lens (\CreateLayer' {customSecurityGroupIds} -> customSecurityGroupIds) (\s@CreateLayer' {} a -> s {customSecurityGroupIds = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- To ensure that your instances have the latest security updates, we
-- strongly recommend using the default value of @true@.
createLayer_installUpdatesOnBoot :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_installUpdatesOnBoot = Lens.lens (\CreateLayer' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@CreateLayer' {} a -> s {installUpdatesOnBoot = a} :: CreateLayer)

-- | Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
createLayer_cloudWatchLogsConfiguration :: Lens.Lens' CreateLayer (Prelude.Maybe CloudWatchLogsConfiguration)
createLayer_cloudWatchLogsConfiguration = Lens.lens (\CreateLayer' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@CreateLayer' {} a -> s {cloudWatchLogsConfiguration = a} :: CreateLayer)

-- | A @LifeCycleEventConfiguration@ object that you can use to configure the
-- Shutdown event to specify an execution timeout and enable or disable
-- Elastic Load Balancer connection draining.
createLayer_lifecycleEventConfiguration :: Lens.Lens' CreateLayer (Prelude.Maybe LifecycleEventConfiguration)
createLayer_lifecycleEventConfiguration = Lens.lens (\CreateLayer' {lifecycleEventConfiguration} -> lifecycleEventConfiguration) (\s@CreateLayer' {} a -> s {lifecycleEventConfiguration = a} :: CreateLayer)

-- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
createLayer_customRecipes :: Lens.Lens' CreateLayer (Prelude.Maybe Recipes)
createLayer_customRecipes = Lens.lens (\CreateLayer' {customRecipes} -> customRecipes) (\s@CreateLayer' {} a -> s {customRecipes = a} :: CreateLayer)

-- | A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
-- This feature is supported as of version 1.7.42 of the AWS CLI.
createLayer_customJson :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Text)
createLayer_customJson = Lens.lens (\CreateLayer' {customJson} -> customJson) (\s@CreateLayer' {} a -> s {customJson = a} :: CreateLayer)

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
createLayer_volumeConfigurations :: Lens.Lens' CreateLayer (Prelude.Maybe [VolumeConfiguration])
createLayer_volumeConfigurations = Lens.lens (\CreateLayer' {volumeConfigurations} -> volumeConfigurations) (\s@CreateLayer' {} a -> s {volumeConfigurations = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

-- | Whether to disable auto healing for the layer.
createLayer_enableAutoHealing :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_enableAutoHealing = Lens.lens (\CreateLayer' {enableAutoHealing} -> enableAutoHealing) (\s@CreateLayer' {} a -> s {enableAutoHealing = a} :: CreateLayer)

-- | An array of @Package@ objects that describes the layer packages.
createLayer_packages :: Lens.Lens' CreateLayer (Prelude.Maybe [Prelude.Text])
createLayer_packages = Lens.lens (\CreateLayer' {packages} -> packages) (\s@CreateLayer' {} a -> s {packages = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

-- | One or more user-defined key-value pairs to be added to the stack
-- attributes.
--
-- To create a cluster layer, set the @EcsClusterArn@ attribute to the
-- cluster\'s ARN.
createLayer_attributes :: Lens.Lens' CreateLayer (Prelude.Maybe (Prelude.HashMap LayerAttributesKeys (Prelude.Maybe Prelude.Text)))
createLayer_attributes = Lens.lens (\CreateLayer' {attributes} -> attributes) (\s@CreateLayer' {} a -> s {attributes = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
createLayer_autoAssignPublicIps :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_autoAssignPublicIps = Lens.lens (\CreateLayer' {autoAssignPublicIps} -> autoAssignPublicIps) (\s@CreateLayer' {} a -> s {autoAssignPublicIps = a} :: CreateLayer)

-- | Whether to use Amazon EBS-optimized instances.
createLayer_useEbsOptimizedInstances :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_useEbsOptimizedInstances = Lens.lens (\CreateLayer' {useEbsOptimizedInstances} -> useEbsOptimizedInstances) (\s@CreateLayer' {} a -> s {useEbsOptimizedInstances = a} :: CreateLayer)

-- | Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
createLayer_autoAssignElasticIps :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_autoAssignElasticIps = Lens.lens (\CreateLayer' {autoAssignElasticIps} -> autoAssignElasticIps) (\s@CreateLayer' {} a -> s {autoAssignElasticIps = a} :: CreateLayer)

-- | The layer stack ID.
createLayer_stackId :: Lens.Lens' CreateLayer Prelude.Text
createLayer_stackId = Lens.lens (\CreateLayer' {stackId} -> stackId) (\s@CreateLayer' {} a -> s {stackId = a} :: CreateLayer)

-- | The layer type. A stack cannot have more than one built-in layer of the
-- same type. It can have any number of custom layers. Built-in layers are
-- not available in Chef 12 stacks.
createLayer_type :: Lens.Lens' CreateLayer LayerType
createLayer_type = Lens.lens (\CreateLayer' {type'} -> type') (\s@CreateLayer' {} a -> s {type' = a} :: CreateLayer)

-- | The layer name, which is used by the console.
createLayer_name :: Lens.Lens' CreateLayer Prelude.Text
createLayer_name = Lens.lens (\CreateLayer' {name} -> name) (\s@CreateLayer' {} a -> s {name = a} :: CreateLayer)

-- | For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorks Stacks and by Chef
-- recipes. The short name is also used as the name for the directory where
-- your app files are installed. It can have a maximum of 200 characters,
-- which are limited to the alphanumeric characters, \'-\', \'_\', and
-- \'.\'.
--
-- The built-in layers\' short names are defined by AWS OpsWorks Stacks.
-- For more information, see the
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>.
createLayer_shortname :: Lens.Lens' CreateLayer Prelude.Text
createLayer_shortname = Lens.lens (\CreateLayer' {shortname} -> shortname) (\s@CreateLayer' {} a -> s {shortname = a} :: CreateLayer)

instance Core.AWSRequest CreateLayer where
  type AWSResponse CreateLayer = CreateLayerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLayerResponse'
            Prelude.<$> (x Core..?> "LayerId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLayer

instance Prelude.NFData CreateLayer

instance Core.ToHeaders CreateLayer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.CreateLayer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLayer where
  toJSON CreateLayer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomInstanceProfileArn" Core..=)
              Prelude.<$> customInstanceProfileArn,
            ("CustomSecurityGroupIds" Core..=)
              Prelude.<$> customSecurityGroupIds,
            ("InstallUpdatesOnBoot" Core..=)
              Prelude.<$> installUpdatesOnBoot,
            ("CloudWatchLogsConfiguration" Core..=)
              Prelude.<$> cloudWatchLogsConfiguration,
            ("LifecycleEventConfiguration" Core..=)
              Prelude.<$> lifecycleEventConfiguration,
            ("CustomRecipes" Core..=) Prelude.<$> customRecipes,
            ("CustomJson" Core..=) Prelude.<$> customJson,
            ("VolumeConfigurations" Core..=)
              Prelude.<$> volumeConfigurations,
            ("EnableAutoHealing" Core..=)
              Prelude.<$> enableAutoHealing,
            ("Packages" Core..=) Prelude.<$> packages,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("AutoAssignPublicIps" Core..=)
              Prelude.<$> autoAssignPublicIps,
            ("UseEbsOptimizedInstances" Core..=)
              Prelude.<$> useEbsOptimizedInstances,
            ("AutoAssignElasticIps" Core..=)
              Prelude.<$> autoAssignElasticIps,
            Prelude.Just ("StackId" Core..= stackId),
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Shortname" Core..= shortname)
          ]
      )

instance Core.ToPath CreateLayer where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLayer where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @CreateLayer@ request.
--
-- /See:/ 'newCreateLayerResponse' smart constructor.
data CreateLayerResponse = CreateLayerResponse'
  { -- | The layer ID.
    layerId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLayerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerId', 'createLayerResponse_layerId' - The layer ID.
--
-- 'httpStatus', 'createLayerResponse_httpStatus' - The response's http status code.
newCreateLayerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLayerResponse
newCreateLayerResponse pHttpStatus_ =
  CreateLayerResponse'
    { layerId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The layer ID.
createLayerResponse_layerId :: Lens.Lens' CreateLayerResponse (Prelude.Maybe Prelude.Text)
createLayerResponse_layerId = Lens.lens (\CreateLayerResponse' {layerId} -> layerId) (\s@CreateLayerResponse' {} a -> s {layerId = a} :: CreateLayerResponse)

-- | The response's http status code.
createLayerResponse_httpStatus :: Lens.Lens' CreateLayerResponse Prelude.Int
createLayerResponse_httpStatus = Lens.lens (\CreateLayerResponse' {httpStatus} -> httpStatus) (\s@CreateLayerResponse' {} a -> s {httpStatus = a} :: CreateLayerResponse)

instance Prelude.NFData CreateLayerResponse
