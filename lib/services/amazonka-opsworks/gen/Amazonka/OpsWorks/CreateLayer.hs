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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createLayer_attributes,
    createLayer_autoAssignElasticIps,
    createLayer_autoAssignPublicIps,
    createLayer_cloudWatchLogsConfiguration,
    createLayer_customInstanceProfileArn,
    createLayer_customJson,
    createLayer_customRecipes,
    createLayer_customSecurityGroupIds,
    createLayer_enableAutoHealing,
    createLayer_installUpdatesOnBoot,
    createLayer_lifecycleEventConfiguration,
    createLayer_packages,
    createLayer_useEbsOptimizedInstances,
    createLayer_volumeConfigurations,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLayer' smart constructor.
data CreateLayer = CreateLayer'
  { -- | One or more user-defined key-value pairs to be added to the stack
    -- attributes.
    --
    -- To create a cluster layer, set the @EcsClusterArn@ attribute to the
    -- cluster\'s ARN.
    attributes :: Prelude.Maybe (Prelude.HashMap LayerAttributesKeys (Prelude.Maybe Prelude.Text)),
    -- | Whether to automatically assign an
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignElasticIps :: Prelude.Maybe Prelude.Bool,
    -- | For stacks that are running in a VPC, whether to automatically assign a
    -- public IP address to the layer\'s instances. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
    autoAssignPublicIps :: Prelude.Maybe Prelude.Bool,
    -- | Specifies CloudWatch Logs configuration options for the layer. For more
    -- information, see CloudWatchLogsLogStream.
    cloudWatchLogsConfiguration :: Prelude.Maybe CloudWatchLogsConfiguration,
    -- | The ARN of an IAM profile to be used for the layer\'s EC2 instances. For
    -- more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    customInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted string containing custom stack configuration and
    -- deployment attributes to be installed on the layer\'s instances. For
    -- more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
    -- This feature is supported as of version 1.7.42 of the AWS CLI.
    customJson :: Prelude.Maybe Prelude.Text,
    -- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
    customRecipes :: Prelude.Maybe Recipes,
    -- | An array containing the layer custom security group IDs.
    customSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Whether to disable auto healing for the layer.
    enableAutoHealing :: Prelude.Maybe Prelude.Bool,
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
    -- | A @LifeCycleEventConfiguration@ object that you can use to configure the
    -- Shutdown event to specify an execution timeout and enable or disable
    -- Elastic Load Balancer connection draining.
    lifecycleEventConfiguration :: Prelude.Maybe LifecycleEventConfiguration,
    -- | An array of @Package@ objects that describes the layer packages.
    packages :: Prelude.Maybe [Prelude.Text],
    -- | Whether to use Amazon EBS-optimized instances.
    useEbsOptimizedInstances :: Prelude.Maybe Prelude.Bool,
    -- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
    -- volumes.
    volumeConfigurations :: Prelude.Maybe [VolumeConfiguration],
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
-- 'attributes', 'createLayer_attributes' - One or more user-defined key-value pairs to be added to the stack
-- attributes.
--
-- To create a cluster layer, set the @EcsClusterArn@ attribute to the
-- cluster\'s ARN.
--
-- 'autoAssignElasticIps', 'createLayer_autoAssignElasticIps' - Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'autoAssignPublicIps', 'createLayer_autoAssignPublicIps' - For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
--
-- 'cloudWatchLogsConfiguration', 'createLayer_cloudWatchLogsConfiguration' - Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
--
-- 'customInstanceProfileArn', 'createLayer_customInstanceProfileArn' - The ARN of an IAM profile to be used for the layer\'s EC2 instances. For
-- more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'customJson', 'createLayer_customJson' - A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
-- This feature is supported as of version 1.7.42 of the AWS CLI.
--
-- 'customRecipes', 'createLayer_customRecipes' - A @LayerCustomRecipes@ object that specifies the layer custom recipes.
--
-- 'customSecurityGroupIds', 'createLayer_customSecurityGroupIds' - An array containing the layer custom security group IDs.
--
-- 'enableAutoHealing', 'createLayer_enableAutoHealing' - Whether to disable auto healing for the layer.
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
-- 'lifecycleEventConfiguration', 'createLayer_lifecycleEventConfiguration' - A @LifeCycleEventConfiguration@ object that you can use to configure the
-- Shutdown event to specify an execution timeout and enable or disable
-- Elastic Load Balancer connection draining.
--
-- 'packages', 'createLayer_packages' - An array of @Package@ objects that describes the layer packages.
--
-- 'useEbsOptimizedInstances', 'createLayer_useEbsOptimizedInstances' - Whether to use Amazon EBS-optimized instances.
--
-- 'volumeConfigurations', 'createLayer_volumeConfigurations' - A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
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
    { attributes = Prelude.Nothing,
      autoAssignElasticIps = Prelude.Nothing,
      autoAssignPublicIps = Prelude.Nothing,
      cloudWatchLogsConfiguration = Prelude.Nothing,
      customInstanceProfileArn = Prelude.Nothing,
      customJson = Prelude.Nothing,
      customRecipes = Prelude.Nothing,
      customSecurityGroupIds = Prelude.Nothing,
      enableAutoHealing = Prelude.Nothing,
      installUpdatesOnBoot = Prelude.Nothing,
      lifecycleEventConfiguration = Prelude.Nothing,
      packages = Prelude.Nothing,
      useEbsOptimizedInstances = Prelude.Nothing,
      volumeConfigurations = Prelude.Nothing,
      stackId = pStackId_,
      type' = pType_,
      name = pName_,
      shortname = pShortname_
    }

-- | One or more user-defined key-value pairs to be added to the stack
-- attributes.
--
-- To create a cluster layer, set the @EcsClusterArn@ attribute to the
-- cluster\'s ARN.
createLayer_attributes :: Lens.Lens' CreateLayer (Prelude.Maybe (Prelude.HashMap LayerAttributesKeys (Prelude.Maybe Prelude.Text)))
createLayer_attributes = Lens.lens (\CreateLayer' {attributes} -> attributes) (\s@CreateLayer' {} a -> s {attributes = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

-- | Whether to automatically assign an
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
createLayer_autoAssignElasticIps :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_autoAssignElasticIps = Lens.lens (\CreateLayer' {autoAssignElasticIps} -> autoAssignElasticIps) (\s@CreateLayer' {} a -> s {autoAssignElasticIps = a} :: CreateLayer)

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
createLayer_autoAssignPublicIps :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_autoAssignPublicIps = Lens.lens (\CreateLayer' {autoAssignPublicIps} -> autoAssignPublicIps) (\s@CreateLayer' {} a -> s {autoAssignPublicIps = a} :: CreateLayer)

-- | Specifies CloudWatch Logs configuration options for the layer. For more
-- information, see CloudWatchLogsLogStream.
createLayer_cloudWatchLogsConfiguration :: Lens.Lens' CreateLayer (Prelude.Maybe CloudWatchLogsConfiguration)
createLayer_cloudWatchLogsConfiguration = Lens.lens (\CreateLayer' {cloudWatchLogsConfiguration} -> cloudWatchLogsConfiguration) (\s@CreateLayer' {} a -> s {cloudWatchLogsConfiguration = a} :: CreateLayer)

-- | The ARN of an IAM profile to be used for the layer\'s EC2 instances. For
-- more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
createLayer_customInstanceProfileArn :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Text)
createLayer_customInstanceProfileArn = Lens.lens (\CreateLayer' {customInstanceProfileArn} -> customInstanceProfileArn) (\s@CreateLayer' {} a -> s {customInstanceProfileArn = a} :: CreateLayer)

-- | A JSON-formatted string containing custom stack configuration and
-- deployment attributes to be installed on the layer\'s instances. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>.
-- This feature is supported as of version 1.7.42 of the AWS CLI.
createLayer_customJson :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Text)
createLayer_customJson = Lens.lens (\CreateLayer' {customJson} -> customJson) (\s@CreateLayer' {} a -> s {customJson = a} :: CreateLayer)

-- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
createLayer_customRecipes :: Lens.Lens' CreateLayer (Prelude.Maybe Recipes)
createLayer_customRecipes = Lens.lens (\CreateLayer' {customRecipes} -> customRecipes) (\s@CreateLayer' {} a -> s {customRecipes = a} :: CreateLayer)

-- | An array containing the layer custom security group IDs.
createLayer_customSecurityGroupIds :: Lens.Lens' CreateLayer (Prelude.Maybe [Prelude.Text])
createLayer_customSecurityGroupIds = Lens.lens (\CreateLayer' {customSecurityGroupIds} -> customSecurityGroupIds) (\s@CreateLayer' {} a -> s {customSecurityGroupIds = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

-- | Whether to disable auto healing for the layer.
createLayer_enableAutoHealing :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_enableAutoHealing = Lens.lens (\CreateLayer' {enableAutoHealing} -> enableAutoHealing) (\s@CreateLayer' {} a -> s {enableAutoHealing = a} :: CreateLayer)

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

-- | A @LifeCycleEventConfiguration@ object that you can use to configure the
-- Shutdown event to specify an execution timeout and enable or disable
-- Elastic Load Balancer connection draining.
createLayer_lifecycleEventConfiguration :: Lens.Lens' CreateLayer (Prelude.Maybe LifecycleEventConfiguration)
createLayer_lifecycleEventConfiguration = Lens.lens (\CreateLayer' {lifecycleEventConfiguration} -> lifecycleEventConfiguration) (\s@CreateLayer' {} a -> s {lifecycleEventConfiguration = a} :: CreateLayer)

-- | An array of @Package@ objects that describes the layer packages.
createLayer_packages :: Lens.Lens' CreateLayer (Prelude.Maybe [Prelude.Text])
createLayer_packages = Lens.lens (\CreateLayer' {packages} -> packages) (\s@CreateLayer' {} a -> s {packages = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

-- | Whether to use Amazon EBS-optimized instances.
createLayer_useEbsOptimizedInstances :: Lens.Lens' CreateLayer (Prelude.Maybe Prelude.Bool)
createLayer_useEbsOptimizedInstances = Lens.lens (\CreateLayer' {useEbsOptimizedInstances} -> useEbsOptimizedInstances) (\s@CreateLayer' {} a -> s {useEbsOptimizedInstances = a} :: CreateLayer)

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
createLayer_volumeConfigurations :: Lens.Lens' CreateLayer (Prelude.Maybe [VolumeConfiguration])
createLayer_volumeConfigurations = Lens.lens (\CreateLayer' {volumeConfigurations} -> volumeConfigurations) (\s@CreateLayer' {} a -> s {volumeConfigurations = a} :: CreateLayer) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLayerResponse'
            Prelude.<$> (x Data..?> "LayerId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLayer where
  hashWithSalt _salt CreateLayer' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` autoAssignElasticIps
      `Prelude.hashWithSalt` autoAssignPublicIps
      `Prelude.hashWithSalt` cloudWatchLogsConfiguration
      `Prelude.hashWithSalt` customInstanceProfileArn
      `Prelude.hashWithSalt` customJson
      `Prelude.hashWithSalt` customRecipes
      `Prelude.hashWithSalt` customSecurityGroupIds
      `Prelude.hashWithSalt` enableAutoHealing
      `Prelude.hashWithSalt` installUpdatesOnBoot
      `Prelude.hashWithSalt` lifecycleEventConfiguration
      `Prelude.hashWithSalt` packages
      `Prelude.hashWithSalt` useEbsOptimizedInstances
      `Prelude.hashWithSalt` volumeConfigurations
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` shortname

instance Prelude.NFData CreateLayer where
  rnf CreateLayer' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf autoAssignElasticIps
      `Prelude.seq` Prelude.rnf autoAssignPublicIps
      `Prelude.seq` Prelude.rnf cloudWatchLogsConfiguration
      `Prelude.seq` Prelude.rnf customInstanceProfileArn
      `Prelude.seq` Prelude.rnf customJson
      `Prelude.seq` Prelude.rnf customRecipes
      `Prelude.seq` Prelude.rnf customSecurityGroupIds
      `Prelude.seq` Prelude.rnf enableAutoHealing
      `Prelude.seq` Prelude.rnf installUpdatesOnBoot
      `Prelude.seq` Prelude.rnf lifecycleEventConfiguration
      `Prelude.seq` Prelude.rnf packages
      `Prelude.seq` Prelude.rnf useEbsOptimizedInstances
      `Prelude.seq` Prelude.rnf volumeConfigurations
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf shortname

instance Data.ToHeaders CreateLayer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.CreateLayer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLayer where
  toJSON CreateLayer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            ("AutoAssignElasticIps" Data..=)
              Prelude.<$> autoAssignElasticIps,
            ("AutoAssignPublicIps" Data..=)
              Prelude.<$> autoAssignPublicIps,
            ("CloudWatchLogsConfiguration" Data..=)
              Prelude.<$> cloudWatchLogsConfiguration,
            ("CustomInstanceProfileArn" Data..=)
              Prelude.<$> customInstanceProfileArn,
            ("CustomJson" Data..=) Prelude.<$> customJson,
            ("CustomRecipes" Data..=) Prelude.<$> customRecipes,
            ("CustomSecurityGroupIds" Data..=)
              Prelude.<$> customSecurityGroupIds,
            ("EnableAutoHealing" Data..=)
              Prelude.<$> enableAutoHealing,
            ("InstallUpdatesOnBoot" Data..=)
              Prelude.<$> installUpdatesOnBoot,
            ("LifecycleEventConfiguration" Data..=)
              Prelude.<$> lifecycleEventConfiguration,
            ("Packages" Data..=) Prelude.<$> packages,
            ("UseEbsOptimizedInstances" Data..=)
              Prelude.<$> useEbsOptimizedInstances,
            ("VolumeConfigurations" Data..=)
              Prelude.<$> volumeConfigurations,
            Prelude.Just ("StackId" Data..= stackId),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Shortname" Data..= shortname)
          ]
      )

instance Data.ToPath CreateLayer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLayer where
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

instance Prelude.NFData CreateLayerResponse where
  rnf CreateLayerResponse' {..} =
    Prelude.rnf layerId
      `Prelude.seq` Prelude.rnf httpStatus
