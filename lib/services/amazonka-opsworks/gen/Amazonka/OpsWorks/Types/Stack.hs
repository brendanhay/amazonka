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
-- Module      : Amazonka.OpsWorks.Types.Stack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.Stack where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.ChefConfiguration
import Amazonka.OpsWorks.Types.RootDeviceType
import Amazonka.OpsWorks.Types.Source
import Amazonka.OpsWorks.Types.StackAttributesKeys
import Amazonka.OpsWorks.Types.StackConfigurationManager
import qualified Amazonka.Prelude as Prelude

-- | Describes a stack.
--
-- /See:/ 'newStack' smart constructor.
data Stack = Stack'
  { -- | The agent version. This parameter is set to @LATEST@ for auto-update. or
    -- a version number for a fixed agent version.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap StackAttributesKeys (Prelude.Maybe Prelude.Text)),
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
    -- and the Berkshelf version. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
    chefConfiguration :: Prelude.Maybe ChefConfiguration,
    -- | The configuration manager.
    configurationManager :: Prelude.Maybe StackConfigurationManager,
    -- | The date when the stack was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | Contains the information required to retrieve an app or cookbook from a
    -- repository. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
    -- or
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
    customCookbooksSource :: Prelude.Maybe Source,
    -- | A JSON object that contains user-defined attributes to be added to the
    -- stack configuration and deployment attributes. You can use custom JSON
    -- to override the corresponding default stack configuration attribute
    -- values or to pass data to recipes. The string should be in the following
    -- format:
    --
    -- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
    --
    -- For more information on custom JSON, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
    customJson :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s default Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    defaultAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an IAM profile that is the default profile for all of the
    -- stack\'s EC2 instances. For more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    defaultInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s default operating system.
    defaultOs :: Prelude.Maybe Prelude.Text,
    -- | The default root device type. This value is used by default for all
    -- instances in the stack, but you can override it when you create an
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
    defaultRootDeviceType :: Prelude.Maybe RootDeviceType,
    -- | A default Amazon EC2 key pair for the stack\'s instances. You can
    -- override this value when you create or update an instance.
    defaultSshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The default subnet ID; applicable only if the stack is running in a VPC.
    defaultSubnetId :: Prelude.Maybe Prelude.Text,
    -- | The stack host name theme, with spaces replaced by underscores.
    hostnameTheme :: Prelude.Maybe Prelude.Text,
    -- | The stack name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The stack AWS region, such as \"ap-northeast-2\". For more information
    -- about AWS regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    region :: Prelude.Maybe Prelude.Text,
    -- | The stack AWS Identity and Access Management (IAM) role.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | Whether the stack uses custom cookbooks.
    useCustomCookbooks :: Prelude.Maybe Prelude.Bool,
    -- | Whether the stack automatically associates the AWS OpsWorks Stacks
    -- built-in security groups with the stack\'s layers.
    useOpsworksSecurityGroups :: Prelude.Maybe Prelude.Bool,
    -- | The VPC ID; applicable only if the stack is running in a VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Stack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentVersion', 'stack_agentVersion' - The agent version. This parameter is set to @LATEST@ for auto-update. or
-- a version number for a fixed agent version.
--
-- 'arn', 'stack_arn' - The stack\'s ARN.
--
-- 'attributes', 'stack_attributes' - The stack\'s attributes.
--
-- 'chefConfiguration', 'stack_chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
--
-- 'configurationManager', 'stack_configurationManager' - The configuration manager.
--
-- 'createdAt', 'stack_createdAt' - The date when the stack was created.
--
-- 'customCookbooksSource', 'stack_customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
--
-- 'customJson', 'stack_customJson' - A JSON object that contains user-defined attributes to be added to the
-- stack configuration and deployment attributes. You can use custom JSON
-- to override the corresponding default stack configuration attribute
-- values or to pass data to recipes. The string should be in the following
-- format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
--
-- 'defaultAvailabilityZone', 'stack_defaultAvailabilityZone' - The stack\'s default Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'defaultInstanceProfileArn', 'stack_defaultInstanceProfileArn' - The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'defaultOs', 'stack_defaultOs' - The stack\'s default operating system.
--
-- 'defaultRootDeviceType', 'stack_defaultRootDeviceType' - The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'defaultSshKeyName', 'stack_defaultSshKeyName' - A default Amazon EC2 key pair for the stack\'s instances. You can
-- override this value when you create or update an instance.
--
-- 'defaultSubnetId', 'stack_defaultSubnetId' - The default subnet ID; applicable only if the stack is running in a VPC.
--
-- 'hostnameTheme', 'stack_hostnameTheme' - The stack host name theme, with spaces replaced by underscores.
--
-- 'name', 'stack_name' - The stack name.
--
-- 'region', 'stack_region' - The stack AWS region, such as \"ap-northeast-2\". For more information
-- about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'serviceRoleArn', 'stack_serviceRoleArn' - The stack AWS Identity and Access Management (IAM) role.
--
-- 'stackId', 'stack_stackId' - The stack ID.
--
-- 'useCustomCookbooks', 'stack_useCustomCookbooks' - Whether the stack uses custom cookbooks.
--
-- 'useOpsworksSecurityGroups', 'stack_useOpsworksSecurityGroups' - Whether the stack automatically associates the AWS OpsWorks Stacks
-- built-in security groups with the stack\'s layers.
--
-- 'vpcId', 'stack_vpcId' - The VPC ID; applicable only if the stack is running in a VPC.
newStack ::
  Stack
newStack =
  Stack'
    { agentVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      attributes = Prelude.Nothing,
      chefConfiguration = Prelude.Nothing,
      configurationManager = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      customCookbooksSource = Prelude.Nothing,
      customJson = Prelude.Nothing,
      defaultAvailabilityZone = Prelude.Nothing,
      defaultInstanceProfileArn = Prelude.Nothing,
      defaultOs = Prelude.Nothing,
      defaultRootDeviceType = Prelude.Nothing,
      defaultSshKeyName = Prelude.Nothing,
      defaultSubnetId = Prelude.Nothing,
      hostnameTheme = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      stackId = Prelude.Nothing,
      useCustomCookbooks = Prelude.Nothing,
      useOpsworksSecurityGroups = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or
-- a version number for a fixed agent version.
stack_agentVersion :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_agentVersion = Lens.lens (\Stack' {agentVersion} -> agentVersion) (\s@Stack' {} a -> s {agentVersion = a} :: Stack)

-- | The stack\'s ARN.
stack_arn :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_arn = Lens.lens (\Stack' {arn} -> arn) (\s@Stack' {} a -> s {arn = a} :: Stack)

-- | The stack\'s attributes.
stack_attributes :: Lens.Lens' Stack (Prelude.Maybe (Prelude.HashMap StackAttributesKeys (Prelude.Maybe Prelude.Text)))
stack_attributes = Lens.lens (\Stack' {attributes} -> attributes) (\s@Stack' {} a -> s {attributes = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
stack_chefConfiguration :: Lens.Lens' Stack (Prelude.Maybe ChefConfiguration)
stack_chefConfiguration = Lens.lens (\Stack' {chefConfiguration} -> chefConfiguration) (\s@Stack' {} a -> s {chefConfiguration = a} :: Stack)

-- | The configuration manager.
stack_configurationManager :: Lens.Lens' Stack (Prelude.Maybe StackConfigurationManager)
stack_configurationManager = Lens.lens (\Stack' {configurationManager} -> configurationManager) (\s@Stack' {} a -> s {configurationManager = a} :: Stack)

-- | The date when the stack was created.
stack_createdAt :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_createdAt = Lens.lens (\Stack' {createdAt} -> createdAt) (\s@Stack' {} a -> s {createdAt = a} :: Stack)

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
stack_customCookbooksSource :: Lens.Lens' Stack (Prelude.Maybe Source)
stack_customCookbooksSource = Lens.lens (\Stack' {customCookbooksSource} -> customCookbooksSource) (\s@Stack' {} a -> s {customCookbooksSource = a} :: Stack)

-- | A JSON object that contains user-defined attributes to be added to the
-- stack configuration and deployment attributes. You can use custom JSON
-- to override the corresponding default stack configuration attribute
-- values or to pass data to recipes. The string should be in the following
-- format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
stack_customJson :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_customJson = Lens.lens (\Stack' {customJson} -> customJson) (\s@Stack' {} a -> s {customJson = a} :: Stack)

-- | The stack\'s default Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
stack_defaultAvailabilityZone :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_defaultAvailabilityZone = Lens.lens (\Stack' {defaultAvailabilityZone} -> defaultAvailabilityZone) (\s@Stack' {} a -> s {defaultAvailabilityZone = a} :: Stack)

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
stack_defaultInstanceProfileArn :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_defaultInstanceProfileArn = Lens.lens (\Stack' {defaultInstanceProfileArn} -> defaultInstanceProfileArn) (\s@Stack' {} a -> s {defaultInstanceProfileArn = a} :: Stack)

-- | The stack\'s default operating system.
stack_defaultOs :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_defaultOs = Lens.lens (\Stack' {defaultOs} -> defaultOs) (\s@Stack' {} a -> s {defaultOs = a} :: Stack)

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
stack_defaultRootDeviceType :: Lens.Lens' Stack (Prelude.Maybe RootDeviceType)
stack_defaultRootDeviceType = Lens.lens (\Stack' {defaultRootDeviceType} -> defaultRootDeviceType) (\s@Stack' {} a -> s {defaultRootDeviceType = a} :: Stack)

-- | A default Amazon EC2 key pair for the stack\'s instances. You can
-- override this value when you create or update an instance.
stack_defaultSshKeyName :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_defaultSshKeyName = Lens.lens (\Stack' {defaultSshKeyName} -> defaultSshKeyName) (\s@Stack' {} a -> s {defaultSshKeyName = a} :: Stack)

-- | The default subnet ID; applicable only if the stack is running in a VPC.
stack_defaultSubnetId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_defaultSubnetId = Lens.lens (\Stack' {defaultSubnetId} -> defaultSubnetId) (\s@Stack' {} a -> s {defaultSubnetId = a} :: Stack)

-- | The stack host name theme, with spaces replaced by underscores.
stack_hostnameTheme :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_hostnameTheme = Lens.lens (\Stack' {hostnameTheme} -> hostnameTheme) (\s@Stack' {} a -> s {hostnameTheme = a} :: Stack)

-- | The stack name.
stack_name :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_name = Lens.lens (\Stack' {name} -> name) (\s@Stack' {} a -> s {name = a} :: Stack)

-- | The stack AWS region, such as \"ap-northeast-2\". For more information
-- about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
stack_region :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_region = Lens.lens (\Stack' {region} -> region) (\s@Stack' {} a -> s {region = a} :: Stack)

-- | The stack AWS Identity and Access Management (IAM) role.
stack_serviceRoleArn :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_serviceRoleArn = Lens.lens (\Stack' {serviceRoleArn} -> serviceRoleArn) (\s@Stack' {} a -> s {serviceRoleArn = a} :: Stack)

-- | The stack ID.
stack_stackId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_stackId = Lens.lens (\Stack' {stackId} -> stackId) (\s@Stack' {} a -> s {stackId = a} :: Stack)

-- | Whether the stack uses custom cookbooks.
stack_useCustomCookbooks :: Lens.Lens' Stack (Prelude.Maybe Prelude.Bool)
stack_useCustomCookbooks = Lens.lens (\Stack' {useCustomCookbooks} -> useCustomCookbooks) (\s@Stack' {} a -> s {useCustomCookbooks = a} :: Stack)

-- | Whether the stack automatically associates the AWS OpsWorks Stacks
-- built-in security groups with the stack\'s layers.
stack_useOpsworksSecurityGroups :: Lens.Lens' Stack (Prelude.Maybe Prelude.Bool)
stack_useOpsworksSecurityGroups = Lens.lens (\Stack' {useOpsworksSecurityGroups} -> useOpsworksSecurityGroups) (\s@Stack' {} a -> s {useOpsworksSecurityGroups = a} :: Stack)

-- | The VPC ID; applicable only if the stack is running in a VPC.
stack_vpcId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_vpcId = Lens.lens (\Stack' {vpcId} -> vpcId) (\s@Stack' {} a -> s {vpcId = a} :: Stack)

instance Data.FromJSON Stack where
  parseJSON =
    Data.withObject
      "Stack"
      ( \x ->
          Stack'
            Prelude.<$> (x Data..:? "AgentVersion")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChefConfiguration")
            Prelude.<*> (x Data..:? "ConfigurationManager")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "CustomCookbooksSource")
            Prelude.<*> (x Data..:? "CustomJson")
            Prelude.<*> (x Data..:? "DefaultAvailabilityZone")
            Prelude.<*> (x Data..:? "DefaultInstanceProfileArn")
            Prelude.<*> (x Data..:? "DefaultOs")
            Prelude.<*> (x Data..:? "DefaultRootDeviceType")
            Prelude.<*> (x Data..:? "DefaultSshKeyName")
            Prelude.<*> (x Data..:? "DefaultSubnetId")
            Prelude.<*> (x Data..:? "HostnameTheme")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "ServiceRoleArn")
            Prelude.<*> (x Data..:? "StackId")
            Prelude.<*> (x Data..:? "UseCustomCookbooks")
            Prelude.<*> (x Data..:? "UseOpsworksSecurityGroups")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable Stack where
  hashWithSalt _salt Stack' {..} =
    _salt
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` chefConfiguration
      `Prelude.hashWithSalt` configurationManager
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` customCookbooksSource
      `Prelude.hashWithSalt` customJson
      `Prelude.hashWithSalt` defaultAvailabilityZone
      `Prelude.hashWithSalt` defaultInstanceProfileArn
      `Prelude.hashWithSalt` defaultOs
      `Prelude.hashWithSalt` defaultRootDeviceType
      `Prelude.hashWithSalt` defaultSshKeyName
      `Prelude.hashWithSalt` defaultSubnetId
      `Prelude.hashWithSalt` hostnameTheme
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` useCustomCookbooks
      `Prelude.hashWithSalt` useOpsworksSecurityGroups
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData Stack where
  rnf Stack' {..} =
    Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf chefConfiguration
      `Prelude.seq` Prelude.rnf configurationManager
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf customCookbooksSource
      `Prelude.seq` Prelude.rnf customJson
      `Prelude.seq` Prelude.rnf defaultAvailabilityZone
      `Prelude.seq` Prelude.rnf defaultInstanceProfileArn
      `Prelude.seq` Prelude.rnf defaultOs
      `Prelude.seq` Prelude.rnf defaultRootDeviceType
      `Prelude.seq` Prelude.rnf defaultSshKeyName
      `Prelude.seq` Prelude.rnf defaultSubnetId
      `Prelude.seq` Prelude.rnf hostnameTheme
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf useCustomCookbooks
      `Prelude.seq` Prelude.rnf
        useOpsworksSecurityGroups
      `Prelude.seq` Prelude.rnf vpcId
