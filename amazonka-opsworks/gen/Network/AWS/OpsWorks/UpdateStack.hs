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
-- Module      : Network.AWS.OpsWorks.UpdateStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified stack.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UpdateStack
  ( -- * Creating a Request
    UpdateStack (..),
    newUpdateStack,

    -- * Request Lenses
    updateStack_defaultOs,
    updateStack_useOpsworksSecurityGroups,
    updateStack_customCookbooksSource,
    updateStack_serviceRoleArn,
    updateStack_defaultAvailabilityZone,
    updateStack_agentVersion,
    updateStack_customJson,
    updateStack_defaultRootDeviceType,
    updateStack_attributes,
    updateStack_name,
    updateStack_defaultInstanceProfileArn,
    updateStack_hostnameTheme,
    updateStack_defaultSshKeyName,
    updateStack_configurationManager,
    updateStack_chefConfiguration,
    updateStack_defaultSubnetId,
    updateStack_useCustomCookbooks,
    updateStack_stackId,

    -- * Destructuring the Response
    UpdateStackResponse (..),
    newUpdateStackResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { -- | The stack\'s operating system, which must be set to one of the
    -- following:
    --
    -- -   A supported Linux operating system: An Amazon Linux version, such as
    --     @Amazon Linux 2018.03@, @Amazon Linux 2017.09@,
    --     @Amazon Linux 2017.03@, @Amazon Linux 2016.09@,
    --     @Amazon Linux 2016.03@, @Amazon Linux 2015.09@, or
    --     @Amazon Linux 2015.03@.
    --
    -- -   A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@,
    --     @Ubuntu 14.04 LTS@, or @Ubuntu 12.04 LTS@.
    --
    -- -   @CentOS Linux 7@
    --
    -- -   @Red Hat Enterprise Linux 7@
    --
    -- -   A supported Windows operating system, such as
    --     @Microsoft Windows Server 2012 R2 Base@,
    --     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
    --     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
    --     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
    --
    -- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
    --     when you create instances. For more information about how to use
    --     custom AMIs with OpsWorks, see
    --     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
    --
    -- The default option is the stack\'s current operating system. For more
    -- information about supported operating systems, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
    defaultOs :: Core.Maybe Core.Text,
    -- | Whether to associate the AWS OpsWorks Stacks built-in security groups
    -- with the stack\'s layers.
    --
    -- AWS OpsWorks Stacks provides a standard set of built-in security groups,
    -- one for each layer, which are associated with layers by default.
    -- @UseOpsworksSecurityGroups@ allows you to provide your own custom
    -- security groups instead of using the built-in groups.
    -- @UseOpsworksSecurityGroups@ has the following settings:
    --
    -- -   True - AWS OpsWorks Stacks automatically associates the appropriate
    --     built-in security group with each layer (default setting). You can
    --     associate additional security groups with a layer after you create
    --     it, but you cannot delete the built-in security group.
    --
    -- -   False - AWS OpsWorks Stacks does not associate built-in security
    --     groups with layers. You must create appropriate EC2 security groups
    --     and associate a security group with each layer that you create.
    --     However, you can still manually associate a built-in security group
    --     with a layer on. Custom security groups are required only for those
    --     layers that need custom settings.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
    useOpsworksSecurityGroups :: Core.Maybe Core.Bool,
    -- | Contains the information required to retrieve an app or cookbook from a
    -- repository. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
    -- or
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
    customCookbooksSource :: Core.Maybe Source,
    -- | Do not use this parameter. You cannot update a stack\'s service role.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The stack\'s default Availability Zone, which must be in the stack\'s
    -- region. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    -- If you also specify a value for @DefaultSubnetId@, the subnet must be in
    -- the same zone. For more information, see CreateStack.
    defaultAvailabilityZone :: Core.Maybe Core.Text,
    -- | The default AWS OpsWorks Stacks agent version. You have the following
    -- options:
    --
    -- -   Auto-update - Set this parameter to @LATEST@. AWS OpsWorks Stacks
    --     automatically installs new agent versions on the stack\'s instances
    --     as soon as they are available.
    --
    -- -   Fixed version - Set this parameter to your preferred agent version.
    --     To update the agent version, you must edit the stack configuration
    --     and specify a new version. AWS OpsWorks Stacks then automatically
    --     installs that version on the stack\'s instances.
    --
    -- The default setting is @LATEST@. To specify an agent version, you must
    -- use the complete version number, not the abbreviated number shown on the
    -- console. For a list of available agent version numbers, call
    -- DescribeAgentVersions. AgentVersion cannot be set to Chef 12.2.
    --
    -- You can also specify an agent version when you create or update an
    -- instance, which overrides the stack\'s default setting.
    agentVersion :: Core.Maybe Core.Text,
    -- | A string that contains user-defined, custom JSON. It can be used to
    -- override the corresponding default stack configuration JSON values or to
    -- pass data to recipes. The string should be in the following format:
    --
    -- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
    --
    -- For more information about custom JSON, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
    customJson :: Core.Maybe Core.Text,
    -- | The default root device type. This value is used by default for all
    -- instances in the stack, but you can override it when you create an
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
    defaultRootDeviceType :: Core.Maybe RootDeviceType,
    -- | One or more user-defined key-value pairs to be added to the stack
    -- attributes.
    attributes :: Core.Maybe (Core.HashMap StackAttributesKeys (Core.Maybe Core.Text)),
    -- | The stack\'s new name.
    name :: Core.Maybe Core.Text,
    -- | The ARN of an IAM profile that is the default profile for all of the
    -- stack\'s EC2 instances. For more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    defaultInstanceProfileArn :: Core.Maybe Core.Text,
    -- | The stack\'s new host name theme, with spaces replaced by underscores.
    -- The theme is used to generate host names for the stack\'s instances. By
    -- default, @HostnameTheme@ is set to @Layer_Dependent@, which creates host
    -- names by appending integers to the layer\'s short name. The other themes
    -- are:
    --
    -- -   @Baked_Goods@
    --
    -- -   @Clouds@
    --
    -- -   @Europe_Cities@
    --
    -- -   @Fruits@
    --
    -- -   @Greek_Deities_and_Titans@
    --
    -- -   @Legendary_creatures_from_Japan@
    --
    -- -   @Planets_and_Moons@
    --
    -- -   @Roman_Deities@
    --
    -- -   @Scottish_Islands@
    --
    -- -   @US_Cities@
    --
    -- -   @Wild_Cats@
    --
    -- To obtain a generated host name, call @GetHostNameSuggestion@, which
    -- returns a host name based on the current theme.
    hostnameTheme :: Core.Maybe Core.Text,
    -- | A default Amazon EC2 key-pair name. The default value is @none@. If you
    -- specify a key-pair name, AWS OpsWorks Stacks installs the public key on
    -- the instance and you can use the private key with an SSH client to log
    -- in to the instance. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance>
    -- and
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>.
    -- You can override this setting by specifying a different key pair, or no
    -- key pair, when you
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance>.
    defaultSshKeyName :: Core.Maybe Core.Text,
    -- | The configuration manager. When you update a stack, we recommend that
    -- you use the configuration manager to specify the Chef version: 12,
    -- 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default
    -- value for Linux stacks is currently 12.
    configurationManager :: Core.Maybe StackConfigurationManager,
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
    -- and the Berkshelf version on Chef 11.10 stacks. For more information,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
    chefConfiguration :: Core.Maybe ChefConfiguration,
    -- | The stack\'s default VPC subnet ID. This parameter is required if you
    -- specify a value for the @VpcId@ parameter. All instances are launched
    -- into this subnet unless you specify otherwise when you create the
    -- instance. If you also specify a value for @DefaultAvailabilityZone@, the
    -- subnet must be in that zone. For information on default values and when
    -- this parameter is required, see the @VpcId@ parameter description.
    defaultSubnetId :: Core.Maybe Core.Text,
    -- | Whether the stack uses custom cookbooks.
    useCustomCookbooks :: Core.Maybe Core.Bool,
    -- | The stack ID.
    stackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultOs', 'updateStack_defaultOs' - The stack\'s operating system, which must be set to one of the
-- following:
--
-- -   A supported Linux operating system: An Amazon Linux version, such as
--     @Amazon Linux 2018.03@, @Amazon Linux 2017.09@,
--     @Amazon Linux 2017.03@, @Amazon Linux 2016.09@,
--     @Amazon Linux 2016.03@, @Amazon Linux 2015.09@, or
--     @Amazon Linux 2015.03@.
--
-- -   A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@,
--     @Ubuntu 14.04 LTS@, or @Ubuntu 12.04 LTS@.
--
-- -   @CentOS Linux 7@
--
-- -   @Red Hat Enterprise Linux 7@
--
-- -   A supported Windows operating system, such as
--     @Microsoft Windows Server 2012 R2 Base@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
--     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
--
-- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
--     when you create instances. For more information about how to use
--     custom AMIs with OpsWorks, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the stack\'s current operating system. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- 'useOpsworksSecurityGroups', 'updateStack_useOpsworksSecurityGroups' - Whether to associate the AWS OpsWorks Stacks built-in security groups
-- with the stack\'s layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups,
-- one for each layer, which are associated with layers by default.
-- @UseOpsworksSecurityGroups@ allows you to provide your own custom
-- security groups instead of using the built-in groups.
-- @UseOpsworksSecurityGroups@ has the following settings:
--
-- -   True - AWS OpsWorks Stacks automatically associates the appropriate
--     built-in security group with each layer (default setting). You can
--     associate additional security groups with a layer after you create
--     it, but you cannot delete the built-in security group.
--
-- -   False - AWS OpsWorks Stacks does not associate built-in security
--     groups with layers. You must create appropriate EC2 security groups
--     and associate a security group with each layer that you create.
--     However, you can still manually associate a built-in security group
--     with a layer on. Custom security groups are required only for those
--     layers that need custom settings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
--
-- 'customCookbooksSource', 'updateStack_customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
--
-- 'serviceRoleArn', 'updateStack_serviceRoleArn' - Do not use this parameter. You cannot update a stack\'s service role.
--
-- 'defaultAvailabilityZone', 'updateStack_defaultAvailabilityZone' - The stack\'s default Availability Zone, which must be in the stack\'s
-- region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see CreateStack.
--
-- 'agentVersion', 'updateStack_agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following
-- options:
--
-- -   Auto-update - Set this parameter to @LATEST@. AWS OpsWorks Stacks
--     automatically installs new agent versions on the stack\'s instances
--     as soon as they are available.
--
-- -   Fixed version - Set this parameter to your preferred agent version.
--     To update the agent version, you must edit the stack configuration
--     and specify a new version. AWS OpsWorks Stacks then automatically
--     installs that version on the stack\'s instances.
--
-- The default setting is @LATEST@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions. AgentVersion cannot be set to Chef 12.2.
--
-- You can also specify an agent version when you create or update an
-- instance, which overrides the stack\'s default setting.
--
-- 'customJson', 'updateStack_customJson' - A string that contains user-defined, custom JSON. It can be used to
-- override the corresponding default stack configuration JSON values or to
-- pass data to recipes. The string should be in the following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
--
-- 'defaultRootDeviceType', 'updateStack_defaultRootDeviceType' - The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'attributes', 'updateStack_attributes' - One or more user-defined key-value pairs to be added to the stack
-- attributes.
--
-- 'name', 'updateStack_name' - The stack\'s new name.
--
-- 'defaultInstanceProfileArn', 'updateStack_defaultInstanceProfileArn' - The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'hostnameTheme', 'updateStack_hostnameTheme' - The stack\'s new host name theme, with spaces replaced by underscores.
-- The theme is used to generate host names for the stack\'s instances. By
-- default, @HostnameTheme@ is set to @Layer_Dependent@, which creates host
-- names by appending integers to the layer\'s short name. The other themes
-- are:
--
-- -   @Baked_Goods@
--
-- -   @Clouds@
--
-- -   @Europe_Cities@
--
-- -   @Fruits@
--
-- -   @Greek_Deities_and_Titans@
--
-- -   @Legendary_creatures_from_Japan@
--
-- -   @Planets_and_Moons@
--
-- -   @Roman_Deities@
--
-- -   @Scottish_Islands@
--
-- -   @US_Cities@
--
-- -   @Wild_Cats@
--
-- To obtain a generated host name, call @GetHostNameSuggestion@, which
-- returns a host name based on the current theme.
--
-- 'defaultSshKeyName', 'updateStack_defaultSshKeyName' - A default Amazon EC2 key-pair name. The default value is @none@. If you
-- specify a key-pair name, AWS OpsWorks Stacks installs the public key on
-- the instance and you can use the private key with an SSH client to log
-- in to the instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance>
-- and
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>.
-- You can override this setting by specifying a different key pair, or no
-- key pair, when you
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance>.
--
-- 'configurationManager', 'updateStack_configurationManager' - The configuration manager. When you update a stack, we recommend that
-- you use the configuration manager to specify the Chef version: 12,
-- 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default
-- value for Linux stacks is currently 12.
--
-- 'chefConfiguration', 'updateStack_chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
--
-- 'defaultSubnetId', 'updateStack_defaultSubnetId' - The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
--
-- 'useCustomCookbooks', 'updateStack_useCustomCookbooks' - Whether the stack uses custom cookbooks.
--
-- 'stackId', 'updateStack_stackId' - The stack ID.
newUpdateStack ::
  -- | 'stackId'
  Core.Text ->
  UpdateStack
newUpdateStack pStackId_ =
  UpdateStack'
    { defaultOs = Core.Nothing,
      useOpsworksSecurityGroups = Core.Nothing,
      customCookbooksSource = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      defaultAvailabilityZone = Core.Nothing,
      agentVersion = Core.Nothing,
      customJson = Core.Nothing,
      defaultRootDeviceType = Core.Nothing,
      attributes = Core.Nothing,
      name = Core.Nothing,
      defaultInstanceProfileArn = Core.Nothing,
      hostnameTheme = Core.Nothing,
      defaultSshKeyName = Core.Nothing,
      configurationManager = Core.Nothing,
      chefConfiguration = Core.Nothing,
      defaultSubnetId = Core.Nothing,
      useCustomCookbooks = Core.Nothing,
      stackId = pStackId_
    }

-- | The stack\'s operating system, which must be set to one of the
-- following:
--
-- -   A supported Linux operating system: An Amazon Linux version, such as
--     @Amazon Linux 2018.03@, @Amazon Linux 2017.09@,
--     @Amazon Linux 2017.03@, @Amazon Linux 2016.09@,
--     @Amazon Linux 2016.03@, @Amazon Linux 2015.09@, or
--     @Amazon Linux 2015.03@.
--
-- -   A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@,
--     @Ubuntu 14.04 LTS@, or @Ubuntu 12.04 LTS@.
--
-- -   @CentOS Linux 7@
--
-- -   @Red Hat Enterprise Linux 7@
--
-- -   A supported Windows operating system, such as
--     @Microsoft Windows Server 2012 R2 Base@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
--     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
--
-- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
--     when you create instances. For more information about how to use
--     custom AMIs with OpsWorks, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the stack\'s current operating system. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
updateStack_defaultOs :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_defaultOs = Lens.lens (\UpdateStack' {defaultOs} -> defaultOs) (\s@UpdateStack' {} a -> s {defaultOs = a} :: UpdateStack)

-- | Whether to associate the AWS OpsWorks Stacks built-in security groups
-- with the stack\'s layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups,
-- one for each layer, which are associated with layers by default.
-- @UseOpsworksSecurityGroups@ allows you to provide your own custom
-- security groups instead of using the built-in groups.
-- @UseOpsworksSecurityGroups@ has the following settings:
--
-- -   True - AWS OpsWorks Stacks automatically associates the appropriate
--     built-in security group with each layer (default setting). You can
--     associate additional security groups with a layer after you create
--     it, but you cannot delete the built-in security group.
--
-- -   False - AWS OpsWorks Stacks does not associate built-in security
--     groups with layers. You must create appropriate EC2 security groups
--     and associate a security group with each layer that you create.
--     However, you can still manually associate a built-in security group
--     with a layer on. Custom security groups are required only for those
--     layers that need custom settings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
updateStack_useOpsworksSecurityGroups :: Lens.Lens' UpdateStack (Core.Maybe Core.Bool)
updateStack_useOpsworksSecurityGroups = Lens.lens (\UpdateStack' {useOpsworksSecurityGroups} -> useOpsworksSecurityGroups) (\s@UpdateStack' {} a -> s {useOpsworksSecurityGroups = a} :: UpdateStack)

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
updateStack_customCookbooksSource :: Lens.Lens' UpdateStack (Core.Maybe Source)
updateStack_customCookbooksSource = Lens.lens (\UpdateStack' {customCookbooksSource} -> customCookbooksSource) (\s@UpdateStack' {} a -> s {customCookbooksSource = a} :: UpdateStack)

-- | Do not use this parameter. You cannot update a stack\'s service role.
updateStack_serviceRoleArn :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_serviceRoleArn = Lens.lens (\UpdateStack' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateStack' {} a -> s {serviceRoleArn = a} :: UpdateStack)

-- | The stack\'s default Availability Zone, which must be in the stack\'s
-- region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see CreateStack.
updateStack_defaultAvailabilityZone :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_defaultAvailabilityZone = Lens.lens (\UpdateStack' {defaultAvailabilityZone} -> defaultAvailabilityZone) (\s@UpdateStack' {} a -> s {defaultAvailabilityZone = a} :: UpdateStack)

-- | The default AWS OpsWorks Stacks agent version. You have the following
-- options:
--
-- -   Auto-update - Set this parameter to @LATEST@. AWS OpsWorks Stacks
--     automatically installs new agent versions on the stack\'s instances
--     as soon as they are available.
--
-- -   Fixed version - Set this parameter to your preferred agent version.
--     To update the agent version, you must edit the stack configuration
--     and specify a new version. AWS OpsWorks Stacks then automatically
--     installs that version on the stack\'s instances.
--
-- The default setting is @LATEST@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions. AgentVersion cannot be set to Chef 12.2.
--
-- You can also specify an agent version when you create or update an
-- instance, which overrides the stack\'s default setting.
updateStack_agentVersion :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_agentVersion = Lens.lens (\UpdateStack' {agentVersion} -> agentVersion) (\s@UpdateStack' {} a -> s {agentVersion = a} :: UpdateStack)

-- | A string that contains user-defined, custom JSON. It can be used to
-- override the corresponding default stack configuration JSON values or to
-- pass data to recipes. The string should be in the following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
updateStack_customJson :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_customJson = Lens.lens (\UpdateStack' {customJson} -> customJson) (\s@UpdateStack' {} a -> s {customJson = a} :: UpdateStack)

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
updateStack_defaultRootDeviceType :: Lens.Lens' UpdateStack (Core.Maybe RootDeviceType)
updateStack_defaultRootDeviceType = Lens.lens (\UpdateStack' {defaultRootDeviceType} -> defaultRootDeviceType) (\s@UpdateStack' {} a -> s {defaultRootDeviceType = a} :: UpdateStack)

-- | One or more user-defined key-value pairs to be added to the stack
-- attributes.
updateStack_attributes :: Lens.Lens' UpdateStack (Core.Maybe (Core.HashMap StackAttributesKeys (Core.Maybe Core.Text)))
updateStack_attributes = Lens.lens (\UpdateStack' {attributes} -> attributes) (\s@UpdateStack' {} a -> s {attributes = a} :: UpdateStack) Core.. Lens.mapping Lens._Coerce

-- | The stack\'s new name.
updateStack_name :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_name = Lens.lens (\UpdateStack' {name} -> name) (\s@UpdateStack' {} a -> s {name = a} :: UpdateStack)

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
updateStack_defaultInstanceProfileArn :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_defaultInstanceProfileArn = Lens.lens (\UpdateStack' {defaultInstanceProfileArn} -> defaultInstanceProfileArn) (\s@UpdateStack' {} a -> s {defaultInstanceProfileArn = a} :: UpdateStack)

-- | The stack\'s new host name theme, with spaces replaced by underscores.
-- The theme is used to generate host names for the stack\'s instances. By
-- default, @HostnameTheme@ is set to @Layer_Dependent@, which creates host
-- names by appending integers to the layer\'s short name. The other themes
-- are:
--
-- -   @Baked_Goods@
--
-- -   @Clouds@
--
-- -   @Europe_Cities@
--
-- -   @Fruits@
--
-- -   @Greek_Deities_and_Titans@
--
-- -   @Legendary_creatures_from_Japan@
--
-- -   @Planets_and_Moons@
--
-- -   @Roman_Deities@
--
-- -   @Scottish_Islands@
--
-- -   @US_Cities@
--
-- -   @Wild_Cats@
--
-- To obtain a generated host name, call @GetHostNameSuggestion@, which
-- returns a host name based on the current theme.
updateStack_hostnameTheme :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_hostnameTheme = Lens.lens (\UpdateStack' {hostnameTheme} -> hostnameTheme) (\s@UpdateStack' {} a -> s {hostnameTheme = a} :: UpdateStack)

-- | A default Amazon EC2 key-pair name. The default value is @none@. If you
-- specify a key-pair name, AWS OpsWorks Stacks installs the public key on
-- the instance and you can use the private key with an SSH client to log
-- in to the instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance>
-- and
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>.
-- You can override this setting by specifying a different key pair, or no
-- key pair, when you
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance>.
updateStack_defaultSshKeyName :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_defaultSshKeyName = Lens.lens (\UpdateStack' {defaultSshKeyName} -> defaultSshKeyName) (\s@UpdateStack' {} a -> s {defaultSshKeyName = a} :: UpdateStack)

-- | The configuration manager. When you update a stack, we recommend that
-- you use the configuration manager to specify the Chef version: 12,
-- 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default
-- value for Linux stacks is currently 12.
updateStack_configurationManager :: Lens.Lens' UpdateStack (Core.Maybe StackConfigurationManager)
updateStack_configurationManager = Lens.lens (\UpdateStack' {configurationManager} -> configurationManager) (\s@UpdateStack' {} a -> s {configurationManager = a} :: UpdateStack)

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
updateStack_chefConfiguration :: Lens.Lens' UpdateStack (Core.Maybe ChefConfiguration)
updateStack_chefConfiguration = Lens.lens (\UpdateStack' {chefConfiguration} -> chefConfiguration) (\s@UpdateStack' {} a -> s {chefConfiguration = a} :: UpdateStack)

-- | The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
updateStack_defaultSubnetId :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_defaultSubnetId = Lens.lens (\UpdateStack' {defaultSubnetId} -> defaultSubnetId) (\s@UpdateStack' {} a -> s {defaultSubnetId = a} :: UpdateStack)

-- | Whether the stack uses custom cookbooks.
updateStack_useCustomCookbooks :: Lens.Lens' UpdateStack (Core.Maybe Core.Bool)
updateStack_useCustomCookbooks = Lens.lens (\UpdateStack' {useCustomCookbooks} -> useCustomCookbooks) (\s@UpdateStack' {} a -> s {useCustomCookbooks = a} :: UpdateStack)

-- | The stack ID.
updateStack_stackId :: Lens.Lens' UpdateStack Core.Text
updateStack_stackId = Lens.lens (\UpdateStack' {stackId} -> stackId) (\s@UpdateStack' {} a -> s {stackId = a} :: UpdateStack)

instance Core.AWSRequest UpdateStack where
  type AWSResponse UpdateStack = UpdateStackResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull UpdateStackResponse'

instance Core.Hashable UpdateStack

instance Core.NFData UpdateStack

instance Core.ToHeaders UpdateStack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OpsWorks_20130218.UpdateStack" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateStack where
  toJSON UpdateStack' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultOs" Core..=) Core.<$> defaultOs,
            ("UseOpsworksSecurityGroups" Core..=)
              Core.<$> useOpsworksSecurityGroups,
            ("CustomCookbooksSource" Core..=)
              Core.<$> customCookbooksSource,
            ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
            ("DefaultAvailabilityZone" Core..=)
              Core.<$> defaultAvailabilityZone,
            ("AgentVersion" Core..=) Core.<$> agentVersion,
            ("CustomJson" Core..=) Core.<$> customJson,
            ("DefaultRootDeviceType" Core..=)
              Core.<$> defaultRootDeviceType,
            ("Attributes" Core..=) Core.<$> attributes,
            ("Name" Core..=) Core.<$> name,
            ("DefaultInstanceProfileArn" Core..=)
              Core.<$> defaultInstanceProfileArn,
            ("HostnameTheme" Core..=) Core.<$> hostnameTheme,
            ("DefaultSshKeyName" Core..=)
              Core.<$> defaultSshKeyName,
            ("ConfigurationManager" Core..=)
              Core.<$> configurationManager,
            ("ChefConfiguration" Core..=)
              Core.<$> chefConfiguration,
            ("DefaultSubnetId" Core..=) Core.<$> defaultSubnetId,
            ("UseCustomCookbooks" Core..=)
              Core.<$> useCustomCookbooks,
            Core.Just ("StackId" Core..= stackId)
          ]
      )

instance Core.ToPath UpdateStack where
  toPath = Core.const "/"

instance Core.ToQuery UpdateStack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateStackResponse ::
  UpdateStackResponse
newUpdateStackResponse = UpdateStackResponse'

instance Core.NFData UpdateStackResponse
