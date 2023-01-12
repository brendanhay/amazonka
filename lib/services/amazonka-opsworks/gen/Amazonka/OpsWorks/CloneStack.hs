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
-- Module      : Amazonka.OpsWorks.CloneStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a clone of a specified stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-cloning.html Clone a Stack>.
-- By default, all parameters are set to the values used by the parent
-- stack.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.CloneStack
  ( -- * Creating a Request
    CloneStack (..),
    newCloneStack,

    -- * Request Lenses
    cloneStack_agentVersion,
    cloneStack_attributes,
    cloneStack_chefConfiguration,
    cloneStack_cloneAppIds,
    cloneStack_clonePermissions,
    cloneStack_configurationManager,
    cloneStack_customCookbooksSource,
    cloneStack_customJson,
    cloneStack_defaultAvailabilityZone,
    cloneStack_defaultInstanceProfileArn,
    cloneStack_defaultOs,
    cloneStack_defaultRootDeviceType,
    cloneStack_defaultSshKeyName,
    cloneStack_defaultSubnetId,
    cloneStack_hostnameTheme,
    cloneStack_name,
    cloneStack_region,
    cloneStack_useCustomCookbooks,
    cloneStack_useOpsworksSecurityGroups,
    cloneStack_vpcId,
    cloneStack_sourceStackId,
    cloneStack_serviceRoleArn,

    -- * Destructuring the Response
    CloneStackResponse (..),
    newCloneStackResponse,

    -- * Response Lenses
    cloneStackResponse_stackId,
    cloneStackResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCloneStack' smart constructor.
data CloneStack = CloneStack'
  { -- | The default AWS OpsWorks Stacks agent version. You have the following
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
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of stack attributes and values as key\/value pairs to be added to
    -- the cloned stack.
    attributes :: Prelude.Maybe (Prelude.HashMap StackAttributesKeys (Prelude.Maybe Prelude.Text)),
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
    -- and the Berkshelf version on Chef 11.10 stacks. For more information,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
    chefConfiguration :: Prelude.Maybe ChefConfiguration,
    -- | A list of source stack app IDs to be included in the cloned stack.
    cloneAppIds :: Prelude.Maybe [Prelude.Text],
    -- | Whether to clone the source stack\'s permissions.
    clonePermissions :: Prelude.Maybe Prelude.Bool,
    -- | The configuration manager. When you clone a stack we recommend that you
    -- use the configuration manager to specify the Chef version: 12, 11.10, or
    -- 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for
    -- Linux stacks is currently 12.
    configurationManager :: Prelude.Maybe StackConfigurationManager,
    -- | Contains the information required to retrieve an app or cookbook from a
    -- repository. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
    -- or
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
    customCookbooksSource :: Prelude.Maybe Source,
    -- | A string that contains user-defined, custom JSON. It is used to override
    -- the corresponding default stack configuration JSON values. The string
    -- should be in the following format:
    --
    -- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
    --
    -- For more information about custom JSON, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
    customJson :: Prelude.Maybe Prelude.Text,
    -- | The cloned stack\'s default Availability Zone, which must be in the
    -- specified region. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    -- If you also specify a value for @DefaultSubnetId@, the subnet must be in
    -- the same zone. For more information, see the @VpcId@ parameter
    -- description.
    defaultAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM profile that is the default
    -- profile for all of the stack\'s EC2 instances. For more information
    -- about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    defaultInstanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s operating system, which must be set to one of the
    -- following.
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
    -- -   @Microsoft Windows Server 2012 R2 Base@,
    --     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
    --     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
    --     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
    --
    -- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
    --     when you create instances. For more information about how to use
    --     custom AMIs with OpsWorks, see
    --     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
    --
    -- The default option is the parent stack\'s operating system. For more
    -- information about supported operating systems, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
    --
    -- You can specify a different Linux operating system for the cloned stack,
    -- but you cannot change from Linux to Windows or Windows to Linux.
    defaultOs :: Prelude.Maybe Prelude.Text,
    -- | The default root device type. This value is used by default for all
    -- instances in the cloned stack, but you can override it when you create
    -- an instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
    defaultRootDeviceType :: Prelude.Maybe RootDeviceType,
    -- | A default Amazon EC2 key pair name. The default value is none. If you
    -- specify a key pair name, AWS OpsWorks installs the public key on the
    -- instance and you can use the private key with an SSH client to log in to
    -- the instance. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance>
    -- and
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>.
    -- You can override this setting by specifying a different key pair, or no
    -- key pair, when you
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance>.
    defaultSshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s default VPC subnet ID. This parameter is required if you
    -- specify a value for the @VpcId@ parameter. All instances are launched
    -- into this subnet unless you specify otherwise when you create the
    -- instance. If you also specify a value for @DefaultAvailabilityZone@, the
    -- subnet must be in that zone. For information on default values and when
    -- this parameter is required, see the @VpcId@ parameter description.
    defaultSubnetId :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s host name theme, with spaces are replaced by underscores.
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
    hostnameTheme :: Prelude.Maybe Prelude.Text,
    -- | The cloned stack name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The cloned stack AWS region, such as \"ap-northeast-2\". For more
    -- information about AWS regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    region :: Prelude.Maybe Prelude.Text,
    -- | Whether to use custom cookbooks.
    useCustomCookbooks :: Prelude.Maybe Prelude.Bool,
    -- | Whether to associate the AWS OpsWorks Stacks built-in security groups
    -- with the stack\'s layers.
    --
    -- AWS OpsWorks Stacks provides a standard set of built-in security groups,
    -- one for each layer, which are associated with layers by default. With
    -- @UseOpsworksSecurityGroups@ you can instead provide your own custom
    -- security groups. @UseOpsworksSecurityGroups@ has the following settings:
    --
    -- -   True - AWS OpsWorks Stacks automatically associates the appropriate
    --     built-in security group with each layer (default setting). You can
    --     associate additional security groups with a layer after you create
    --     it but you cannot delete the built-in security group.
    --
    -- -   False - AWS OpsWorks Stacks does not associate built-in security
    --     groups with layers. You must create appropriate Amazon Elastic
    --     Compute Cloud (Amazon EC2) security groups and associate a security
    --     group with each layer that you create. However, you can still
    --     manually associate a built-in security group with a layer on
    --     creation; custom security groups are required only for those layers
    --     that need custom settings.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
    useOpsworksSecurityGroups :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC that the cloned stack is to be launched into. It must
    -- be in the specified region. All instances are launched into this VPC,
    -- and you cannot change the ID later.
    --
    -- -   If your account supports EC2 Classic, the default value is no VPC.
    --
    -- -   If your account does not support EC2 Classic, the default value is
    --     the default VPC for the specified region.
    --
    -- If the VPC ID corresponds to a default VPC and you have specified either
    -- the @DefaultAvailabilityZone@ or the @DefaultSubnetId@ parameter only,
    -- AWS OpsWorks Stacks infers the value of the other parameter. If you
    -- specify neither parameter, AWS OpsWorks Stacks sets these parameters to
    -- the first valid Availability Zone for the specified region and the
    -- corresponding default VPC subnet ID, respectively.
    --
    -- If you specify a nondefault VPC ID, note the following:
    --
    -- -   It must belong to a VPC in your account that is in the specified
    --     region.
    --
    -- -   You must specify a value for @DefaultSubnetId@.
    --
    -- For more information about how to use AWS OpsWorks Stacks with a VPC,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC>.
    -- For more information about default VPC and EC2 Classic, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The source stack ID.
    sourceStackId :: Prelude.Text,
    -- | The stack AWS Identity and Access Management (IAM) role, which allows
    -- AWS OpsWorks Stacks to work with AWS resources on your behalf. You must
    -- set this parameter to the Amazon Resource Name (ARN) for an existing IAM
    -- role. If you create a stack by using the AWS OpsWorks Stacks console, it
    -- creates the role for you. You can obtain an existing stack\'s IAM ARN
    -- programmatically by calling DescribePermissions. For more information
    -- about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    --
    -- You must set this parameter to a valid service role ARN or the action
    -- will fail; there is no default value. You can specify the source
    -- stack\'s service role ARN, if you prefer, but you must do so explicitly.
    serviceRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloneStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentVersion', 'cloneStack_agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following
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
-- 'attributes', 'cloneStack_attributes' - A list of stack attributes and values as key\/value pairs to be added to
-- the cloned stack.
--
-- 'chefConfiguration', 'cloneStack_chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
--
-- 'cloneAppIds', 'cloneStack_cloneAppIds' - A list of source stack app IDs to be included in the cloned stack.
--
-- 'clonePermissions', 'cloneStack_clonePermissions' - Whether to clone the source stack\'s permissions.
--
-- 'configurationManager', 'cloneStack_configurationManager' - The configuration manager. When you clone a stack we recommend that you
-- use the configuration manager to specify the Chef version: 12, 11.10, or
-- 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for
-- Linux stacks is currently 12.
--
-- 'customCookbooksSource', 'cloneStack_customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
--
-- 'customJson', 'cloneStack_customJson' - A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
--
-- 'defaultAvailabilityZone', 'cloneStack_defaultAvailabilityZone' - The cloned stack\'s default Availability Zone, which must be in the
-- specified region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
--
-- 'defaultInstanceProfileArn', 'cloneStack_defaultInstanceProfileArn' - The Amazon Resource Name (ARN) of an IAM profile that is the default
-- profile for all of the stack\'s EC2 instances. For more information
-- about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'defaultOs', 'cloneStack_defaultOs' - The stack\'s operating system, which must be set to one of the
-- following.
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
-- -   @Microsoft Windows Server 2012 R2 Base@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
--     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
--
-- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
--     when you create instances. For more information about how to use
--     custom AMIs with OpsWorks, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the parent stack\'s operating system. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- You can specify a different Linux operating system for the cloned stack,
-- but you cannot change from Linux to Windows or Windows to Linux.
--
-- 'defaultRootDeviceType', 'cloneStack_defaultRootDeviceType' - The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create
-- an instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'defaultSshKeyName', 'cloneStack_defaultSshKeyName' - A default Amazon EC2 key pair name. The default value is none. If you
-- specify a key pair name, AWS OpsWorks installs the public key on the
-- instance and you can use the private key with an SSH client to log in to
-- the instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance>
-- and
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>.
-- You can override this setting by specifying a different key pair, or no
-- key pair, when you
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance>.
--
-- 'defaultSubnetId', 'cloneStack_defaultSubnetId' - The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
--
-- 'hostnameTheme', 'cloneStack_hostnameTheme' - The stack\'s host name theme, with spaces are replaced by underscores.
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
-- 'name', 'cloneStack_name' - The cloned stack name.
--
-- 'region', 'cloneStack_region' - The cloned stack AWS region, such as \"ap-northeast-2\". For more
-- information about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'useCustomCookbooks', 'cloneStack_useCustomCookbooks' - Whether to use custom cookbooks.
--
-- 'useOpsworksSecurityGroups', 'cloneStack_useOpsworksSecurityGroups' - Whether to associate the AWS OpsWorks Stacks built-in security groups
-- with the stack\'s layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups,
-- one for each layer, which are associated with layers by default. With
-- @UseOpsworksSecurityGroups@ you can instead provide your own custom
-- security groups. @UseOpsworksSecurityGroups@ has the following settings:
--
-- -   True - AWS OpsWorks Stacks automatically associates the appropriate
--     built-in security group with each layer (default setting). You can
--     associate additional security groups with a layer after you create
--     it but you cannot delete the built-in security group.
--
-- -   False - AWS OpsWorks Stacks does not associate built-in security
--     groups with layers. You must create appropriate Amazon Elastic
--     Compute Cloud (Amazon EC2) security groups and associate a security
--     group with each layer that you create. However, you can still
--     manually associate a built-in security group with a layer on
--     creation; custom security groups are required only for those layers
--     that need custom settings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
--
-- 'vpcId', 'cloneStack_vpcId' - The ID of the VPC that the cloned stack is to be launched into. It must
-- be in the specified region. All instances are launched into this VPC,
-- and you cannot change the ID later.
--
-- -   If your account supports EC2 Classic, the default value is no VPC.
--
-- -   If your account does not support EC2 Classic, the default value is
--     the default VPC for the specified region.
--
-- If the VPC ID corresponds to a default VPC and you have specified either
-- the @DefaultAvailabilityZone@ or the @DefaultSubnetId@ parameter only,
-- AWS OpsWorks Stacks infers the value of the other parameter. If you
-- specify neither parameter, AWS OpsWorks Stacks sets these parameters to
-- the first valid Availability Zone for the specified region and the
-- corresponding default VPC subnet ID, respectively.
--
-- If you specify a nondefault VPC ID, note the following:
--
-- -   It must belong to a VPC in your account that is in the specified
--     region.
--
-- -   You must specify a value for @DefaultSubnetId@.
--
-- For more information about how to use AWS OpsWorks Stacks with a VPC,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC>.
-- For more information about default VPC and EC2 Classic, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
--
-- 'sourceStackId', 'cloneStack_sourceStackId' - The source stack ID.
--
-- 'serviceRoleArn', 'cloneStack_serviceRoleArn' - The stack AWS Identity and Access Management (IAM) role, which allows
-- AWS OpsWorks Stacks to work with AWS resources on your behalf. You must
-- set this parameter to the Amazon Resource Name (ARN) for an existing IAM
-- role. If you create a stack by using the AWS OpsWorks Stacks console, it
-- creates the role for you. You can obtain an existing stack\'s IAM ARN
-- programmatically by calling DescribePermissions. For more information
-- about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- You must set this parameter to a valid service role ARN or the action
-- will fail; there is no default value. You can specify the source
-- stack\'s service role ARN, if you prefer, but you must do so explicitly.
newCloneStack ::
  -- | 'sourceStackId'
  Prelude.Text ->
  -- | 'serviceRoleArn'
  Prelude.Text ->
  CloneStack
newCloneStack pSourceStackId_ pServiceRoleArn_ =
  CloneStack'
    { agentVersion = Prelude.Nothing,
      attributes = Prelude.Nothing,
      chefConfiguration = Prelude.Nothing,
      cloneAppIds = Prelude.Nothing,
      clonePermissions = Prelude.Nothing,
      configurationManager = Prelude.Nothing,
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
      useCustomCookbooks = Prelude.Nothing,
      useOpsworksSecurityGroups = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      sourceStackId = pSourceStackId_,
      serviceRoleArn = pServiceRoleArn_
    }

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
cloneStack_agentVersion :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_agentVersion = Lens.lens (\CloneStack' {agentVersion} -> agentVersion) (\s@CloneStack' {} a -> s {agentVersion = a} :: CloneStack)

-- | A list of stack attributes and values as key\/value pairs to be added to
-- the cloned stack.
cloneStack_attributes :: Lens.Lens' CloneStack (Prelude.Maybe (Prelude.HashMap StackAttributesKeys (Prelude.Maybe Prelude.Text)))
cloneStack_attributes = Lens.lens (\CloneStack' {attributes} -> attributes) (\s@CloneStack' {} a -> s {attributes = a} :: CloneStack) Prelude.. Lens.mapping Lens.coerced

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
cloneStack_chefConfiguration :: Lens.Lens' CloneStack (Prelude.Maybe ChefConfiguration)
cloneStack_chefConfiguration = Lens.lens (\CloneStack' {chefConfiguration} -> chefConfiguration) (\s@CloneStack' {} a -> s {chefConfiguration = a} :: CloneStack)

-- | A list of source stack app IDs to be included in the cloned stack.
cloneStack_cloneAppIds :: Lens.Lens' CloneStack (Prelude.Maybe [Prelude.Text])
cloneStack_cloneAppIds = Lens.lens (\CloneStack' {cloneAppIds} -> cloneAppIds) (\s@CloneStack' {} a -> s {cloneAppIds = a} :: CloneStack) Prelude.. Lens.mapping Lens.coerced

-- | Whether to clone the source stack\'s permissions.
cloneStack_clonePermissions :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Bool)
cloneStack_clonePermissions = Lens.lens (\CloneStack' {clonePermissions} -> clonePermissions) (\s@CloneStack' {} a -> s {clonePermissions = a} :: CloneStack)

-- | The configuration manager. When you clone a stack we recommend that you
-- use the configuration manager to specify the Chef version: 12, 11.10, or
-- 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for
-- Linux stacks is currently 12.
cloneStack_configurationManager :: Lens.Lens' CloneStack (Prelude.Maybe StackConfigurationManager)
cloneStack_configurationManager = Lens.lens (\CloneStack' {configurationManager} -> configurationManager) (\s@CloneStack' {} a -> s {configurationManager = a} :: CloneStack)

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
cloneStack_customCookbooksSource :: Lens.Lens' CloneStack (Prelude.Maybe Source)
cloneStack_customCookbooksSource = Lens.lens (\CloneStack' {customCookbooksSource} -> customCookbooksSource) (\s@CloneStack' {} a -> s {customCookbooksSource = a} :: CloneStack)

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
cloneStack_customJson :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_customJson = Lens.lens (\CloneStack' {customJson} -> customJson) (\s@CloneStack' {} a -> s {customJson = a} :: CloneStack)

-- | The cloned stack\'s default Availability Zone, which must be in the
-- specified region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
cloneStack_defaultAvailabilityZone :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_defaultAvailabilityZone = Lens.lens (\CloneStack' {defaultAvailabilityZone} -> defaultAvailabilityZone) (\s@CloneStack' {} a -> s {defaultAvailabilityZone = a} :: CloneStack)

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default
-- profile for all of the stack\'s EC2 instances. For more information
-- about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
cloneStack_defaultInstanceProfileArn :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_defaultInstanceProfileArn = Lens.lens (\CloneStack' {defaultInstanceProfileArn} -> defaultInstanceProfileArn) (\s@CloneStack' {} a -> s {defaultInstanceProfileArn = a} :: CloneStack)

-- | The stack\'s operating system, which must be set to one of the
-- following.
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
-- -   @Microsoft Windows Server 2012 R2 Base@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
--     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
--
-- -   A custom AMI: @Custom@. You specify the custom AMI you want to use
--     when you create instances. For more information about how to use
--     custom AMIs with OpsWorks, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the parent stack\'s operating system. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- You can specify a different Linux operating system for the cloned stack,
-- but you cannot change from Linux to Windows or Windows to Linux.
cloneStack_defaultOs :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_defaultOs = Lens.lens (\CloneStack' {defaultOs} -> defaultOs) (\s@CloneStack' {} a -> s {defaultOs = a} :: CloneStack)

-- | The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create
-- an instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
cloneStack_defaultRootDeviceType :: Lens.Lens' CloneStack (Prelude.Maybe RootDeviceType)
cloneStack_defaultRootDeviceType = Lens.lens (\CloneStack' {defaultRootDeviceType} -> defaultRootDeviceType) (\s@CloneStack' {} a -> s {defaultRootDeviceType = a} :: CloneStack)

-- | A default Amazon EC2 key pair name. The default value is none. If you
-- specify a key pair name, AWS OpsWorks installs the public key on the
-- instance and you can use the private key with an SSH client to log in to
-- the instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance>
-- and
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access>.
-- You can override this setting by specifying a different key pair, or no
-- key pair, when you
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance>.
cloneStack_defaultSshKeyName :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_defaultSshKeyName = Lens.lens (\CloneStack' {defaultSshKeyName} -> defaultSshKeyName) (\s@CloneStack' {} a -> s {defaultSshKeyName = a} :: CloneStack)

-- | The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
cloneStack_defaultSubnetId :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_defaultSubnetId = Lens.lens (\CloneStack' {defaultSubnetId} -> defaultSubnetId) (\s@CloneStack' {} a -> s {defaultSubnetId = a} :: CloneStack)

-- | The stack\'s host name theme, with spaces are replaced by underscores.
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
cloneStack_hostnameTheme :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_hostnameTheme = Lens.lens (\CloneStack' {hostnameTheme} -> hostnameTheme) (\s@CloneStack' {} a -> s {hostnameTheme = a} :: CloneStack)

-- | The cloned stack name.
cloneStack_name :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_name = Lens.lens (\CloneStack' {name} -> name) (\s@CloneStack' {} a -> s {name = a} :: CloneStack)

-- | The cloned stack AWS region, such as \"ap-northeast-2\". For more
-- information about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
cloneStack_region :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_region = Lens.lens (\CloneStack' {region} -> region) (\s@CloneStack' {} a -> s {region = a} :: CloneStack)

-- | Whether to use custom cookbooks.
cloneStack_useCustomCookbooks :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Bool)
cloneStack_useCustomCookbooks = Lens.lens (\CloneStack' {useCustomCookbooks} -> useCustomCookbooks) (\s@CloneStack' {} a -> s {useCustomCookbooks = a} :: CloneStack)

-- | Whether to associate the AWS OpsWorks Stacks built-in security groups
-- with the stack\'s layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups,
-- one for each layer, which are associated with layers by default. With
-- @UseOpsworksSecurityGroups@ you can instead provide your own custom
-- security groups. @UseOpsworksSecurityGroups@ has the following settings:
--
-- -   True - AWS OpsWorks Stacks automatically associates the appropriate
--     built-in security group with each layer (default setting). You can
--     associate additional security groups with a layer after you create
--     it but you cannot delete the built-in security group.
--
-- -   False - AWS OpsWorks Stacks does not associate built-in security
--     groups with layers. You must create appropriate Amazon Elastic
--     Compute Cloud (Amazon EC2) security groups and associate a security
--     group with each layer that you create. However, you can still
--     manually associate a built-in security group with a layer on
--     creation; custom security groups are required only for those layers
--     that need custom settings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
cloneStack_useOpsworksSecurityGroups :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Bool)
cloneStack_useOpsworksSecurityGroups = Lens.lens (\CloneStack' {useOpsworksSecurityGroups} -> useOpsworksSecurityGroups) (\s@CloneStack' {} a -> s {useOpsworksSecurityGroups = a} :: CloneStack)

-- | The ID of the VPC that the cloned stack is to be launched into. It must
-- be in the specified region. All instances are launched into this VPC,
-- and you cannot change the ID later.
--
-- -   If your account supports EC2 Classic, the default value is no VPC.
--
-- -   If your account does not support EC2 Classic, the default value is
--     the default VPC for the specified region.
--
-- If the VPC ID corresponds to a default VPC and you have specified either
-- the @DefaultAvailabilityZone@ or the @DefaultSubnetId@ parameter only,
-- AWS OpsWorks Stacks infers the value of the other parameter. If you
-- specify neither parameter, AWS OpsWorks Stacks sets these parameters to
-- the first valid Availability Zone for the specified region and the
-- corresponding default VPC subnet ID, respectively.
--
-- If you specify a nondefault VPC ID, note the following:
--
-- -   It must belong to a VPC in your account that is in the specified
--     region.
--
-- -   You must specify a value for @DefaultSubnetId@.
--
-- For more information about how to use AWS OpsWorks Stacks with a VPC,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC>.
-- For more information about default VPC and EC2 Classic, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
cloneStack_vpcId :: Lens.Lens' CloneStack (Prelude.Maybe Prelude.Text)
cloneStack_vpcId = Lens.lens (\CloneStack' {vpcId} -> vpcId) (\s@CloneStack' {} a -> s {vpcId = a} :: CloneStack)

-- | The source stack ID.
cloneStack_sourceStackId :: Lens.Lens' CloneStack Prelude.Text
cloneStack_sourceStackId = Lens.lens (\CloneStack' {sourceStackId} -> sourceStackId) (\s@CloneStack' {} a -> s {sourceStackId = a} :: CloneStack)

-- | The stack AWS Identity and Access Management (IAM) role, which allows
-- AWS OpsWorks Stacks to work with AWS resources on your behalf. You must
-- set this parameter to the Amazon Resource Name (ARN) for an existing IAM
-- role. If you create a stack by using the AWS OpsWorks Stacks console, it
-- creates the role for you. You can obtain an existing stack\'s IAM ARN
-- programmatically by calling DescribePermissions. For more information
-- about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- You must set this parameter to a valid service role ARN or the action
-- will fail; there is no default value. You can specify the source
-- stack\'s service role ARN, if you prefer, but you must do so explicitly.
cloneStack_serviceRoleArn :: Lens.Lens' CloneStack Prelude.Text
cloneStack_serviceRoleArn = Lens.lens (\CloneStack' {serviceRoleArn} -> serviceRoleArn) (\s@CloneStack' {} a -> s {serviceRoleArn = a} :: CloneStack)

instance Core.AWSRequest CloneStack where
  type AWSResponse CloneStack = CloneStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CloneStackResponse'
            Prelude.<$> (x Data..?> "StackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CloneStack where
  hashWithSalt _salt CloneStack' {..} =
    _salt `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` chefConfiguration
      `Prelude.hashWithSalt` cloneAppIds
      `Prelude.hashWithSalt` clonePermissions
      `Prelude.hashWithSalt` configurationManager
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
      `Prelude.hashWithSalt` useCustomCookbooks
      `Prelude.hashWithSalt` useOpsworksSecurityGroups
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` sourceStackId
      `Prelude.hashWithSalt` serviceRoleArn

instance Prelude.NFData CloneStack where
  rnf CloneStack' {..} =
    Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf chefConfiguration
      `Prelude.seq` Prelude.rnf cloneAppIds
      `Prelude.seq` Prelude.rnf clonePermissions
      `Prelude.seq` Prelude.rnf configurationManager
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
      `Prelude.seq` Prelude.rnf useCustomCookbooks
      `Prelude.seq` Prelude.rnf
        useOpsworksSecurityGroups
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf sourceStackId
      `Prelude.seq` Prelude.rnf serviceRoleArn

instance Data.ToHeaders CloneStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.CloneStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CloneStack where
  toJSON CloneStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentVersion" Data..=) Prelude.<$> agentVersion,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("ChefConfiguration" Data..=)
              Prelude.<$> chefConfiguration,
            ("CloneAppIds" Data..=) Prelude.<$> cloneAppIds,
            ("ClonePermissions" Data..=)
              Prelude.<$> clonePermissions,
            ("ConfigurationManager" Data..=)
              Prelude.<$> configurationManager,
            ("CustomCookbooksSource" Data..=)
              Prelude.<$> customCookbooksSource,
            ("CustomJson" Data..=) Prelude.<$> customJson,
            ("DefaultAvailabilityZone" Data..=)
              Prelude.<$> defaultAvailabilityZone,
            ("DefaultInstanceProfileArn" Data..=)
              Prelude.<$> defaultInstanceProfileArn,
            ("DefaultOs" Data..=) Prelude.<$> defaultOs,
            ("DefaultRootDeviceType" Data..=)
              Prelude.<$> defaultRootDeviceType,
            ("DefaultSshKeyName" Data..=)
              Prelude.<$> defaultSshKeyName,
            ("DefaultSubnetId" Data..=)
              Prelude.<$> defaultSubnetId,
            ("HostnameTheme" Data..=) Prelude.<$> hostnameTheme,
            ("Name" Data..=) Prelude.<$> name,
            ("Region" Data..=) Prelude.<$> region,
            ("UseCustomCookbooks" Data..=)
              Prelude.<$> useCustomCookbooks,
            ("UseOpsworksSecurityGroups" Data..=)
              Prelude.<$> useOpsworksSecurityGroups,
            ("VpcId" Data..=) Prelude.<$> vpcId,
            Prelude.Just ("SourceStackId" Data..= sourceStackId),
            Prelude.Just
              ("ServiceRoleArn" Data..= serviceRoleArn)
          ]
      )

instance Data.ToPath CloneStack where
  toPath = Prelude.const "/"

instance Data.ToQuery CloneStack where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @CloneStack@ request.
--
-- /See:/ 'newCloneStackResponse' smart constructor.
data CloneStackResponse = CloneStackResponse'
  { -- | The cloned stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloneStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'cloneStackResponse_stackId' - The cloned stack ID.
--
-- 'httpStatus', 'cloneStackResponse_httpStatus' - The response's http status code.
newCloneStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CloneStackResponse
newCloneStackResponse pHttpStatus_ =
  CloneStackResponse'
    { stackId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cloned stack ID.
cloneStackResponse_stackId :: Lens.Lens' CloneStackResponse (Prelude.Maybe Prelude.Text)
cloneStackResponse_stackId = Lens.lens (\CloneStackResponse' {stackId} -> stackId) (\s@CloneStackResponse' {} a -> s {stackId = a} :: CloneStackResponse)

-- | The response's http status code.
cloneStackResponse_httpStatus :: Lens.Lens' CloneStackResponse Prelude.Int
cloneStackResponse_httpStatus = Lens.lens (\CloneStackResponse' {httpStatus} -> httpStatus) (\s@CloneStackResponse' {} a -> s {httpStatus = a} :: CloneStackResponse)

instance Prelude.NFData CloneStackResponse where
  rnf CloneStackResponse' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf httpStatus
