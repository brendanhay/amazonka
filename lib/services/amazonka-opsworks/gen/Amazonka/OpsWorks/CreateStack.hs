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
-- Module      : Amazonka.OpsWorks.CreateStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-edit.html Create a New Stack>.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.CreateStack
  ( -- * Creating a Request
    CreateStack (..),
    newCreateStack,

    -- * Request Lenses
    createStack_agentVersion,
    createStack_attributes,
    createStack_chefConfiguration,
    createStack_configurationManager,
    createStack_customCookbooksSource,
    createStack_customJson,
    createStack_defaultAvailabilityZone,
    createStack_defaultOs,
    createStack_defaultRootDeviceType,
    createStack_defaultSshKeyName,
    createStack_defaultSubnetId,
    createStack_hostnameTheme,
    createStack_useCustomCookbooks,
    createStack_useOpsworksSecurityGroups,
    createStack_vpcId,
    createStack_name,
    createStack_region,
    createStack_serviceRoleArn,
    createStack_defaultInstanceProfileArn,

    -- * Destructuring the Response
    CreateStackResponse (..),
    newCreateStackResponse,

    -- * Response Lenses
    createStackResponse_stackId,
    createStackResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStack' smart constructor.
data CreateStack = CreateStack'
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
    -- The default setting is the most recent release of the agent. To specify
    -- an agent version, you must use the complete version number, not the
    -- abbreviated number shown on the console. For a list of available agent
    -- version numbers, call DescribeAgentVersions. AgentVersion cannot be set
    -- to Chef 12.2.
    --
    -- You can also specify an agent version when you create or update an
    -- instance, which overrides the stack\'s default setting.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | One or more user-defined key-value pairs to be added to the stack
    -- attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap StackAttributesKeys (Prelude.Maybe Prelude.Text)),
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
    -- and the Berkshelf version on Chef 11.10 stacks. For more information,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
    chefConfiguration :: Prelude.Maybe ChefConfiguration,
    -- | The configuration manager. When you create a stack we recommend that you
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
    -- | A string that contains user-defined, custom JSON. It can be used to
    -- override the corresponding default stack configuration attribute values
    -- or to pass data to recipes. The string should be in the following
    -- format:
    --
    -- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
    --
    -- For more information about custom JSON, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
    customJson :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s default Availability Zone, which must be in the specified
    -- region. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    -- If you also specify a value for @DefaultSubnetId@, the subnet must be in
    -- the same zone. For more information, see the @VpcId@ parameter
    -- description.
    defaultAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The stack\'s default operating system, which is installed on every
    -- instance unless you specify a different operating system when you create
    -- the instance. You can specify one of the following.
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
    --     when you create instances. For more information, see
    --     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
    --
    -- The default option is the current Amazon Linux version. For more
    -- information about supported operating systems, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
    defaultOs :: Prelude.Maybe Prelude.Text,
    -- | The default root device type. This value is the default for all
    -- instances in the stack, but you can override it when you create an
    -- instance. The default option is @instance-store@. For more information,
    -- see
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
    -- | The stack\'s host name theme, with spaces replaced by underscores. The
    -- theme is used to generate host names for the stack\'s instances. By
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
    -- | Whether the stack uses custom cookbooks.
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
    --     it, but you cannot delete the built-in security group.
    --
    -- -   False - AWS OpsWorks Stacks does not associate built-in security
    --     groups with layers. You must create appropriate EC2 security groups
    --     and associate a security group with each layer that you create.
    --     However, you can still manually associate a built-in security group
    --     with a layer on creation; custom security groups are required only
    --     for those layers that need custom settings.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
    useOpsworksSecurityGroups :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC that the stack is to be launched into. The VPC must be
    -- in the stack\'s region. All instances are launched into this VPC. You
    -- cannot change the ID later.
    --
    -- -   If your account supports EC2-Classic, the default value is @no VPC@.
    --
    -- -   If your account does not support EC2-Classic, the default value is
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
    -- For more information about default VPC and EC2-Classic, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The stack name.
    name :: Prelude.Text,
    -- | The stack\'s AWS region, such as @ap-south-1@. For more information
    -- about Amazon regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    --
    -- In the AWS CLI, this API maps to the @--stack-region@ parameter. If the
    -- @--stack-region@ parameter and the AWS CLI common parameter @--region@
    -- are set to the same value, the stack uses a /regional/ endpoint. If the
    -- @--stack-region@ parameter is not set, but the AWS CLI @--region@
    -- parameter is, this also results in a stack with a /regional/ endpoint.
    -- However, if the @--region@ parameter is set to @us-east-1@, and the
    -- @--stack-region@ parameter is set to one of the following, then the
    -- stack uses a legacy or /classic/ region:
    -- @us-west-1, us-west-2, sa-east-1, eu-central-1, eu-west-1, ap-northeast-1, ap-southeast-1, ap-southeast-2@.
    -- In this case, the actual API endpoint of the stack is in @us-east-1@.
    -- Only the preceding regions are supported as classic regions in the
    -- @us-east-1@ API endpoint. Because it is a best practice to choose the
    -- regional endpoint that is closest to where you manage AWS, we recommend
    -- that you use regional endpoints for new stacks. The AWS CLI common
    -- @--region@ parameter always specifies a regional API endpoint; it cannot
    -- be used to specify a classic AWS OpsWorks Stacks region.
    region :: Prelude.Text,
    -- | The stack\'s AWS Identity and Access Management (IAM) role, which allows
    -- AWS OpsWorks Stacks to work with AWS resources on your behalf. You must
    -- set this parameter to the Amazon Resource Name (ARN) for an existing IAM
    -- role. For more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    serviceRoleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM profile that is the default
    -- profile for all of the stack\'s EC2 instances. For more information
    -- about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    defaultInstanceProfileArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentVersion', 'createStack_agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following
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
-- The default setting is the most recent release of the agent. To specify
-- an agent version, you must use the complete version number, not the
-- abbreviated number shown on the console. For a list of available agent
-- version numbers, call DescribeAgentVersions. AgentVersion cannot be set
-- to Chef 12.2.
--
-- You can also specify an agent version when you create or update an
-- instance, which overrides the stack\'s default setting.
--
-- 'attributes', 'createStack_attributes' - One or more user-defined key-value pairs to be added to the stack
-- attributes.
--
-- 'chefConfiguration', 'createStack_chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
--
-- 'configurationManager', 'createStack_configurationManager' - The configuration manager. When you create a stack we recommend that you
-- use the configuration manager to specify the Chef version: 12, 11.10, or
-- 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for
-- Linux stacks is currently 12.
--
-- 'customCookbooksSource', 'createStack_customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
--
-- 'customJson', 'createStack_customJson' - A string that contains user-defined, custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- or to pass data to recipes. The string should be in the following
-- format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
--
-- 'defaultAvailabilityZone', 'createStack_defaultAvailabilityZone' - The stack\'s default Availability Zone, which must be in the specified
-- region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
--
-- 'defaultOs', 'createStack_defaultOs' - The stack\'s default operating system, which is installed on every
-- instance unless you specify a different operating system when you create
-- the instance. You can specify one of the following.
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
--     when you create instances. For more information, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the current Amazon Linux version. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- 'defaultRootDeviceType', 'createStack_defaultRootDeviceType' - The default root device type. This value is the default for all
-- instances in the stack, but you can override it when you create an
-- instance. The default option is @instance-store@. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'defaultSshKeyName', 'createStack_defaultSshKeyName' - A default Amazon EC2 key pair name. The default value is none. If you
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
-- 'defaultSubnetId', 'createStack_defaultSubnetId' - The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
--
-- 'hostnameTheme', 'createStack_hostnameTheme' - The stack\'s host name theme, with spaces replaced by underscores. The
-- theme is used to generate host names for the stack\'s instances. By
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
-- 'useCustomCookbooks', 'createStack_useCustomCookbooks' - Whether the stack uses custom cookbooks.
--
-- 'useOpsworksSecurityGroups', 'createStack_useOpsworksSecurityGroups' - Whether to associate the AWS OpsWorks Stacks built-in security groups
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
--     it, but you cannot delete the built-in security group.
--
-- -   False - AWS OpsWorks Stacks does not associate built-in security
--     groups with layers. You must create appropriate EC2 security groups
--     and associate a security group with each layer that you create.
--     However, you can still manually associate a built-in security group
--     with a layer on creation; custom security groups are required only
--     for those layers that need custom settings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
--
-- 'vpcId', 'createStack_vpcId' - The ID of the VPC that the stack is to be launched into. The VPC must be
-- in the stack\'s region. All instances are launched into this VPC. You
-- cannot change the ID later.
--
-- -   If your account supports EC2-Classic, the default value is @no VPC@.
--
-- -   If your account does not support EC2-Classic, the default value is
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
-- For more information about default VPC and EC2-Classic, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
--
-- 'name', 'createStack_name' - The stack name.
--
-- 'region', 'createStack_region' - The stack\'s AWS region, such as @ap-south-1@. For more information
-- about Amazon regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- In the AWS CLI, this API maps to the @--stack-region@ parameter. If the
-- @--stack-region@ parameter and the AWS CLI common parameter @--region@
-- are set to the same value, the stack uses a /regional/ endpoint. If the
-- @--stack-region@ parameter is not set, but the AWS CLI @--region@
-- parameter is, this also results in a stack with a /regional/ endpoint.
-- However, if the @--region@ parameter is set to @us-east-1@, and the
-- @--stack-region@ parameter is set to one of the following, then the
-- stack uses a legacy or /classic/ region:
-- @us-west-1, us-west-2, sa-east-1, eu-central-1, eu-west-1, ap-northeast-1, ap-southeast-1, ap-southeast-2@.
-- In this case, the actual API endpoint of the stack is in @us-east-1@.
-- Only the preceding regions are supported as classic regions in the
-- @us-east-1@ API endpoint. Because it is a best practice to choose the
-- regional endpoint that is closest to where you manage AWS, we recommend
-- that you use regional endpoints for new stacks. The AWS CLI common
-- @--region@ parameter always specifies a regional API endpoint; it cannot
-- be used to specify a classic AWS OpsWorks Stacks region.
--
-- 'serviceRoleArn', 'createStack_serviceRoleArn' - The stack\'s AWS Identity and Access Management (IAM) role, which allows
-- AWS OpsWorks Stacks to work with AWS resources on your behalf. You must
-- set this parameter to the Amazon Resource Name (ARN) for an existing IAM
-- role. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'defaultInstanceProfileArn', 'createStack_defaultInstanceProfileArn' - The Amazon Resource Name (ARN) of an IAM profile that is the default
-- profile for all of the stack\'s EC2 instances. For more information
-- about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
newCreateStack ::
  -- | 'name'
  Prelude.Text ->
  -- | 'region'
  Prelude.Text ->
  -- | 'serviceRoleArn'
  Prelude.Text ->
  -- | 'defaultInstanceProfileArn'
  Prelude.Text ->
  CreateStack
newCreateStack
  pName_
  pRegion_
  pServiceRoleArn_
  pDefaultInstanceProfileArn_ =
    CreateStack'
      { agentVersion = Prelude.Nothing,
        attributes = Prelude.Nothing,
        chefConfiguration = Prelude.Nothing,
        configurationManager = Prelude.Nothing,
        customCookbooksSource = Prelude.Nothing,
        customJson = Prelude.Nothing,
        defaultAvailabilityZone = Prelude.Nothing,
        defaultOs = Prelude.Nothing,
        defaultRootDeviceType = Prelude.Nothing,
        defaultSshKeyName = Prelude.Nothing,
        defaultSubnetId = Prelude.Nothing,
        hostnameTheme = Prelude.Nothing,
        useCustomCookbooks = Prelude.Nothing,
        useOpsworksSecurityGroups = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        name = pName_,
        region = pRegion_,
        serviceRoleArn = pServiceRoleArn_,
        defaultInstanceProfileArn =
          pDefaultInstanceProfileArn_
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
-- The default setting is the most recent release of the agent. To specify
-- an agent version, you must use the complete version number, not the
-- abbreviated number shown on the console. For a list of available agent
-- version numbers, call DescribeAgentVersions. AgentVersion cannot be set
-- to Chef 12.2.
--
-- You can also specify an agent version when you create or update an
-- instance, which overrides the stack\'s default setting.
createStack_agentVersion :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_agentVersion = Lens.lens (\CreateStack' {agentVersion} -> agentVersion) (\s@CreateStack' {} a -> s {agentVersion = a} :: CreateStack)

-- | One or more user-defined key-value pairs to be added to the stack
-- attributes.
createStack_attributes :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.HashMap StackAttributesKeys (Prelude.Maybe Prelude.Text)))
createStack_attributes = Lens.lens (\CreateStack' {attributes} -> attributes) (\s@CreateStack' {} a -> s {attributes = a} :: CreateStack) Prelude.. Lens.mapping Lens.coerced

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version on Chef 11.10 stacks. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
createStack_chefConfiguration :: Lens.Lens' CreateStack (Prelude.Maybe ChefConfiguration)
createStack_chefConfiguration = Lens.lens (\CreateStack' {chefConfiguration} -> chefConfiguration) (\s@CreateStack' {} a -> s {chefConfiguration = a} :: CreateStack)

-- | The configuration manager. When you create a stack we recommend that you
-- use the configuration manager to specify the Chef version: 12, 11.10, or
-- 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for
-- Linux stacks is currently 12.
createStack_configurationManager :: Lens.Lens' CreateStack (Prelude.Maybe StackConfigurationManager)
createStack_configurationManager = Lens.lens (\CreateStack' {configurationManager} -> configurationManager) (\s@CreateStack' {} a -> s {configurationManager = a} :: CreateStack)

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps>
-- or
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes>.
createStack_customCookbooksSource :: Lens.Lens' CreateStack (Prelude.Maybe Source)
createStack_customCookbooksSource = Lens.lens (\CreateStack' {customCookbooksSource} -> customCookbooksSource) (\s@CreateStack' {} a -> s {customCookbooksSource = a} :: CreateStack)

-- | A string that contains user-defined, custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- or to pass data to recipes. The string should be in the following
-- format:
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information about custom JSON, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
createStack_customJson :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_customJson = Lens.lens (\CreateStack' {customJson} -> customJson) (\s@CreateStack' {} a -> s {customJson = a} :: CreateStack)

-- | The stack\'s default Availability Zone, which must be in the specified
-- region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
-- If you also specify a value for @DefaultSubnetId@, the subnet must be in
-- the same zone. For more information, see the @VpcId@ parameter
-- description.
createStack_defaultAvailabilityZone :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_defaultAvailabilityZone = Lens.lens (\CreateStack' {defaultAvailabilityZone} -> defaultAvailabilityZone) (\s@CreateStack' {} a -> s {defaultAvailabilityZone = a} :: CreateStack)

-- | The stack\'s default operating system, which is installed on every
-- instance unless you specify a different operating system when you create
-- the instance. You can specify one of the following.
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
--     when you create instances. For more information, see
--     <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- The default option is the current Amazon Linux version. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
createStack_defaultOs :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_defaultOs = Lens.lens (\CreateStack' {defaultOs} -> defaultOs) (\s@CreateStack' {} a -> s {defaultOs = a} :: CreateStack)

-- | The default root device type. This value is the default for all
-- instances in the stack, but you can override it when you create an
-- instance. The default option is @instance-store@. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
createStack_defaultRootDeviceType :: Lens.Lens' CreateStack (Prelude.Maybe RootDeviceType)
createStack_defaultRootDeviceType = Lens.lens (\CreateStack' {defaultRootDeviceType} -> defaultRootDeviceType) (\s@CreateStack' {} a -> s {defaultRootDeviceType = a} :: CreateStack)

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
createStack_defaultSshKeyName :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_defaultSshKeyName = Lens.lens (\CreateStack' {defaultSshKeyName} -> defaultSshKeyName) (\s@CreateStack' {} a -> s {defaultSshKeyName = a} :: CreateStack)

-- | The stack\'s default VPC subnet ID. This parameter is required if you
-- specify a value for the @VpcId@ parameter. All instances are launched
-- into this subnet unless you specify otherwise when you create the
-- instance. If you also specify a value for @DefaultAvailabilityZone@, the
-- subnet must be in that zone. For information on default values and when
-- this parameter is required, see the @VpcId@ parameter description.
createStack_defaultSubnetId :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_defaultSubnetId = Lens.lens (\CreateStack' {defaultSubnetId} -> defaultSubnetId) (\s@CreateStack' {} a -> s {defaultSubnetId = a} :: CreateStack)

-- | The stack\'s host name theme, with spaces replaced by underscores. The
-- theme is used to generate host names for the stack\'s instances. By
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
createStack_hostnameTheme :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_hostnameTheme = Lens.lens (\CreateStack' {hostnameTheme} -> hostnameTheme) (\s@CreateStack' {} a -> s {hostnameTheme = a} :: CreateStack)

-- | Whether the stack uses custom cookbooks.
createStack_useCustomCookbooks :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Bool)
createStack_useCustomCookbooks = Lens.lens (\CreateStack' {useCustomCookbooks} -> useCustomCookbooks) (\s@CreateStack' {} a -> s {useCustomCookbooks = a} :: CreateStack)

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
--     it, but you cannot delete the built-in security group.
--
-- -   False - AWS OpsWorks Stacks does not associate built-in security
--     groups with layers. You must create appropriate EC2 security groups
--     and associate a security group with each layer that you create.
--     However, you can still manually associate a built-in security group
--     with a layer on creation; custom security groups are required only
--     for those layers that need custom settings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
createStack_useOpsworksSecurityGroups :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Bool)
createStack_useOpsworksSecurityGroups = Lens.lens (\CreateStack' {useOpsworksSecurityGroups} -> useOpsworksSecurityGroups) (\s@CreateStack' {} a -> s {useOpsworksSecurityGroups = a} :: CreateStack)

-- | The ID of the VPC that the stack is to be launched into. The VPC must be
-- in the stack\'s region. All instances are launched into this VPC. You
-- cannot change the ID later.
--
-- -   If your account supports EC2-Classic, the default value is @no VPC@.
--
-- -   If your account does not support EC2-Classic, the default value is
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
-- For more information about default VPC and EC2-Classic, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms>.
createStack_vpcId :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_vpcId = Lens.lens (\CreateStack' {vpcId} -> vpcId) (\s@CreateStack' {} a -> s {vpcId = a} :: CreateStack)

-- | The stack name.
createStack_name :: Lens.Lens' CreateStack Prelude.Text
createStack_name = Lens.lens (\CreateStack' {name} -> name) (\s@CreateStack' {} a -> s {name = a} :: CreateStack)

-- | The stack\'s AWS region, such as @ap-south-1@. For more information
-- about Amazon regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- In the AWS CLI, this API maps to the @--stack-region@ parameter. If the
-- @--stack-region@ parameter and the AWS CLI common parameter @--region@
-- are set to the same value, the stack uses a /regional/ endpoint. If the
-- @--stack-region@ parameter is not set, but the AWS CLI @--region@
-- parameter is, this also results in a stack with a /regional/ endpoint.
-- However, if the @--region@ parameter is set to @us-east-1@, and the
-- @--stack-region@ parameter is set to one of the following, then the
-- stack uses a legacy or /classic/ region:
-- @us-west-1, us-west-2, sa-east-1, eu-central-1, eu-west-1, ap-northeast-1, ap-southeast-1, ap-southeast-2@.
-- In this case, the actual API endpoint of the stack is in @us-east-1@.
-- Only the preceding regions are supported as classic regions in the
-- @us-east-1@ API endpoint. Because it is a best practice to choose the
-- regional endpoint that is closest to where you manage AWS, we recommend
-- that you use regional endpoints for new stacks. The AWS CLI common
-- @--region@ parameter always specifies a regional API endpoint; it cannot
-- be used to specify a classic AWS OpsWorks Stacks region.
createStack_region :: Lens.Lens' CreateStack Prelude.Text
createStack_region = Lens.lens (\CreateStack' {region} -> region) (\s@CreateStack' {} a -> s {region = a} :: CreateStack)

-- | The stack\'s AWS Identity and Access Management (IAM) role, which allows
-- AWS OpsWorks Stacks to work with AWS resources on your behalf. You must
-- set this parameter to the Amazon Resource Name (ARN) for an existing IAM
-- role. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
createStack_serviceRoleArn :: Lens.Lens' CreateStack Prelude.Text
createStack_serviceRoleArn = Lens.lens (\CreateStack' {serviceRoleArn} -> serviceRoleArn) (\s@CreateStack' {} a -> s {serviceRoleArn = a} :: CreateStack)

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default
-- profile for all of the stack\'s EC2 instances. For more information
-- about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
createStack_defaultInstanceProfileArn :: Lens.Lens' CreateStack Prelude.Text
createStack_defaultInstanceProfileArn = Lens.lens (\CreateStack' {defaultInstanceProfileArn} -> defaultInstanceProfileArn) (\s@CreateStack' {} a -> s {defaultInstanceProfileArn = a} :: CreateStack)

instance Core.AWSRequest CreateStack where
  type AWSResponse CreateStack = CreateStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Prelude.<$> (x Data..?> "StackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStack where
  hashWithSalt _salt CreateStack' {..} =
    _salt
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` chefConfiguration
      `Prelude.hashWithSalt` configurationManager
      `Prelude.hashWithSalt` customCookbooksSource
      `Prelude.hashWithSalt` customJson
      `Prelude.hashWithSalt` defaultAvailabilityZone
      `Prelude.hashWithSalt` defaultOs
      `Prelude.hashWithSalt` defaultRootDeviceType
      `Prelude.hashWithSalt` defaultSshKeyName
      `Prelude.hashWithSalt` defaultSubnetId
      `Prelude.hashWithSalt` hostnameTheme
      `Prelude.hashWithSalt` useCustomCookbooks
      `Prelude.hashWithSalt` useOpsworksSecurityGroups
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` defaultInstanceProfileArn

instance Prelude.NFData CreateStack where
  rnf CreateStack' {..} =
    Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf chefConfiguration
      `Prelude.seq` Prelude.rnf configurationManager
      `Prelude.seq` Prelude.rnf customCookbooksSource
      `Prelude.seq` Prelude.rnf customJson
      `Prelude.seq` Prelude.rnf defaultAvailabilityZone
      `Prelude.seq` Prelude.rnf defaultOs
      `Prelude.seq` Prelude.rnf defaultRootDeviceType
      `Prelude.seq` Prelude.rnf defaultSshKeyName
      `Prelude.seq` Prelude.rnf defaultSubnetId
      `Prelude.seq` Prelude.rnf hostnameTheme
      `Prelude.seq` Prelude.rnf useCustomCookbooks
      `Prelude.seq` Prelude.rnf useOpsworksSecurityGroups
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf
        defaultInstanceProfileArn

instance Data.ToHeaders CreateStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.CreateStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStack where
  toJSON CreateStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentVersion" Data..=) Prelude.<$> agentVersion,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("ChefConfiguration" Data..=)
              Prelude.<$> chefConfiguration,
            ("ConfigurationManager" Data..=)
              Prelude.<$> configurationManager,
            ("CustomCookbooksSource" Data..=)
              Prelude.<$> customCookbooksSource,
            ("CustomJson" Data..=) Prelude.<$> customJson,
            ("DefaultAvailabilityZone" Data..=)
              Prelude.<$> defaultAvailabilityZone,
            ("DefaultOs" Data..=) Prelude.<$> defaultOs,
            ("DefaultRootDeviceType" Data..=)
              Prelude.<$> defaultRootDeviceType,
            ("DefaultSshKeyName" Data..=)
              Prelude.<$> defaultSshKeyName,
            ("DefaultSubnetId" Data..=)
              Prelude.<$> defaultSubnetId,
            ("HostnameTheme" Data..=) Prelude.<$> hostnameTheme,
            ("UseCustomCookbooks" Data..=)
              Prelude.<$> useCustomCookbooks,
            ("UseOpsworksSecurityGroups" Data..=)
              Prelude.<$> useOpsworksSecurityGroups,
            ("VpcId" Data..=) Prelude.<$> vpcId,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Region" Data..= region),
            Prelude.Just
              ("ServiceRoleArn" Data..= serviceRoleArn),
            Prelude.Just
              ( "DefaultInstanceProfileArn"
                  Data..= defaultInstanceProfileArn
              )
          ]
      )

instance Data.ToPath CreateStack where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStack where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @CreateStack@ request.
--
-- /See:/ 'newCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | The stack ID, which is an opaque string that you use to identify the
    -- stack when performing actions such as @DescribeStacks@.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'createStackResponse_stackId' - The stack ID, which is an opaque string that you use to identify the
-- stack when performing actions such as @DescribeStacks@.
--
-- 'httpStatus', 'createStackResponse_httpStatus' - The response's http status code.
newCreateStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStackResponse
newCreateStackResponse pHttpStatus_ =
  CreateStackResponse'
    { stackId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stack ID, which is an opaque string that you use to identify the
-- stack when performing actions such as @DescribeStacks@.
createStackResponse_stackId :: Lens.Lens' CreateStackResponse (Prelude.Maybe Prelude.Text)
createStackResponse_stackId = Lens.lens (\CreateStackResponse' {stackId} -> stackId) (\s@CreateStackResponse' {} a -> s {stackId = a} :: CreateStackResponse)

-- | The response's http status code.
createStackResponse_httpStatus :: Lens.Lens' CreateStackResponse Prelude.Int
createStackResponse_httpStatus = Lens.lens (\CreateStackResponse' {httpStatus} -> httpStatus) (\s@CreateStackResponse' {} a -> s {httpStatus = a} :: CreateStackResponse)

instance Prelude.NFData CreateStackResponse where
  rnf CreateStackResponse' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf httpStatus
