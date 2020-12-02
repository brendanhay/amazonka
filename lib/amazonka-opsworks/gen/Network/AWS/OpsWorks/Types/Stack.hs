{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Stack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Stack where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.ChefConfiguration
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.StackAttributesKeys
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import Network.AWS.Prelude

-- | Describes a stack.
--
--
--
-- /See:/ 'stack' smart constructor.
data Stack = Stack'
  { _sDefaultInstanceProfileARN :: !(Maybe Text),
    _sServiceRoleARN :: !(Maybe Text),
    _sDefaultRootDeviceType :: !(Maybe RootDeviceType),
    _sARN :: !(Maybe Text),
    _sCreatedAt :: !(Maybe Text),
    _sVPCId :: !(Maybe Text),
    _sChefConfiguration :: !(Maybe ChefConfiguration),
    _sAgentVersion :: !(Maybe Text),
    _sDefaultSSHKeyName :: !(Maybe Text),
    _sCustomJSON :: !(Maybe Text),
    _sCustomCookbooksSource :: !(Maybe Source),
    _sDefaultAvailabilityZone :: !(Maybe Text),
    _sAttributes :: !(Maybe (Map StackAttributesKeys (Maybe Text))),
    _sName :: !(Maybe Text),
    _sDefaultOS :: !(Maybe Text),
    _sUseOpsworksSecurityGroups :: !(Maybe Bool),
    _sUseCustomCookbooks :: !(Maybe Bool),
    _sDefaultSubnetId :: !(Maybe Text),
    _sRegion :: !(Maybe Text),
    _sConfigurationManager :: !(Maybe StackConfigurationManager),
    _sStackId :: !(Maybe Text),
    _sHostnameTheme :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDefaultInstanceProfileARN' - The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'sServiceRoleARN' - The stack AWS Identity and Access Management (IAM) role.
--
-- * 'sDefaultRootDeviceType' - The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- * 'sARN' - The stack's ARN.
--
-- * 'sCreatedAt' - The date when the stack was created.
--
-- * 'sVPCId' - The VPC ID; applicable only if the stack is running in a VPC.
--
-- * 'sChefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- * 'sAgentVersion' - The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
--
-- * 'sDefaultSSHKeyName' - A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
--
-- * 'sCustomJSON' - A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- * 'sCustomCookbooksSource' - Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- * 'sDefaultAvailabilityZone' - The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'sAttributes' - The stack's attributes.
--
-- * 'sName' - The stack name.
--
-- * 'sDefaultOS' - The stack's default operating system.
--
-- * 'sUseOpsworksSecurityGroups' - Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- * 'sUseCustomCookbooks' - Whether the stack uses custom cookbooks.
--
-- * 'sDefaultSubnetId' - The default subnet ID; applicable only if the stack is running in a VPC.
--
-- * 'sRegion' - The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'sConfigurationManager' - The configuration manager.
--
-- * 'sStackId' - The stack ID.
--
-- * 'sHostnameTheme' - The stack host name theme, with spaces replaced by underscores.
stack ::
  Stack
stack =
  Stack'
    { _sDefaultInstanceProfileARN = Nothing,
      _sServiceRoleARN = Nothing,
      _sDefaultRootDeviceType = Nothing,
      _sARN = Nothing,
      _sCreatedAt = Nothing,
      _sVPCId = Nothing,
      _sChefConfiguration = Nothing,
      _sAgentVersion = Nothing,
      _sDefaultSSHKeyName = Nothing,
      _sCustomJSON = Nothing,
      _sCustomCookbooksSource = Nothing,
      _sDefaultAvailabilityZone = Nothing,
      _sAttributes = Nothing,
      _sName = Nothing,
      _sDefaultOS = Nothing,
      _sUseOpsworksSecurityGroups = Nothing,
      _sUseCustomCookbooks = Nothing,
      _sDefaultSubnetId = Nothing,
      _sRegion = Nothing,
      _sConfigurationManager = Nothing,
      _sStackId = Nothing,
      _sHostnameTheme = Nothing
    }

-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
sDefaultInstanceProfileARN :: Lens' Stack (Maybe Text)
sDefaultInstanceProfileARN = lens _sDefaultInstanceProfileARN (\s a -> s {_sDefaultInstanceProfileARN = a})

-- | The stack AWS Identity and Access Management (IAM) role.
sServiceRoleARN :: Lens' Stack (Maybe Text)
sServiceRoleARN = lens _sServiceRoleARN (\s a -> s {_sServiceRoleARN = a})

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
sDefaultRootDeviceType :: Lens' Stack (Maybe RootDeviceType)
sDefaultRootDeviceType = lens _sDefaultRootDeviceType (\s a -> s {_sDefaultRootDeviceType = a})

-- | The stack's ARN.
sARN :: Lens' Stack (Maybe Text)
sARN = lens _sARN (\s a -> s {_sARN = a})

-- | The date when the stack was created.
sCreatedAt :: Lens' Stack (Maybe Text)
sCreatedAt = lens _sCreatedAt (\s a -> s {_sCreatedAt = a})

-- | The VPC ID; applicable only if the stack is running in a VPC.
sVPCId :: Lens' Stack (Maybe Text)
sVPCId = lens _sVPCId (\s a -> s {_sVPCId = a})

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
sChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
sChefConfiguration = lens _sChefConfiguration (\s a -> s {_sChefConfiguration = a})

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
sAgentVersion :: Lens' Stack (Maybe Text)
sAgentVersion = lens _sAgentVersion (\s a -> s {_sAgentVersion = a})

-- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
sDefaultSSHKeyName :: Lens' Stack (Maybe Text)
sDefaultSSHKeyName = lens _sDefaultSSHKeyName (\s a -> s {_sDefaultSSHKeyName = a})

-- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
sCustomJSON :: Lens' Stack (Maybe Text)
sCustomJSON = lens _sCustomJSON (\s a -> s {_sCustomJSON = a})

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
sCustomCookbooksSource :: Lens' Stack (Maybe Source)
sCustomCookbooksSource = lens _sCustomCookbooksSource (\s a -> s {_sCustomCookbooksSource = a})

-- | The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
sDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
sDefaultAvailabilityZone = lens _sDefaultAvailabilityZone (\s a -> s {_sDefaultAvailabilityZone = a})

-- | The stack's attributes.
sAttributes :: Lens' Stack (HashMap StackAttributesKeys (Maybe Text))
sAttributes = lens _sAttributes (\s a -> s {_sAttributes = a}) . _Default . _Map

-- | The stack name.
sName :: Lens' Stack (Maybe Text)
sName = lens _sName (\s a -> s {_sName = a})

-- | The stack's default operating system.
sDefaultOS :: Lens' Stack (Maybe Text)
sDefaultOS = lens _sDefaultOS (\s a -> s {_sDefaultOS = a})

-- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
sUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
sUseOpsworksSecurityGroups = lens _sUseOpsworksSecurityGroups (\s a -> s {_sUseOpsworksSecurityGroups = a})

-- | Whether the stack uses custom cookbooks.
sUseCustomCookbooks :: Lens' Stack (Maybe Bool)
sUseCustomCookbooks = lens _sUseCustomCookbooks (\s a -> s {_sUseCustomCookbooks = a})

-- | The default subnet ID; applicable only if the stack is running in a VPC.
sDefaultSubnetId :: Lens' Stack (Maybe Text)
sDefaultSubnetId = lens _sDefaultSubnetId (\s a -> s {_sDefaultSubnetId = a})

-- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
sRegion :: Lens' Stack (Maybe Text)
sRegion = lens _sRegion (\s a -> s {_sRegion = a})

-- | The configuration manager.
sConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
sConfigurationManager = lens _sConfigurationManager (\s a -> s {_sConfigurationManager = a})

-- | The stack ID.
sStackId :: Lens' Stack (Maybe Text)
sStackId = lens _sStackId (\s a -> s {_sStackId = a})

-- | The stack host name theme, with spaces replaced by underscores.
sHostnameTheme :: Lens' Stack (Maybe Text)
sHostnameTheme = lens _sHostnameTheme (\s a -> s {_sHostnameTheme = a})

instance FromJSON Stack where
  parseJSON =
    withObject
      "Stack"
      ( \x ->
          Stack'
            <$> (x .:? "DefaultInstanceProfileArn")
            <*> (x .:? "ServiceRoleArn")
            <*> (x .:? "DefaultRootDeviceType")
            <*> (x .:? "Arn")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "VpcId")
            <*> (x .:? "ChefConfiguration")
            <*> (x .:? "AgentVersion")
            <*> (x .:? "DefaultSshKeyName")
            <*> (x .:? "CustomJson")
            <*> (x .:? "CustomCookbooksSource")
            <*> (x .:? "DefaultAvailabilityZone")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "DefaultOs")
            <*> (x .:? "UseOpsworksSecurityGroups")
            <*> (x .:? "UseCustomCookbooks")
            <*> (x .:? "DefaultSubnetId")
            <*> (x .:? "Region")
            <*> (x .:? "ConfigurationManager")
            <*> (x .:? "StackId")
            <*> (x .:? "HostnameTheme")
      )

instance Hashable Stack

instance NFData Stack
