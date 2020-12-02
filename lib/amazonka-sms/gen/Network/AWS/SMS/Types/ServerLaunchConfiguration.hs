{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerLaunchConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.S3Location
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.UserData

-- | Launch configuration for a server.
--
--
--
-- /See:/ 'serverLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { _slcEc2KeyName ::
      !(Maybe Text),
    _slcConfigureScriptType ::
      !(Maybe ScriptType),
    _slcAssociatePublicIPAddress ::
      !(Maybe Bool),
    _slcIamInstanceProfileName ::
      !(Maybe Text),
    _slcSubnet :: !(Maybe Text),
    _slcLogicalId :: !(Maybe Text),
    _slcSecurityGroup :: !(Maybe Text),
    _slcUserData :: !(Maybe UserData),
    _slcInstanceType :: !(Maybe Text),
    _slcConfigureScript ::
      !(Maybe S3Location),
    _slcServer :: !(Maybe Server),
    _slcVpc :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcEc2KeyName' - The name of the Amazon EC2 SSH key to be used for connecting to the launched server.
--
-- * 'slcConfigureScriptType' - The type of configuration script.
--
-- * 'slcAssociatePublicIPAddress' - Indicates whether a publicly accessible IP address is created when launching the server.
--
-- * 'slcIamInstanceProfileName' - The name of the IAM instance profile.
--
-- * 'slcSubnet' - The ID of the subnet the server should be launched into.
--
-- * 'slcLogicalId' - The logical ID of the server in the AWS CloudFormation template.
--
-- * 'slcSecurityGroup' - The ID of the security group that applies to the launched server.
--
-- * 'slcUserData' - Location of the user-data script to be executed when launching the server.
--
-- * 'slcInstanceType' - The instance type to use when launching the server.
--
-- * 'slcConfigureScript' - Undocumented member.
--
-- * 'slcServer' - The ID of the server with which the launch configuration is associated.
--
-- * 'slcVpc' - The ID of the VPC into which the server should be launched.
serverLaunchConfiguration ::
  ServerLaunchConfiguration
serverLaunchConfiguration =
  ServerLaunchConfiguration'
    { _slcEc2KeyName = Nothing,
      _slcConfigureScriptType = Nothing,
      _slcAssociatePublicIPAddress = Nothing,
      _slcIamInstanceProfileName = Nothing,
      _slcSubnet = Nothing,
      _slcLogicalId = Nothing,
      _slcSecurityGroup = Nothing,
      _slcUserData = Nothing,
      _slcInstanceType = Nothing,
      _slcConfigureScript = Nothing,
      _slcServer = Nothing,
      _slcVpc = Nothing
    }

-- | The name of the Amazon EC2 SSH key to be used for connecting to the launched server.
slcEc2KeyName :: Lens' ServerLaunchConfiguration (Maybe Text)
slcEc2KeyName = lens _slcEc2KeyName (\s a -> s {_slcEc2KeyName = a})

-- | The type of configuration script.
slcConfigureScriptType :: Lens' ServerLaunchConfiguration (Maybe ScriptType)
slcConfigureScriptType = lens _slcConfigureScriptType (\s a -> s {_slcConfigureScriptType = a})

-- | Indicates whether a publicly accessible IP address is created when launching the server.
slcAssociatePublicIPAddress :: Lens' ServerLaunchConfiguration (Maybe Bool)
slcAssociatePublicIPAddress = lens _slcAssociatePublicIPAddress (\s a -> s {_slcAssociatePublicIPAddress = a})

-- | The name of the IAM instance profile.
slcIamInstanceProfileName :: Lens' ServerLaunchConfiguration (Maybe Text)
slcIamInstanceProfileName = lens _slcIamInstanceProfileName (\s a -> s {_slcIamInstanceProfileName = a})

-- | The ID of the subnet the server should be launched into.
slcSubnet :: Lens' ServerLaunchConfiguration (Maybe Text)
slcSubnet = lens _slcSubnet (\s a -> s {_slcSubnet = a})

-- | The logical ID of the server in the AWS CloudFormation template.
slcLogicalId :: Lens' ServerLaunchConfiguration (Maybe Text)
slcLogicalId = lens _slcLogicalId (\s a -> s {_slcLogicalId = a})

-- | The ID of the security group that applies to the launched server.
slcSecurityGroup :: Lens' ServerLaunchConfiguration (Maybe Text)
slcSecurityGroup = lens _slcSecurityGroup (\s a -> s {_slcSecurityGroup = a})

-- | Location of the user-data script to be executed when launching the server.
slcUserData :: Lens' ServerLaunchConfiguration (Maybe UserData)
slcUserData = lens _slcUserData (\s a -> s {_slcUserData = a})

-- | The instance type to use when launching the server.
slcInstanceType :: Lens' ServerLaunchConfiguration (Maybe Text)
slcInstanceType = lens _slcInstanceType (\s a -> s {_slcInstanceType = a})

-- | Undocumented member.
slcConfigureScript :: Lens' ServerLaunchConfiguration (Maybe S3Location)
slcConfigureScript = lens _slcConfigureScript (\s a -> s {_slcConfigureScript = a})

-- | The ID of the server with which the launch configuration is associated.
slcServer :: Lens' ServerLaunchConfiguration (Maybe Server)
slcServer = lens _slcServer (\s a -> s {_slcServer = a})

-- | The ID of the VPC into which the server should be launched.
slcVpc :: Lens' ServerLaunchConfiguration (Maybe Text)
slcVpc = lens _slcVpc (\s a -> s {_slcVpc = a})

instance FromJSON ServerLaunchConfiguration where
  parseJSON =
    withObject
      "ServerLaunchConfiguration"
      ( \x ->
          ServerLaunchConfiguration'
            <$> (x .:? "ec2KeyName")
            <*> (x .:? "configureScriptType")
            <*> (x .:? "associatePublicIpAddress")
            <*> (x .:? "iamInstanceProfileName")
            <*> (x .:? "subnet")
            <*> (x .:? "logicalId")
            <*> (x .:? "securityGroup")
            <*> (x .:? "userData")
            <*> (x .:? "instanceType")
            <*> (x .:? "configureScript")
            <*> (x .:? "server")
            <*> (x .:? "vpc")
      )

instance Hashable ServerLaunchConfiguration

instance NFData ServerLaunchConfiguration

instance ToJSON ServerLaunchConfiguration where
  toJSON ServerLaunchConfiguration' {..} =
    object
      ( catMaybes
          [ ("ec2KeyName" .=) <$> _slcEc2KeyName,
            ("configureScriptType" .=) <$> _slcConfigureScriptType,
            ("associatePublicIpAddress" .=) <$> _slcAssociatePublicIPAddress,
            ("iamInstanceProfileName" .=) <$> _slcIamInstanceProfileName,
            ("subnet" .=) <$> _slcSubnet,
            ("logicalId" .=) <$> _slcLogicalId,
            ("securityGroup" .=) <$> _slcSecurityGroup,
            ("userData" .=) <$> _slcUserData,
            ("instanceType" .=) <$> _slcInstanceType,
            ("configureScript" .=) <$> _slcConfigureScript,
            ("server" .=) <$> _slcServer,
            ("vpc" .=) <$> _slcVpc
          ]
      )
