{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
import Network.AWS.SSM.Types.PingStatus
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.ResourceType

-- | Describes a filter for a specific list of instances.
--
--
--
-- /See:/ 'instanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
  { _iiInstanceId ::
      !(Maybe Text),
    _iiPingStatus :: !(Maybe PingStatus),
    _iiIPAddress :: !(Maybe Text),
    _iiResourceType :: !(Maybe ResourceType),
    _iiRegistrationDate :: !(Maybe POSIX),
    _iiPlatformVersion :: !(Maybe Text),
    _iiIsLatestVersion :: !(Maybe Bool),
    _iiAgentVersion :: !(Maybe Text),
    _iiLastPingDateTime :: !(Maybe POSIX),
    _iiLastSuccessfulAssociationExecutionDate ::
      !(Maybe POSIX),
    _iiActivationId :: !(Maybe Text),
    _iiName :: !(Maybe Text),
    _iiPlatformType :: !(Maybe PlatformType),
    _iiAssociationOverview ::
      !(Maybe InstanceAggregatedAssociationOverview),
    _iiAssociationStatus :: !(Maybe Text),
    _iiLastAssociationExecutionDate :: !(Maybe POSIX),
    _iiPlatformName :: !(Maybe Text),
    _iiComputerName :: !(Maybe Text),
    _iiIAMRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInstanceId' - The instance ID.
--
-- * 'iiPingStatus' - Connection status of SSM Agent.
--
-- * 'iiIPAddress' - The IP address of the managed instance.
--
-- * 'iiResourceType' - The type of instance. Instances are either EC2 instances or managed instances.
--
-- * 'iiRegistrationDate' - The date the server or VM was registered with AWS as a managed instance.
--
-- * 'iiPlatformVersion' - The version of the OS platform running on your instance.
--
-- * 'iiIsLatestVersion' - Indicates whether the latest version of SSM Agent is running on your Linux Managed Instance. This field does not indicate whether or not the latest version is installed on Windows managed instances, because some older versions of Windows Server use the EC2Config service to process SSM requests.
--
-- * 'iiAgentVersion' - The version of SSM Agent running on your Linux instance.
--
-- * 'iiLastPingDateTime' - The date and time when agent last pinged Systems Manager service.
--
-- * 'iiLastSuccessfulAssociationExecutionDate' - The last date the association was successfully run.
--
-- * 'iiActivationId' - The activation ID created by Systems Manager when the server or VM was registered.
--
-- * 'iiName' - The name assigned to an on-premises server or virtual machine (VM) when it is activated as a Systems Manager managed instance. The name is specified as the @DefaultInstanceName@ property using the 'CreateActivation' command. It is applied to the managed instance by specifying the Activation Code and Activation ID when you install SSM Agent on the instance, as explained in <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)> and <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)> . To retrieve the Name tag of an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
--
-- * 'iiPlatformType' - The operating system platform type.
--
-- * 'iiAssociationOverview' - Information about the association.
--
-- * 'iiAssociationStatus' - The status of the association.
--
-- * 'iiLastAssociationExecutionDate' - The date the association was last run.
--
-- * 'iiPlatformName' - The name of the operating system platform running on your instance.
--
-- * 'iiComputerName' - The fully qualified host name of the managed instance.
--
-- * 'iiIAMRole' - The Amazon Identity and Access Management (IAM) role assigned to the on-premises Systems Manager managed instance. This call does not return the IAM role for EC2 instances. To retrieve the IAM role for an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
instanceInformation ::
  InstanceInformation
instanceInformation =
  InstanceInformation'
    { _iiInstanceId = Nothing,
      _iiPingStatus = Nothing,
      _iiIPAddress = Nothing,
      _iiResourceType = Nothing,
      _iiRegistrationDate = Nothing,
      _iiPlatformVersion = Nothing,
      _iiIsLatestVersion = Nothing,
      _iiAgentVersion = Nothing,
      _iiLastPingDateTime = Nothing,
      _iiLastSuccessfulAssociationExecutionDate = Nothing,
      _iiActivationId = Nothing,
      _iiName = Nothing,
      _iiPlatformType = Nothing,
      _iiAssociationOverview = Nothing,
      _iiAssociationStatus = Nothing,
      _iiLastAssociationExecutionDate = Nothing,
      _iiPlatformName = Nothing,
      _iiComputerName = Nothing,
      _iiIAMRole = Nothing
    }

-- | The instance ID.
iiInstanceId :: Lens' InstanceInformation (Maybe Text)
iiInstanceId = lens _iiInstanceId (\s a -> s {_iiInstanceId = a})

-- | Connection status of SSM Agent.
iiPingStatus :: Lens' InstanceInformation (Maybe PingStatus)
iiPingStatus = lens _iiPingStatus (\s a -> s {_iiPingStatus = a})

-- | The IP address of the managed instance.
iiIPAddress :: Lens' InstanceInformation (Maybe Text)
iiIPAddress = lens _iiIPAddress (\s a -> s {_iiIPAddress = a})

-- | The type of instance. Instances are either EC2 instances or managed instances.
iiResourceType :: Lens' InstanceInformation (Maybe ResourceType)
iiResourceType = lens _iiResourceType (\s a -> s {_iiResourceType = a})

-- | The date the server or VM was registered with AWS as a managed instance.
iiRegistrationDate :: Lens' InstanceInformation (Maybe UTCTime)
iiRegistrationDate = lens _iiRegistrationDate (\s a -> s {_iiRegistrationDate = a}) . mapping _Time

-- | The version of the OS platform running on your instance.
iiPlatformVersion :: Lens' InstanceInformation (Maybe Text)
iiPlatformVersion = lens _iiPlatformVersion (\s a -> s {_iiPlatformVersion = a})

-- | Indicates whether the latest version of SSM Agent is running on your Linux Managed Instance. This field does not indicate whether or not the latest version is installed on Windows managed instances, because some older versions of Windows Server use the EC2Config service to process SSM requests.
iiIsLatestVersion :: Lens' InstanceInformation (Maybe Bool)
iiIsLatestVersion = lens _iiIsLatestVersion (\s a -> s {_iiIsLatestVersion = a})

-- | The version of SSM Agent running on your Linux instance.
iiAgentVersion :: Lens' InstanceInformation (Maybe Text)
iiAgentVersion = lens _iiAgentVersion (\s a -> s {_iiAgentVersion = a})

-- | The date and time when agent last pinged Systems Manager service.
iiLastPingDateTime :: Lens' InstanceInformation (Maybe UTCTime)
iiLastPingDateTime = lens _iiLastPingDateTime (\s a -> s {_iiLastPingDateTime = a}) . mapping _Time

-- | The last date the association was successfully run.
iiLastSuccessfulAssociationExecutionDate :: Lens' InstanceInformation (Maybe UTCTime)
iiLastSuccessfulAssociationExecutionDate = lens _iiLastSuccessfulAssociationExecutionDate (\s a -> s {_iiLastSuccessfulAssociationExecutionDate = a}) . mapping _Time

-- | The activation ID created by Systems Manager when the server or VM was registered.
iiActivationId :: Lens' InstanceInformation (Maybe Text)
iiActivationId = lens _iiActivationId (\s a -> s {_iiActivationId = a})

-- | The name assigned to an on-premises server or virtual machine (VM) when it is activated as a Systems Manager managed instance. The name is specified as the @DefaultInstanceName@ property using the 'CreateActivation' command. It is applied to the managed instance by specifying the Activation Code and Activation ID when you install SSM Agent on the instance, as explained in <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)> and <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)> . To retrieve the Name tag of an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
iiName :: Lens' InstanceInformation (Maybe Text)
iiName = lens _iiName (\s a -> s {_iiName = a})

-- | The operating system platform type.
iiPlatformType :: Lens' InstanceInformation (Maybe PlatformType)
iiPlatformType = lens _iiPlatformType (\s a -> s {_iiPlatformType = a})

-- | Information about the association.
iiAssociationOverview :: Lens' InstanceInformation (Maybe InstanceAggregatedAssociationOverview)
iiAssociationOverview = lens _iiAssociationOverview (\s a -> s {_iiAssociationOverview = a})

-- | The status of the association.
iiAssociationStatus :: Lens' InstanceInformation (Maybe Text)
iiAssociationStatus = lens _iiAssociationStatus (\s a -> s {_iiAssociationStatus = a})

-- | The date the association was last run.
iiLastAssociationExecutionDate :: Lens' InstanceInformation (Maybe UTCTime)
iiLastAssociationExecutionDate = lens _iiLastAssociationExecutionDate (\s a -> s {_iiLastAssociationExecutionDate = a}) . mapping _Time

-- | The name of the operating system platform running on your instance.
iiPlatformName :: Lens' InstanceInformation (Maybe Text)
iiPlatformName = lens _iiPlatformName (\s a -> s {_iiPlatformName = a})

-- | The fully qualified host name of the managed instance.
iiComputerName :: Lens' InstanceInformation (Maybe Text)
iiComputerName = lens _iiComputerName (\s a -> s {_iiComputerName = a})

-- | The Amazon Identity and Access Management (IAM) role assigned to the on-premises Systems Manager managed instance. This call does not return the IAM role for EC2 instances. To retrieve the IAM role for an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
iiIAMRole :: Lens' InstanceInformation (Maybe Text)
iiIAMRole = lens _iiIAMRole (\s a -> s {_iiIAMRole = a})

instance FromJSON InstanceInformation where
  parseJSON =
    withObject
      "InstanceInformation"
      ( \x ->
          InstanceInformation'
            <$> (x .:? "InstanceId")
            <*> (x .:? "PingStatus")
            <*> (x .:? "IPAddress")
            <*> (x .:? "ResourceType")
            <*> (x .:? "RegistrationDate")
            <*> (x .:? "PlatformVersion")
            <*> (x .:? "IsLatestVersion")
            <*> (x .:? "AgentVersion")
            <*> (x .:? "LastPingDateTime")
            <*> (x .:? "LastSuccessfulAssociationExecutionDate")
            <*> (x .:? "ActivationId")
            <*> (x .:? "Name")
            <*> (x .:? "PlatformType")
            <*> (x .:? "AssociationOverview")
            <*> (x .:? "AssociationStatus")
            <*> (x .:? "LastAssociationExecutionDate")
            <*> (x .:? "PlatformName")
            <*> (x .:? "ComputerName")
            <*> (x .:? "IamRole")
      )

instance Hashable InstanceInformation

instance NFData InstanceInformation
