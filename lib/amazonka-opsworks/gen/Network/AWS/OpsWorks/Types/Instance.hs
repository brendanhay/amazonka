{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Instance where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.Architecture
import Network.AWS.OpsWorks.Types.AutoScalingType
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.ReportedOS
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.VirtualizationType
import Network.AWS.Prelude

-- | Describes an instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iPrivateDNS :: !(Maybe Text),
    _iReportedAgentVersion :: !(Maybe Text),
    _iInstanceId :: !(Maybe Text),
    _iStatus :: !(Maybe Text),
    _iPrivateIP :: !(Maybe Text),
    _iInstallUpdatesOnBoot :: !(Maybe Bool),
    _iVirtualizationType :: !(Maybe VirtualizationType),
    _iInstanceProfileARN :: !(Maybe Text),
    _iPlatform :: !(Maybe Text),
    _iHostname :: !(Maybe Text),
    _iSSHHostRsaKeyFingerprint :: !(Maybe Text),
    _iSecurityGroupIds :: !(Maybe [Text]),
    _iEcsClusterARN :: !(Maybe Text),
    _iARN :: !(Maybe Text),
    _iCreatedAt :: !(Maybe Text),
    _iEC2InstanceId :: !(Maybe Text),
    _iSSHKeyName :: !(Maybe Text),
    _iAgentVersion :: !(Maybe Text),
    _iRootDeviceVolumeId :: !(Maybe Text),
    _iSubnetId :: !(Maybe Text),
    _iInfrastructureClass :: !(Maybe Text),
    _iSSHHostDsaKeyFingerprint :: !(Maybe Text),
    _iInstanceType :: !(Maybe Text),
    _iEBSOptimized :: !(Maybe Bool),
    _iElasticIP :: !(Maybe Text),
    _iOS :: !(Maybe Text),
    _iAvailabilityZone :: !(Maybe Text),
    _iLastServiceErrorId :: !(Maybe Text),
    _iTenancy :: !(Maybe Text),
    _iAutoScalingType :: !(Maybe AutoScalingType),
    _iLayerIds :: !(Maybe [Text]),
    _iArchitecture :: !(Maybe Architecture),
    _iPublicDNS :: !(Maybe Text),
    _iAMIId :: !(Maybe Text),
    _iPublicIP :: !(Maybe Text),
    _iReportedOS :: !(Maybe ReportedOS),
    _iRegisteredBy :: !(Maybe Text),
    _iStackId :: !(Maybe Text),
    _iRootDeviceType :: !(Maybe RootDeviceType),
    _iEcsContainerInstanceARN :: !(Maybe Text),
    _iBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iPrivateDNS' - The instance's private DNS name.
--
-- * 'iReportedAgentVersion' - The instance's reported AWS OpsWorks Stacks agent version.
--
-- * 'iInstanceId' - The instance ID.
--
-- * 'iStatus' - The instance status:     * @booting@      * @connection_lost@      * @online@      * @pending@      * @rebooting@      * @requested@      * @running_setup@      * @setup_failed@      * @shutting_down@      * @start_failed@      * @stop_failed@      * @stopped@      * @stopping@      * @terminated@      * @terminating@
--
-- * 'iPrivateIP' - The instance's private IP address.
--
-- * 'iInstallUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- * 'iVirtualizationType' - The instance's virtualization type: @paravirtual@ or @hvm@ .
--
-- * 'iInstanceProfileARN' - The ARN of the instance's IAM profile. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'iPlatform' - The instance's platform.
--
-- * 'iHostname' - The instance host name.
--
-- * 'iSSHHostRsaKeyFingerprint' - The SSH key's RSA fingerprint.
--
-- * 'iSecurityGroupIds' - An array containing the instance security group IDs.
--
-- * 'iEcsClusterARN' - For container instances, the Amazon ECS cluster's ARN.
--
-- * 'iARN' - The instance's Amazon Resource Number (ARN).
--
-- * 'iCreatedAt' - The time that the instance was created.
--
-- * 'iEC2InstanceId' - The ID of the associated Amazon EC2 instance.
--
-- * 'iSSHKeyName' - The instance's Amazon EC2 key-pair name.
--
-- * 'iAgentVersion' - The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
--
-- * 'iRootDeviceVolumeId' - The root device volume ID.
--
-- * 'iSubnetId' - The instance's subnet ID; applicable only if the stack is running in a VPC.
--
-- * 'iInfrastructureClass' - For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
--
-- * 'iSSHHostDsaKeyFingerprint' - The SSH key's Deep Security Agent (DSA) fingerprint.
--
-- * 'iInstanceType' - The instance type, such as @t2.micro@ .
--
-- * 'iEBSOptimized' - Whether this is an Amazon EBS-optimized instance.
--
-- * 'iElasticIP' - The instance <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
--
-- * 'iOS' - The instance's operating system.
--
-- * 'iAvailabilityZone' - The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'iLastServiceErrorId' - The ID of the last service error. For more information, call 'DescribeServiceErrors' .
--
-- * 'iTenancy' - The instance's tenancy option, such as @dedicated@ or @host@ .
--
-- * 'iAutoScalingType' - For load-based or time-based instances, the type.
--
-- * 'iLayerIds' - An array containing the instance layer IDs.
--
-- * 'iArchitecture' - The instance architecture: "i386" or "x86_64".
--
-- * 'iPublicDNS' - The instance public DNS name.
--
-- * 'iAMIId' - A custom AMI ID to be used to create the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- * 'iPublicIP' - The instance public IP address.
--
-- * 'iReportedOS' - For registered instances, the reported operating system.
--
-- * 'iRegisteredBy' - For registered instances, who performed the registration.
--
-- * 'iStackId' - The stack ID.
--
-- * 'iRootDeviceType' - The instance's root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- * 'iEcsContainerInstanceARN' - For container instances, the instance's ARN.
--
-- * 'iBlockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
instance' ::
  Instance
instance' =
  Instance'
    { _iPrivateDNS = Nothing,
      _iReportedAgentVersion = Nothing,
      _iInstanceId = Nothing,
      _iStatus = Nothing,
      _iPrivateIP = Nothing,
      _iInstallUpdatesOnBoot = Nothing,
      _iVirtualizationType = Nothing,
      _iInstanceProfileARN = Nothing,
      _iPlatform = Nothing,
      _iHostname = Nothing,
      _iSSHHostRsaKeyFingerprint = Nothing,
      _iSecurityGroupIds = Nothing,
      _iEcsClusterARN = Nothing,
      _iARN = Nothing,
      _iCreatedAt = Nothing,
      _iEC2InstanceId = Nothing,
      _iSSHKeyName = Nothing,
      _iAgentVersion = Nothing,
      _iRootDeviceVolumeId = Nothing,
      _iSubnetId = Nothing,
      _iInfrastructureClass = Nothing,
      _iSSHHostDsaKeyFingerprint = Nothing,
      _iInstanceType = Nothing,
      _iEBSOptimized = Nothing,
      _iElasticIP = Nothing,
      _iOS = Nothing,
      _iAvailabilityZone = Nothing,
      _iLastServiceErrorId = Nothing,
      _iTenancy = Nothing,
      _iAutoScalingType = Nothing,
      _iLayerIds = Nothing,
      _iArchitecture = Nothing,
      _iPublicDNS = Nothing,
      _iAMIId = Nothing,
      _iPublicIP = Nothing,
      _iReportedOS = Nothing,
      _iRegisteredBy = Nothing,
      _iStackId = Nothing,
      _iRootDeviceType = Nothing,
      _iEcsContainerInstanceARN = Nothing,
      _iBlockDeviceMappings = Nothing
    }

-- | The instance's private DNS name.
iPrivateDNS :: Lens' Instance (Maybe Text)
iPrivateDNS = lens _iPrivateDNS (\s a -> s {_iPrivateDNS = a})

-- | The instance's reported AWS OpsWorks Stacks agent version.
iReportedAgentVersion :: Lens' Instance (Maybe Text)
iReportedAgentVersion = lens _iReportedAgentVersion (\s a -> s {_iReportedAgentVersion = a})

-- | The instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s {_iInstanceId = a})

-- | The instance status:     * @booting@      * @connection_lost@      * @online@      * @pending@      * @rebooting@      * @requested@      * @running_setup@      * @setup_failed@      * @shutting_down@      * @start_failed@      * @stop_failed@      * @stopped@      * @stopping@      * @terminated@      * @terminating@
iStatus :: Lens' Instance (Maybe Text)
iStatus = lens _iStatus (\s a -> s {_iStatus = a})

-- | The instance's private IP address.
iPrivateIP :: Lens' Instance (Maybe Text)
iPrivateIP = lens _iPrivateIP (\s a -> s {_iPrivateIP = a})

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
iInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
iInstallUpdatesOnBoot = lens _iInstallUpdatesOnBoot (\s a -> s {_iInstallUpdatesOnBoot = a})

-- | The instance's virtualization type: @paravirtual@ or @hvm@ .
iVirtualizationType :: Lens' Instance (Maybe VirtualizationType)
iVirtualizationType = lens _iVirtualizationType (\s a -> s {_iVirtualizationType = a})

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
iInstanceProfileARN :: Lens' Instance (Maybe Text)
iInstanceProfileARN = lens _iInstanceProfileARN (\s a -> s {_iInstanceProfileARN = a})

-- | The instance's platform.
iPlatform :: Lens' Instance (Maybe Text)
iPlatform = lens _iPlatform (\s a -> s {_iPlatform = a})

-- | The instance host name.
iHostname :: Lens' Instance (Maybe Text)
iHostname = lens _iHostname (\s a -> s {_iHostname = a})

-- | The SSH key's RSA fingerprint.
iSSHHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSSHHostRsaKeyFingerprint = lens _iSSHHostRsaKeyFingerprint (\s a -> s {_iSSHHostRsaKeyFingerprint = a})

-- | An array containing the instance security group IDs.
iSecurityGroupIds :: Lens' Instance [Text]
iSecurityGroupIds = lens _iSecurityGroupIds (\s a -> s {_iSecurityGroupIds = a}) . _Default . _Coerce

-- | For container instances, the Amazon ECS cluster's ARN.
iEcsClusterARN :: Lens' Instance (Maybe Text)
iEcsClusterARN = lens _iEcsClusterARN (\s a -> s {_iEcsClusterARN = a})

-- | The instance's Amazon Resource Number (ARN).
iARN :: Lens' Instance (Maybe Text)
iARN = lens _iARN (\s a -> s {_iARN = a})

-- | The time that the instance was created.
iCreatedAt :: Lens' Instance (Maybe Text)
iCreatedAt = lens _iCreatedAt (\s a -> s {_iCreatedAt = a})

-- | The ID of the associated Amazon EC2 instance.
iEC2InstanceId :: Lens' Instance (Maybe Text)
iEC2InstanceId = lens _iEC2InstanceId (\s a -> s {_iEC2InstanceId = a})

-- | The instance's Amazon EC2 key-pair name.
iSSHKeyName :: Lens' Instance (Maybe Text)
iSSHKeyName = lens _iSSHKeyName (\s a -> s {_iSSHKeyName = a})

-- | The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
iAgentVersion :: Lens' Instance (Maybe Text)
iAgentVersion = lens _iAgentVersion (\s a -> s {_iAgentVersion = a})

-- | The root device volume ID.
iRootDeviceVolumeId :: Lens' Instance (Maybe Text)
iRootDeviceVolumeId = lens _iRootDeviceVolumeId (\s a -> s {_iRootDeviceVolumeId = a})

-- | The instance's subnet ID; applicable only if the stack is running in a VPC.
iSubnetId :: Lens' Instance (Maybe Text)
iSubnetId = lens _iSubnetId (\s a -> s {_iSubnetId = a})

-- | For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
iInfrastructureClass :: Lens' Instance (Maybe Text)
iInfrastructureClass = lens _iInfrastructureClass (\s a -> s {_iInfrastructureClass = a})

-- | The SSH key's Deep Security Agent (DSA) fingerprint.
iSSHHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSSHHostDsaKeyFingerprint = lens _iSSHHostDsaKeyFingerprint (\s a -> s {_iSSHHostDsaKeyFingerprint = a})

-- | The instance type, such as @t2.micro@ .
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\s a -> s {_iInstanceType = a})

-- | Whether this is an Amazon EBS-optimized instance.
iEBSOptimized :: Lens' Instance (Maybe Bool)
iEBSOptimized = lens _iEBSOptimized (\s a -> s {_iEBSOptimized = a})

-- | The instance <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
iElasticIP :: Lens' Instance (Maybe Text)
iElasticIP = lens _iElasticIP (\s a -> s {_iElasticIP = a})

-- | The instance's operating system.
iOS :: Lens' Instance (Maybe Text)
iOS = lens _iOS (\s a -> s {_iOS = a})

-- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
iAvailabilityZone :: Lens' Instance (Maybe Text)
iAvailabilityZone = lens _iAvailabilityZone (\s a -> s {_iAvailabilityZone = a})

-- | The ID of the last service error. For more information, call 'DescribeServiceErrors' .
iLastServiceErrorId :: Lens' Instance (Maybe Text)
iLastServiceErrorId = lens _iLastServiceErrorId (\s a -> s {_iLastServiceErrorId = a})

-- | The instance's tenancy option, such as @dedicated@ or @host@ .
iTenancy :: Lens' Instance (Maybe Text)
iTenancy = lens _iTenancy (\s a -> s {_iTenancy = a})

-- | For load-based or time-based instances, the type.
iAutoScalingType :: Lens' Instance (Maybe AutoScalingType)
iAutoScalingType = lens _iAutoScalingType (\s a -> s {_iAutoScalingType = a})

-- | An array containing the instance layer IDs.
iLayerIds :: Lens' Instance [Text]
iLayerIds = lens _iLayerIds (\s a -> s {_iLayerIds = a}) . _Default . _Coerce

-- | The instance architecture: "i386" or "x86_64".
iArchitecture :: Lens' Instance (Maybe Architecture)
iArchitecture = lens _iArchitecture (\s a -> s {_iArchitecture = a})

-- | The instance public DNS name.
iPublicDNS :: Lens' Instance (Maybe Text)
iPublicDNS = lens _iPublicDNS (\s a -> s {_iPublicDNS = a})

-- | A custom AMI ID to be used to create the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
iAMIId :: Lens' Instance (Maybe Text)
iAMIId = lens _iAMIId (\s a -> s {_iAMIId = a})

-- | The instance public IP address.
iPublicIP :: Lens' Instance (Maybe Text)
iPublicIP = lens _iPublicIP (\s a -> s {_iPublicIP = a})

-- | For registered instances, the reported operating system.
iReportedOS :: Lens' Instance (Maybe ReportedOS)
iReportedOS = lens _iReportedOS (\s a -> s {_iReportedOS = a})

-- | For registered instances, who performed the registration.
iRegisteredBy :: Lens' Instance (Maybe Text)
iRegisteredBy = lens _iRegisteredBy (\s a -> s {_iRegisteredBy = a})

-- | The stack ID.
iStackId :: Lens' Instance (Maybe Text)
iStackId = lens _iStackId (\s a -> s {_iStackId = a})

-- | The instance's root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
iRootDeviceType :: Lens' Instance (Maybe RootDeviceType)
iRootDeviceType = lens _iRootDeviceType (\s a -> s {_iRootDeviceType = a})

-- | For container instances, the instance's ARN.
iEcsContainerInstanceARN :: Lens' Instance (Maybe Text)
iEcsContainerInstanceARN = lens _iEcsContainerInstanceARN (\s a -> s {_iEcsContainerInstanceARN = a})

-- | An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
iBlockDeviceMappings :: Lens' Instance [BlockDeviceMapping]
iBlockDeviceMappings = lens _iBlockDeviceMappings (\s a -> s {_iBlockDeviceMappings = a}) . _Default . _Coerce

instance FromJSON Instance where
  parseJSON =
    withObject
      "Instance"
      ( \x ->
          Instance'
            <$> (x .:? "PrivateDns")
            <*> (x .:? "ReportedAgentVersion")
            <*> (x .:? "InstanceId")
            <*> (x .:? "Status")
            <*> (x .:? "PrivateIp")
            <*> (x .:? "InstallUpdatesOnBoot")
            <*> (x .:? "VirtualizationType")
            <*> (x .:? "InstanceProfileArn")
            <*> (x .:? "Platform")
            <*> (x .:? "Hostname")
            <*> (x .:? "SshHostRsaKeyFingerprint")
            <*> (x .:? "SecurityGroupIds" .!= mempty)
            <*> (x .:? "EcsClusterArn")
            <*> (x .:? "Arn")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "Ec2InstanceId")
            <*> (x .:? "SshKeyName")
            <*> (x .:? "AgentVersion")
            <*> (x .:? "RootDeviceVolumeId")
            <*> (x .:? "SubnetId")
            <*> (x .:? "InfrastructureClass")
            <*> (x .:? "SshHostDsaKeyFingerprint")
            <*> (x .:? "InstanceType")
            <*> (x .:? "EbsOptimized")
            <*> (x .:? "ElasticIp")
            <*> (x .:? "Os")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "LastServiceErrorId")
            <*> (x .:? "Tenancy")
            <*> (x .:? "AutoScalingType")
            <*> (x .:? "LayerIds" .!= mempty)
            <*> (x .:? "Architecture")
            <*> (x .:? "PublicDns")
            <*> (x .:? "AmiId")
            <*> (x .:? "PublicIp")
            <*> (x .:? "ReportedOs")
            <*> (x .:? "RegisteredBy")
            <*> (x .:? "StackId")
            <*> (x .:? "RootDeviceType")
            <*> (x .:? "EcsContainerInstanceArn")
            <*> (x .:? "BlockDeviceMappings" .!= mempty)
      )

instance Hashable Instance

instance NFData Instance
