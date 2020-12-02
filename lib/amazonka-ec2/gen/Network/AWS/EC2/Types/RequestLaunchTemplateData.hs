{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RequestLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RequestLaunchTemplateData where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CreditSpecificationRequest
import Network.AWS.EC2.Types.ElasticGpuSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Network.AWS.EC2.Types.LaunchTemplateCPUOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
import Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
import Network.AWS.EC2.Types.ShutdownBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The information to include in the launch template.
--
--
--
-- /See:/ 'requestLaunchTemplateData' smart constructor.
data RequestLaunchTemplateData = RequestLaunchTemplateData'
  { _rltdSecurityGroupIds ::
      !(Maybe [Text]),
    _rltdSecurityGroups :: !(Maybe [Text]),
    _rltdElasticInferenceAccelerators ::
      !( Maybe
           [LaunchTemplateElasticInferenceAccelerator]
       ),
    _rltdInstanceMarketOptions ::
      !( Maybe
           LaunchTemplateInstanceMarketOptionsRequest
       ),
    _rltdLicenseSpecifications ::
      !( Maybe
           [LaunchTemplateLicenseConfigurationRequest]
       ),
    _rltdDisableAPITermination ::
      !(Maybe Bool),
    _rltdKeyName :: !(Maybe Text),
    _rltdNetworkInterfaces ::
      !( Maybe
           [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest]
       ),
    _rltdEnclaveOptions ::
      !( Maybe
           LaunchTemplateEnclaveOptionsRequest
       ),
    _rltdCPUOptions ::
      !( Maybe
           LaunchTemplateCPUOptionsRequest
       ),
    _rltdRamDiskId :: !(Maybe Text),
    _rltdKernelId :: !(Maybe Text),
    _rltdElasticGpuSpecifications ::
      !(Maybe [ElasticGpuSpecification]),
    _rltdInstanceType ::
      !(Maybe InstanceType),
    _rltdCapacityReservationSpecification ::
      !( Maybe
           LaunchTemplateCapacityReservationSpecificationRequest
       ),
    _rltdEBSOptimized :: !(Maybe Bool),
    _rltdUserData :: !(Maybe Text),
    _rltdMonitoring ::
      !( Maybe
           LaunchTemplatesMonitoringRequest
       ),
    _rltdTagSpecifications ::
      !( Maybe
           [LaunchTemplateTagSpecificationRequest]
       ),
    _rltdHibernationOptions ::
      !( Maybe
           LaunchTemplateHibernationOptionsRequest
       ),
    _rltdIAMInstanceProfile ::
      !( Maybe
           LaunchTemplateIAMInstanceProfileSpecificationRequest
       ),
    _rltdImageId :: !(Maybe Text),
    _rltdInstanceInitiatedShutdownBehavior ::
      !(Maybe ShutdownBehavior),
    _rltdMetadataOptions ::
      !( Maybe
           LaunchTemplateInstanceMetadataOptionsRequest
       ),
    _rltdCreditSpecification ::
      !(Maybe CreditSpecificationRequest),
    _rltdBlockDeviceMappings ::
      !( Maybe
           [LaunchTemplateBlockDeviceMappingRequest]
       ),
    _rltdPlacement ::
      !(Maybe LaunchTemplatePlacementRequest)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestLaunchTemplateData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rltdSecurityGroupIds' - One or more security group IDs. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> . You cannot specify both a security group ID and security name in the same request.
--
-- * 'rltdSecurityGroups' - [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
--
-- * 'rltdElasticInferenceAccelerators' - The elastic inference accelerator for the instance.
--
-- * 'rltdInstanceMarketOptions' - The market (purchasing) option for the instances.
--
-- * 'rltdLicenseSpecifications' - The license configurations.
--
-- * 'rltdDisableAPITermination' - If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
--
-- * 'rltdKeyName' - The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- * 'rltdNetworkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
--
-- * 'rltdEnclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ . You can't enable AWS Nitro Enclaves and hibernation on the same instance.
--
-- * 'rltdCPUOptions' - The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdRamDiskId' - The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdKernelId' - The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdElasticGpuSpecifications' - An elastic GPU to associate with the instance.
--
-- * 'rltdInstanceType' - The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdCapacityReservationSpecification' - The Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
-- * 'rltdEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- * 'rltdUserData' - The Base64-encoded user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
--
-- * 'rltdMonitoring' - The monitoring for the instance.
--
-- * 'rltdTagSpecifications' - The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- * 'rltdHibernationOptions' - Indicates whether an instance is enabled for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdIAMInstanceProfile' - The IAM instance profile.
--
-- * 'rltdImageId' - The ID of the AMI.
--
-- * 'rltdInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
--
-- * 'rltdMetadataOptions' - The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdCreditSpecification' - The credit option for CPU usage of the instance. Valid for T2, T3, or T3a instances only.
--
-- * 'rltdBlockDeviceMappings' - The block device mapping.
--
-- * 'rltdPlacement' - The placement for the instance.
requestLaunchTemplateData ::
  RequestLaunchTemplateData
requestLaunchTemplateData =
  RequestLaunchTemplateData'
    { _rltdSecurityGroupIds = Nothing,
      _rltdSecurityGroups = Nothing,
      _rltdElasticInferenceAccelerators = Nothing,
      _rltdInstanceMarketOptions = Nothing,
      _rltdLicenseSpecifications = Nothing,
      _rltdDisableAPITermination = Nothing,
      _rltdKeyName = Nothing,
      _rltdNetworkInterfaces = Nothing,
      _rltdEnclaveOptions = Nothing,
      _rltdCPUOptions = Nothing,
      _rltdRamDiskId = Nothing,
      _rltdKernelId = Nothing,
      _rltdElasticGpuSpecifications = Nothing,
      _rltdInstanceType = Nothing,
      _rltdCapacityReservationSpecification = Nothing,
      _rltdEBSOptimized = Nothing,
      _rltdUserData = Nothing,
      _rltdMonitoring = Nothing,
      _rltdTagSpecifications = Nothing,
      _rltdHibernationOptions = Nothing,
      _rltdIAMInstanceProfile = Nothing,
      _rltdImageId = Nothing,
      _rltdInstanceInitiatedShutdownBehavior = Nothing,
      _rltdMetadataOptions = Nothing,
      _rltdCreditSpecification = Nothing,
      _rltdBlockDeviceMappings = Nothing,
      _rltdPlacement = Nothing
    }

-- | One or more security group IDs. You can create a security group using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup> . You cannot specify both a security group ID and security name in the same request.
rltdSecurityGroupIds :: Lens' RequestLaunchTemplateData [Text]
rltdSecurityGroupIds = lens _rltdSecurityGroupIds (\s a -> s {_rltdSecurityGroupIds = a}) . _Default . _Coerce

-- | [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
rltdSecurityGroups :: Lens' RequestLaunchTemplateData [Text]
rltdSecurityGroups = lens _rltdSecurityGroups (\s a -> s {_rltdSecurityGroups = a}) . _Default . _Coerce

-- | The elastic inference accelerator for the instance.
rltdElasticInferenceAccelerators :: Lens' RequestLaunchTemplateData [LaunchTemplateElasticInferenceAccelerator]
rltdElasticInferenceAccelerators = lens _rltdElasticInferenceAccelerators (\s a -> s {_rltdElasticInferenceAccelerators = a}) . _Default . _Coerce

-- | The market (purchasing) option for the instances.
rltdInstanceMarketOptions :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateInstanceMarketOptionsRequest)
rltdInstanceMarketOptions = lens _rltdInstanceMarketOptions (\s a -> s {_rltdInstanceMarketOptions = a})

-- | The license configurations.
rltdLicenseSpecifications :: Lens' RequestLaunchTemplateData [LaunchTemplateLicenseConfigurationRequest]
rltdLicenseSpecifications = lens _rltdLicenseSpecifications (\s a -> s {_rltdLicenseSpecifications = a}) . _Default . _Coerce

-- | If you set this parameter to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. To change this attribute after launch, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute> . Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to @terminate@ , you can terminate the instance by running the shutdown command from the instance.
rltdDisableAPITermination :: Lens' RequestLaunchTemplateData (Maybe Bool)
rltdDisableAPITermination = lens _rltdDisableAPITermination (\s a -> s {_rltdDisableAPITermination = a})

-- | The name of the key pair. You can create a key pair using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair> or <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair> . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
rltdKeyName :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdKeyName = lens _rltdKeyName (\s a -> s {_rltdKeyName = a})

-- | One or more network interfaces. If you specify a network interface, you must specify any security groups and subnets as part of the network interface.
rltdNetworkInterfaces :: Lens' RequestLaunchTemplateData [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest]
rltdNetworkInterfaces = lens _rltdNetworkInterfaces (\s a -> s {_rltdNetworkInterfaces = a}) . _Default . _Coerce

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ . You can't enable AWS Nitro Enclaves and hibernation on the same instance.
rltdEnclaveOptions :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateEnclaveOptionsRequest)
rltdEnclaveOptions = lens _rltdEnclaveOptions (\s a -> s {_rltdEnclaveOptions = a})

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdCPUOptions :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateCPUOptionsRequest)
rltdCPUOptions = lens _rltdCPUOptions (\s a -> s {_rltdCPUOptions = a})

-- | The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdRamDiskId :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdRamDiskId = lens _rltdRamDiskId (\s a -> s {_rltdRamDiskId = a})

-- | The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdKernelId :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdKernelId = lens _rltdKernelId (\s a -> s {_rltdKernelId = a})

-- | An elastic GPU to associate with the instance.
rltdElasticGpuSpecifications :: Lens' RequestLaunchTemplateData [ElasticGpuSpecification]
rltdElasticGpuSpecifications = lens _rltdElasticGpuSpecifications (\s a -> s {_rltdElasticGpuSpecifications = a}) . _Default . _Coerce

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdInstanceType :: Lens' RequestLaunchTemplateData (Maybe InstanceType)
rltdInstanceType = lens _rltdInstanceType (\s a -> s {_rltdInstanceType = a})

-- | The Capacity Reservation targeting option. If you do not specify this parameter, the instance's Capacity Reservation preference defaults to @open@ , which enables it to run in any open Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
rltdCapacityReservationSpecification :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateCapacityReservationSpecificationRequest)
rltdCapacityReservationSpecification = lens _rltdCapacityReservationSpecification (\s a -> s {_rltdCapacityReservationSpecification = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
rltdEBSOptimized :: Lens' RequestLaunchTemplateData (Maybe Bool)
rltdEBSOptimized = lens _rltdEBSOptimized (\s a -> s {_rltdEBSOptimized = a})

-- | The Base64-encoded user data to make available to the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
rltdUserData :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdUserData = lens _rltdUserData (\s a -> s {_rltdUserData = a})

-- | The monitoring for the instance.
rltdMonitoring :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplatesMonitoringRequest)
rltdMonitoring = lens _rltdMonitoring (\s a -> s {_rltdMonitoring = a})

-- | The tags to apply to the resources during launch. You can only tag instances and volumes on launch. The specified tags are applied to all instances or volumes that are created during launch. To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
rltdTagSpecifications :: Lens' RequestLaunchTemplateData [LaunchTemplateTagSpecificationRequest]
rltdTagSpecifications = lens _rltdTagSpecifications (\s a -> s {_rltdTagSpecifications = a}) . _Default . _Coerce

-- | Indicates whether an instance is enabled for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdHibernationOptions :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateHibernationOptionsRequest)
rltdHibernationOptions = lens _rltdHibernationOptions (\s a -> s {_rltdHibernationOptions = a})

-- | The IAM instance profile.
rltdIAMInstanceProfile :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateIAMInstanceProfileSpecificationRequest)
rltdIAMInstanceProfile = lens _rltdIAMInstanceProfile (\s a -> s {_rltdIAMInstanceProfile = a})

-- | The ID of the AMI.
rltdImageId :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdImageId = lens _rltdImageId (\s a -> s {_rltdImageId = a})

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
rltdInstanceInitiatedShutdownBehavior :: Lens' RequestLaunchTemplateData (Maybe ShutdownBehavior)
rltdInstanceInitiatedShutdownBehavior = lens _rltdInstanceInitiatedShutdownBehavior (\s a -> s {_rltdInstanceInitiatedShutdownBehavior = a})

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdMetadataOptions :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateInstanceMetadataOptionsRequest)
rltdMetadataOptions = lens _rltdMetadataOptions (\s a -> s {_rltdMetadataOptions = a})

-- | The credit option for CPU usage of the instance. Valid for T2, T3, or T3a instances only.
rltdCreditSpecification :: Lens' RequestLaunchTemplateData (Maybe CreditSpecificationRequest)
rltdCreditSpecification = lens _rltdCreditSpecification (\s a -> s {_rltdCreditSpecification = a})

-- | The block device mapping.
rltdBlockDeviceMappings :: Lens' RequestLaunchTemplateData [LaunchTemplateBlockDeviceMappingRequest]
rltdBlockDeviceMappings = lens _rltdBlockDeviceMappings (\s a -> s {_rltdBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement for the instance.
rltdPlacement :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplatePlacementRequest)
rltdPlacement = lens _rltdPlacement (\s a -> s {_rltdPlacement = a})

instance Hashable RequestLaunchTemplateData

instance NFData RequestLaunchTemplateData

instance ToQuery RequestLaunchTemplateData where
  toQuery RequestLaunchTemplateData' {..} =
    mconcat
      [ toQuery (toQueryList "SecurityGroupId" <$> _rltdSecurityGroupIds),
        toQuery (toQueryList "SecurityGroup" <$> _rltdSecurityGroups),
        toQuery
          ( toQueryList "ElasticInferenceAccelerator"
              <$> _rltdElasticInferenceAccelerators
          ),
        "InstanceMarketOptions" =: _rltdInstanceMarketOptions,
        toQuery
          ( toQueryList "LicenseSpecification"
              <$> _rltdLicenseSpecifications
          ),
        "DisableApiTermination" =: _rltdDisableAPITermination,
        "KeyName" =: _rltdKeyName,
        toQuery
          (toQueryList "NetworkInterface" <$> _rltdNetworkInterfaces),
        "EnclaveOptions" =: _rltdEnclaveOptions,
        "CpuOptions" =: _rltdCPUOptions,
        "RamDiskId" =: _rltdRamDiskId,
        "KernelId" =: _rltdKernelId,
        toQuery
          ( toQueryList "ElasticGpuSpecification"
              <$> _rltdElasticGpuSpecifications
          ),
        "InstanceType" =: _rltdInstanceType,
        "CapacityReservationSpecification"
          =: _rltdCapacityReservationSpecification,
        "EbsOptimized" =: _rltdEBSOptimized,
        "UserData" =: _rltdUserData,
        "Monitoring" =: _rltdMonitoring,
        toQuery
          (toQueryList "TagSpecification" <$> _rltdTagSpecifications),
        "HibernationOptions" =: _rltdHibernationOptions,
        "IamInstanceProfile" =: _rltdIAMInstanceProfile,
        "ImageId" =: _rltdImageId,
        "InstanceInitiatedShutdownBehavior"
          =: _rltdInstanceInitiatedShutdownBehavior,
        "MetadataOptions" =: _rltdMetadataOptions,
        "CreditSpecification" =: _rltdCreditSpecification,
        toQuery
          (toQueryList "BlockDeviceMapping" <$> _rltdBlockDeviceMappings),
        "Placement" =: _rltdPlacement
      ]
