{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResponseLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseLaunchTemplateData where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CreditSpecification
import Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
import Network.AWS.EC2.Types.LaunchTemplateCPUOptions
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
import Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecification
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
import Network.AWS.EC2.Types.LaunchTemplatePlacement
import Network.AWS.EC2.Types.LaunchTemplateTagSpecification
import Network.AWS.EC2.Types.LaunchTemplatesMonitoring
import Network.AWS.EC2.Types.ShutdownBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The information for a launch template.
--
--
--
-- /See:/ 'responseLaunchTemplateData' smart constructor.
data ResponseLaunchTemplateData = ResponseLaunchTemplateData'
  { _rSecurityGroupIds ::
      !(Maybe [Text]),
    _rSecurityGroups :: !(Maybe [Text]),
    _rElasticInferenceAccelerators ::
      !( Maybe
           [LaunchTemplateElasticInferenceAcceleratorResponse]
       ),
    _rInstanceMarketOptions ::
      !( Maybe
           LaunchTemplateInstanceMarketOptions
       ),
    _rLicenseSpecifications ::
      !( Maybe
           [LaunchTemplateLicenseConfiguration]
       ),
    _rDisableAPITermination ::
      !(Maybe Bool),
    _rKeyName :: !(Maybe Text),
    _rNetworkInterfaces ::
      !( Maybe
           [LaunchTemplateInstanceNetworkInterfaceSpecification]
       ),
    _rEnclaveOptions ::
      !(Maybe LaunchTemplateEnclaveOptions),
    _rCPUOptions ::
      !(Maybe LaunchTemplateCPUOptions),
    _rRamDiskId :: !(Maybe Text),
    _rKernelId :: !(Maybe Text),
    _rElasticGpuSpecifications ::
      !( Maybe
           [ElasticGpuSpecificationResponse]
       ),
    _rInstanceType ::
      !(Maybe InstanceType),
    _rCapacityReservationSpecification ::
      !( Maybe
           LaunchTemplateCapacityReservationSpecificationResponse
       ),
    _rEBSOptimized :: !(Maybe Bool),
    _rUserData :: !(Maybe Text),
    _rMonitoring ::
      !(Maybe LaunchTemplatesMonitoring),
    _rTagSpecifications ::
      !( Maybe
           [LaunchTemplateTagSpecification]
       ),
    _rHibernationOptions ::
      !( Maybe
           LaunchTemplateHibernationOptions
       ),
    _rIAMInstanceProfile ::
      !( Maybe
           LaunchTemplateIAMInstanceProfileSpecification
       ),
    _rImageId :: !(Maybe Text),
    _rInstanceInitiatedShutdownBehavior ::
      !(Maybe ShutdownBehavior),
    _rMetadataOptions ::
      !( Maybe
           LaunchTemplateInstanceMetadataOptions
       ),
    _rCreditSpecification ::
      !(Maybe CreditSpecification),
    _rBlockDeviceMappings ::
      !( Maybe
           [LaunchTemplateBlockDeviceMapping]
       ),
    _rPlacement ::
      !(Maybe LaunchTemplatePlacement)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResponseLaunchTemplateData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rSecurityGroupIds' - The security group IDs.
--
-- * 'rSecurityGroups' - The security group names.
--
-- * 'rElasticInferenceAccelerators' - The elastic inference accelerator for the instance.
--
-- * 'rInstanceMarketOptions' - The market (purchasing) option for the instances.
--
-- * 'rLicenseSpecifications' - The license configurations.
--
-- * 'rDisableAPITermination' - If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
--
-- * 'rKeyName' - The name of the key pair.
--
-- * 'rNetworkInterfaces' - The network interfaces.
--
-- * 'rEnclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- * 'rCPUOptions' - The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rRamDiskId' - The ID of the RAM disk, if applicable.
--
-- * 'rKernelId' - The ID of the kernel, if applicable.
--
-- * 'rElasticGpuSpecifications' - The elastic GPU specification.
--
-- * 'rInstanceType' - The instance type.
--
-- * 'rCapacityReservationSpecification' - Information about the Capacity Reservation targeting option.
--
-- * 'rEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- * 'rUserData' - The user data for the instance.
--
-- * 'rMonitoring' - The monitoring for the instance.
--
-- * 'rTagSpecifications' - The tags.
--
-- * 'rHibernationOptions' - Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rIAMInstanceProfile' - The IAM instance profile.
--
-- * 'rImageId' - The ID of the AMI that was used to launch the instance.
--
-- * 'rInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- * 'rMetadataOptions' - The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rCreditSpecification' - The credit option for CPU usage of the instance.
--
-- * 'rBlockDeviceMappings' - The block device mappings.
--
-- * 'rPlacement' - The placement of the instance.
responseLaunchTemplateData ::
  ResponseLaunchTemplateData
responseLaunchTemplateData =
  ResponseLaunchTemplateData'
    { _rSecurityGroupIds = Nothing,
      _rSecurityGroups = Nothing,
      _rElasticInferenceAccelerators = Nothing,
      _rInstanceMarketOptions = Nothing,
      _rLicenseSpecifications = Nothing,
      _rDisableAPITermination = Nothing,
      _rKeyName = Nothing,
      _rNetworkInterfaces = Nothing,
      _rEnclaveOptions = Nothing,
      _rCPUOptions = Nothing,
      _rRamDiskId = Nothing,
      _rKernelId = Nothing,
      _rElasticGpuSpecifications = Nothing,
      _rInstanceType = Nothing,
      _rCapacityReservationSpecification = Nothing,
      _rEBSOptimized = Nothing,
      _rUserData = Nothing,
      _rMonitoring = Nothing,
      _rTagSpecifications = Nothing,
      _rHibernationOptions = Nothing,
      _rIAMInstanceProfile = Nothing,
      _rImageId = Nothing,
      _rInstanceInitiatedShutdownBehavior = Nothing,
      _rMetadataOptions = Nothing,
      _rCreditSpecification = Nothing,
      _rBlockDeviceMappings = Nothing,
      _rPlacement = Nothing
    }

-- | The security group IDs.
rSecurityGroupIds :: Lens' ResponseLaunchTemplateData [Text]
rSecurityGroupIds = lens _rSecurityGroupIds (\s a -> s {_rSecurityGroupIds = a}) . _Default . _Coerce

-- | The security group names.
rSecurityGroups :: Lens' ResponseLaunchTemplateData [Text]
rSecurityGroups = lens _rSecurityGroups (\s a -> s {_rSecurityGroups = a}) . _Default . _Coerce

-- | The elastic inference accelerator for the instance.
rElasticInferenceAccelerators :: Lens' ResponseLaunchTemplateData [LaunchTemplateElasticInferenceAcceleratorResponse]
rElasticInferenceAccelerators = lens _rElasticInferenceAccelerators (\s a -> s {_rElasticInferenceAccelerators = a}) . _Default . _Coerce

-- | The market (purchasing) option for the instances.
rInstanceMarketOptions :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateInstanceMarketOptions)
rInstanceMarketOptions = lens _rInstanceMarketOptions (\s a -> s {_rInstanceMarketOptions = a})

-- | The license configurations.
rLicenseSpecifications :: Lens' ResponseLaunchTemplateData [LaunchTemplateLicenseConfiguration]
rLicenseSpecifications = lens _rLicenseSpecifications (\s a -> s {_rLicenseSpecifications = a}) . _Default . _Coerce

-- | If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
rDisableAPITermination :: Lens' ResponseLaunchTemplateData (Maybe Bool)
rDisableAPITermination = lens _rDisableAPITermination (\s a -> s {_rDisableAPITermination = a})

-- | The name of the key pair.
rKeyName :: Lens' ResponseLaunchTemplateData (Maybe Text)
rKeyName = lens _rKeyName (\s a -> s {_rKeyName = a})

-- | The network interfaces.
rNetworkInterfaces :: Lens' ResponseLaunchTemplateData [LaunchTemplateInstanceNetworkInterfaceSpecification]
rNetworkInterfaces = lens _rNetworkInterfaces (\s a -> s {_rNetworkInterfaces = a}) . _Default . _Coerce

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
rEnclaveOptions :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateEnclaveOptions)
rEnclaveOptions = lens _rEnclaveOptions (\s a -> s {_rEnclaveOptions = a})

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
rCPUOptions :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateCPUOptions)
rCPUOptions = lens _rCPUOptions (\s a -> s {_rCPUOptions = a})

-- | The ID of the RAM disk, if applicable.
rRamDiskId :: Lens' ResponseLaunchTemplateData (Maybe Text)
rRamDiskId = lens _rRamDiskId (\s a -> s {_rRamDiskId = a})

-- | The ID of the kernel, if applicable.
rKernelId :: Lens' ResponseLaunchTemplateData (Maybe Text)
rKernelId = lens _rKernelId (\s a -> s {_rKernelId = a})

-- | The elastic GPU specification.
rElasticGpuSpecifications :: Lens' ResponseLaunchTemplateData [ElasticGpuSpecificationResponse]
rElasticGpuSpecifications = lens _rElasticGpuSpecifications (\s a -> s {_rElasticGpuSpecifications = a}) . _Default . _Coerce

-- | The instance type.
rInstanceType :: Lens' ResponseLaunchTemplateData (Maybe InstanceType)
rInstanceType = lens _rInstanceType (\s a -> s {_rInstanceType = a})

-- | Information about the Capacity Reservation targeting option.
rCapacityReservationSpecification :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateCapacityReservationSpecificationResponse)
rCapacityReservationSpecification = lens _rCapacityReservationSpecification (\s a -> s {_rCapacityReservationSpecification = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
rEBSOptimized :: Lens' ResponseLaunchTemplateData (Maybe Bool)
rEBSOptimized = lens _rEBSOptimized (\s a -> s {_rEBSOptimized = a})

-- | The user data for the instance.
rUserData :: Lens' ResponseLaunchTemplateData (Maybe Text)
rUserData = lens _rUserData (\s a -> s {_rUserData = a})

-- | The monitoring for the instance.
rMonitoring :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplatesMonitoring)
rMonitoring = lens _rMonitoring (\s a -> s {_rMonitoring = a})

-- | The tags.
rTagSpecifications :: Lens' ResponseLaunchTemplateData [LaunchTemplateTagSpecification]
rTagSpecifications = lens _rTagSpecifications (\s a -> s {_rTagSpecifications = a}) . _Default . _Coerce

-- | Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
rHibernationOptions :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateHibernationOptions)
rHibernationOptions = lens _rHibernationOptions (\s a -> s {_rHibernationOptions = a})

-- | The IAM instance profile.
rIAMInstanceProfile :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateIAMInstanceProfileSpecification)
rIAMInstanceProfile = lens _rIAMInstanceProfile (\s a -> s {_rIAMInstanceProfile = a})

-- | The ID of the AMI that was used to launch the instance.
rImageId :: Lens' ResponseLaunchTemplateData (Maybe Text)
rImageId = lens _rImageId (\s a -> s {_rImageId = a})

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
rInstanceInitiatedShutdownBehavior :: Lens' ResponseLaunchTemplateData (Maybe ShutdownBehavior)
rInstanceInitiatedShutdownBehavior = lens _rInstanceInitiatedShutdownBehavior (\s a -> s {_rInstanceInitiatedShutdownBehavior = a})

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
rMetadataOptions :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateInstanceMetadataOptions)
rMetadataOptions = lens _rMetadataOptions (\s a -> s {_rMetadataOptions = a})

-- | The credit option for CPU usage of the instance.
rCreditSpecification :: Lens' ResponseLaunchTemplateData (Maybe CreditSpecification)
rCreditSpecification = lens _rCreditSpecification (\s a -> s {_rCreditSpecification = a})

-- | The block device mappings.
rBlockDeviceMappings :: Lens' ResponseLaunchTemplateData [LaunchTemplateBlockDeviceMapping]
rBlockDeviceMappings = lens _rBlockDeviceMappings (\s a -> s {_rBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement of the instance.
rPlacement :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplatePlacement)
rPlacement = lens _rPlacement (\s a -> s {_rPlacement = a})

instance FromXML ResponseLaunchTemplateData where
  parseXML x =
    ResponseLaunchTemplateData'
      <$> ( x .@? "securityGroupIdSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "securityGroupSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> ( x .@? "elasticInferenceAcceleratorSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "instanceMarketOptions")
      <*> (x .@? "licenseSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "disableApiTermination")
      <*> (x .@? "keyName")
      <*> ( x .@? "networkInterfaceSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "enclaveOptions")
      <*> (x .@? "cpuOptions")
      <*> (x .@? "ramDiskId")
      <*> (x .@? "kernelId")
      <*> ( x .@? "elasticGpuSpecificationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "instanceType")
      <*> (x .@? "capacityReservationSpecification")
      <*> (x .@? "ebsOptimized")
      <*> (x .@? "userData")
      <*> (x .@? "monitoring")
      <*> ( x .@? "tagSpecificationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "hibernationOptions")
      <*> (x .@? "iamInstanceProfile")
      <*> (x .@? "imageId")
      <*> (x .@? "instanceInitiatedShutdownBehavior")
      <*> (x .@? "metadataOptions")
      <*> (x .@? "creditSpecification")
      <*> ( x .@? "blockDeviceMappingSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "placement")

instance Hashable ResponseLaunchTemplateData

instance NFData ResponseLaunchTemplateData
