{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResponseLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseLaunchTemplateData
  ( ResponseLaunchTemplateData (..),

    -- * Smart constructor
    mkResponseLaunchTemplateData,

    -- * Lenses
    rSecurityGroupIds,
    rSecurityGroups,
    rElasticInferenceAccelerators,
    rInstanceMarketOptions,
    rLicenseSpecifications,
    rDisableAPITermination,
    rKeyName,
    rNetworkInterfaces,
    rEnclaveOptions,
    rCPUOptions,
    rRamDiskId,
    rKernelId,
    rElasticGpuSpecifications,
    rInstanceType,
    rCapacityReservationSpecification,
    rEBSOptimized,
    rUserData,
    rMonitoring,
    rTagSpecifications,
    rHibernationOptions,
    rIAMInstanceProfile,
    rImageId,
    rInstanceInitiatedShutdownBehavior,
    rMetadataOptions,
    rCreditSpecification,
    rBlockDeviceMappings,
    rPlacement,
  )
where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The information for a launch template.
--
-- /See:/ 'mkResponseLaunchTemplateData' smart constructor.
data ResponseLaunchTemplateData = ResponseLaunchTemplateData'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    securityGroups ::
      Lude.Maybe [Lude.Text],
    elasticInferenceAccelerators ::
      Lude.Maybe
        [LaunchTemplateElasticInferenceAcceleratorResponse],
    instanceMarketOptions ::
      Lude.Maybe
        LaunchTemplateInstanceMarketOptions,
    licenseSpecifications ::
      Lude.Maybe
        [LaunchTemplateLicenseConfiguration],
    disableAPITermination ::
      Lude.Maybe Lude.Bool,
    keyName :: Lude.Maybe Lude.Text,
    networkInterfaces ::
      Lude.Maybe
        [LaunchTemplateInstanceNetworkInterfaceSpecification],
    enclaveOptions ::
      Lude.Maybe
        LaunchTemplateEnclaveOptions,
    cpuOptions ::
      Lude.Maybe LaunchTemplateCPUOptions,
    ramDiskId :: Lude.Maybe Lude.Text,
    kernelId :: Lude.Maybe Lude.Text,
    elasticGpuSpecifications ::
      Lude.Maybe
        [ElasticGpuSpecificationResponse],
    instanceType ::
      Lude.Maybe InstanceType,
    capacityReservationSpecification ::
      Lude.Maybe
        LaunchTemplateCapacityReservationSpecificationResponse,
    ebsOptimized :: Lude.Maybe Lude.Bool,
    userData :: Lude.Maybe Lude.Text,
    monitoring ::
      Lude.Maybe LaunchTemplatesMonitoring,
    tagSpecifications ::
      Lude.Maybe
        [LaunchTemplateTagSpecification],
    hibernationOptions ::
      Lude.Maybe
        LaunchTemplateHibernationOptions,
    iamInstanceProfile ::
      Lude.Maybe
        LaunchTemplateIAMInstanceProfileSpecification,
    imageId :: Lude.Maybe Lude.Text,
    instanceInitiatedShutdownBehavior ::
      Lude.Maybe ShutdownBehavior,
    metadataOptions ::
      Lude.Maybe
        LaunchTemplateInstanceMetadataOptions,
    creditSpecification ::
      Lude.Maybe CreditSpecification,
    blockDeviceMappings ::
      Lude.Maybe
        [LaunchTemplateBlockDeviceMapping],
    placement ::
      Lude.Maybe LaunchTemplatePlacement
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResponseLaunchTemplateData' with the minimum fields required to make a request.
--
-- * 'blockDeviceMappings' - The block device mappings.
-- * 'capacityReservationSpecification' - Information about the Capacity Reservation targeting option.
-- * 'cpuOptions' - The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'creditSpecification' - The credit option for CPU usage of the instance.
-- * 'disableAPITermination' - If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O.
-- * 'elasticGpuSpecifications' - The elastic GPU specification.
-- * 'elasticInferenceAccelerators' - The elastic inference accelerator for the instance.
-- * 'enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves.
-- * 'hibernationOptions' - Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'imageId' - The ID of the AMI that was used to launch the instance.
-- * 'instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
-- * 'instanceMarketOptions' - The market (purchasing) option for the instances.
-- * 'instanceType' - The instance type.
-- * 'kernelId' - The ID of the kernel, if applicable.
-- * 'keyName' - The name of the key pair.
-- * 'licenseSpecifications' - The license configurations.
-- * 'metadataOptions' - The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'monitoring' - The monitoring for the instance.
-- * 'networkInterfaces' - The network interfaces.
-- * 'placement' - The placement of the instance.
-- * 'ramDiskId' - The ID of the RAM disk, if applicable.
-- * 'securityGroupIds' - The security group IDs.
-- * 'securityGroups' - The security group names.
-- * 'tagSpecifications' - The tags.
-- * 'userData' - The user data for the instance.
mkResponseLaunchTemplateData ::
  ResponseLaunchTemplateData
mkResponseLaunchTemplateData =
  ResponseLaunchTemplateData'
    { securityGroupIds = Lude.Nothing,
      securityGroups = Lude.Nothing,
      elasticInferenceAccelerators = Lude.Nothing,
      instanceMarketOptions = Lude.Nothing,
      licenseSpecifications = Lude.Nothing,
      disableAPITermination = Lude.Nothing,
      keyName = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      enclaveOptions = Lude.Nothing,
      cpuOptions = Lude.Nothing,
      ramDiskId = Lude.Nothing,
      kernelId = Lude.Nothing,
      elasticGpuSpecifications = Lude.Nothing,
      instanceType = Lude.Nothing,
      capacityReservationSpecification = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      monitoring = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      hibernationOptions = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      imageId = Lude.Nothing,
      instanceInitiatedShutdownBehavior = Lude.Nothing,
      metadataOptions = Lude.Nothing,
      creditSpecification = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      placement = Lude.Nothing
    }

-- | The security group IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecurityGroupIds :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [Lude.Text])
rSecurityGroupIds = Lens.lens (securityGroupIds :: ResponseLaunchTemplateData -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The security group names.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecurityGroups :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [Lude.Text])
rSecurityGroups = Lens.lens (securityGroups :: ResponseLaunchTemplateData -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The elastic inference accelerator for the instance.
--
-- /Note:/ Consider using 'elasticInferenceAccelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rElasticInferenceAccelerators :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [LaunchTemplateElasticInferenceAcceleratorResponse])
rElasticInferenceAccelerators = Lens.lens (elasticInferenceAccelerators :: ResponseLaunchTemplateData -> Lude.Maybe [LaunchTemplateElasticInferenceAcceleratorResponse]) (\s a -> s {elasticInferenceAccelerators = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rElasticInferenceAccelerators "Use generic-lens or generic-optics with 'elasticInferenceAccelerators' instead." #-}

-- | The market (purchasing) option for the instances.
--
-- /Note:/ Consider using 'instanceMarketOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceMarketOptions :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplateInstanceMarketOptions)
rInstanceMarketOptions = Lens.lens (instanceMarketOptions :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplateInstanceMarketOptions) (\s a -> s {instanceMarketOptions = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rInstanceMarketOptions "Use generic-lens or generic-optics with 'instanceMarketOptions' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLicenseSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [LaunchTemplateLicenseConfiguration])
rLicenseSpecifications = Lens.lens (licenseSpecifications :: ResponseLaunchTemplateData -> Lude.Maybe [LaunchTemplateLicenseConfiguration]) (\s a -> s {licenseSpecifications = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
--
-- /Note:/ Consider using 'disableAPITermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDisableAPITermination :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe Lude.Bool)
rDisableAPITermination = Lens.lens (disableAPITermination :: ResponseLaunchTemplateData -> Lude.Maybe Lude.Bool) (\s a -> s {disableAPITermination = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rDisableAPITermination "Use generic-lens or generic-optics with 'disableAPITermination' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKeyName :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe Lude.Text)
rKeyName = Lens.lens (keyName :: ResponseLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The network interfaces.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkInterfaces :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecification])
rNetworkInterfaces = Lens.lens (networkInterfaces :: ResponseLaunchTemplateData -> Lude.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecification]) (\s a -> s {networkInterfaces = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnclaveOptions :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplateEnclaveOptions)
rEnclaveOptions = Lens.lens (enclaveOptions :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplateEnclaveOptions) (\s a -> s {enclaveOptions = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The CPU options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCPUOptions :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplateCPUOptions)
rCPUOptions = Lens.lens (cpuOptions :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplateCPUOptions) (\s a -> s {cpuOptions = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rCPUOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | The ID of the RAM disk, if applicable.
--
-- /Note:/ Consider using 'ramDiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRamDiskId :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe Lude.Text)
rRamDiskId = Lens.lens (ramDiskId :: ResponseLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {ramDiskId = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rRamDiskId "Use generic-lens or generic-optics with 'ramDiskId' instead." #-}

-- | The ID of the kernel, if applicable.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKernelId :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe Lude.Text)
rKernelId = Lens.lens (kernelId :: ResponseLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The elastic GPU specification.
--
-- /Note:/ Consider using 'elasticGpuSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rElasticGpuSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [ElasticGpuSpecificationResponse])
rElasticGpuSpecifications = Lens.lens (elasticGpuSpecifications :: ResponseLaunchTemplateData -> Lude.Maybe [ElasticGpuSpecificationResponse]) (\s a -> s {elasticGpuSpecifications = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rElasticGpuSpecifications "Use generic-lens or generic-optics with 'elasticGpuSpecifications' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceType :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe InstanceType)
rInstanceType = Lens.lens (instanceType :: ResponseLaunchTemplateData -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCapacityReservationSpecification :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplateCapacityReservationSpecificationResponse)
rCapacityReservationSpecification = Lens.lens (capacityReservationSpecification :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplateCapacityReservationSpecificationResponse) (\s a -> s {capacityReservationSpecification = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEBSOptimized :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe Lude.Bool)
rEBSOptimized = Lens.lens (ebsOptimized :: ResponseLaunchTemplateData -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The user data for the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUserData :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe Lude.Text)
rUserData = Lens.lens (userData :: ResponseLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMonitoring :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplatesMonitoring)
rMonitoring = Lens.lens (monitoring :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplatesMonitoring) (\s a -> s {monitoring = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTagSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [LaunchTemplateTagSpecification])
rTagSpecifications = Lens.lens (tagSpecifications :: ResponseLaunchTemplateData -> Lude.Maybe [LaunchTemplateTagSpecification]) (\s a -> s {tagSpecifications = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Indicates whether an instance is configured for hibernation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHibernationOptions :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplateHibernationOptions)
rHibernationOptions = Lens.lens (hibernationOptions :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplateHibernationOptions) (\s a -> s {hibernationOptions = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIAMInstanceProfile :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplateIAMInstanceProfileSpecification)
rIAMInstanceProfile = Lens.lens (iamInstanceProfile :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplateIAMInstanceProfileSpecification) (\s a -> s {iamInstanceProfile = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI that was used to launch the instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rImageId :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe Lude.Text)
rImageId = Lens.lens (imageId :: ResponseLaunchTemplateData -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceInitiatedShutdownBehavior :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe ShutdownBehavior)
rInstanceInitiatedShutdownBehavior = Lens.lens (instanceInitiatedShutdownBehavior :: ResponseLaunchTemplateData -> Lude.Maybe ShutdownBehavior) (\s a -> s {instanceInitiatedShutdownBehavior = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMetadataOptions :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplateInstanceMetadataOptions)
rMetadataOptions = Lens.lens (metadataOptions :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplateInstanceMetadataOptions) (\s a -> s {metadataOptions = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The credit option for CPU usage of the instance.
--
-- /Note:/ Consider using 'creditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreditSpecification :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe CreditSpecification)
rCreditSpecification = Lens.lens (creditSpecification :: ResponseLaunchTemplateData -> Lude.Maybe CreditSpecification) (\s a -> s {creditSpecification = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rCreditSpecification "Use generic-lens or generic-optics with 'creditSpecification' instead." #-}

-- | The block device mappings.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBlockDeviceMappings :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe [LaunchTemplateBlockDeviceMapping])
rBlockDeviceMappings = Lens.lens (blockDeviceMappings :: ResponseLaunchTemplateData -> Lude.Maybe [LaunchTemplateBlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The placement of the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPlacement :: Lens.Lens' ResponseLaunchTemplateData (Lude.Maybe LaunchTemplatePlacement)
rPlacement = Lens.lens (placement :: ResponseLaunchTemplateData -> Lude.Maybe LaunchTemplatePlacement) (\s a -> s {placement = a} :: ResponseLaunchTemplateData)
{-# DEPRECATED rPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

instance Lude.FromXML ResponseLaunchTemplateData where
  parseXML x =
    ResponseLaunchTemplateData'
      Lude.<$> ( x Lude..@? "securityGroupIdSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "securityGroupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "elasticInferenceAcceleratorSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "instanceMarketOptions")
      Lude.<*> ( x Lude..@? "licenseSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "disableApiTermination")
      Lude.<*> (x Lude..@? "keyName")
      Lude.<*> ( x Lude..@? "networkInterfaceSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "enclaveOptions")
      Lude.<*> (x Lude..@? "cpuOptions")
      Lude.<*> (x Lude..@? "ramDiskId")
      Lude.<*> (x Lude..@? "kernelId")
      Lude.<*> ( x Lude..@? "elasticGpuSpecificationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "capacityReservationSpecification")
      Lude.<*> (x Lude..@? "ebsOptimized")
      Lude.<*> (x Lude..@? "userData")
      Lude.<*> (x Lude..@? "monitoring")
      Lude.<*> ( x Lude..@? "tagSpecificationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "hibernationOptions")
      Lude.<*> (x Lude..@? "iamInstanceProfile")
      Lude.<*> (x Lude..@? "imageId")
      Lude.<*> (x Lude..@? "instanceInitiatedShutdownBehavior")
      Lude.<*> (x Lude..@? "metadataOptions")
      Lude.<*> (x Lude..@? "creditSpecification")
      Lude.<*> ( x Lude..@? "blockDeviceMappingSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "placement")
