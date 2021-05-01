{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetLaunchSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetLaunchSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.IamInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.SpotFleetMonitoring
import Network.AWS.EC2.Types.SpotFleetTagSpecification
import Network.AWS.EC2.Types.SpotPlacement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the launch specification for one or more Spot Instances. If
-- you include On-Demand capacity in your fleet request or want to specify
-- an EFA network device, you can\'t use @SpotFleetLaunchSpecification@;
-- you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
--
-- /See:/ 'newSpotFleetLaunchSpecification' smart constructor.
data SpotFleetLaunchSpecification = SpotFleetLaunchSpecification'
  { -- | The tags to apply during creation.
    tagSpecifications :: Prelude.Maybe [SpotFleetTagSpecification],
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | Indicates whether the instances are optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The Base64-encoded user data that instances use when starting up.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The placement information.
    placement :: Prelude.Maybe SpotPlacement,
    -- | Deprecated.
    addressingType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RAM disk. Some kernels require additional drivers at
    -- launch. Check the kernel requirements for information about whether you
    -- need to specify a RAM disk. To find kernel requirements, refer to the
    -- AWS Resource Center and search for the kernel ID.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. If this value is not specified, the default is the Spot price
    -- specified for the fleet. To determine the Spot price per unit hour,
    -- divide the Spot price by the value of @WeightedCapacity@.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Prelude.Maybe [GroupIdentifier],
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    -- | Enable or disable monitoring for the instances.
    monitoring :: Prelude.Maybe SpotFleetMonitoring,
    -- | One or more block devices that are mapped to the Spot Instances. You
    -- can\'t specify both a snapshot ID and an encryption value. This is
    -- because only blank volumes can be encrypted on creation. If a snapshot
    -- is the basis for a volume, it is not blank and its encryption status is
    -- used for the volume encryption status.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The IDs of the subnets in which to launch the instances. To specify
    -- multiple subnets, separate them using commas; for example,
    -- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    --
    -- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
    -- Adapter (EFA). To specify an EFA, you must use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification],
    -- | The number of units provided by the specified instance type. These are
    -- the same units that you chose to set the target capacity in terms of
    -- instances, or a performance characteristic such as vCPUs, memory, or
    -- I\/O.
    --
    -- If the target capacity divided by this value is not a whole number,
    -- Amazon EC2 rounds the number of instances to the next whole number. If
    -- this value is not specified, the default is 1.
    weightedCapacity :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SpotFleetLaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'spotFleetLaunchSpecification_tagSpecifications' - The tags to apply during creation.
--
-- 'instanceType', 'spotFleetLaunchSpecification_instanceType' - The instance type.
--
-- 'ebsOptimized', 'spotFleetLaunchSpecification_ebsOptimized' - Indicates whether the instances are optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- 'userData', 'spotFleetLaunchSpecification_userData' - The Base64-encoded user data that instances use when starting up.
--
-- 'placement', 'spotFleetLaunchSpecification_placement' - The placement information.
--
-- 'addressingType', 'spotFleetLaunchSpecification_addressingType' - Deprecated.
--
-- 'ramdiskId', 'spotFleetLaunchSpecification_ramdiskId' - The ID of the RAM disk. Some kernels require additional drivers at
-- launch. Check the kernel requirements for information about whether you
-- need to specify a RAM disk. To find kernel requirements, refer to the
-- AWS Resource Center and search for the kernel ID.
--
-- 'spotPrice', 'spotFleetLaunchSpecification_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If this value is not specified, the default is the Spot price
-- specified for the fleet. To determine the Spot price per unit hour,
-- divide the Spot price by the value of @WeightedCapacity@.
--
-- 'imageId', 'spotFleetLaunchSpecification_imageId' - The ID of the AMI.
--
-- 'securityGroups', 'spotFleetLaunchSpecification_securityGroups' - One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
--
-- 'iamInstanceProfile', 'spotFleetLaunchSpecification_iamInstanceProfile' - The IAM instance profile.
--
-- 'monitoring', 'spotFleetLaunchSpecification_monitoring' - Enable or disable monitoring for the instances.
--
-- 'blockDeviceMappings', 'spotFleetLaunchSpecification_blockDeviceMappings' - One or more block devices that are mapped to the Spot Instances. You
-- can\'t specify both a snapshot ID and an encryption value. This is
-- because only blank volumes can be encrypted on creation. If a snapshot
-- is the basis for a volume, it is not blank and its encryption status is
-- used for the volume encryption status.
--
-- 'subnetId', 'spotFleetLaunchSpecification_subnetId' - The IDs of the subnets in which to launch the instances. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
--
-- 'kernelId', 'spotFleetLaunchSpecification_kernelId' - The ID of the kernel.
--
-- 'keyName', 'spotFleetLaunchSpecification_keyName' - The name of the key pair.
--
-- 'networkInterfaces', 'spotFleetLaunchSpecification_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
--
-- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
-- Adapter (EFA). To specify an EFA, you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
--
-- 'weightedCapacity', 'spotFleetLaunchSpecification_weightedCapacity' - The number of units provided by the specified instance type. These are
-- the same units that you chose to set the target capacity in terms of
-- instances, or a performance characteristic such as vCPUs, memory, or
-- I\/O.
--
-- If the target capacity divided by this value is not a whole number,
-- Amazon EC2 rounds the number of instances to the next whole number. If
-- this value is not specified, the default is 1.
newSpotFleetLaunchSpecification ::
  SpotFleetLaunchSpecification
newSpotFleetLaunchSpecification =
  SpotFleetLaunchSpecification'
    { tagSpecifications =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      placement = Prelude.Nothing,
      addressingType = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      imageId = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

-- | The tags to apply during creation.
spotFleetLaunchSpecification_tagSpecifications :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [SpotFleetTagSpecification])
spotFleetLaunchSpecification_tagSpecifications = Lens.lens (\SpotFleetLaunchSpecification' {tagSpecifications} -> tagSpecifications) (\s@SpotFleetLaunchSpecification' {} a -> s {tagSpecifications = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The instance type.
spotFleetLaunchSpecification_instanceType :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe InstanceType)
spotFleetLaunchSpecification_instanceType = Lens.lens (\SpotFleetLaunchSpecification' {instanceType} -> instanceType) (\s@SpotFleetLaunchSpecification' {} a -> s {instanceType = a} :: SpotFleetLaunchSpecification)

-- | Indicates whether the instances are optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
spotFleetLaunchSpecification_ebsOptimized :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Bool)
spotFleetLaunchSpecification_ebsOptimized = Lens.lens (\SpotFleetLaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@SpotFleetLaunchSpecification' {} a -> s {ebsOptimized = a} :: SpotFleetLaunchSpecification)

-- | The Base64-encoded user data that instances use when starting up.
spotFleetLaunchSpecification_userData :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_userData = Lens.lens (\SpotFleetLaunchSpecification' {userData} -> userData) (\s@SpotFleetLaunchSpecification' {} a -> s {userData = a} :: SpotFleetLaunchSpecification)

-- | The placement information.
spotFleetLaunchSpecification_placement :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe SpotPlacement)
spotFleetLaunchSpecification_placement = Lens.lens (\SpotFleetLaunchSpecification' {placement} -> placement) (\s@SpotFleetLaunchSpecification' {} a -> s {placement = a} :: SpotFleetLaunchSpecification)

-- | Deprecated.
spotFleetLaunchSpecification_addressingType :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_addressingType = Lens.lens (\SpotFleetLaunchSpecification' {addressingType} -> addressingType) (\s@SpotFleetLaunchSpecification' {} a -> s {addressingType = a} :: SpotFleetLaunchSpecification)

-- | The ID of the RAM disk. Some kernels require additional drivers at
-- launch. Check the kernel requirements for information about whether you
-- need to specify a RAM disk. To find kernel requirements, refer to the
-- AWS Resource Center and search for the kernel ID.
spotFleetLaunchSpecification_ramdiskId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_ramdiskId = Lens.lens (\SpotFleetLaunchSpecification' {ramdiskId} -> ramdiskId) (\s@SpotFleetLaunchSpecification' {} a -> s {ramdiskId = a} :: SpotFleetLaunchSpecification)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If this value is not specified, the default is the Spot price
-- specified for the fleet. To determine the Spot price per unit hour,
-- divide the Spot price by the value of @WeightedCapacity@.
spotFleetLaunchSpecification_spotPrice :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_spotPrice = Lens.lens (\SpotFleetLaunchSpecification' {spotPrice} -> spotPrice) (\s@SpotFleetLaunchSpecification' {} a -> s {spotPrice = a} :: SpotFleetLaunchSpecification)

-- | The ID of the AMI.
spotFleetLaunchSpecification_imageId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_imageId = Lens.lens (\SpotFleetLaunchSpecification' {imageId} -> imageId) (\s@SpotFleetLaunchSpecification' {} a -> s {imageId = a} :: SpotFleetLaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
spotFleetLaunchSpecification_securityGroups :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [GroupIdentifier])
spotFleetLaunchSpecification_securityGroups = Lens.lens (\SpotFleetLaunchSpecification' {securityGroups} -> securityGroups) (\s@SpotFleetLaunchSpecification' {} a -> s {securityGroups = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The IAM instance profile.
spotFleetLaunchSpecification_iamInstanceProfile :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe IamInstanceProfileSpecification)
spotFleetLaunchSpecification_iamInstanceProfile = Lens.lens (\SpotFleetLaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@SpotFleetLaunchSpecification' {} a -> s {iamInstanceProfile = a} :: SpotFleetLaunchSpecification)

-- | Enable or disable monitoring for the instances.
spotFleetLaunchSpecification_monitoring :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe SpotFleetMonitoring)
spotFleetLaunchSpecification_monitoring = Lens.lens (\SpotFleetLaunchSpecification' {monitoring} -> monitoring) (\s@SpotFleetLaunchSpecification' {} a -> s {monitoring = a} :: SpotFleetLaunchSpecification)

-- | One or more block devices that are mapped to the Spot Instances. You
-- can\'t specify both a snapshot ID and an encryption value. This is
-- because only blank volumes can be encrypted on creation. If a snapshot
-- is the basis for a volume, it is not blank and its encryption status is
-- used for the volume encryption status.
spotFleetLaunchSpecification_blockDeviceMappings :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [BlockDeviceMapping])
spotFleetLaunchSpecification_blockDeviceMappings = Lens.lens (\SpotFleetLaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@SpotFleetLaunchSpecification' {} a -> s {blockDeviceMappings = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The IDs of the subnets in which to launch the instances. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
spotFleetLaunchSpecification_subnetId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_subnetId = Lens.lens (\SpotFleetLaunchSpecification' {subnetId} -> subnetId) (\s@SpotFleetLaunchSpecification' {} a -> s {subnetId = a} :: SpotFleetLaunchSpecification)

-- | The ID of the kernel.
spotFleetLaunchSpecification_kernelId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_kernelId = Lens.lens (\SpotFleetLaunchSpecification' {kernelId} -> kernelId) (\s@SpotFleetLaunchSpecification' {} a -> s {kernelId = a} :: SpotFleetLaunchSpecification)

-- | The name of the key pair.
spotFleetLaunchSpecification_keyName :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_keyName = Lens.lens (\SpotFleetLaunchSpecification' {keyName} -> keyName) (\s@SpotFleetLaunchSpecification' {} a -> s {keyName = a} :: SpotFleetLaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
--
-- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
-- Adapter (EFA). To specify an EFA, you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
spotFleetLaunchSpecification_networkInterfaces :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
spotFleetLaunchSpecification_networkInterfaces = Lens.lens (\SpotFleetLaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@SpotFleetLaunchSpecification' {} a -> s {networkInterfaces = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of units provided by the specified instance type. These are
-- the same units that you chose to set the target capacity in terms of
-- instances, or a performance characteristic such as vCPUs, memory, or
-- I\/O.
--
-- If the target capacity divided by this value is not a whole number,
-- Amazon EC2 rounds the number of instances to the next whole number. If
-- this value is not specified, the default is 1.
spotFleetLaunchSpecification_weightedCapacity :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Double)
spotFleetLaunchSpecification_weightedCapacity = Lens.lens (\SpotFleetLaunchSpecification' {weightedCapacity} -> weightedCapacity) (\s@SpotFleetLaunchSpecification' {} a -> s {weightedCapacity = a} :: SpotFleetLaunchSpecification)

instance Prelude.FromXML SpotFleetLaunchSpecification where
  parseXML x =
    SpotFleetLaunchSpecification'
      Prelude.<$> ( x Prelude..@? "tagSpecificationSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "ebsOptimized")
      Prelude.<*> (x Prelude..@? "userData")
      Prelude.<*> (x Prelude..@? "placement")
      Prelude.<*> (x Prelude..@? "addressingType")
      Prelude.<*> (x Prelude..@? "ramdiskId")
      Prelude.<*> (x Prelude..@? "spotPrice")
      Prelude.<*> (x Prelude..@? "imageId")
      Prelude.<*> ( x Prelude..@? "groupSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "iamInstanceProfile")
      Prelude.<*> (x Prelude..@? "monitoring")
      Prelude.<*> ( x Prelude..@? "blockDeviceMapping"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "subnetId")
      Prelude.<*> (x Prelude..@? "kernelId")
      Prelude.<*> (x Prelude..@? "keyName")
      Prelude.<*> ( x Prelude..@? "networkInterfaceSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "weightedCapacity")

instance
  Prelude.Hashable
    SpotFleetLaunchSpecification

instance Prelude.NFData SpotFleetLaunchSpecification

instance Prelude.ToQuery SpotFleetLaunchSpecification where
  toQuery SpotFleetLaunchSpecification' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          ( Prelude.toQueryList "TagSpecificationSet"
              Prelude.<$> tagSpecifications
          ),
        "InstanceType" Prelude.=: instanceType,
        "EbsOptimized" Prelude.=: ebsOptimized,
        "UserData" Prelude.=: userData,
        "Placement" Prelude.=: placement,
        "AddressingType" Prelude.=: addressingType,
        "RamdiskId" Prelude.=: ramdiskId,
        "SpotPrice" Prelude.=: spotPrice,
        "ImageId" Prelude.=: imageId,
        Prelude.toQuery
          ( Prelude.toQueryList "GroupSet"
              Prelude.<$> securityGroups
          ),
        "IamInstanceProfile" Prelude.=: iamInstanceProfile,
        "Monitoring" Prelude.=: monitoring,
        Prelude.toQuery
          ( Prelude.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "SubnetId" Prelude.=: subnetId,
        "KernelId" Prelude.=: kernelId,
        "KeyName" Prelude.=: keyName,
        Prelude.toQuery
          ( Prelude.toQueryList "NetworkInterfaceSet"
              Prelude.<$> networkInterfaces
          ),
        "WeightedCapacity" Prelude.=: weightedCapacity
      ]
