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
-- Module      : Network.AWS.EC2.Types.LaunchSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.IamInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.SpotPlacement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the launch specification for an instance.
--
-- /See:/ 'newLaunchSpecification' smart constructor.
data LaunchSpecification = LaunchSpecification'
  { -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | Indicates whether the instance is optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The Base64-encoded user data for the instance.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The placement information for the instance.
    placement :: Prelude.Maybe SpotPlacement,
    -- | Deprecated.
    addressingType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RAM disk.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Prelude.Maybe [GroupIdentifier],
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    monitoring :: Prelude.Maybe RunInstancesMonitoringEnabled,
    -- | One or more block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The ID of the subnet in which to launch the instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'launchSpecification_instanceType' - The instance type.
--
-- 'ebsOptimized', 'launchSpecification_ebsOptimized' - Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- 'userData', 'launchSpecification_userData' - The Base64-encoded user data for the instance.
--
-- 'placement', 'launchSpecification_placement' - The placement information for the instance.
--
-- 'addressingType', 'launchSpecification_addressingType' - Deprecated.
--
-- 'ramdiskId', 'launchSpecification_ramdiskId' - The ID of the RAM disk.
--
-- 'imageId', 'launchSpecification_imageId' - The ID of the AMI.
--
-- 'securityGroups', 'launchSpecification_securityGroups' - One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
--
-- 'iamInstanceProfile', 'launchSpecification_iamInstanceProfile' - The IAM instance profile.
--
-- 'monitoring', 'launchSpecification_monitoring' - Undocumented member.
--
-- 'blockDeviceMappings', 'launchSpecification_blockDeviceMappings' - One or more block device mapping entries.
--
-- 'subnetId', 'launchSpecification_subnetId' - The ID of the subnet in which to launch the instance.
--
-- 'kernelId', 'launchSpecification_kernelId' - The ID of the kernel.
--
-- 'keyName', 'launchSpecification_keyName' - The name of the key pair.
--
-- 'networkInterfaces', 'launchSpecification_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
newLaunchSpecification ::
  LaunchSpecification
newLaunchSpecification =
  LaunchSpecification'
    { instanceType =
        Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      placement = Prelude.Nothing,
      addressingType = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      imageId = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | The instance type.
launchSpecification_instanceType :: Lens.Lens' LaunchSpecification (Prelude.Maybe InstanceType)
launchSpecification_instanceType = Lens.lens (\LaunchSpecification' {instanceType} -> instanceType) (\s@LaunchSpecification' {} a -> s {instanceType = a} :: LaunchSpecification)

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
launchSpecification_ebsOptimized :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Bool)
launchSpecification_ebsOptimized = Lens.lens (\LaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@LaunchSpecification' {} a -> s {ebsOptimized = a} :: LaunchSpecification)

-- | The Base64-encoded user data for the instance.
launchSpecification_userData :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_userData = Lens.lens (\LaunchSpecification' {userData} -> userData) (\s@LaunchSpecification' {} a -> s {userData = a} :: LaunchSpecification)

-- | The placement information for the instance.
launchSpecification_placement :: Lens.Lens' LaunchSpecification (Prelude.Maybe SpotPlacement)
launchSpecification_placement = Lens.lens (\LaunchSpecification' {placement} -> placement) (\s@LaunchSpecification' {} a -> s {placement = a} :: LaunchSpecification)

-- | Deprecated.
launchSpecification_addressingType :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_addressingType = Lens.lens (\LaunchSpecification' {addressingType} -> addressingType) (\s@LaunchSpecification' {} a -> s {addressingType = a} :: LaunchSpecification)

-- | The ID of the RAM disk.
launchSpecification_ramdiskId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_ramdiskId = Lens.lens (\LaunchSpecification' {ramdiskId} -> ramdiskId) (\s@LaunchSpecification' {} a -> s {ramdiskId = a} :: LaunchSpecification)

-- | The ID of the AMI.
launchSpecification_imageId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_imageId = Lens.lens (\LaunchSpecification' {imageId} -> imageId) (\s@LaunchSpecification' {} a -> s {imageId = a} :: LaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
launchSpecification_securityGroups :: Lens.Lens' LaunchSpecification (Prelude.Maybe [GroupIdentifier])
launchSpecification_securityGroups = Lens.lens (\LaunchSpecification' {securityGroups} -> securityGroups) (\s@LaunchSpecification' {} a -> s {securityGroups = a} :: LaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The IAM instance profile.
launchSpecification_iamInstanceProfile :: Lens.Lens' LaunchSpecification (Prelude.Maybe IamInstanceProfileSpecification)
launchSpecification_iamInstanceProfile = Lens.lens (\LaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@LaunchSpecification' {} a -> s {iamInstanceProfile = a} :: LaunchSpecification)

-- | Undocumented member.
launchSpecification_monitoring :: Lens.Lens' LaunchSpecification (Prelude.Maybe RunInstancesMonitoringEnabled)
launchSpecification_monitoring = Lens.lens (\LaunchSpecification' {monitoring} -> monitoring) (\s@LaunchSpecification' {} a -> s {monitoring = a} :: LaunchSpecification)

-- | One or more block device mapping entries.
launchSpecification_blockDeviceMappings :: Lens.Lens' LaunchSpecification (Prelude.Maybe [BlockDeviceMapping])
launchSpecification_blockDeviceMappings = Lens.lens (\LaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@LaunchSpecification' {} a -> s {blockDeviceMappings = a} :: LaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the subnet in which to launch the instance.
launchSpecification_subnetId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_subnetId = Lens.lens (\LaunchSpecification' {subnetId} -> subnetId) (\s@LaunchSpecification' {} a -> s {subnetId = a} :: LaunchSpecification)

-- | The ID of the kernel.
launchSpecification_kernelId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_kernelId = Lens.lens (\LaunchSpecification' {kernelId} -> kernelId) (\s@LaunchSpecification' {} a -> s {kernelId = a} :: LaunchSpecification)

-- | The name of the key pair.
launchSpecification_keyName :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_keyName = Lens.lens (\LaunchSpecification' {keyName} -> keyName) (\s@LaunchSpecification' {} a -> s {keyName = a} :: LaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
launchSpecification_networkInterfaces :: Lens.Lens' LaunchSpecification (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
launchSpecification_networkInterfaces = Lens.lens (\LaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@LaunchSpecification' {} a -> s {networkInterfaces = a} :: LaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML LaunchSpecification where
  parseXML x =
    LaunchSpecification'
      Prelude.<$> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "ebsOptimized")
      Prelude.<*> (x Prelude..@? "userData")
      Prelude.<*> (x Prelude..@? "placement")
      Prelude.<*> (x Prelude..@? "addressingType")
      Prelude.<*> (x Prelude..@? "ramdiskId")
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

instance Prelude.Hashable LaunchSpecification

instance Prelude.NFData LaunchSpecification
