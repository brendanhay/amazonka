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
-- Module      : Amazonka.EC2.Types.LaunchSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.IamInstanceProfileSpecification
import Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.RunInstancesMonitoringEnabled
import Amazonka.EC2.Types.SpotPlacement
import qualified Amazonka.Prelude as Prelude

-- | Describes the launch specification for an instance.
--
-- /See:/ 'newLaunchSpecification' smart constructor.
data LaunchSpecification = LaunchSpecification'
  { -- | Indicates whether the instance is optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    -- | The placement information for the instance.
    placement :: Prelude.Maybe SpotPlacement,
    -- | The Base64-encoded user data for the instance.
    userData :: Prelude.Maybe Prelude.Text,
    -- | One or more block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | Deprecated.
    addressingType :: Prelude.Maybe Prelude.Text,
    monitoring :: Prelude.Maybe RunInstancesMonitoringEnabled,
    -- | The ID of the subnet in which to launch the instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The instance type. Only one instance type can be specified.
    instanceType :: Prelude.Maybe InstanceType,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Prelude.Maybe [GroupIdentifier],
    -- | The ID of the RAM disk.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsOptimized', 'launchSpecification_ebsOptimized' - Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- 'iamInstanceProfile', 'launchSpecification_iamInstanceProfile' - The IAM instance profile.
--
-- 'placement', 'launchSpecification_placement' - The placement information for the instance.
--
-- 'userData', 'launchSpecification_userData' - The Base64-encoded user data for the instance.
--
-- 'blockDeviceMappings', 'launchSpecification_blockDeviceMappings' - One or more block device mapping entries.
--
-- 'addressingType', 'launchSpecification_addressingType' - Deprecated.
--
-- 'monitoring', 'launchSpecification_monitoring' - Undocumented member.
--
-- 'subnetId', 'launchSpecification_subnetId' - The ID of the subnet in which to launch the instance.
--
-- 'instanceType', 'launchSpecification_instanceType' - The instance type. Only one instance type can be specified.
--
-- 'securityGroups', 'launchSpecification_securityGroups' - One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
--
-- 'ramdiskId', 'launchSpecification_ramdiskId' - The ID of the RAM disk.
--
-- 'keyName', 'launchSpecification_keyName' - The name of the key pair.
--
-- 'kernelId', 'launchSpecification_kernelId' - The ID of the kernel.
--
-- 'imageId', 'launchSpecification_imageId' - The ID of the AMI.
--
-- 'networkInterfaces', 'launchSpecification_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
newLaunchSpecification ::
  LaunchSpecification
newLaunchSpecification =
  LaunchSpecification'
    { ebsOptimized =
        Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      placement = Prelude.Nothing,
      userData = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      addressingType = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      imageId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
launchSpecification_ebsOptimized :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Bool)
launchSpecification_ebsOptimized = Lens.lens (\LaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@LaunchSpecification' {} a -> s {ebsOptimized = a} :: LaunchSpecification)

-- | The IAM instance profile.
launchSpecification_iamInstanceProfile :: Lens.Lens' LaunchSpecification (Prelude.Maybe IamInstanceProfileSpecification)
launchSpecification_iamInstanceProfile = Lens.lens (\LaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@LaunchSpecification' {} a -> s {iamInstanceProfile = a} :: LaunchSpecification)

-- | The placement information for the instance.
launchSpecification_placement :: Lens.Lens' LaunchSpecification (Prelude.Maybe SpotPlacement)
launchSpecification_placement = Lens.lens (\LaunchSpecification' {placement} -> placement) (\s@LaunchSpecification' {} a -> s {placement = a} :: LaunchSpecification)

-- | The Base64-encoded user data for the instance.
launchSpecification_userData :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_userData = Lens.lens (\LaunchSpecification' {userData} -> userData) (\s@LaunchSpecification' {} a -> s {userData = a} :: LaunchSpecification)

-- | One or more block device mapping entries.
launchSpecification_blockDeviceMappings :: Lens.Lens' LaunchSpecification (Prelude.Maybe [BlockDeviceMapping])
launchSpecification_blockDeviceMappings = Lens.lens (\LaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@LaunchSpecification' {} a -> s {blockDeviceMappings = a} :: LaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Deprecated.
launchSpecification_addressingType :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_addressingType = Lens.lens (\LaunchSpecification' {addressingType} -> addressingType) (\s@LaunchSpecification' {} a -> s {addressingType = a} :: LaunchSpecification)

-- | Undocumented member.
launchSpecification_monitoring :: Lens.Lens' LaunchSpecification (Prelude.Maybe RunInstancesMonitoringEnabled)
launchSpecification_monitoring = Lens.lens (\LaunchSpecification' {monitoring} -> monitoring) (\s@LaunchSpecification' {} a -> s {monitoring = a} :: LaunchSpecification)

-- | The ID of the subnet in which to launch the instance.
launchSpecification_subnetId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_subnetId = Lens.lens (\LaunchSpecification' {subnetId} -> subnetId) (\s@LaunchSpecification' {} a -> s {subnetId = a} :: LaunchSpecification)

-- | The instance type. Only one instance type can be specified.
launchSpecification_instanceType :: Lens.Lens' LaunchSpecification (Prelude.Maybe InstanceType)
launchSpecification_instanceType = Lens.lens (\LaunchSpecification' {instanceType} -> instanceType) (\s@LaunchSpecification' {} a -> s {instanceType = a} :: LaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
launchSpecification_securityGroups :: Lens.Lens' LaunchSpecification (Prelude.Maybe [GroupIdentifier])
launchSpecification_securityGroups = Lens.lens (\LaunchSpecification' {securityGroups} -> securityGroups) (\s@LaunchSpecification' {} a -> s {securityGroups = a} :: LaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the RAM disk.
launchSpecification_ramdiskId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_ramdiskId = Lens.lens (\LaunchSpecification' {ramdiskId} -> ramdiskId) (\s@LaunchSpecification' {} a -> s {ramdiskId = a} :: LaunchSpecification)

-- | The name of the key pair.
launchSpecification_keyName :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_keyName = Lens.lens (\LaunchSpecification' {keyName} -> keyName) (\s@LaunchSpecification' {} a -> s {keyName = a} :: LaunchSpecification)

-- | The ID of the kernel.
launchSpecification_kernelId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_kernelId = Lens.lens (\LaunchSpecification' {kernelId} -> kernelId) (\s@LaunchSpecification' {} a -> s {kernelId = a} :: LaunchSpecification)

-- | The ID of the AMI.
launchSpecification_imageId :: Lens.Lens' LaunchSpecification (Prelude.Maybe Prelude.Text)
launchSpecification_imageId = Lens.lens (\LaunchSpecification' {imageId} -> imageId) (\s@LaunchSpecification' {} a -> s {imageId = a} :: LaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
launchSpecification_networkInterfaces :: Lens.Lens' LaunchSpecification (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
launchSpecification_networkInterfaces = Lens.lens (\LaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@LaunchSpecification' {} a -> s {networkInterfaces = a} :: LaunchSpecification) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML LaunchSpecification where
  parseXML x =
    LaunchSpecification'
      Prelude.<$> (x Core..@? "ebsOptimized")
      Prelude.<*> (x Core..@? "iamInstanceProfile")
      Prelude.<*> (x Core..@? "placement")
      Prelude.<*> (x Core..@? "userData")
      Prelude.<*> ( x Core..@? "blockDeviceMapping"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "addressingType")
      Prelude.<*> (x Core..@? "monitoring")
      Prelude.<*> (x Core..@? "subnetId")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> ( x Core..@? "groupSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "ramdiskId")
      Prelude.<*> (x Core..@? "keyName")
      Prelude.<*> (x Core..@? "kernelId")
      Prelude.<*> (x Core..@? "imageId")
      Prelude.<*> ( x Core..@? "networkInterfaceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable LaunchSpecification where
  hashWithSalt _salt LaunchSpecification' {..} =
    _salt `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` addressingType
      `Prelude.hashWithSalt` monitoring
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` networkInterfaces

instance Prelude.NFData LaunchSpecification where
  rnf LaunchSpecification' {..} =
    Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf addressingType
      `Prelude.seq` Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf networkInterfaces
