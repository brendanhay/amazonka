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
-- Module      : Amazonka.EC2.Types.SpotFleetLaunchSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotFleetLaunchSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.IamInstanceProfileSpecification
import Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.InstanceRequirements
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.SpotFleetMonitoring
import Amazonka.EC2.Types.SpotFleetTagSpecification
import Amazonka.EC2.Types.SpotPlacement
import qualified Amazonka.Prelude as Prelude

-- | Describes the launch specification for one or more Spot Instances. If
-- you include On-Demand capacity in your fleet request or want to specify
-- an EFA network device, you can\'t use @SpotFleetLaunchSpecification@;
-- you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
--
-- /See:/ 'newSpotFleetLaunchSpecification' smart constructor.
data SpotFleetLaunchSpecification = SpotFleetLaunchSpecification'
  { -- | Indicates whether the instances are optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    -- | The placement information.
    placement :: Prelude.Maybe SpotPlacement,
    -- | The Base64-encoded user data that instances use when starting up.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The attributes for the instance types. When you specify instance
    -- attributes, Amazon EC2 will identify instance types with those
    -- attributes.
    --
    -- If you specify @InstanceRequirements@, you can\'t specify
    -- @InstanceType@.
    instanceRequirements :: Prelude.Maybe InstanceRequirements,
    -- | One or more block devices that are mapped to the Spot Instances. You
    -- can\'t specify both a snapshot ID and an encryption value. This is
    -- because only blank volumes can be encrypted on creation. If a snapshot
    -- is the basis for a volume, it is not blank and its encryption status is
    -- used for the volume encryption status.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | Deprecated.
    addressingType :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable monitoring for the instances.
    monitoring :: Prelude.Maybe SpotFleetMonitoring,
    -- | The IDs of the subnets in which to launch the instances. To specify
    -- multiple subnets, separate them using commas; for example,
    -- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Prelude.Maybe [GroupIdentifier],
    -- | The ID of the RAM disk. Some kernels require additional drivers at
    -- launch. Check the kernel requirements for information about whether you
    -- need to specify a RAM disk. To find kernel requirements, refer to the
    -- Amazon Web Services Resource Center and search for the kernel ID.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The number of units provided by the specified instance type. These are
    -- the same units that you chose to set the target capacity in terms of
    -- instances, or a performance characteristic such as vCPUs, memory, or
    -- I\/O.
    --
    -- If the target capacity divided by this value is not a whole number,
    -- Amazon EC2 rounds the number of instances to the next whole number. If
    -- this value is not specified, the default is 1.
    weightedCapacity :: Prelude.Maybe Prelude.Double,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply during creation.
    tagSpecifications :: Prelude.Maybe [SpotFleetTagSpecification],
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify this parameter.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    --
    -- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
    -- Adapter (EFA). To specify an EFA, you must use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotFleetLaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsOptimized', 'spotFleetLaunchSpecification_ebsOptimized' - Indicates whether the instances are optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- 'iamInstanceProfile', 'spotFleetLaunchSpecification_iamInstanceProfile' - The IAM instance profile.
--
-- 'placement', 'spotFleetLaunchSpecification_placement' - The placement information.
--
-- 'userData', 'spotFleetLaunchSpecification_userData' - The Base64-encoded user data that instances use when starting up.
--
-- 'instanceRequirements', 'spotFleetLaunchSpecification_instanceRequirements' - The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
--
-- 'blockDeviceMappings', 'spotFleetLaunchSpecification_blockDeviceMappings' - One or more block devices that are mapped to the Spot Instances. You
-- can\'t specify both a snapshot ID and an encryption value. This is
-- because only blank volumes can be encrypted on creation. If a snapshot
-- is the basis for a volume, it is not blank and its encryption status is
-- used for the volume encryption status.
--
-- 'addressingType', 'spotFleetLaunchSpecification_addressingType' - Deprecated.
--
-- 'monitoring', 'spotFleetLaunchSpecification_monitoring' - Enable or disable monitoring for the instances.
--
-- 'subnetId', 'spotFleetLaunchSpecification_subnetId' - The IDs of the subnets in which to launch the instances. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
--
-- 'instanceType', 'spotFleetLaunchSpecification_instanceType' - The instance type.
--
-- 'securityGroups', 'spotFleetLaunchSpecification_securityGroups' - One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
--
-- 'ramdiskId', 'spotFleetLaunchSpecification_ramdiskId' - The ID of the RAM disk. Some kernels require additional drivers at
-- launch. Check the kernel requirements for information about whether you
-- need to specify a RAM disk. To find kernel requirements, refer to the
-- Amazon Web Services Resource Center and search for the kernel ID.
--
-- 'weightedCapacity', 'spotFleetLaunchSpecification_weightedCapacity' - The number of units provided by the specified instance type. These are
-- the same units that you chose to set the target capacity in terms of
-- instances, or a performance characteristic such as vCPUs, memory, or
-- I\/O.
--
-- If the target capacity divided by this value is not a whole number,
-- Amazon EC2 rounds the number of instances to the next whole number. If
-- this value is not specified, the default is 1.
--
-- 'keyName', 'spotFleetLaunchSpecification_keyName' - The name of the key pair.
--
-- 'kernelId', 'spotFleetLaunchSpecification_kernelId' - The ID of the kernel.
--
-- 'tagSpecifications', 'spotFleetLaunchSpecification_tagSpecifications' - The tags to apply during creation.
--
-- 'spotPrice', 'spotFleetLaunchSpecification_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
--
-- 'imageId', 'spotFleetLaunchSpecification_imageId' - The ID of the AMI.
--
-- 'networkInterfaces', 'spotFleetLaunchSpecification_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
--
-- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
-- Adapter (EFA). To specify an EFA, you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
newSpotFleetLaunchSpecification ::
  SpotFleetLaunchSpecification
newSpotFleetLaunchSpecification =
  SpotFleetLaunchSpecification'
    { ebsOptimized =
        Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      placement = Prelude.Nothing,
      userData = Prelude.Nothing,
      instanceRequirements = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      addressingType = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing,
      keyName = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      imageId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | Indicates whether the instances are optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
spotFleetLaunchSpecification_ebsOptimized :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Bool)
spotFleetLaunchSpecification_ebsOptimized = Lens.lens (\SpotFleetLaunchSpecification' {ebsOptimized} -> ebsOptimized) (\s@SpotFleetLaunchSpecification' {} a -> s {ebsOptimized = a} :: SpotFleetLaunchSpecification)

-- | The IAM instance profile.
spotFleetLaunchSpecification_iamInstanceProfile :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe IamInstanceProfileSpecification)
spotFleetLaunchSpecification_iamInstanceProfile = Lens.lens (\SpotFleetLaunchSpecification' {iamInstanceProfile} -> iamInstanceProfile) (\s@SpotFleetLaunchSpecification' {} a -> s {iamInstanceProfile = a} :: SpotFleetLaunchSpecification)

-- | The placement information.
spotFleetLaunchSpecification_placement :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe SpotPlacement)
spotFleetLaunchSpecification_placement = Lens.lens (\SpotFleetLaunchSpecification' {placement} -> placement) (\s@SpotFleetLaunchSpecification' {} a -> s {placement = a} :: SpotFleetLaunchSpecification)

-- | The Base64-encoded user data that instances use when starting up.
spotFleetLaunchSpecification_userData :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_userData = Lens.lens (\SpotFleetLaunchSpecification' {userData} -> userData) (\s@SpotFleetLaunchSpecification' {} a -> s {userData = a} :: SpotFleetLaunchSpecification)

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
spotFleetLaunchSpecification_instanceRequirements :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe InstanceRequirements)
spotFleetLaunchSpecification_instanceRequirements = Lens.lens (\SpotFleetLaunchSpecification' {instanceRequirements} -> instanceRequirements) (\s@SpotFleetLaunchSpecification' {} a -> s {instanceRequirements = a} :: SpotFleetLaunchSpecification)

-- | One or more block devices that are mapped to the Spot Instances. You
-- can\'t specify both a snapshot ID and an encryption value. This is
-- because only blank volumes can be encrypted on creation. If a snapshot
-- is the basis for a volume, it is not blank and its encryption status is
-- used for the volume encryption status.
spotFleetLaunchSpecification_blockDeviceMappings :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [BlockDeviceMapping])
spotFleetLaunchSpecification_blockDeviceMappings = Lens.lens (\SpotFleetLaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@SpotFleetLaunchSpecification' {} a -> s {blockDeviceMappings = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Deprecated.
spotFleetLaunchSpecification_addressingType :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_addressingType = Lens.lens (\SpotFleetLaunchSpecification' {addressingType} -> addressingType) (\s@SpotFleetLaunchSpecification' {} a -> s {addressingType = a} :: SpotFleetLaunchSpecification)

-- | Enable or disable monitoring for the instances.
spotFleetLaunchSpecification_monitoring :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe SpotFleetMonitoring)
spotFleetLaunchSpecification_monitoring = Lens.lens (\SpotFleetLaunchSpecification' {monitoring} -> monitoring) (\s@SpotFleetLaunchSpecification' {} a -> s {monitoring = a} :: SpotFleetLaunchSpecification)

-- | The IDs of the subnets in which to launch the instances. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
spotFleetLaunchSpecification_subnetId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_subnetId = Lens.lens (\SpotFleetLaunchSpecification' {subnetId} -> subnetId) (\s@SpotFleetLaunchSpecification' {} a -> s {subnetId = a} :: SpotFleetLaunchSpecification)

-- | The instance type.
spotFleetLaunchSpecification_instanceType :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe InstanceType)
spotFleetLaunchSpecification_instanceType = Lens.lens (\SpotFleetLaunchSpecification' {instanceType} -> instanceType) (\s@SpotFleetLaunchSpecification' {} a -> s {instanceType = a} :: SpotFleetLaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
spotFleetLaunchSpecification_securityGroups :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [GroupIdentifier])
spotFleetLaunchSpecification_securityGroups = Lens.lens (\SpotFleetLaunchSpecification' {securityGroups} -> securityGroups) (\s@SpotFleetLaunchSpecification' {} a -> s {securityGroups = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the RAM disk. Some kernels require additional drivers at
-- launch. Check the kernel requirements for information about whether you
-- need to specify a RAM disk. To find kernel requirements, refer to the
-- Amazon Web Services Resource Center and search for the kernel ID.
spotFleetLaunchSpecification_ramdiskId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_ramdiskId = Lens.lens (\SpotFleetLaunchSpecification' {ramdiskId} -> ramdiskId) (\s@SpotFleetLaunchSpecification' {} a -> s {ramdiskId = a} :: SpotFleetLaunchSpecification)

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

-- | The name of the key pair.
spotFleetLaunchSpecification_keyName :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_keyName = Lens.lens (\SpotFleetLaunchSpecification' {keyName} -> keyName) (\s@SpotFleetLaunchSpecification' {} a -> s {keyName = a} :: SpotFleetLaunchSpecification)

-- | The ID of the kernel.
spotFleetLaunchSpecification_kernelId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_kernelId = Lens.lens (\SpotFleetLaunchSpecification' {kernelId} -> kernelId) (\s@SpotFleetLaunchSpecification' {} a -> s {kernelId = a} :: SpotFleetLaunchSpecification)

-- | The tags to apply during creation.
spotFleetLaunchSpecification_tagSpecifications :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [SpotFleetTagSpecification])
spotFleetLaunchSpecification_tagSpecifications = Lens.lens (\SpotFleetLaunchSpecification' {tagSpecifications} -> tagSpecifications) (\s@SpotFleetLaunchSpecification' {} a -> s {tagSpecifications = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
spotFleetLaunchSpecification_spotPrice :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_spotPrice = Lens.lens (\SpotFleetLaunchSpecification' {spotPrice} -> spotPrice) (\s@SpotFleetLaunchSpecification' {} a -> s {spotPrice = a} :: SpotFleetLaunchSpecification)

-- | The ID of the AMI.
spotFleetLaunchSpecification_imageId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_imageId = Lens.lens (\SpotFleetLaunchSpecification' {imageId} -> imageId) (\s@SpotFleetLaunchSpecification' {} a -> s {imageId = a} :: SpotFleetLaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
--
-- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
-- Adapter (EFA). To specify an EFA, you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
spotFleetLaunchSpecification_networkInterfaces :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
spotFleetLaunchSpecification_networkInterfaces = Lens.lens (\SpotFleetLaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@SpotFleetLaunchSpecification' {} a -> s {networkInterfaces = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML SpotFleetLaunchSpecification where
  parseXML x =
    SpotFleetLaunchSpecification'
      Prelude.<$> (x Core..@? "ebsOptimized")
      Prelude.<*> (x Core..@? "iamInstanceProfile")
      Prelude.<*> (x Core..@? "placement")
      Prelude.<*> (x Core..@? "userData")
      Prelude.<*> (x Core..@? "instanceRequirements")
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
      Prelude.<*> (x Core..@? "weightedCapacity")
      Prelude.<*> (x Core..@? "keyName")
      Prelude.<*> (x Core..@? "kernelId")
      Prelude.<*> ( x Core..@? "tagSpecificationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "spotPrice")
      Prelude.<*> (x Core..@? "imageId")
      Prelude.<*> ( x Core..@? "networkInterfaceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    SpotFleetLaunchSpecification
  where
  hashWithSalt _salt SpotFleetLaunchSpecification' {..} =
    _salt `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` instanceRequirements
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` addressingType
      `Prelude.hashWithSalt` monitoring
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` weightedCapacity
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` spotPrice
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` networkInterfaces

instance Prelude.NFData SpotFleetLaunchSpecification where
  rnf SpotFleetLaunchSpecification' {..} =
    Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf instanceRequirements
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf addressingType
      `Prelude.seq` Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf weightedCapacity
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf spotPrice
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf networkInterfaces

instance Core.ToQuery SpotFleetLaunchSpecification where
  toQuery SpotFleetLaunchSpecification' {..} =
    Prelude.mconcat
      [ "EbsOptimized" Core.=: ebsOptimized,
        "IamInstanceProfile" Core.=: iamInstanceProfile,
        "Placement" Core.=: placement,
        "UserData" Core.=: userData,
        "InstanceRequirements" Core.=: instanceRequirements,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "AddressingType" Core.=: addressingType,
        "Monitoring" Core.=: monitoring,
        "SubnetId" Core.=: subnetId,
        "InstanceType" Core.=: instanceType,
        Core.toQuery
          ( Core.toQueryList "GroupSet"
              Prelude.<$> securityGroups
          ),
        "RamdiskId" Core.=: ramdiskId,
        "WeightedCapacity" Core.=: weightedCapacity,
        "KeyName" Core.=: keyName,
        "KernelId" Core.=: kernelId,
        Core.toQuery
          ( Core.toQueryList "TagSpecificationSet"
              Prelude.<$> tagSpecifications
          ),
        "SpotPrice" Core.=: spotPrice,
        "ImageId" Core.=: imageId,
        Core.toQuery
          ( Core.toQueryList "NetworkInterfaceSet"
              Prelude.<$> networkInterfaces
          )
      ]
