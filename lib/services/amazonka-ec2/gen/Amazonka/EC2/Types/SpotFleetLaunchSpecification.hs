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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotFleetLaunchSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Deprecated.
    addressingType :: Prelude.Maybe Prelude.Text,
    -- | One or more block devices that are mapped to the Spot Instances. You
    -- can\'t specify both a snapshot ID and an encryption value. This is
    -- because only blank volumes can be encrypted on creation. If a snapshot
    -- is the basis for a volume, it is not blank and its encryption status is
    -- used for the volume encryption status.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | Indicates whether the instances are optimized for EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The attributes for the instance types. When you specify instance
    -- attributes, Amazon EC2 will identify instance types with those
    -- attributes.
    --
    -- If you specify @InstanceRequirements@, you can\'t specify
    -- @InstanceType@.
    instanceRequirements :: Prelude.Maybe InstanceRequirements,
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable monitoring for the instances.
    monitoring :: Prelude.Maybe SpotFleetMonitoring,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify subnet IDs and security group IDs using the network
    -- interface.
    --
    -- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
    -- Adapter (EFA). To specify an EFA, you must use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification],
    -- | The placement information.
    placement :: Prelude.Maybe SpotPlacement,
    -- | The ID of the RAM disk. Some kernels require additional drivers at
    -- launch. Check the kernel requirements for information about whether you
    -- need to specify a RAM disk. To find kernel requirements, refer to the
    -- Amazon Web Services Resource Center and search for the kernel ID.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | One or more security groups. When requesting instances in a VPC, you
    -- must specify the IDs of the security groups. When requesting instances
    -- in EC2-Classic, you can specify the names or the IDs of the security
    -- groups.
    securityGroups :: Prelude.Maybe [GroupIdentifier],
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify this parameter.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the subnets in which to launch the instances. To specify
    -- multiple subnets, separate them using commas; for example,
    -- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply during creation.
    tagSpecifications :: Prelude.Maybe [SpotFleetTagSpecification],
    -- | The Base64-encoded user data that instances use when starting up.
    userData :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotFleetLaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressingType', 'spotFleetLaunchSpecification_addressingType' - Deprecated.
--
-- 'blockDeviceMappings', 'spotFleetLaunchSpecification_blockDeviceMappings' - One or more block devices that are mapped to the Spot Instances. You
-- can\'t specify both a snapshot ID and an encryption value. This is
-- because only blank volumes can be encrypted on creation. If a snapshot
-- is the basis for a volume, it is not blank and its encryption status is
-- used for the volume encryption status.
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
-- 'imageId', 'spotFleetLaunchSpecification_imageId' - The ID of the AMI.
--
-- 'instanceRequirements', 'spotFleetLaunchSpecification_instanceRequirements' - The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
--
-- 'instanceType', 'spotFleetLaunchSpecification_instanceType' - The instance type.
--
-- 'kernelId', 'spotFleetLaunchSpecification_kernelId' - The ID of the kernel.
--
-- 'keyName', 'spotFleetLaunchSpecification_keyName' - The name of the key pair.
--
-- 'monitoring', 'spotFleetLaunchSpecification_monitoring' - Enable or disable monitoring for the instances.
--
-- 'networkInterfaces', 'spotFleetLaunchSpecification_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
--
-- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
-- Adapter (EFA). To specify an EFA, you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
--
-- 'placement', 'spotFleetLaunchSpecification_placement' - The placement information.
--
-- 'ramdiskId', 'spotFleetLaunchSpecification_ramdiskId' - The ID of the RAM disk. Some kernels require additional drivers at
-- launch. Check the kernel requirements for information about whether you
-- need to specify a RAM disk. To find kernel requirements, refer to the
-- Amazon Web Services Resource Center and search for the kernel ID.
--
-- 'securityGroups', 'spotFleetLaunchSpecification_securityGroups' - One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
--
-- 'spotPrice', 'spotFleetLaunchSpecification_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
--
-- 'subnetId', 'spotFleetLaunchSpecification_subnetId' - The IDs of the subnets in which to launch the instances. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
--
-- 'tagSpecifications', 'spotFleetLaunchSpecification_tagSpecifications' - The tags to apply during creation.
--
-- 'userData', 'spotFleetLaunchSpecification_userData' - The Base64-encoded user data that instances use when starting up.
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
    { addressingType =
        Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      imageId = Prelude.Nothing,
      instanceRequirements = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      placement = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      userData = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

-- | Deprecated.
spotFleetLaunchSpecification_addressingType :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_addressingType = Lens.lens (\SpotFleetLaunchSpecification' {addressingType} -> addressingType) (\s@SpotFleetLaunchSpecification' {} a -> s {addressingType = a} :: SpotFleetLaunchSpecification)

-- | One or more block devices that are mapped to the Spot Instances. You
-- can\'t specify both a snapshot ID and an encryption value. This is
-- because only blank volumes can be encrypted on creation. If a snapshot
-- is the basis for a volume, it is not blank and its encryption status is
-- used for the volume encryption status.
spotFleetLaunchSpecification_blockDeviceMappings :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [BlockDeviceMapping])
spotFleetLaunchSpecification_blockDeviceMappings = Lens.lens (\SpotFleetLaunchSpecification' {blockDeviceMappings} -> blockDeviceMappings) (\s@SpotFleetLaunchSpecification' {} a -> s {blockDeviceMappings = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

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

-- | The ID of the AMI.
spotFleetLaunchSpecification_imageId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_imageId = Lens.lens (\SpotFleetLaunchSpecification' {imageId} -> imageId) (\s@SpotFleetLaunchSpecification' {} a -> s {imageId = a} :: SpotFleetLaunchSpecification)

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
spotFleetLaunchSpecification_instanceRequirements :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe InstanceRequirements)
spotFleetLaunchSpecification_instanceRequirements = Lens.lens (\SpotFleetLaunchSpecification' {instanceRequirements} -> instanceRequirements) (\s@SpotFleetLaunchSpecification' {} a -> s {instanceRequirements = a} :: SpotFleetLaunchSpecification)

-- | The instance type.
spotFleetLaunchSpecification_instanceType :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe InstanceType)
spotFleetLaunchSpecification_instanceType = Lens.lens (\SpotFleetLaunchSpecification' {instanceType} -> instanceType) (\s@SpotFleetLaunchSpecification' {} a -> s {instanceType = a} :: SpotFleetLaunchSpecification)

-- | The ID of the kernel.
spotFleetLaunchSpecification_kernelId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_kernelId = Lens.lens (\SpotFleetLaunchSpecification' {kernelId} -> kernelId) (\s@SpotFleetLaunchSpecification' {} a -> s {kernelId = a} :: SpotFleetLaunchSpecification)

-- | The name of the key pair.
spotFleetLaunchSpecification_keyName :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_keyName = Lens.lens (\SpotFleetLaunchSpecification' {keyName} -> keyName) (\s@SpotFleetLaunchSpecification' {} a -> s {keyName = a} :: SpotFleetLaunchSpecification)

-- | Enable or disable monitoring for the instances.
spotFleetLaunchSpecification_monitoring :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe SpotFleetMonitoring)
spotFleetLaunchSpecification_monitoring = Lens.lens (\SpotFleetLaunchSpecification' {monitoring} -> monitoring) (\s@SpotFleetLaunchSpecification' {} a -> s {monitoring = a} :: SpotFleetLaunchSpecification)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify subnet IDs and security group IDs using the network
-- interface.
--
-- @SpotFleetLaunchSpecification@ currently does not support Elastic Fabric
-- Adapter (EFA). To specify an EFA, you must use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig>.
spotFleetLaunchSpecification_networkInterfaces :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
spotFleetLaunchSpecification_networkInterfaces = Lens.lens (\SpotFleetLaunchSpecification' {networkInterfaces} -> networkInterfaces) (\s@SpotFleetLaunchSpecification' {} a -> s {networkInterfaces = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The placement information.
spotFleetLaunchSpecification_placement :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe SpotPlacement)
spotFleetLaunchSpecification_placement = Lens.lens (\SpotFleetLaunchSpecification' {placement} -> placement) (\s@SpotFleetLaunchSpecification' {} a -> s {placement = a} :: SpotFleetLaunchSpecification)

-- | The ID of the RAM disk. Some kernels require additional drivers at
-- launch. Check the kernel requirements for information about whether you
-- need to specify a RAM disk. To find kernel requirements, refer to the
-- Amazon Web Services Resource Center and search for the kernel ID.
spotFleetLaunchSpecification_ramdiskId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_ramdiskId = Lens.lens (\SpotFleetLaunchSpecification' {ramdiskId} -> ramdiskId) (\s@SpotFleetLaunchSpecification' {} a -> s {ramdiskId = a} :: SpotFleetLaunchSpecification)

-- | One or more security groups. When requesting instances in a VPC, you
-- must specify the IDs of the security groups. When requesting instances
-- in EC2-Classic, you can specify the names or the IDs of the security
-- groups.
spotFleetLaunchSpecification_securityGroups :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [GroupIdentifier])
spotFleetLaunchSpecification_securityGroups = Lens.lens (\SpotFleetLaunchSpecification' {securityGroups} -> securityGroups) (\s@SpotFleetLaunchSpecification' {} a -> s {securityGroups = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
spotFleetLaunchSpecification_spotPrice :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_spotPrice = Lens.lens (\SpotFleetLaunchSpecification' {spotPrice} -> spotPrice) (\s@SpotFleetLaunchSpecification' {} a -> s {spotPrice = a} :: SpotFleetLaunchSpecification)

-- | The IDs of the subnets in which to launch the instances. To specify
-- multiple subnets, separate them using commas; for example,
-- \"subnet-1234abcdeexample1, subnet-0987cdef6example2\".
spotFleetLaunchSpecification_subnetId :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_subnetId = Lens.lens (\SpotFleetLaunchSpecification' {subnetId} -> subnetId) (\s@SpotFleetLaunchSpecification' {} a -> s {subnetId = a} :: SpotFleetLaunchSpecification)

-- | The tags to apply during creation.
spotFleetLaunchSpecification_tagSpecifications :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe [SpotFleetTagSpecification])
spotFleetLaunchSpecification_tagSpecifications = Lens.lens (\SpotFleetLaunchSpecification' {tagSpecifications} -> tagSpecifications) (\s@SpotFleetLaunchSpecification' {} a -> s {tagSpecifications = a} :: SpotFleetLaunchSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The Base64-encoded user data that instances use when starting up.
spotFleetLaunchSpecification_userData :: Lens.Lens' SpotFleetLaunchSpecification (Prelude.Maybe Prelude.Text)
spotFleetLaunchSpecification_userData = Lens.lens (\SpotFleetLaunchSpecification' {userData} -> userData) (\s@SpotFleetLaunchSpecification' {} a -> s {userData = a} :: SpotFleetLaunchSpecification)

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

instance Data.FromXML SpotFleetLaunchSpecification where
  parseXML x =
    SpotFleetLaunchSpecification'
      Prelude.<$> (x Data..@? "addressingType")
      Prelude.<*> ( x
                      Data..@? "blockDeviceMapping"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ebsOptimized")
      Prelude.<*> (x Data..@? "iamInstanceProfile")
      Prelude.<*> (x Data..@? "imageId")
      Prelude.<*> (x Data..@? "instanceRequirements")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "kernelId")
      Prelude.<*> (x Data..@? "keyName")
      Prelude.<*> (x Data..@? "monitoring")
      Prelude.<*> ( x
                      Data..@? "networkInterfaceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "placement")
      Prelude.<*> (x Data..@? "ramdiskId")
      Prelude.<*> ( x
                      Data..@? "groupSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "spotPrice")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> ( x
                      Data..@? "tagSpecificationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "userData")
      Prelude.<*> (x Data..@? "weightedCapacity")

instance
  Prelude.Hashable
    SpotFleetLaunchSpecification
  where
  hashWithSalt _salt SpotFleetLaunchSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` addressingType
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` instanceRequirements
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` monitoring
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` spotPrice
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` weightedCapacity

instance Prelude.NFData SpotFleetLaunchSpecification where
  rnf SpotFleetLaunchSpecification' {..} =
    Prelude.rnf addressingType
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf instanceRequirements
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf spotPrice
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf weightedCapacity

instance Data.ToQuery SpotFleetLaunchSpecification where
  toQuery SpotFleetLaunchSpecification' {..} =
    Prelude.mconcat
      [ "AddressingType" Data.=: addressingType,
        Data.toQuery
          ( Data.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "EbsOptimized" Data.=: ebsOptimized,
        "IamInstanceProfile" Data.=: iamInstanceProfile,
        "ImageId" Data.=: imageId,
        "InstanceRequirements" Data.=: instanceRequirements,
        "InstanceType" Data.=: instanceType,
        "KernelId" Data.=: kernelId,
        "KeyName" Data.=: keyName,
        "Monitoring" Data.=: monitoring,
        Data.toQuery
          ( Data.toQueryList "NetworkInterfaceSet"
              Prelude.<$> networkInterfaces
          ),
        "Placement" Data.=: placement,
        "RamdiskId" Data.=: ramdiskId,
        Data.toQuery
          ( Data.toQueryList "GroupSet"
              Prelude.<$> securityGroups
          ),
        "SpotPrice" Data.=: spotPrice,
        "SubnetId" Data.=: subnetId,
        Data.toQuery
          ( Data.toQueryList "TagSpecificationSet"
              Prelude.<$> tagSpecifications
          ),
        "UserData" Data.=: userData,
        "WeightedCapacity" Data.=: weightedCapacity
      ]
