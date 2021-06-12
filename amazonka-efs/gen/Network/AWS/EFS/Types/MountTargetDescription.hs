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
-- Module      : Network.AWS.EFS.Types.MountTargetDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.MountTargetDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types.LifeCycleState
import qualified Network.AWS.Lens as Lens

-- | Provides a description of a mount target.
--
-- /See:/ 'newMountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { -- | AWS account ID that owns the resource.
    ownerId :: Core.Maybe Core.Text,
    -- | The name of the Availability Zone (AZ) that the mount target resides in.
    -- AZs are independently mapped to names for each AWS account. For example,
    -- the Availability Zone @us-east-1a@ for your AWS account might not be the
    -- same location as @us-east-1a@ for another AWS account.
    availabilityZoneName :: Core.Maybe Core.Text,
    -- | The unique and consistent identifier of the Availability Zone (AZ) that
    -- the mount target resides in. For example, @use1-az1@ is an AZ ID for the
    -- us-east-1 Region and it has the same location in every AWS account.
    availabilityZoneId :: Core.Maybe Core.Text,
    -- | Address at which the file system can be mounted by using the mount
    -- target.
    ipAddress :: Core.Maybe Core.Text,
    -- | The ID of the network interface that Amazon EFS created when it created
    -- the mount target.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The Virtual Private Cloud (VPC) ID that the mount target is configured
    -- in.
    vpcId :: Core.Maybe Core.Text,
    -- | System-assigned mount target ID.
    mountTargetId :: Core.Text,
    -- | The ID of the file system for which the mount target is intended.
    fileSystemId :: Core.Text,
    -- | The ID of the mount target\'s subnet.
    subnetId :: Core.Text,
    -- | Lifecycle state of the mount target.
    lifeCycleState :: LifeCycleState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MountTargetDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'mountTargetDescription_ownerId' - AWS account ID that owns the resource.
--
-- 'availabilityZoneName', 'mountTargetDescription_availabilityZoneName' - The name of the Availability Zone (AZ) that the mount target resides in.
-- AZs are independently mapped to names for each AWS account. For example,
-- the Availability Zone @us-east-1a@ for your AWS account might not be the
-- same location as @us-east-1a@ for another AWS account.
--
-- 'availabilityZoneId', 'mountTargetDescription_availabilityZoneId' - The unique and consistent identifier of the Availability Zone (AZ) that
-- the mount target resides in. For example, @use1-az1@ is an AZ ID for the
-- us-east-1 Region and it has the same location in every AWS account.
--
-- 'ipAddress', 'mountTargetDescription_ipAddress' - Address at which the file system can be mounted by using the mount
-- target.
--
-- 'networkInterfaceId', 'mountTargetDescription_networkInterfaceId' - The ID of the network interface that Amazon EFS created when it created
-- the mount target.
--
-- 'vpcId', 'mountTargetDescription_vpcId' - The Virtual Private Cloud (VPC) ID that the mount target is configured
-- in.
--
-- 'mountTargetId', 'mountTargetDescription_mountTargetId' - System-assigned mount target ID.
--
-- 'fileSystemId', 'mountTargetDescription_fileSystemId' - The ID of the file system for which the mount target is intended.
--
-- 'subnetId', 'mountTargetDescription_subnetId' - The ID of the mount target\'s subnet.
--
-- 'lifeCycleState', 'mountTargetDescription_lifeCycleState' - Lifecycle state of the mount target.
newMountTargetDescription ::
  -- | 'mountTargetId'
  Core.Text ->
  -- | 'fileSystemId'
  Core.Text ->
  -- | 'subnetId'
  Core.Text ->
  -- | 'lifeCycleState'
  LifeCycleState ->
  MountTargetDescription
newMountTargetDescription
  pMountTargetId_
  pFileSystemId_
  pSubnetId_
  pLifeCycleState_ =
    MountTargetDescription'
      { ownerId = Core.Nothing,
        availabilityZoneName = Core.Nothing,
        availabilityZoneId = Core.Nothing,
        ipAddress = Core.Nothing,
        networkInterfaceId = Core.Nothing,
        vpcId = Core.Nothing,
        mountTargetId = pMountTargetId_,
        fileSystemId = pFileSystemId_,
        subnetId = pSubnetId_,
        lifeCycleState = pLifeCycleState_
      }

-- | AWS account ID that owns the resource.
mountTargetDescription_ownerId :: Lens.Lens' MountTargetDescription (Core.Maybe Core.Text)
mountTargetDescription_ownerId = Lens.lens (\MountTargetDescription' {ownerId} -> ownerId) (\s@MountTargetDescription' {} a -> s {ownerId = a} :: MountTargetDescription)

-- | The name of the Availability Zone (AZ) that the mount target resides in.
-- AZs are independently mapped to names for each AWS account. For example,
-- the Availability Zone @us-east-1a@ for your AWS account might not be the
-- same location as @us-east-1a@ for another AWS account.
mountTargetDescription_availabilityZoneName :: Lens.Lens' MountTargetDescription (Core.Maybe Core.Text)
mountTargetDescription_availabilityZoneName = Lens.lens (\MountTargetDescription' {availabilityZoneName} -> availabilityZoneName) (\s@MountTargetDescription' {} a -> s {availabilityZoneName = a} :: MountTargetDescription)

-- | The unique and consistent identifier of the Availability Zone (AZ) that
-- the mount target resides in. For example, @use1-az1@ is an AZ ID for the
-- us-east-1 Region and it has the same location in every AWS account.
mountTargetDescription_availabilityZoneId :: Lens.Lens' MountTargetDescription (Core.Maybe Core.Text)
mountTargetDescription_availabilityZoneId = Lens.lens (\MountTargetDescription' {availabilityZoneId} -> availabilityZoneId) (\s@MountTargetDescription' {} a -> s {availabilityZoneId = a} :: MountTargetDescription)

-- | Address at which the file system can be mounted by using the mount
-- target.
mountTargetDescription_ipAddress :: Lens.Lens' MountTargetDescription (Core.Maybe Core.Text)
mountTargetDescription_ipAddress = Lens.lens (\MountTargetDescription' {ipAddress} -> ipAddress) (\s@MountTargetDescription' {} a -> s {ipAddress = a} :: MountTargetDescription)

-- | The ID of the network interface that Amazon EFS created when it created
-- the mount target.
mountTargetDescription_networkInterfaceId :: Lens.Lens' MountTargetDescription (Core.Maybe Core.Text)
mountTargetDescription_networkInterfaceId = Lens.lens (\MountTargetDescription' {networkInterfaceId} -> networkInterfaceId) (\s@MountTargetDescription' {} a -> s {networkInterfaceId = a} :: MountTargetDescription)

-- | The Virtual Private Cloud (VPC) ID that the mount target is configured
-- in.
mountTargetDescription_vpcId :: Lens.Lens' MountTargetDescription (Core.Maybe Core.Text)
mountTargetDescription_vpcId = Lens.lens (\MountTargetDescription' {vpcId} -> vpcId) (\s@MountTargetDescription' {} a -> s {vpcId = a} :: MountTargetDescription)

-- | System-assigned mount target ID.
mountTargetDescription_mountTargetId :: Lens.Lens' MountTargetDescription Core.Text
mountTargetDescription_mountTargetId = Lens.lens (\MountTargetDescription' {mountTargetId} -> mountTargetId) (\s@MountTargetDescription' {} a -> s {mountTargetId = a} :: MountTargetDescription)

-- | The ID of the file system for which the mount target is intended.
mountTargetDescription_fileSystemId :: Lens.Lens' MountTargetDescription Core.Text
mountTargetDescription_fileSystemId = Lens.lens (\MountTargetDescription' {fileSystemId} -> fileSystemId) (\s@MountTargetDescription' {} a -> s {fileSystemId = a} :: MountTargetDescription)

-- | The ID of the mount target\'s subnet.
mountTargetDescription_subnetId :: Lens.Lens' MountTargetDescription Core.Text
mountTargetDescription_subnetId = Lens.lens (\MountTargetDescription' {subnetId} -> subnetId) (\s@MountTargetDescription' {} a -> s {subnetId = a} :: MountTargetDescription)

-- | Lifecycle state of the mount target.
mountTargetDescription_lifeCycleState :: Lens.Lens' MountTargetDescription LifeCycleState
mountTargetDescription_lifeCycleState = Lens.lens (\MountTargetDescription' {lifeCycleState} -> lifeCycleState) (\s@MountTargetDescription' {} a -> s {lifeCycleState = a} :: MountTargetDescription)

instance Core.FromJSON MountTargetDescription where
  parseJSON =
    Core.withObject
      "MountTargetDescription"
      ( \x ->
          MountTargetDescription'
            Core.<$> (x Core..:? "OwnerId")
            Core.<*> (x Core..:? "AvailabilityZoneName")
            Core.<*> (x Core..:? "AvailabilityZoneId")
            Core.<*> (x Core..:? "IpAddress")
            Core.<*> (x Core..:? "NetworkInterfaceId")
            Core.<*> (x Core..:? "VpcId")
            Core.<*> (x Core..: "MountTargetId")
            Core.<*> (x Core..: "FileSystemId")
            Core.<*> (x Core..: "SubnetId")
            Core.<*> (x Core..: "LifeCycleState")
      )

instance Core.Hashable MountTargetDescription

instance Core.NFData MountTargetDescription
