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
-- Module      : Network.AWS.EFS.Types.MountTargetDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.MountTargetDescription where

import Network.AWS.EFS.Types.LifeCycleState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a description of a mount target.
--
-- /See:/ 'newMountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { -- | AWS account ID that owns the resource.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Availability Zone (AZ) that the mount target resides in.
    -- AZs are independently mapped to names for each AWS account. For example,
    -- the Availability Zone @us-east-1a@ for your AWS account might not be the
    -- same location as @us-east-1a@ for another AWS account.
    availabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | The unique and consistent identifier of the Availability Zone (AZ) that
    -- the mount target resides in. For example, @use1-az1@ is an AZ ID for the
    -- us-east-1 Region and it has the same location in every AWS account.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | Address at which the file system can be mounted by using the mount
    -- target.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface that Amazon EFS created when it created
    -- the mount target.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The Virtual Private Cloud (VPC) ID that the mount target is configured
    -- in.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | System-assigned mount target ID.
    mountTargetId :: Prelude.Text,
    -- | The ID of the file system for which the mount target is intended.
    fileSystemId :: Prelude.Text,
    -- | The ID of the mount target\'s subnet.
    subnetId :: Prelude.Text,
    -- | Lifecycle state of the mount target.
    lifeCycleState :: LifeCycleState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  -- | 'lifeCycleState'
  LifeCycleState ->
  MountTargetDescription
newMountTargetDescription
  pMountTargetId_
  pFileSystemId_
  pSubnetId_
  pLifeCycleState_ =
    MountTargetDescription'
      { ownerId = Prelude.Nothing,
        availabilityZoneName = Prelude.Nothing,
        availabilityZoneId = Prelude.Nothing,
        ipAddress = Prelude.Nothing,
        networkInterfaceId = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        mountTargetId = pMountTargetId_,
        fileSystemId = pFileSystemId_,
        subnetId = pSubnetId_,
        lifeCycleState = pLifeCycleState_
      }

-- | AWS account ID that owns the resource.
mountTargetDescription_ownerId :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_ownerId = Lens.lens (\MountTargetDescription' {ownerId} -> ownerId) (\s@MountTargetDescription' {} a -> s {ownerId = a} :: MountTargetDescription)

-- | The name of the Availability Zone (AZ) that the mount target resides in.
-- AZs are independently mapped to names for each AWS account. For example,
-- the Availability Zone @us-east-1a@ for your AWS account might not be the
-- same location as @us-east-1a@ for another AWS account.
mountTargetDescription_availabilityZoneName :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_availabilityZoneName = Lens.lens (\MountTargetDescription' {availabilityZoneName} -> availabilityZoneName) (\s@MountTargetDescription' {} a -> s {availabilityZoneName = a} :: MountTargetDescription)

-- | The unique and consistent identifier of the Availability Zone (AZ) that
-- the mount target resides in. For example, @use1-az1@ is an AZ ID for the
-- us-east-1 Region and it has the same location in every AWS account.
mountTargetDescription_availabilityZoneId :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_availabilityZoneId = Lens.lens (\MountTargetDescription' {availabilityZoneId} -> availabilityZoneId) (\s@MountTargetDescription' {} a -> s {availabilityZoneId = a} :: MountTargetDescription)

-- | Address at which the file system can be mounted by using the mount
-- target.
mountTargetDescription_ipAddress :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_ipAddress = Lens.lens (\MountTargetDescription' {ipAddress} -> ipAddress) (\s@MountTargetDescription' {} a -> s {ipAddress = a} :: MountTargetDescription)

-- | The ID of the network interface that Amazon EFS created when it created
-- the mount target.
mountTargetDescription_networkInterfaceId :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_networkInterfaceId = Lens.lens (\MountTargetDescription' {networkInterfaceId} -> networkInterfaceId) (\s@MountTargetDescription' {} a -> s {networkInterfaceId = a} :: MountTargetDescription)

-- | The Virtual Private Cloud (VPC) ID that the mount target is configured
-- in.
mountTargetDescription_vpcId :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_vpcId = Lens.lens (\MountTargetDescription' {vpcId} -> vpcId) (\s@MountTargetDescription' {} a -> s {vpcId = a} :: MountTargetDescription)

-- | System-assigned mount target ID.
mountTargetDescription_mountTargetId :: Lens.Lens' MountTargetDescription Prelude.Text
mountTargetDescription_mountTargetId = Lens.lens (\MountTargetDescription' {mountTargetId} -> mountTargetId) (\s@MountTargetDescription' {} a -> s {mountTargetId = a} :: MountTargetDescription)

-- | The ID of the file system for which the mount target is intended.
mountTargetDescription_fileSystemId :: Lens.Lens' MountTargetDescription Prelude.Text
mountTargetDescription_fileSystemId = Lens.lens (\MountTargetDescription' {fileSystemId} -> fileSystemId) (\s@MountTargetDescription' {} a -> s {fileSystemId = a} :: MountTargetDescription)

-- | The ID of the mount target\'s subnet.
mountTargetDescription_subnetId :: Lens.Lens' MountTargetDescription Prelude.Text
mountTargetDescription_subnetId = Lens.lens (\MountTargetDescription' {subnetId} -> subnetId) (\s@MountTargetDescription' {} a -> s {subnetId = a} :: MountTargetDescription)

-- | Lifecycle state of the mount target.
mountTargetDescription_lifeCycleState :: Lens.Lens' MountTargetDescription LifeCycleState
mountTargetDescription_lifeCycleState = Lens.lens (\MountTargetDescription' {lifeCycleState} -> lifeCycleState) (\s@MountTargetDescription' {} a -> s {lifeCycleState = a} :: MountTargetDescription)

instance Prelude.FromJSON MountTargetDescription where
  parseJSON =
    Prelude.withObject
      "MountTargetDescription"
      ( \x ->
          MountTargetDescription'
            Prelude.<$> (x Prelude..:? "OwnerId")
            Prelude.<*> (x Prelude..:? "AvailabilityZoneName")
            Prelude.<*> (x Prelude..:? "AvailabilityZoneId")
            Prelude.<*> (x Prelude..:? "IpAddress")
            Prelude.<*> (x Prelude..:? "NetworkInterfaceId")
            Prelude.<*> (x Prelude..:? "VpcId")
            Prelude.<*> (x Prelude..: "MountTargetId")
            Prelude.<*> (x Prelude..: "FileSystemId")
            Prelude.<*> (x Prelude..: "SubnetId")
            Prelude.<*> (x Prelude..: "LifeCycleState")
      )

instance Prelude.Hashable MountTargetDescription

instance Prelude.NFData MountTargetDescription
