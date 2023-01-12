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
-- Module      : Amazonka.EFS.Types.MountTargetDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.MountTargetDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types.LifeCycleState
import qualified Amazonka.Prelude as Prelude

-- | Provides a description of a mount target.
--
-- /See:/ 'newMountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { -- | The unique and consistent identifier of the Availability Zone that the
    -- mount target resides in. For example, @use1-az1@ is an AZ ID for the
    -- us-east-1 Region and it has the same location in every Amazon Web
    -- Services account.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Availability Zone in which the mount target is located.
    -- Availability Zones are independently mapped to names for each Amazon Web
    -- Services account. For example, the Availability Zone @us-east-1a@ for
    -- your Amazon Web Services account might not be the same location as
    -- @us-east-1a@ for another Amazon Web Services account.
    availabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | Address at which the file system can be mounted by using the mount
    -- target.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface that Amazon EFS created when it created
    -- the mount target.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Web Services account ID that owns the resource.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The virtual private cloud (VPC) ID that the mount target is configured
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MountTargetDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneId', 'mountTargetDescription_availabilityZoneId' - The unique and consistent identifier of the Availability Zone that the
-- mount target resides in. For example, @use1-az1@ is an AZ ID for the
-- us-east-1 Region and it has the same location in every Amazon Web
-- Services account.
--
-- 'availabilityZoneName', 'mountTargetDescription_availabilityZoneName' - The name of the Availability Zone in which the mount target is located.
-- Availability Zones are independently mapped to names for each Amazon Web
-- Services account. For example, the Availability Zone @us-east-1a@ for
-- your Amazon Web Services account might not be the same location as
-- @us-east-1a@ for another Amazon Web Services account.
--
-- 'ipAddress', 'mountTargetDescription_ipAddress' - Address at which the file system can be mounted by using the mount
-- target.
--
-- 'networkInterfaceId', 'mountTargetDescription_networkInterfaceId' - The ID of the network interface that Amazon EFS created when it created
-- the mount target.
--
-- 'ownerId', 'mountTargetDescription_ownerId' - Amazon Web Services account ID that owns the resource.
--
-- 'vpcId', 'mountTargetDescription_vpcId' - The virtual private cloud (VPC) ID that the mount target is configured
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
      { availabilityZoneId =
          Prelude.Nothing,
        availabilityZoneName = Prelude.Nothing,
        ipAddress = Prelude.Nothing,
        networkInterfaceId = Prelude.Nothing,
        ownerId = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        mountTargetId = pMountTargetId_,
        fileSystemId = pFileSystemId_,
        subnetId = pSubnetId_,
        lifeCycleState = pLifeCycleState_
      }

-- | The unique and consistent identifier of the Availability Zone that the
-- mount target resides in. For example, @use1-az1@ is an AZ ID for the
-- us-east-1 Region and it has the same location in every Amazon Web
-- Services account.
mountTargetDescription_availabilityZoneId :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_availabilityZoneId = Lens.lens (\MountTargetDescription' {availabilityZoneId} -> availabilityZoneId) (\s@MountTargetDescription' {} a -> s {availabilityZoneId = a} :: MountTargetDescription)

-- | The name of the Availability Zone in which the mount target is located.
-- Availability Zones are independently mapped to names for each Amazon Web
-- Services account. For example, the Availability Zone @us-east-1a@ for
-- your Amazon Web Services account might not be the same location as
-- @us-east-1a@ for another Amazon Web Services account.
mountTargetDescription_availabilityZoneName :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_availabilityZoneName = Lens.lens (\MountTargetDescription' {availabilityZoneName} -> availabilityZoneName) (\s@MountTargetDescription' {} a -> s {availabilityZoneName = a} :: MountTargetDescription)

-- | Address at which the file system can be mounted by using the mount
-- target.
mountTargetDescription_ipAddress :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_ipAddress = Lens.lens (\MountTargetDescription' {ipAddress} -> ipAddress) (\s@MountTargetDescription' {} a -> s {ipAddress = a} :: MountTargetDescription)

-- | The ID of the network interface that Amazon EFS created when it created
-- the mount target.
mountTargetDescription_networkInterfaceId :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_networkInterfaceId = Lens.lens (\MountTargetDescription' {networkInterfaceId} -> networkInterfaceId) (\s@MountTargetDescription' {} a -> s {networkInterfaceId = a} :: MountTargetDescription)

-- | Amazon Web Services account ID that owns the resource.
mountTargetDescription_ownerId :: Lens.Lens' MountTargetDescription (Prelude.Maybe Prelude.Text)
mountTargetDescription_ownerId = Lens.lens (\MountTargetDescription' {ownerId} -> ownerId) (\s@MountTargetDescription' {} a -> s {ownerId = a} :: MountTargetDescription)

-- | The virtual private cloud (VPC) ID that the mount target is configured
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

instance Data.FromJSON MountTargetDescription where
  parseJSON =
    Data.withObject
      "MountTargetDescription"
      ( \x ->
          MountTargetDescription'
            Prelude.<$> (x Data..:? "AvailabilityZoneId")
            Prelude.<*> (x Data..:? "AvailabilityZoneName")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "NetworkInterfaceId")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..: "MountTargetId")
            Prelude.<*> (x Data..: "FileSystemId")
            Prelude.<*> (x Data..: "SubnetId")
            Prelude.<*> (x Data..: "LifeCycleState")
      )

instance Prelude.Hashable MountTargetDescription where
  hashWithSalt _salt MountTargetDescription' {..} =
    _salt `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` availabilityZoneName
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` mountTargetId
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` lifeCycleState

instance Prelude.NFData MountTargetDescription where
  rnf MountTargetDescription' {..} =
    Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf availabilityZoneName
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf mountTargetId
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf lifeCycleState
