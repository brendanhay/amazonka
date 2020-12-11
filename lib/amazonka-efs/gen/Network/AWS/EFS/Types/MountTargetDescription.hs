-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.MountTargetDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.MountTargetDescription
  ( MountTargetDescription (..),

    -- * Smart constructor
    mkMountTargetDescription,

    -- * Lenses
    mtdIPAddress,
    mtdAvailabilityZoneId,
    mtdVPCId,
    mtdAvailabilityZoneName,
    mtdNetworkInterfaceId,
    mtdOwnerId,
    mtdMountTargetId,
    mtdFileSystemId,
    mtdSubnetId,
    mtdLifeCycleState,
  )
where

import Network.AWS.EFS.Types.LifeCycleState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a description of a mount target.
--
-- /See:/ 'mkMountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { ipAddress ::
      Lude.Maybe Lude.Text,
    availabilityZoneId :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    availabilityZoneName :: Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    mountTargetId :: Lude.Text,
    fileSystemId :: Lude.Text,
    subnetId :: Lude.Text,
    lifeCycleState :: LifeCycleState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MountTargetDescription' with the minimum fields required to make a request.
--
-- * 'availabilityZoneId' - The unique and consistent identifier of the Availability Zone (AZ) that the mount target resides in. For example, @use1-az1@ is an AZ ID for the us-east-1 Region and it has the same location in every AWS account.
-- * 'availabilityZoneName' - The name of the Availability Zone (AZ) that the mount target resides in. AZs are independently mapped to names for each AWS account. For example, the Availability Zone @us-east-1a@ for your AWS account might not be the same location as @us-east-1a@ for another AWS account.
-- * 'fileSystemId' - The ID of the file system for which the mount target is intended.
-- * 'ipAddress' - Address at which the file system can be mounted by using the mount target.
-- * 'lifeCycleState' - Lifecycle state of the mount target.
-- * 'mountTargetId' - System-assigned mount target ID.
-- * 'networkInterfaceId' - The ID of the network interface that Amazon EFS created when it created the mount target.
-- * 'ownerId' - AWS account ID that owns the resource.
-- * 'subnetId' - The ID of the mount target's subnet.
-- * 'vpcId' - The Virtual Private Cloud (VPC) ID that the mount target is configured in.
mkMountTargetDescription ::
  -- | 'mountTargetId'
  Lude.Text ->
  -- | 'fileSystemId'
  Lude.Text ->
  -- | 'subnetId'
  Lude.Text ->
  -- | 'lifeCycleState'
  LifeCycleState ->
  MountTargetDescription
mkMountTargetDescription
  pMountTargetId_
  pFileSystemId_
  pSubnetId_
  pLifeCycleState_ =
    MountTargetDescription'
      { ipAddress = Lude.Nothing,
        availabilityZoneId = Lude.Nothing,
        vpcId = Lude.Nothing,
        availabilityZoneName = Lude.Nothing,
        networkInterfaceId = Lude.Nothing,
        ownerId = Lude.Nothing,
        mountTargetId = pMountTargetId_,
        fileSystemId = pFileSystemId_,
        subnetId = pSubnetId_,
        lifeCycleState = pLifeCycleState_
      }

-- | Address at which the file system can be mounted by using the mount target.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdIPAddress :: Lens.Lens' MountTargetDescription (Lude.Maybe Lude.Text)
mtdIPAddress = Lens.lens (ipAddress :: MountTargetDescription -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: MountTargetDescription)
{-# DEPRECATED mtdIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The unique and consistent identifier of the Availability Zone (AZ) that the mount target resides in. For example, @use1-az1@ is an AZ ID for the us-east-1 Region and it has the same location in every AWS account.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdAvailabilityZoneId :: Lens.Lens' MountTargetDescription (Lude.Maybe Lude.Text)
mtdAvailabilityZoneId = Lens.lens (availabilityZoneId :: MountTargetDescription -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: MountTargetDescription)
{-# DEPRECATED mtdAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The Virtual Private Cloud (VPC) ID that the mount target is configured in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdVPCId :: Lens.Lens' MountTargetDescription (Lude.Maybe Lude.Text)
mtdVPCId = Lens.lens (vpcId :: MountTargetDescription -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: MountTargetDescription)
{-# DEPRECATED mtdVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The name of the Availability Zone (AZ) that the mount target resides in. AZs are independently mapped to names for each AWS account. For example, the Availability Zone @us-east-1a@ for your AWS account might not be the same location as @us-east-1a@ for another AWS account.
--
-- /Note:/ Consider using 'availabilityZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdAvailabilityZoneName :: Lens.Lens' MountTargetDescription (Lude.Maybe Lude.Text)
mtdAvailabilityZoneName = Lens.lens (availabilityZoneName :: MountTargetDescription -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneName = a} :: MountTargetDescription)
{-# DEPRECATED mtdAvailabilityZoneName "Use generic-lens or generic-optics with 'availabilityZoneName' instead." #-}

-- | The ID of the network interface that Amazon EFS created when it created the mount target.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdNetworkInterfaceId :: Lens.Lens' MountTargetDescription (Lude.Maybe Lude.Text)
mtdNetworkInterfaceId = Lens.lens (networkInterfaceId :: MountTargetDescription -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: MountTargetDescription)
{-# DEPRECATED mtdNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | AWS account ID that owns the resource.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdOwnerId :: Lens.Lens' MountTargetDescription (Lude.Maybe Lude.Text)
mtdOwnerId = Lens.lens (ownerId :: MountTargetDescription -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: MountTargetDescription)
{-# DEPRECATED mtdOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | System-assigned mount target ID.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdMountTargetId :: Lens.Lens' MountTargetDescription Lude.Text
mtdMountTargetId = Lens.lens (mountTargetId :: MountTargetDescription -> Lude.Text) (\s a -> s {mountTargetId = a} :: MountTargetDescription)
{-# DEPRECATED mtdMountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead." #-}

-- | The ID of the file system for which the mount target is intended.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdFileSystemId :: Lens.Lens' MountTargetDescription Lude.Text
mtdFileSystemId = Lens.lens (fileSystemId :: MountTargetDescription -> Lude.Text) (\s a -> s {fileSystemId = a} :: MountTargetDescription)
{-# DEPRECATED mtdFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The ID of the mount target's subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdSubnetId :: Lens.Lens' MountTargetDescription Lude.Text
mtdSubnetId = Lens.lens (subnetId :: MountTargetDescription -> Lude.Text) (\s a -> s {subnetId = a} :: MountTargetDescription)
{-# DEPRECATED mtdSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Lifecycle state of the mount target.
--
-- /Note:/ Consider using 'lifeCycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdLifeCycleState :: Lens.Lens' MountTargetDescription LifeCycleState
mtdLifeCycleState = Lens.lens (lifeCycleState :: MountTargetDescription -> LifeCycleState) (\s a -> s {lifeCycleState = a} :: MountTargetDescription)
{-# DEPRECATED mtdLifeCycleState "Use generic-lens or generic-optics with 'lifeCycleState' instead." #-}

instance Lude.FromJSON MountTargetDescription where
  parseJSON =
    Lude.withObject
      "MountTargetDescription"
      ( \x ->
          MountTargetDescription'
            Lude.<$> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "AvailabilityZoneId")
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "AvailabilityZoneName")
            Lude.<*> (x Lude..:? "NetworkInterfaceId")
            Lude.<*> (x Lude..:? "OwnerId")
            Lude.<*> (x Lude..: "MountTargetId")
            Lude.<*> (x Lude..: "FileSystemId")
            Lude.<*> (x Lude..: "SubnetId")
            Lude.<*> (x Lude..: "LifeCycleState")
      )
