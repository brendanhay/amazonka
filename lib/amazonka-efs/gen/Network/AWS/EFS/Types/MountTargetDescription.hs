{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.MountTargetDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.MountTargetDescription
  ( MountTargetDescription (..)
  -- * Smart constructor
  , mkMountTargetDescription
  -- * Lenses
  , mtdMountTargetId
  , mtdFileSystemId
  , mtdSubnetId
  , mtdLifeCycleState
  , mtdAvailabilityZoneId
  , mtdAvailabilityZoneName
  , mtdIpAddress
  , mtdNetworkInterfaceId
  , mtdOwnerId
  , mtdVpcId
  ) where

import qualified Network.AWS.EFS.Types.AvailabilityZoneId as Types
import qualified Network.AWS.EFS.Types.AvailabilityZoneName as Types
import qualified Network.AWS.EFS.Types.FileSystemId as Types
import qualified Network.AWS.EFS.Types.IpAddress as Types
import qualified Network.AWS.EFS.Types.LifeCycleState as Types
import qualified Network.AWS.EFS.Types.MountTargetId as Types
import qualified Network.AWS.EFS.Types.NetworkInterfaceId as Types
import qualified Network.AWS.EFS.Types.OwnerId as Types
import qualified Network.AWS.EFS.Types.SubnetId as Types
import qualified Network.AWS.EFS.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a description of a mount target.
--
-- /See:/ 'mkMountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { mountTargetId :: Types.MountTargetId
    -- ^ System-assigned mount target ID.
  , fileSystemId :: Types.FileSystemId
    -- ^ The ID of the file system for which the mount target is intended.
  , subnetId :: Types.SubnetId
    -- ^ The ID of the mount target's subnet.
  , lifeCycleState :: Types.LifeCycleState
    -- ^ Lifecycle state of the mount target.
  , availabilityZoneId :: Core.Maybe Types.AvailabilityZoneId
    -- ^ The unique and consistent identifier of the Availability Zone (AZ) that the mount target resides in. For example, @use1-az1@ is an AZ ID for the us-east-1 Region and it has the same location in every AWS account.
  , availabilityZoneName :: Core.Maybe Types.AvailabilityZoneName
    -- ^ The name of the Availability Zone (AZ) that the mount target resides in. AZs are independently mapped to names for each AWS account. For example, the Availability Zone @us-east-1a@ for your AWS account might not be the same location as @us-east-1a@ for another AWS account.
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ Address at which the file system can be mounted by using the mount target.
  , networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId
    -- ^ The ID of the network interface that Amazon EFS created when it created the mount target.
  , ownerId :: Core.Maybe Types.OwnerId
    -- ^ AWS account ID that owns the resource.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The Virtual Private Cloud (VPC) ID that the mount target is configured in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MountTargetDescription' value with any optional fields omitted.
mkMountTargetDescription
    :: Types.MountTargetId -- ^ 'mountTargetId'
    -> Types.FileSystemId -- ^ 'fileSystemId'
    -> Types.SubnetId -- ^ 'subnetId'
    -> Types.LifeCycleState -- ^ 'lifeCycleState'
    -> MountTargetDescription
mkMountTargetDescription mountTargetId fileSystemId subnetId
  lifeCycleState
  = MountTargetDescription'{mountTargetId, fileSystemId, subnetId,
                            lifeCycleState, availabilityZoneId = Core.Nothing,
                            availabilityZoneName = Core.Nothing, ipAddress = Core.Nothing,
                            networkInterfaceId = Core.Nothing, ownerId = Core.Nothing,
                            vpcId = Core.Nothing}

-- | System-assigned mount target ID.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdMountTargetId :: Lens.Lens' MountTargetDescription Types.MountTargetId
mtdMountTargetId = Lens.field @"mountTargetId"
{-# INLINEABLE mtdMountTargetId #-}
{-# DEPRECATED mountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead"  #-}

-- | The ID of the file system for which the mount target is intended.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdFileSystemId :: Lens.Lens' MountTargetDescription Types.FileSystemId
mtdFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE mtdFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | The ID of the mount target's subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdSubnetId :: Lens.Lens' MountTargetDescription Types.SubnetId
mtdSubnetId = Lens.field @"subnetId"
{-# INLINEABLE mtdSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | Lifecycle state of the mount target.
--
-- /Note:/ Consider using 'lifeCycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdLifeCycleState :: Lens.Lens' MountTargetDescription Types.LifeCycleState
mtdLifeCycleState = Lens.field @"lifeCycleState"
{-# INLINEABLE mtdLifeCycleState #-}
{-# DEPRECATED lifeCycleState "Use generic-lens or generic-optics with 'lifeCycleState' instead"  #-}

-- | The unique and consistent identifier of the Availability Zone (AZ) that the mount target resides in. For example, @use1-az1@ is an AZ ID for the us-east-1 Region and it has the same location in every AWS account.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdAvailabilityZoneId :: Lens.Lens' MountTargetDescription (Core.Maybe Types.AvailabilityZoneId)
mtdAvailabilityZoneId = Lens.field @"availabilityZoneId"
{-# INLINEABLE mtdAvailabilityZoneId #-}
{-# DEPRECATED availabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead"  #-}

-- | The name of the Availability Zone (AZ) that the mount target resides in. AZs are independently mapped to names for each AWS account. For example, the Availability Zone @us-east-1a@ for your AWS account might not be the same location as @us-east-1a@ for another AWS account.
--
-- /Note:/ Consider using 'availabilityZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdAvailabilityZoneName :: Lens.Lens' MountTargetDescription (Core.Maybe Types.AvailabilityZoneName)
mtdAvailabilityZoneName = Lens.field @"availabilityZoneName"
{-# INLINEABLE mtdAvailabilityZoneName #-}
{-# DEPRECATED availabilityZoneName "Use generic-lens or generic-optics with 'availabilityZoneName' instead"  #-}

-- | Address at which the file system can be mounted by using the mount target.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdIpAddress :: Lens.Lens' MountTargetDescription (Core.Maybe Types.IpAddress)
mtdIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE mtdIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | The ID of the network interface that Amazon EFS created when it created the mount target.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdNetworkInterfaceId :: Lens.Lens' MountTargetDescription (Core.Maybe Types.NetworkInterfaceId)
mtdNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE mtdNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | AWS account ID that owns the resource.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdOwnerId :: Lens.Lens' MountTargetDescription (Core.Maybe Types.OwnerId)
mtdOwnerId = Lens.field @"ownerId"
{-# INLINEABLE mtdOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The Virtual Private Cloud (VPC) ID that the mount target is configured in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtdVpcId :: Lens.Lens' MountTargetDescription (Core.Maybe Types.VpcId)
mtdVpcId = Lens.field @"vpcId"
{-# INLINEABLE mtdVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON MountTargetDescription where
        parseJSON
          = Core.withObject "MountTargetDescription" Core.$
              \ x ->
                MountTargetDescription' Core.<$>
                  (x Core..: "MountTargetId") Core.<*> x Core..: "FileSystemId"
                    Core.<*> x Core..: "SubnetId"
                    Core.<*> x Core..: "LifeCycleState"
                    Core.<*> x Core..:? "AvailabilityZoneId"
                    Core.<*> x Core..:? "AvailabilityZoneName"
                    Core.<*> x Core..:? "IpAddress"
                    Core.<*> x Core..:? "NetworkInterfaceId"
                    Core.<*> x Core..:? "OwnerId"
                    Core.<*> x Core..:? "VpcId"
