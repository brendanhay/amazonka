{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Host
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Host
  ( Host (..),

    -- * Smart constructor
    mkHost,

    -- * Lenses
    hAllocationTime,
    hAllowsMultipleInstanceTypes,
    hAutoPlacement,
    hAvailabilityZone,
    hAvailabilityZoneId,
    hAvailableCapacity,
    hClientToken,
    hHostId,
    hHostProperties,
    hHostRecovery,
    hHostReservationId,
    hInstances,
    hMemberOfServiceLinkedResourceGroup,
    hOwnerId,
    hReleaseTime,
    hState,
    hTags,
  )
where

import qualified Network.AWS.EC2.Types.AllocationState as Types
import qualified Network.AWS.EC2.Types.AllowsMultipleInstanceTypes as Types
import qualified Network.AWS.EC2.Types.AutoPlacement as Types
import qualified Network.AWS.EC2.Types.AvailableCapacity as Types
import qualified Network.AWS.EC2.Types.HostInstance as Types
import qualified Network.AWS.EC2.Types.HostProperties as Types
import qualified Network.AWS.EC2.Types.HostRecovery as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the properties of the Dedicated Host.
--
-- /See:/ 'mkHost' smart constructor.
data Host = Host'
  { -- | The time that the Dedicated Host was allocated.
    allocationTime :: Core.Maybe Core.UTCTime,
    -- | Indicates whether the Dedicated Host supports multiple instance types of the same instance family, or a specific instance type only. @one@ indicates that the Dedicated Host supports multiple instance types in the instance family. @off@ indicates that the Dedicated Host supports a single instance type only.
    allowsMultipleInstanceTypes :: Core.Maybe Types.AllowsMultipleInstanceTypes,
    -- | Whether auto-placement is on or off.
    autoPlacement :: Core.Maybe Types.AutoPlacement,
    -- | The Availability Zone of the Dedicated Host.
    availabilityZone :: Core.Maybe Types.String,
    -- | The ID of the Availability Zone in which the Dedicated Host is allocated.
    availabilityZoneId :: Core.Maybe Types.String,
    -- | Information about the instances running on the Dedicated Host.
    availableCapacity :: Core.Maybe Types.AvailableCapacity,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | The ID of the Dedicated Host.
    hostId :: Core.Maybe Types.String,
    -- | The hardware specifications of the Dedicated Host.
    hostProperties :: Core.Maybe Types.HostProperties,
    -- | Indicates whether host recovery is enabled or disabled for the Dedicated Host.
    hostRecovery :: Core.Maybe Types.HostRecovery,
    -- | The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
    hostReservationId :: Core.Maybe Types.String,
    -- | The IDs and instance type that are currently running on the Dedicated Host.
    instances :: Core.Maybe [Types.HostInstance],
    -- | Indicates whether the Dedicated Host is in a host resource group. If __memberOfServiceLinkedResourceGroup__ is @true@ , the host is in a host resource group; otherwise, it is not.
    memberOfServiceLinkedResourceGroup :: Core.Maybe Core.Bool,
    -- | The ID of the AWS account that owns the Dedicated Host.
    ownerId :: Core.Maybe Types.String,
    -- | The time that the Dedicated Host was released.
    releaseTime :: Core.Maybe Core.UTCTime,
    -- | The Dedicated Host's state.
    state :: Core.Maybe Types.AllocationState,
    -- | Any tags assigned to the Dedicated Host.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Host' value with any optional fields omitted.
mkHost ::
  Host
mkHost =
  Host'
    { allocationTime = Core.Nothing,
      allowsMultipleInstanceTypes = Core.Nothing,
      autoPlacement = Core.Nothing,
      availabilityZone = Core.Nothing,
      availabilityZoneId = Core.Nothing,
      availableCapacity = Core.Nothing,
      clientToken = Core.Nothing,
      hostId = Core.Nothing,
      hostProperties = Core.Nothing,
      hostRecovery = Core.Nothing,
      hostReservationId = Core.Nothing,
      instances = Core.Nothing,
      memberOfServiceLinkedResourceGroup = Core.Nothing,
      ownerId = Core.Nothing,
      releaseTime = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing
    }

-- | The time that the Dedicated Host was allocated.
--
-- /Note:/ Consider using 'allocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAllocationTime :: Lens.Lens' Host (Core.Maybe Core.UTCTime)
hAllocationTime = Lens.field @"allocationTime"
{-# DEPRECATED hAllocationTime "Use generic-lens or generic-optics with 'allocationTime' instead." #-}

-- | Indicates whether the Dedicated Host supports multiple instance types of the same instance family, or a specific instance type only. @one@ indicates that the Dedicated Host supports multiple instance types in the instance family. @off@ indicates that the Dedicated Host supports a single instance type only.
--
-- /Note:/ Consider using 'allowsMultipleInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAllowsMultipleInstanceTypes :: Lens.Lens' Host (Core.Maybe Types.AllowsMultipleInstanceTypes)
hAllowsMultipleInstanceTypes = Lens.field @"allowsMultipleInstanceTypes"
{-# DEPRECATED hAllowsMultipleInstanceTypes "Use generic-lens or generic-optics with 'allowsMultipleInstanceTypes' instead." #-}

-- | Whether auto-placement is on or off.
--
-- /Note:/ Consider using 'autoPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAutoPlacement :: Lens.Lens' Host (Core.Maybe Types.AutoPlacement)
hAutoPlacement = Lens.field @"autoPlacement"
{-# DEPRECATED hAutoPlacement "Use generic-lens or generic-optics with 'autoPlacement' instead." #-}

-- | The Availability Zone of the Dedicated Host.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAvailabilityZone :: Lens.Lens' Host (Core.Maybe Types.String)
hAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED hAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID of the Availability Zone in which the Dedicated Host is allocated.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAvailabilityZoneId :: Lens.Lens' Host (Core.Maybe Types.String)
hAvailabilityZoneId = Lens.field @"availabilityZoneId"
{-# DEPRECATED hAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | Information about the instances running on the Dedicated Host.
--
-- /Note:/ Consider using 'availableCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAvailableCapacity :: Lens.Lens' Host (Core.Maybe Types.AvailableCapacity)
hAvailableCapacity = Lens.field @"availableCapacity"
{-# DEPRECATED hAvailableCapacity "Use generic-lens or generic-optics with 'availableCapacity' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hClientToken :: Lens.Lens' Host (Core.Maybe Types.String)
hClientToken = Lens.field @"clientToken"
{-# DEPRECATED hClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the Dedicated Host.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostId :: Lens.Lens' Host (Core.Maybe Types.String)
hHostId = Lens.field @"hostId"
{-# DEPRECATED hHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The hardware specifications of the Dedicated Host.
--
-- /Note:/ Consider using 'hostProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostProperties :: Lens.Lens' Host (Core.Maybe Types.HostProperties)
hHostProperties = Lens.field @"hostProperties"
{-# DEPRECATED hHostProperties "Use generic-lens or generic-optics with 'hostProperties' instead." #-}

-- | Indicates whether host recovery is enabled or disabled for the Dedicated Host.
--
-- /Note:/ Consider using 'hostRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostRecovery :: Lens.Lens' Host (Core.Maybe Types.HostRecovery)
hHostRecovery = Lens.field @"hostRecovery"
{-# DEPRECATED hHostRecovery "Use generic-lens or generic-optics with 'hostRecovery' instead." #-}

-- | The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
--
-- /Note:/ Consider using 'hostReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostReservationId :: Lens.Lens' Host (Core.Maybe Types.String)
hHostReservationId = Lens.field @"hostReservationId"
{-# DEPRECATED hHostReservationId "Use generic-lens or generic-optics with 'hostReservationId' instead." #-}

-- | The IDs and instance type that are currently running on the Dedicated Host.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hInstances :: Lens.Lens' Host (Core.Maybe [Types.HostInstance])
hInstances = Lens.field @"instances"
{-# DEPRECATED hInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | Indicates whether the Dedicated Host is in a host resource group. If __memberOfServiceLinkedResourceGroup__ is @true@ , the host is in a host resource group; otherwise, it is not.
--
-- /Note:/ Consider using 'memberOfServiceLinkedResourceGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMemberOfServiceLinkedResourceGroup :: Lens.Lens' Host (Core.Maybe Core.Bool)
hMemberOfServiceLinkedResourceGroup = Lens.field @"memberOfServiceLinkedResourceGroup"
{-# DEPRECATED hMemberOfServiceLinkedResourceGroup "Use generic-lens or generic-optics with 'memberOfServiceLinkedResourceGroup' instead." #-}

-- | The ID of the AWS account that owns the Dedicated Host.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hOwnerId :: Lens.Lens' Host (Core.Maybe Types.String)
hOwnerId = Lens.field @"ownerId"
{-# DEPRECATED hOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The time that the Dedicated Host was released.
--
-- /Note:/ Consider using 'releaseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hReleaseTime :: Lens.Lens' Host (Core.Maybe Core.UTCTime)
hReleaseTime = Lens.field @"releaseTime"
{-# DEPRECATED hReleaseTime "Use generic-lens or generic-optics with 'releaseTime' instead." #-}

-- | The Dedicated Host's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hState :: Lens.Lens' Host (Core.Maybe Types.AllocationState)
hState = Lens.field @"state"
{-# DEPRECATED hState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Any tags assigned to the Dedicated Host.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTags :: Lens.Lens' Host (Core.Maybe [Types.Tag])
hTags = Lens.field @"tags"
{-# DEPRECATED hTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML Host where
  parseXML x =
    Host'
      Core.<$> (x Core..@? "allocationTime")
      Core.<*> (x Core..@? "allowsMultipleInstanceTypes")
      Core.<*> (x Core..@? "autoPlacement")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "availabilityZoneId")
      Core.<*> (x Core..@? "availableCapacity")
      Core.<*> (x Core..@? "clientToken")
      Core.<*> (x Core..@? "hostId")
      Core.<*> (x Core..@? "hostProperties")
      Core.<*> (x Core..@? "hostRecovery")
      Core.<*> (x Core..@? "hostReservationId")
      Core.<*> (x Core..@? "instances" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "memberOfServiceLinkedResourceGroup")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "releaseTime")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
