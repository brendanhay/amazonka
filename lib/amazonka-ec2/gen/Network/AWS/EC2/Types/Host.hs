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
    hReleaseTime,
    hState,
    hClientToken,
    hAvailabilityZoneId,
    hHostId,
    hAvailableCapacity,
    hHostReservationId,
    hAllowsMultipleInstanceTypes,
    hHostProperties,
    hOwnerId,
    hAvailabilityZone,
    hInstances,
    hAllocationTime,
    hMemberOfServiceLinkedResourceGroup,
    hHostRecovery,
    hAutoPlacement,
    hTags,
  )
where

import Network.AWS.EC2.Types.AllocationState
import Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
import Network.AWS.EC2.Types.AutoPlacement
import Network.AWS.EC2.Types.AvailableCapacity
import Network.AWS.EC2.Types.HostInstance
import Network.AWS.EC2.Types.HostProperties
import Network.AWS.EC2.Types.HostRecovery
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the properties of the Dedicated Host.
--
-- /See:/ 'mkHost' smart constructor.
data Host = Host'
  { releaseTime :: Lude.Maybe Lude.DateTime,
    state :: Lude.Maybe AllocationState,
    clientToken :: Lude.Maybe Lude.Text,
    availabilityZoneId :: Lude.Maybe Lude.Text,
    hostId :: Lude.Maybe Lude.Text,
    availableCapacity :: Lude.Maybe AvailableCapacity,
    hostReservationId :: Lude.Maybe Lude.Text,
    allowsMultipleInstanceTypes ::
      Lude.Maybe AllowsMultipleInstanceTypes,
    hostProperties :: Lude.Maybe HostProperties,
    ownerId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    instances :: Lude.Maybe [HostInstance],
    allocationTime :: Lude.Maybe Lude.DateTime,
    memberOfServiceLinkedResourceGroup :: Lude.Maybe Lude.Bool,
    hostRecovery :: Lude.Maybe HostRecovery,
    autoPlacement :: Lude.Maybe AutoPlacement,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Host' with the minimum fields required to make a request.
--
-- * 'allocationTime' - The time that the Dedicated Host was allocated.
-- * 'allowsMultipleInstanceTypes' - Indicates whether the Dedicated Host supports multiple instance types of the same instance family, or a specific instance type only. @one@ indicates that the Dedicated Host supports multiple instance types in the instance family. @off@ indicates that the Dedicated Host supports a single instance type only.
-- * 'autoPlacement' - Whether auto-placement is on or off.
-- * 'availabilityZone' - The Availability Zone of the Dedicated Host.
-- * 'availabilityZoneId' - The ID of the Availability Zone in which the Dedicated Host is allocated.
-- * 'availableCapacity' - Information about the instances running on the Dedicated Host.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'hostId' - The ID of the Dedicated Host.
-- * 'hostProperties' - The hardware specifications of the Dedicated Host.
-- * 'hostRecovery' - Indicates whether host recovery is enabled or disabled for the Dedicated Host.
-- * 'hostReservationId' - The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
-- * 'instances' - The IDs and instance type that are currently running on the Dedicated Host.
-- * 'memberOfServiceLinkedResourceGroup' - Indicates whether the Dedicated Host is in a host resource group. If __memberOfServiceLinkedResourceGroup__ is @true@ , the host is in a host resource group; otherwise, it is not.
-- * 'ownerId' - The ID of the AWS account that owns the Dedicated Host.
-- * 'releaseTime' - The time that the Dedicated Host was released.
-- * 'state' - The Dedicated Host's state.
-- * 'tags' - Any tags assigned to the Dedicated Host.
mkHost ::
  Host
mkHost =
  Host'
    { releaseTime = Lude.Nothing,
      state = Lude.Nothing,
      clientToken = Lude.Nothing,
      availabilityZoneId = Lude.Nothing,
      hostId = Lude.Nothing,
      availableCapacity = Lude.Nothing,
      hostReservationId = Lude.Nothing,
      allowsMultipleInstanceTypes = Lude.Nothing,
      hostProperties = Lude.Nothing,
      ownerId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      instances = Lude.Nothing,
      allocationTime = Lude.Nothing,
      memberOfServiceLinkedResourceGroup = Lude.Nothing,
      hostRecovery = Lude.Nothing,
      autoPlacement = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The time that the Dedicated Host was released.
--
-- /Note:/ Consider using 'releaseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hReleaseTime :: Lens.Lens' Host (Lude.Maybe Lude.DateTime)
hReleaseTime = Lens.lens (releaseTime :: Host -> Lude.Maybe Lude.DateTime) (\s a -> s {releaseTime = a} :: Host)
{-# DEPRECATED hReleaseTime "Use generic-lens or generic-optics with 'releaseTime' instead." #-}

-- | The Dedicated Host's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hState :: Lens.Lens' Host (Lude.Maybe AllocationState)
hState = Lens.lens (state :: Host -> Lude.Maybe AllocationState) (\s a -> s {state = a} :: Host)
{-# DEPRECATED hState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hClientToken :: Lens.Lens' Host (Lude.Maybe Lude.Text)
hClientToken = Lens.lens (clientToken :: Host -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: Host)
{-# DEPRECATED hClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the Availability Zone in which the Dedicated Host is allocated.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAvailabilityZoneId :: Lens.Lens' Host (Lude.Maybe Lude.Text)
hAvailabilityZoneId = Lens.lens (availabilityZoneId :: Host -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: Host)
{-# DEPRECATED hAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The ID of the Dedicated Host.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostId :: Lens.Lens' Host (Lude.Maybe Lude.Text)
hHostId = Lens.lens (hostId :: Host -> Lude.Maybe Lude.Text) (\s a -> s {hostId = a} :: Host)
{-# DEPRECATED hHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | Information about the instances running on the Dedicated Host.
--
-- /Note:/ Consider using 'availableCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAvailableCapacity :: Lens.Lens' Host (Lude.Maybe AvailableCapacity)
hAvailableCapacity = Lens.lens (availableCapacity :: Host -> Lude.Maybe AvailableCapacity) (\s a -> s {availableCapacity = a} :: Host)
{-# DEPRECATED hAvailableCapacity "Use generic-lens or generic-optics with 'availableCapacity' instead." #-}

-- | The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
--
-- /Note:/ Consider using 'hostReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostReservationId :: Lens.Lens' Host (Lude.Maybe Lude.Text)
hHostReservationId = Lens.lens (hostReservationId :: Host -> Lude.Maybe Lude.Text) (\s a -> s {hostReservationId = a} :: Host)
{-# DEPRECATED hHostReservationId "Use generic-lens or generic-optics with 'hostReservationId' instead." #-}

-- | Indicates whether the Dedicated Host supports multiple instance types of the same instance family, or a specific instance type only. @one@ indicates that the Dedicated Host supports multiple instance types in the instance family. @off@ indicates that the Dedicated Host supports a single instance type only.
--
-- /Note:/ Consider using 'allowsMultipleInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAllowsMultipleInstanceTypes :: Lens.Lens' Host (Lude.Maybe AllowsMultipleInstanceTypes)
hAllowsMultipleInstanceTypes = Lens.lens (allowsMultipleInstanceTypes :: Host -> Lude.Maybe AllowsMultipleInstanceTypes) (\s a -> s {allowsMultipleInstanceTypes = a} :: Host)
{-# DEPRECATED hAllowsMultipleInstanceTypes "Use generic-lens or generic-optics with 'allowsMultipleInstanceTypes' instead." #-}

-- | The hardware specifications of the Dedicated Host.
--
-- /Note:/ Consider using 'hostProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostProperties :: Lens.Lens' Host (Lude.Maybe HostProperties)
hHostProperties = Lens.lens (hostProperties :: Host -> Lude.Maybe HostProperties) (\s a -> s {hostProperties = a} :: Host)
{-# DEPRECATED hHostProperties "Use generic-lens or generic-optics with 'hostProperties' instead." #-}

-- | The ID of the AWS account that owns the Dedicated Host.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hOwnerId :: Lens.Lens' Host (Lude.Maybe Lude.Text)
hOwnerId = Lens.lens (ownerId :: Host -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: Host)
{-# DEPRECATED hOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The Availability Zone of the Dedicated Host.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAvailabilityZone :: Lens.Lens' Host (Lude.Maybe Lude.Text)
hAvailabilityZone = Lens.lens (availabilityZone :: Host -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Host)
{-# DEPRECATED hAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The IDs and instance type that are currently running on the Dedicated Host.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hInstances :: Lens.Lens' Host (Lude.Maybe [HostInstance])
hInstances = Lens.lens (instances :: Host -> Lude.Maybe [HostInstance]) (\s a -> s {instances = a} :: Host)
{-# DEPRECATED hInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The time that the Dedicated Host was allocated.
--
-- /Note:/ Consider using 'allocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAllocationTime :: Lens.Lens' Host (Lude.Maybe Lude.DateTime)
hAllocationTime = Lens.lens (allocationTime :: Host -> Lude.Maybe Lude.DateTime) (\s a -> s {allocationTime = a} :: Host)
{-# DEPRECATED hAllocationTime "Use generic-lens or generic-optics with 'allocationTime' instead." #-}

-- | Indicates whether the Dedicated Host is in a host resource group. If __memberOfServiceLinkedResourceGroup__ is @true@ , the host is in a host resource group; otherwise, it is not.
--
-- /Note:/ Consider using 'memberOfServiceLinkedResourceGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMemberOfServiceLinkedResourceGroup :: Lens.Lens' Host (Lude.Maybe Lude.Bool)
hMemberOfServiceLinkedResourceGroup = Lens.lens (memberOfServiceLinkedResourceGroup :: Host -> Lude.Maybe Lude.Bool) (\s a -> s {memberOfServiceLinkedResourceGroup = a} :: Host)
{-# DEPRECATED hMemberOfServiceLinkedResourceGroup "Use generic-lens or generic-optics with 'memberOfServiceLinkedResourceGroup' instead." #-}

-- | Indicates whether host recovery is enabled or disabled for the Dedicated Host.
--
-- /Note:/ Consider using 'hostRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHostRecovery :: Lens.Lens' Host (Lude.Maybe HostRecovery)
hHostRecovery = Lens.lens (hostRecovery :: Host -> Lude.Maybe HostRecovery) (\s a -> s {hostRecovery = a} :: Host)
{-# DEPRECATED hHostRecovery "Use generic-lens or generic-optics with 'hostRecovery' instead." #-}

-- | Whether auto-placement is on or off.
--
-- /Note:/ Consider using 'autoPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAutoPlacement :: Lens.Lens' Host (Lude.Maybe AutoPlacement)
hAutoPlacement = Lens.lens (autoPlacement :: Host -> Lude.Maybe AutoPlacement) (\s a -> s {autoPlacement = a} :: Host)
{-# DEPRECATED hAutoPlacement "Use generic-lens or generic-optics with 'autoPlacement' instead." #-}

-- | Any tags assigned to the Dedicated Host.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTags :: Lens.Lens' Host (Lude.Maybe [Tag])
hTags = Lens.lens (tags :: Host -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Host)
{-# DEPRECATED hTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML Host where
  parseXML x =
    Host'
      Lude.<$> (x Lude..@? "releaseTime")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "clientToken")
      Lude.<*> (x Lude..@? "availabilityZoneId")
      Lude.<*> (x Lude..@? "hostId")
      Lude.<*> (x Lude..@? "availableCapacity")
      Lude.<*> (x Lude..@? "hostReservationId")
      Lude.<*> (x Lude..@? "allowsMultipleInstanceTypes")
      Lude.<*> (x Lude..@? "hostProperties")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> ( x Lude..@? "instances" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "allocationTime")
      Lude.<*> (x Lude..@? "memberOfServiceLinkedResourceGroup")
      Lude.<*> (x Lude..@? "hostRecovery")
      Lude.<*> (x Lude..@? "autoPlacement")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
