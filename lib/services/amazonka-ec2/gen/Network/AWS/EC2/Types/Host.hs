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
-- Module      : Network.AWS.EC2.Types.Host
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Host where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AllocationState
import Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
import Network.AWS.EC2.Types.AutoPlacement
import Network.AWS.EC2.Types.AvailableCapacity
import Network.AWS.EC2.Types.HostInstance
import Network.AWS.EC2.Types.HostProperties
import Network.AWS.EC2.Types.HostRecovery
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the properties of the Dedicated Host.
--
-- /See:/ 'newHost' smart constructor.
data Host = Host'
  { -- | The time that the Dedicated Host was released.
    releaseTime :: Prelude.Maybe Core.ISO8601,
    -- | The Dedicated Host\'s state.
    state :: Prelude.Maybe AllocationState,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Availability Zone in which the Dedicated Host is
    -- allocated.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | Information about the instances running on the Dedicated Host.
    availableCapacity :: Prelude.Maybe AvailableCapacity,
    -- | The reservation ID of the Dedicated Host. This returns a @null@ response
    -- if the Dedicated Host doesn\'t have an associated reservation.
    hostReservationId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Dedicated Host supports multiple instance types of
    -- the same instance family. If the value is @on@, the Dedicated Host
    -- supports multiple instance types in the instance family. If the value is
    -- @off@, the Dedicated Host supports a single instance type only.
    allowsMultipleInstanceTypes :: Prelude.Maybe AllowsMultipleInstanceTypes,
    -- | The hardware specifications of the Dedicated Host.
    hostProperties :: Prelude.Maybe HostProperties,
    -- | The ID of the Amazon Web Services account that owns the Dedicated Host.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the Dedicated Host.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The IDs and instance type that are currently running on the Dedicated
    -- Host.
    instances :: Prelude.Maybe [HostInstance],
    -- | The time that the Dedicated Host was allocated.
    allocationTime :: Prelude.Maybe Core.ISO8601,
    -- | Indicates whether the Dedicated Host is in a host resource group. If
    -- __memberOfServiceLinkedResourceGroup__ is @true@, the host is in a host
    -- resource group; otherwise, it is not.
    memberOfServiceLinkedResourceGroup :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether host recovery is enabled or disabled for the Dedicated
    -- Host.
    hostRecovery :: Prelude.Maybe HostRecovery,
    -- | Whether auto-placement is on or off.
    autoPlacement :: Prelude.Maybe AutoPlacement,
    -- | Any tags assigned to the Dedicated Host.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Host' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseTime', 'host_releaseTime' - The time that the Dedicated Host was released.
--
-- 'state', 'host_state' - The Dedicated Host\'s state.
--
-- 'clientToken', 'host_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'availabilityZoneId', 'host_availabilityZoneId' - The ID of the Availability Zone in which the Dedicated Host is
-- allocated.
--
-- 'hostId', 'host_hostId' - The ID of the Dedicated Host.
--
-- 'availableCapacity', 'host_availableCapacity' - Information about the instances running on the Dedicated Host.
--
-- 'hostReservationId', 'host_hostReservationId' - The reservation ID of the Dedicated Host. This returns a @null@ response
-- if the Dedicated Host doesn\'t have an associated reservation.
--
-- 'allowsMultipleInstanceTypes', 'host_allowsMultipleInstanceTypes' - Indicates whether the Dedicated Host supports multiple instance types of
-- the same instance family. If the value is @on@, the Dedicated Host
-- supports multiple instance types in the instance family. If the value is
-- @off@, the Dedicated Host supports a single instance type only.
--
-- 'hostProperties', 'host_hostProperties' - The hardware specifications of the Dedicated Host.
--
-- 'ownerId', 'host_ownerId' - The ID of the Amazon Web Services account that owns the Dedicated Host.
--
-- 'availabilityZone', 'host_availabilityZone' - The Availability Zone of the Dedicated Host.
--
-- 'instances', 'host_instances' - The IDs and instance type that are currently running on the Dedicated
-- Host.
--
-- 'allocationTime', 'host_allocationTime' - The time that the Dedicated Host was allocated.
--
-- 'memberOfServiceLinkedResourceGroup', 'host_memberOfServiceLinkedResourceGroup' - Indicates whether the Dedicated Host is in a host resource group. If
-- __memberOfServiceLinkedResourceGroup__ is @true@, the host is in a host
-- resource group; otherwise, it is not.
--
-- 'hostRecovery', 'host_hostRecovery' - Indicates whether host recovery is enabled or disabled for the Dedicated
-- Host.
--
-- 'autoPlacement', 'host_autoPlacement' - Whether auto-placement is on or off.
--
-- 'tags', 'host_tags' - Any tags assigned to the Dedicated Host.
newHost ::
  Host
newHost =
  Host'
    { releaseTime = Prelude.Nothing,
      state = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      hostId = Prelude.Nothing,
      availableCapacity = Prelude.Nothing,
      hostReservationId = Prelude.Nothing,
      allowsMultipleInstanceTypes = Prelude.Nothing,
      hostProperties = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instances = Prelude.Nothing,
      allocationTime = Prelude.Nothing,
      memberOfServiceLinkedResourceGroup = Prelude.Nothing,
      hostRecovery = Prelude.Nothing,
      autoPlacement = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time that the Dedicated Host was released.
host_releaseTime :: Lens.Lens' Host (Prelude.Maybe Prelude.UTCTime)
host_releaseTime = Lens.lens (\Host' {releaseTime} -> releaseTime) (\s@Host' {} a -> s {releaseTime = a} :: Host) Prelude.. Lens.mapping Core._Time

-- | The Dedicated Host\'s state.
host_state :: Lens.Lens' Host (Prelude.Maybe AllocationState)
host_state = Lens.lens (\Host' {state} -> state) (\s@Host' {} a -> s {state = a} :: Host)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
host_clientToken :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_clientToken = Lens.lens (\Host' {clientToken} -> clientToken) (\s@Host' {} a -> s {clientToken = a} :: Host)

-- | The ID of the Availability Zone in which the Dedicated Host is
-- allocated.
host_availabilityZoneId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_availabilityZoneId = Lens.lens (\Host' {availabilityZoneId} -> availabilityZoneId) (\s@Host' {} a -> s {availabilityZoneId = a} :: Host)

-- | The ID of the Dedicated Host.
host_hostId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostId = Lens.lens (\Host' {hostId} -> hostId) (\s@Host' {} a -> s {hostId = a} :: Host)

-- | Information about the instances running on the Dedicated Host.
host_availableCapacity :: Lens.Lens' Host (Prelude.Maybe AvailableCapacity)
host_availableCapacity = Lens.lens (\Host' {availableCapacity} -> availableCapacity) (\s@Host' {} a -> s {availableCapacity = a} :: Host)

-- | The reservation ID of the Dedicated Host. This returns a @null@ response
-- if the Dedicated Host doesn\'t have an associated reservation.
host_hostReservationId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostReservationId = Lens.lens (\Host' {hostReservationId} -> hostReservationId) (\s@Host' {} a -> s {hostReservationId = a} :: Host)

-- | Indicates whether the Dedicated Host supports multiple instance types of
-- the same instance family. If the value is @on@, the Dedicated Host
-- supports multiple instance types in the instance family. If the value is
-- @off@, the Dedicated Host supports a single instance type only.
host_allowsMultipleInstanceTypes :: Lens.Lens' Host (Prelude.Maybe AllowsMultipleInstanceTypes)
host_allowsMultipleInstanceTypes = Lens.lens (\Host' {allowsMultipleInstanceTypes} -> allowsMultipleInstanceTypes) (\s@Host' {} a -> s {allowsMultipleInstanceTypes = a} :: Host)

-- | The hardware specifications of the Dedicated Host.
host_hostProperties :: Lens.Lens' Host (Prelude.Maybe HostProperties)
host_hostProperties = Lens.lens (\Host' {hostProperties} -> hostProperties) (\s@Host' {} a -> s {hostProperties = a} :: Host)

-- | The ID of the Amazon Web Services account that owns the Dedicated Host.
host_ownerId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_ownerId = Lens.lens (\Host' {ownerId} -> ownerId) (\s@Host' {} a -> s {ownerId = a} :: Host)

-- | The Availability Zone of the Dedicated Host.
host_availabilityZone :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_availabilityZone = Lens.lens (\Host' {availabilityZone} -> availabilityZone) (\s@Host' {} a -> s {availabilityZone = a} :: Host)

-- | The IDs and instance type that are currently running on the Dedicated
-- Host.
host_instances :: Lens.Lens' Host (Prelude.Maybe [HostInstance])
host_instances = Lens.lens (\Host' {instances} -> instances) (\s@Host' {} a -> s {instances = a} :: Host) Prelude.. Lens.mapping Lens.coerced

-- | The time that the Dedicated Host was allocated.
host_allocationTime :: Lens.Lens' Host (Prelude.Maybe Prelude.UTCTime)
host_allocationTime = Lens.lens (\Host' {allocationTime} -> allocationTime) (\s@Host' {} a -> s {allocationTime = a} :: Host) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the Dedicated Host is in a host resource group. If
-- __memberOfServiceLinkedResourceGroup__ is @true@, the host is in a host
-- resource group; otherwise, it is not.
host_memberOfServiceLinkedResourceGroup :: Lens.Lens' Host (Prelude.Maybe Prelude.Bool)
host_memberOfServiceLinkedResourceGroup = Lens.lens (\Host' {memberOfServiceLinkedResourceGroup} -> memberOfServiceLinkedResourceGroup) (\s@Host' {} a -> s {memberOfServiceLinkedResourceGroup = a} :: Host)

-- | Indicates whether host recovery is enabled or disabled for the Dedicated
-- Host.
host_hostRecovery :: Lens.Lens' Host (Prelude.Maybe HostRecovery)
host_hostRecovery = Lens.lens (\Host' {hostRecovery} -> hostRecovery) (\s@Host' {} a -> s {hostRecovery = a} :: Host)

-- | Whether auto-placement is on or off.
host_autoPlacement :: Lens.Lens' Host (Prelude.Maybe AutoPlacement)
host_autoPlacement = Lens.lens (\Host' {autoPlacement} -> autoPlacement) (\s@Host' {} a -> s {autoPlacement = a} :: Host)

-- | Any tags assigned to the Dedicated Host.
host_tags :: Lens.Lens' Host (Prelude.Maybe [Tag])
host_tags = Lens.lens (\Host' {tags} -> tags) (\s@Host' {} a -> s {tags = a} :: Host) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML Host where
  parseXML x =
    Host'
      Prelude.<$> (x Core..@? "releaseTime")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "clientToken")
      Prelude.<*> (x Core..@? "availabilityZoneId")
      Prelude.<*> (x Core..@? "hostId")
      Prelude.<*> (x Core..@? "availableCapacity")
      Prelude.<*> (x Core..@? "hostReservationId")
      Prelude.<*> (x Core..@? "allowsMultipleInstanceTypes")
      Prelude.<*> (x Core..@? "hostProperties")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> ( x Core..@? "instances" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "allocationTime")
      Prelude.<*> (x Core..@? "memberOfServiceLinkedResourceGroup")
      Prelude.<*> (x Core..@? "hostRecovery")
      Prelude.<*> (x Core..@? "autoPlacement")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable Host

instance Prelude.NFData Host
