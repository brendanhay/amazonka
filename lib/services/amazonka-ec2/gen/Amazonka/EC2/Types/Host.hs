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
-- Module      : Amazonka.EC2.Types.Host
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Host where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AllocationState
import Amazonka.EC2.Types.AllowsMultipleInstanceTypes
import Amazonka.EC2.Types.AutoPlacement
import Amazonka.EC2.Types.AvailableCapacity
import Amazonka.EC2.Types.HostInstance
import Amazonka.EC2.Types.HostProperties
import Amazonka.EC2.Types.HostRecovery
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes the properties of the Dedicated Host.
--
-- /See:/ 'newHost' smart constructor.
data Host = Host'
  { -- | Any tags assigned to the Dedicated Host.
    tags :: Prelude.Maybe [Tag],
    -- | The IDs and instance type that are currently running on the Dedicated
    -- Host.
    instances :: Prelude.Maybe [HostInstance],
    -- | Whether auto-placement is on or off.
    autoPlacement :: Prelude.Maybe AutoPlacement,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Outpost on
    -- which the Dedicated Host is allocated.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the Dedicated Host.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Information about the instances running on the Dedicated Host.
    availableCapacity :: Prelude.Maybe AvailableCapacity,
    -- | Indicates whether host recovery is enabled or disabled for the Dedicated
    -- Host.
    hostRecovery :: Prelude.Maybe HostRecovery,
    -- | The ID of the Dedicated Host.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The time that the Dedicated Host was allocated.
    allocationTime :: Prelude.Maybe Core.ISO8601,
    -- | The Dedicated Host\'s state.
    state :: Prelude.Maybe AllocationState,
    -- | The Availability Zone of the Dedicated Host.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The hardware specifications of the Dedicated Host.
    hostProperties :: Prelude.Maybe HostProperties,
    -- | The time that the Dedicated Host was released.
    releaseTime :: Prelude.Maybe Core.ISO8601,
    -- | Indicates whether the Dedicated Host supports multiple instance types of
    -- the same instance family. If the value is @on@, the Dedicated Host
    -- supports multiple instance types in the instance family. If the value is
    -- @off@, the Dedicated Host supports a single instance type only.
    allowsMultipleInstanceTypes :: Prelude.Maybe AllowsMultipleInstanceTypes,
    -- | Indicates whether the Dedicated Host is in a host resource group. If
    -- __memberOfServiceLinkedResourceGroup__ is @true@, the host is in a host
    -- resource group; otherwise, it is not.
    memberOfServiceLinkedResourceGroup :: Prelude.Maybe Prelude.Bool,
    -- | The reservation ID of the Dedicated Host. This returns a @null@ response
    -- if the Dedicated Host doesn\'t have an associated reservation.
    hostReservationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Availability Zone in which the Dedicated Host is
    -- allocated.
    availabilityZoneId :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'host_tags' - Any tags assigned to the Dedicated Host.
--
-- 'instances', 'host_instances' - The IDs and instance type that are currently running on the Dedicated
-- Host.
--
-- 'autoPlacement', 'host_autoPlacement' - Whether auto-placement is on or off.
--
-- 'clientToken', 'host_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'outpostArn', 'host_outpostArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Outpost on
-- which the Dedicated Host is allocated.
--
-- 'ownerId', 'host_ownerId' - The ID of the Amazon Web Services account that owns the Dedicated Host.
--
-- 'availableCapacity', 'host_availableCapacity' - Information about the instances running on the Dedicated Host.
--
-- 'hostRecovery', 'host_hostRecovery' - Indicates whether host recovery is enabled or disabled for the Dedicated
-- Host.
--
-- 'hostId', 'host_hostId' - The ID of the Dedicated Host.
--
-- 'allocationTime', 'host_allocationTime' - The time that the Dedicated Host was allocated.
--
-- 'state', 'host_state' - The Dedicated Host\'s state.
--
-- 'availabilityZone', 'host_availabilityZone' - The Availability Zone of the Dedicated Host.
--
-- 'hostProperties', 'host_hostProperties' - The hardware specifications of the Dedicated Host.
--
-- 'releaseTime', 'host_releaseTime' - The time that the Dedicated Host was released.
--
-- 'allowsMultipleInstanceTypes', 'host_allowsMultipleInstanceTypes' - Indicates whether the Dedicated Host supports multiple instance types of
-- the same instance family. If the value is @on@, the Dedicated Host
-- supports multiple instance types in the instance family. If the value is
-- @off@, the Dedicated Host supports a single instance type only.
--
-- 'memberOfServiceLinkedResourceGroup', 'host_memberOfServiceLinkedResourceGroup' - Indicates whether the Dedicated Host is in a host resource group. If
-- __memberOfServiceLinkedResourceGroup__ is @true@, the host is in a host
-- resource group; otherwise, it is not.
--
-- 'hostReservationId', 'host_hostReservationId' - The reservation ID of the Dedicated Host. This returns a @null@ response
-- if the Dedicated Host doesn\'t have an associated reservation.
--
-- 'availabilityZoneId', 'host_availabilityZoneId' - The ID of the Availability Zone in which the Dedicated Host is
-- allocated.
newHost ::
  Host
newHost =
  Host'
    { tags = Prelude.Nothing,
      instances = Prelude.Nothing,
      autoPlacement = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      availableCapacity = Prelude.Nothing,
      hostRecovery = Prelude.Nothing,
      hostId = Prelude.Nothing,
      allocationTime = Prelude.Nothing,
      state = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      hostProperties = Prelude.Nothing,
      releaseTime = Prelude.Nothing,
      allowsMultipleInstanceTypes = Prelude.Nothing,
      memberOfServiceLinkedResourceGroup = Prelude.Nothing,
      hostReservationId = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing
    }

-- | Any tags assigned to the Dedicated Host.
host_tags :: Lens.Lens' Host (Prelude.Maybe [Tag])
host_tags = Lens.lens (\Host' {tags} -> tags) (\s@Host' {} a -> s {tags = a} :: Host) Prelude.. Lens.mapping Lens.coerced

-- | The IDs and instance type that are currently running on the Dedicated
-- Host.
host_instances :: Lens.Lens' Host (Prelude.Maybe [HostInstance])
host_instances = Lens.lens (\Host' {instances} -> instances) (\s@Host' {} a -> s {instances = a} :: Host) Prelude.. Lens.mapping Lens.coerced

-- | Whether auto-placement is on or off.
host_autoPlacement :: Lens.Lens' Host (Prelude.Maybe AutoPlacement)
host_autoPlacement = Lens.lens (\Host' {autoPlacement} -> autoPlacement) (\s@Host' {} a -> s {autoPlacement = a} :: Host)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
host_clientToken :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_clientToken = Lens.lens (\Host' {clientToken} -> clientToken) (\s@Host' {} a -> s {clientToken = a} :: Host)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Outpost on
-- which the Dedicated Host is allocated.
host_outpostArn :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_outpostArn = Lens.lens (\Host' {outpostArn} -> outpostArn) (\s@Host' {} a -> s {outpostArn = a} :: Host)

-- | The ID of the Amazon Web Services account that owns the Dedicated Host.
host_ownerId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_ownerId = Lens.lens (\Host' {ownerId} -> ownerId) (\s@Host' {} a -> s {ownerId = a} :: Host)

-- | Information about the instances running on the Dedicated Host.
host_availableCapacity :: Lens.Lens' Host (Prelude.Maybe AvailableCapacity)
host_availableCapacity = Lens.lens (\Host' {availableCapacity} -> availableCapacity) (\s@Host' {} a -> s {availableCapacity = a} :: Host)

-- | Indicates whether host recovery is enabled or disabled for the Dedicated
-- Host.
host_hostRecovery :: Lens.Lens' Host (Prelude.Maybe HostRecovery)
host_hostRecovery = Lens.lens (\Host' {hostRecovery} -> hostRecovery) (\s@Host' {} a -> s {hostRecovery = a} :: Host)

-- | The ID of the Dedicated Host.
host_hostId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostId = Lens.lens (\Host' {hostId} -> hostId) (\s@Host' {} a -> s {hostId = a} :: Host)

-- | The time that the Dedicated Host was allocated.
host_allocationTime :: Lens.Lens' Host (Prelude.Maybe Prelude.UTCTime)
host_allocationTime = Lens.lens (\Host' {allocationTime} -> allocationTime) (\s@Host' {} a -> s {allocationTime = a} :: Host) Prelude.. Lens.mapping Core._Time

-- | The Dedicated Host\'s state.
host_state :: Lens.Lens' Host (Prelude.Maybe AllocationState)
host_state = Lens.lens (\Host' {state} -> state) (\s@Host' {} a -> s {state = a} :: Host)

-- | The Availability Zone of the Dedicated Host.
host_availabilityZone :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_availabilityZone = Lens.lens (\Host' {availabilityZone} -> availabilityZone) (\s@Host' {} a -> s {availabilityZone = a} :: Host)

-- | The hardware specifications of the Dedicated Host.
host_hostProperties :: Lens.Lens' Host (Prelude.Maybe HostProperties)
host_hostProperties = Lens.lens (\Host' {hostProperties} -> hostProperties) (\s@Host' {} a -> s {hostProperties = a} :: Host)

-- | The time that the Dedicated Host was released.
host_releaseTime :: Lens.Lens' Host (Prelude.Maybe Prelude.UTCTime)
host_releaseTime = Lens.lens (\Host' {releaseTime} -> releaseTime) (\s@Host' {} a -> s {releaseTime = a} :: Host) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the Dedicated Host supports multiple instance types of
-- the same instance family. If the value is @on@, the Dedicated Host
-- supports multiple instance types in the instance family. If the value is
-- @off@, the Dedicated Host supports a single instance type only.
host_allowsMultipleInstanceTypes :: Lens.Lens' Host (Prelude.Maybe AllowsMultipleInstanceTypes)
host_allowsMultipleInstanceTypes = Lens.lens (\Host' {allowsMultipleInstanceTypes} -> allowsMultipleInstanceTypes) (\s@Host' {} a -> s {allowsMultipleInstanceTypes = a} :: Host)

-- | Indicates whether the Dedicated Host is in a host resource group. If
-- __memberOfServiceLinkedResourceGroup__ is @true@, the host is in a host
-- resource group; otherwise, it is not.
host_memberOfServiceLinkedResourceGroup :: Lens.Lens' Host (Prelude.Maybe Prelude.Bool)
host_memberOfServiceLinkedResourceGroup = Lens.lens (\Host' {memberOfServiceLinkedResourceGroup} -> memberOfServiceLinkedResourceGroup) (\s@Host' {} a -> s {memberOfServiceLinkedResourceGroup = a} :: Host)

-- | The reservation ID of the Dedicated Host. This returns a @null@ response
-- if the Dedicated Host doesn\'t have an associated reservation.
host_hostReservationId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostReservationId = Lens.lens (\Host' {hostReservationId} -> hostReservationId) (\s@Host' {} a -> s {hostReservationId = a} :: Host)

-- | The ID of the Availability Zone in which the Dedicated Host is
-- allocated.
host_availabilityZoneId :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_availabilityZoneId = Lens.lens (\Host' {availabilityZoneId} -> availabilityZoneId) (\s@Host' {} a -> s {availabilityZoneId = a} :: Host)

instance Core.FromXML Host where
  parseXML x =
    Host'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "instances" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "autoPlacement")
      Prelude.<*> (x Core..@? "clientToken")
      Prelude.<*> (x Core..@? "outpostArn")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "availableCapacity")
      Prelude.<*> (x Core..@? "hostRecovery")
      Prelude.<*> (x Core..@? "hostId")
      Prelude.<*> (x Core..@? "allocationTime")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "hostProperties")
      Prelude.<*> (x Core..@? "releaseTime")
      Prelude.<*> (x Core..@? "allowsMultipleInstanceTypes")
      Prelude.<*> (x Core..@? "memberOfServiceLinkedResourceGroup")
      Prelude.<*> (x Core..@? "hostReservationId")
      Prelude.<*> (x Core..@? "availabilityZoneId")

instance Prelude.Hashable Host where
  hashWithSalt _salt Host' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` autoPlacement
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` availableCapacity
      `Prelude.hashWithSalt` hostRecovery
      `Prelude.hashWithSalt` hostId
      `Prelude.hashWithSalt` allocationTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` hostProperties
      `Prelude.hashWithSalt` releaseTime
      `Prelude.hashWithSalt` allowsMultipleInstanceTypes
      `Prelude.hashWithSalt` memberOfServiceLinkedResourceGroup
      `Prelude.hashWithSalt` hostReservationId
      `Prelude.hashWithSalt` availabilityZoneId

instance Prelude.NFData Host where
  rnf Host' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf autoPlacement
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf availableCapacity
      `Prelude.seq` Prelude.rnf hostRecovery
      `Prelude.seq` Prelude.rnf hostId
      `Prelude.seq` Prelude.rnf allocationTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf hostProperties
      `Prelude.seq` Prelude.rnf releaseTime
      `Prelude.seq` Prelude.rnf allowsMultipleInstanceTypes
      `Prelude.seq` Prelude.rnf
        memberOfServiceLinkedResourceGroup
      `Prelude.seq` Prelude.rnf hostReservationId
      `Prelude.seq` Prelude.rnf availabilityZoneId
