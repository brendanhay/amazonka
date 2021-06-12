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
-- Module      : Network.AWS.EC2.Types.InstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceState
import Network.AWS.EC2.Types.InstanceStatusEvent
import Network.AWS.EC2.Types.InstanceStatusSummary
import qualified Network.AWS.Lens as Lens

-- | Describes the status of an instance.
--
-- /See:/ 'newInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | Reports impaired functionality that stems from issues related to the
    -- systems that support an instance, such as hardware failures and network
    -- connectivity problems.
    systemStatus :: Core.Maybe InstanceStatusSummary,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Core.Text,
    -- | Reports impaired functionality that stems from issues internal to the
    -- instance, such as impaired reachability.
    instanceStatus :: Core.Maybe InstanceStatusSummary,
    -- | Any scheduled events associated with the instance.
    events :: Core.Maybe [InstanceStatusEvent],
    -- | The Availability Zone of the instance.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The intended state of the instance. DescribeInstanceStatus requires that
    -- an instance be in the @running@ state.
    instanceState :: Core.Maybe InstanceState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceStatus_instanceId' - The ID of the instance.
--
-- 'systemStatus', 'instanceStatus_systemStatus' - Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
--
-- 'outpostArn', 'instanceStatus_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'instanceStatus', 'instanceStatus_instanceStatus' - Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
--
-- 'events', 'instanceStatus_events' - Any scheduled events associated with the instance.
--
-- 'availabilityZone', 'instanceStatus_availabilityZone' - The Availability Zone of the instance.
--
-- 'instanceState', 'instanceStatus_instanceState' - The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
newInstanceStatus ::
  InstanceStatus
newInstanceStatus =
  InstanceStatus'
    { instanceId = Core.Nothing,
      systemStatus = Core.Nothing,
      outpostArn = Core.Nothing,
      instanceStatus = Core.Nothing,
      events = Core.Nothing,
      availabilityZone = Core.Nothing,
      instanceState = Core.Nothing
    }

-- | The ID of the instance.
instanceStatus_instanceId :: Lens.Lens' InstanceStatus (Core.Maybe Core.Text)
instanceStatus_instanceId = Lens.lens (\InstanceStatus' {instanceId} -> instanceId) (\s@InstanceStatus' {} a -> s {instanceId = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
instanceStatus_systemStatus :: Lens.Lens' InstanceStatus (Core.Maybe InstanceStatusSummary)
instanceStatus_systemStatus = Lens.lens (\InstanceStatus' {systemStatus} -> systemStatus) (\s@InstanceStatus' {} a -> s {systemStatus = a} :: InstanceStatus)

-- | The Amazon Resource Name (ARN) of the Outpost.
instanceStatus_outpostArn :: Lens.Lens' InstanceStatus (Core.Maybe Core.Text)
instanceStatus_outpostArn = Lens.lens (\InstanceStatus' {outpostArn} -> outpostArn) (\s@InstanceStatus' {} a -> s {outpostArn = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
instanceStatus_instanceStatus :: Lens.Lens' InstanceStatus (Core.Maybe InstanceStatusSummary)
instanceStatus_instanceStatus = Lens.lens (\InstanceStatus' {instanceStatus} -> instanceStatus) (\s@InstanceStatus' {} a -> s {instanceStatus = a} :: InstanceStatus)

-- | Any scheduled events associated with the instance.
instanceStatus_events :: Lens.Lens' InstanceStatus (Core.Maybe [InstanceStatusEvent])
instanceStatus_events = Lens.lens (\InstanceStatus' {events} -> events) (\s@InstanceStatus' {} a -> s {events = a} :: InstanceStatus) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zone of the instance.
instanceStatus_availabilityZone :: Lens.Lens' InstanceStatus (Core.Maybe Core.Text)
instanceStatus_availabilityZone = Lens.lens (\InstanceStatus' {availabilityZone} -> availabilityZone) (\s@InstanceStatus' {} a -> s {availabilityZone = a} :: InstanceStatus)

-- | The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
instanceStatus_instanceState :: Lens.Lens' InstanceStatus (Core.Maybe InstanceState)
instanceStatus_instanceState = Lens.lens (\InstanceStatus' {instanceState} -> instanceState) (\s@InstanceStatus' {} a -> s {instanceState = a} :: InstanceStatus)

instance Core.FromXML InstanceStatus where
  parseXML x =
    InstanceStatus'
      Core.<$> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "systemStatus")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "instanceStatus")
      Core.<*> ( x Core..@? "eventsSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "instanceState")

instance Core.Hashable InstanceStatus

instance Core.NFData InstanceStatus
