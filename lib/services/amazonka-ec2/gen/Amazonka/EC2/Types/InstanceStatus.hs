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
-- Module      : Amazonka.EC2.Types.InstanceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceState
import Amazonka.EC2.Types.InstanceStatusEvent
import Amazonka.EC2.Types.InstanceStatusSummary
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of an instance.
--
-- /See:/ 'newInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { -- | The intended state of the instance. DescribeInstanceStatus requires that
    -- an instance be in the @running@ state.
    instanceState :: Prelude.Maybe InstanceState,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Reports impaired functionality that stems from issues internal to the
    -- instance, such as impaired reachability.
    instanceStatus :: Prelude.Maybe InstanceStatusSummary,
    -- | The Availability Zone of the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Reports impaired functionality that stems from issues related to the
    -- systems that support an instance, such as hardware failures and network
    -- connectivity problems.
    systemStatus :: Prelude.Maybe InstanceStatusSummary,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Any scheduled events associated with the instance.
    events :: Prelude.Maybe [InstanceStatusEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceState', 'instanceStatus_instanceState' - The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
--
-- 'outpostArn', 'instanceStatus_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'instanceStatus', 'instanceStatus_instanceStatus' - Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
--
-- 'availabilityZone', 'instanceStatus_availabilityZone' - The Availability Zone of the instance.
--
-- 'systemStatus', 'instanceStatus_systemStatus' - Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
--
-- 'instanceId', 'instanceStatus_instanceId' - The ID of the instance.
--
-- 'events', 'instanceStatus_events' - Any scheduled events associated with the instance.
newInstanceStatus ::
  InstanceStatus
newInstanceStatus =
  InstanceStatus'
    { instanceState = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      instanceStatus = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      systemStatus = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      events = Prelude.Nothing
    }

-- | The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
instanceStatus_instanceState :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceState)
instanceStatus_instanceState = Lens.lens (\InstanceStatus' {instanceState} -> instanceState) (\s@InstanceStatus' {} a -> s {instanceState = a} :: InstanceStatus)

-- | The Amazon Resource Name (ARN) of the Outpost.
instanceStatus_outpostArn :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_outpostArn = Lens.lens (\InstanceStatus' {outpostArn} -> outpostArn) (\s@InstanceStatus' {} a -> s {outpostArn = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
instanceStatus_instanceStatus :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceStatusSummary)
instanceStatus_instanceStatus = Lens.lens (\InstanceStatus' {instanceStatus} -> instanceStatus) (\s@InstanceStatus' {} a -> s {instanceStatus = a} :: InstanceStatus)

-- | The Availability Zone of the instance.
instanceStatus_availabilityZone :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_availabilityZone = Lens.lens (\InstanceStatus' {availabilityZone} -> availabilityZone) (\s@InstanceStatus' {} a -> s {availabilityZone = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
instanceStatus_systemStatus :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceStatusSummary)
instanceStatus_systemStatus = Lens.lens (\InstanceStatus' {systemStatus} -> systemStatus) (\s@InstanceStatus' {} a -> s {systemStatus = a} :: InstanceStatus)

-- | The ID of the instance.
instanceStatus_instanceId :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_instanceId = Lens.lens (\InstanceStatus' {instanceId} -> instanceId) (\s@InstanceStatus' {} a -> s {instanceId = a} :: InstanceStatus)

-- | Any scheduled events associated with the instance.
instanceStatus_events :: Lens.Lens' InstanceStatus (Prelude.Maybe [InstanceStatusEvent])
instanceStatus_events = Lens.lens (\InstanceStatus' {events} -> events) (\s@InstanceStatus' {} a -> s {events = a} :: InstanceStatus) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML InstanceStatus where
  parseXML x =
    InstanceStatus'
      Prelude.<$> (x Core..@? "instanceState")
      Prelude.<*> (x Core..@? "outpostArn")
      Prelude.<*> (x Core..@? "instanceStatus")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "systemStatus")
      Prelude.<*> (x Core..@? "instanceId")
      Prelude.<*> ( x Core..@? "eventsSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable InstanceStatus where
  hashWithSalt _salt InstanceStatus' {..} =
    _salt `Prelude.hashWithSalt` instanceState
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` instanceStatus
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` systemStatus
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` events

instance Prelude.NFData InstanceStatus where
  rnf InstanceStatus' {..} =
    Prelude.rnf instanceState
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf instanceStatus
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf systemStatus
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf events
