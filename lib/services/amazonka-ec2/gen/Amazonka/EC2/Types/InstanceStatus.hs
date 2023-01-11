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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceState
import Amazonka.EC2.Types.InstanceStatusEvent
import Amazonka.EC2.Types.InstanceStatusSummary
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of an instance.
--
-- /See:/ 'newInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { -- | The Availability Zone of the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Any scheduled events associated with the instance.
    events :: Prelude.Maybe [InstanceStatusEvent],
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The intended state of the instance. DescribeInstanceStatus requires that
    -- an instance be in the @running@ state.
    instanceState :: Prelude.Maybe InstanceState,
    -- | Reports impaired functionality that stems from issues internal to the
    -- instance, such as impaired reachability.
    instanceStatus :: Prelude.Maybe InstanceStatusSummary,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Reports impaired functionality that stems from issues related to the
    -- systems that support an instance, such as hardware failures and network
    -- connectivity problems.
    systemStatus :: Prelude.Maybe InstanceStatusSummary
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
-- 'availabilityZone', 'instanceStatus_availabilityZone' - The Availability Zone of the instance.
--
-- 'events', 'instanceStatus_events' - Any scheduled events associated with the instance.
--
-- 'instanceId', 'instanceStatus_instanceId' - The ID of the instance.
--
-- 'instanceState', 'instanceStatus_instanceState' - The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
--
-- 'instanceStatus', 'instanceStatus_instanceStatus' - Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
--
-- 'outpostArn', 'instanceStatus_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'systemStatus', 'instanceStatus_systemStatus' - Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
newInstanceStatus ::
  InstanceStatus
newInstanceStatus =
  InstanceStatus'
    { availabilityZone = Prelude.Nothing,
      events = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceState = Prelude.Nothing,
      instanceStatus = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      systemStatus = Prelude.Nothing
    }

-- | The Availability Zone of the instance.
instanceStatus_availabilityZone :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_availabilityZone = Lens.lens (\InstanceStatus' {availabilityZone} -> availabilityZone) (\s@InstanceStatus' {} a -> s {availabilityZone = a} :: InstanceStatus)

-- | Any scheduled events associated with the instance.
instanceStatus_events :: Lens.Lens' InstanceStatus (Prelude.Maybe [InstanceStatusEvent])
instanceStatus_events = Lens.lens (\InstanceStatus' {events} -> events) (\s@InstanceStatus' {} a -> s {events = a} :: InstanceStatus) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the instance.
instanceStatus_instanceId :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_instanceId = Lens.lens (\InstanceStatus' {instanceId} -> instanceId) (\s@InstanceStatus' {} a -> s {instanceId = a} :: InstanceStatus)

-- | The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
instanceStatus_instanceState :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceState)
instanceStatus_instanceState = Lens.lens (\InstanceStatus' {instanceState} -> instanceState) (\s@InstanceStatus' {} a -> s {instanceState = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
instanceStatus_instanceStatus :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceStatusSummary)
instanceStatus_instanceStatus = Lens.lens (\InstanceStatus' {instanceStatus} -> instanceStatus) (\s@InstanceStatus' {} a -> s {instanceStatus = a} :: InstanceStatus)

-- | The Amazon Resource Name (ARN) of the Outpost.
instanceStatus_outpostArn :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_outpostArn = Lens.lens (\InstanceStatus' {outpostArn} -> outpostArn) (\s@InstanceStatus' {} a -> s {outpostArn = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
instanceStatus_systemStatus :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceStatusSummary)
instanceStatus_systemStatus = Lens.lens (\InstanceStatus' {systemStatus} -> systemStatus) (\s@InstanceStatus' {} a -> s {systemStatus = a} :: InstanceStatus)

instance Data.FromXML InstanceStatus where
  parseXML x =
    InstanceStatus'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> ( x Data..@? "eventsSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "instanceState")
      Prelude.<*> (x Data..@? "instanceStatus")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "systemStatus")

instance Prelude.Hashable InstanceStatus where
  hashWithSalt _salt InstanceStatus' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceState
      `Prelude.hashWithSalt` instanceStatus
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` systemStatus

instance Prelude.NFData InstanceStatus where
  rnf InstanceStatus' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceState
      `Prelude.seq` Prelude.rnf instanceStatus
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf systemStatus
