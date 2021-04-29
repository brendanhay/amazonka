{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceState
import Network.AWS.EC2.Types.InstanceStatusEvent
import Network.AWS.EC2.Types.InstanceStatusSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the status of an instance.
--
-- /See:/ 'newInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Reports impaired functionality that stems from issues related to the
    -- systems that support an instance, such as hardware failures and network
    -- connectivity problems.
    systemStatus :: Prelude.Maybe InstanceStatusSummary,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Reports impaired functionality that stems from issues internal to the
    -- instance, such as impaired reachability.
    instanceStatus :: Prelude.Maybe InstanceStatusSummary,
    -- | Any scheduled events associated with the instance.
    events :: Prelude.Maybe [InstanceStatusEvent],
    -- | The Availability Zone of the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The intended state of the instance. DescribeInstanceStatus requires that
    -- an instance be in the @running@ state.
    instanceState :: Prelude.Maybe InstanceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceId = Prelude.Nothing,
      systemStatus = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      instanceStatus = Prelude.Nothing,
      events = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceState = Prelude.Nothing
    }

-- | The ID of the instance.
instanceStatus_instanceId :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_instanceId = Lens.lens (\InstanceStatus' {instanceId} -> instanceId) (\s@InstanceStatus' {} a -> s {instanceId = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
instanceStatus_systemStatus :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceStatusSummary)
instanceStatus_systemStatus = Lens.lens (\InstanceStatus' {systemStatus} -> systemStatus) (\s@InstanceStatus' {} a -> s {systemStatus = a} :: InstanceStatus)

-- | The Amazon Resource Name (ARN) of the Outpost.
instanceStatus_outpostArn :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_outpostArn = Lens.lens (\InstanceStatus' {outpostArn} -> outpostArn) (\s@InstanceStatus' {} a -> s {outpostArn = a} :: InstanceStatus)

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
instanceStatus_instanceStatus :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceStatusSummary)
instanceStatus_instanceStatus = Lens.lens (\InstanceStatus' {instanceStatus} -> instanceStatus) (\s@InstanceStatus' {} a -> s {instanceStatus = a} :: InstanceStatus)

-- | Any scheduled events associated with the instance.
instanceStatus_events :: Lens.Lens' InstanceStatus (Prelude.Maybe [InstanceStatusEvent])
instanceStatus_events = Lens.lens (\InstanceStatus' {events} -> events) (\s@InstanceStatus' {} a -> s {events = a} :: InstanceStatus) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zone of the instance.
instanceStatus_availabilityZone :: Lens.Lens' InstanceStatus (Prelude.Maybe Prelude.Text)
instanceStatus_availabilityZone = Lens.lens (\InstanceStatus' {availabilityZone} -> availabilityZone) (\s@InstanceStatus' {} a -> s {availabilityZone = a} :: InstanceStatus)

-- | The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
instanceStatus_instanceState :: Lens.Lens' InstanceStatus (Prelude.Maybe InstanceState)
instanceStatus_instanceState = Lens.lens (\InstanceStatus' {instanceState} -> instanceState) (\s@InstanceStatus' {} a -> s {instanceState = a} :: InstanceStatus)

instance Prelude.FromXML InstanceStatus where
  parseXML x =
    InstanceStatus'
      Prelude.<$> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "systemStatus")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "instanceStatus")
      Prelude.<*> ( x Prelude..@? "eventsSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "instanceState")

instance Prelude.Hashable InstanceStatus

instance Prelude.NFData InstanceStatus
