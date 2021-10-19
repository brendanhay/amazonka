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
-- Module      : Network.AWS.EC2.Types.InstanceStatusEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusEvent where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EventCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a scheduled event for an instance.
--
-- /See:/ 'newInstanceStatusEvent' smart constructor.
data InstanceStatusEvent = InstanceStatusEvent'
  { -- | The earliest scheduled start time for the event.
    notBefore :: Prelude.Maybe Core.ISO8601,
    -- | The event code.
    code :: Prelude.Maybe EventCode,
    -- | The ID of the event.
    instanceEventId :: Prelude.Maybe Prelude.Text,
    -- | A description of the event.
    --
    -- After a scheduled event is completed, it can still be described for up
    -- to a week. If the event has been completed, this description starts with
    -- the following text: [Completed].
    description :: Prelude.Maybe Prelude.Text,
    -- | The deadline for starting the event.
    notBeforeDeadline :: Prelude.Maybe Core.ISO8601,
    -- | The latest scheduled end time for the event.
    notAfter :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStatusEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notBefore', 'instanceStatusEvent_notBefore' - The earliest scheduled start time for the event.
--
-- 'code', 'instanceStatusEvent_code' - The event code.
--
-- 'instanceEventId', 'instanceStatusEvent_instanceEventId' - The ID of the event.
--
-- 'description', 'instanceStatusEvent_description' - A description of the event.
--
-- After a scheduled event is completed, it can still be described for up
-- to a week. If the event has been completed, this description starts with
-- the following text: [Completed].
--
-- 'notBeforeDeadline', 'instanceStatusEvent_notBeforeDeadline' - The deadline for starting the event.
--
-- 'notAfter', 'instanceStatusEvent_notAfter' - The latest scheduled end time for the event.
newInstanceStatusEvent ::
  InstanceStatusEvent
newInstanceStatusEvent =
  InstanceStatusEvent'
    { notBefore = Prelude.Nothing,
      code = Prelude.Nothing,
      instanceEventId = Prelude.Nothing,
      description = Prelude.Nothing,
      notBeforeDeadline = Prelude.Nothing,
      notAfter = Prelude.Nothing
    }

-- | The earliest scheduled start time for the event.
instanceStatusEvent_notBefore :: Lens.Lens' InstanceStatusEvent (Prelude.Maybe Prelude.UTCTime)
instanceStatusEvent_notBefore = Lens.lens (\InstanceStatusEvent' {notBefore} -> notBefore) (\s@InstanceStatusEvent' {} a -> s {notBefore = a} :: InstanceStatusEvent) Prelude.. Lens.mapping Core._Time

-- | The event code.
instanceStatusEvent_code :: Lens.Lens' InstanceStatusEvent (Prelude.Maybe EventCode)
instanceStatusEvent_code = Lens.lens (\InstanceStatusEvent' {code} -> code) (\s@InstanceStatusEvent' {} a -> s {code = a} :: InstanceStatusEvent)

-- | The ID of the event.
instanceStatusEvent_instanceEventId :: Lens.Lens' InstanceStatusEvent (Prelude.Maybe Prelude.Text)
instanceStatusEvent_instanceEventId = Lens.lens (\InstanceStatusEvent' {instanceEventId} -> instanceEventId) (\s@InstanceStatusEvent' {} a -> s {instanceEventId = a} :: InstanceStatusEvent)

-- | A description of the event.
--
-- After a scheduled event is completed, it can still be described for up
-- to a week. If the event has been completed, this description starts with
-- the following text: [Completed].
instanceStatusEvent_description :: Lens.Lens' InstanceStatusEvent (Prelude.Maybe Prelude.Text)
instanceStatusEvent_description = Lens.lens (\InstanceStatusEvent' {description} -> description) (\s@InstanceStatusEvent' {} a -> s {description = a} :: InstanceStatusEvent)

-- | The deadline for starting the event.
instanceStatusEvent_notBeforeDeadline :: Lens.Lens' InstanceStatusEvent (Prelude.Maybe Prelude.UTCTime)
instanceStatusEvent_notBeforeDeadline = Lens.lens (\InstanceStatusEvent' {notBeforeDeadline} -> notBeforeDeadline) (\s@InstanceStatusEvent' {} a -> s {notBeforeDeadline = a} :: InstanceStatusEvent) Prelude.. Lens.mapping Core._Time

-- | The latest scheduled end time for the event.
instanceStatusEvent_notAfter :: Lens.Lens' InstanceStatusEvent (Prelude.Maybe Prelude.UTCTime)
instanceStatusEvent_notAfter = Lens.lens (\InstanceStatusEvent' {notAfter} -> notAfter) (\s@InstanceStatusEvent' {} a -> s {notAfter = a} :: InstanceStatusEvent) Prelude.. Lens.mapping Core._Time

instance Core.FromXML InstanceStatusEvent where
  parseXML x =
    InstanceStatusEvent'
      Prelude.<$> (x Core..@? "notBefore")
      Prelude.<*> (x Core..@? "code")
      Prelude.<*> (x Core..@? "instanceEventId")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "notBeforeDeadline")
      Prelude.<*> (x Core..@? "notAfter")

instance Prelude.Hashable InstanceStatusEvent

instance Prelude.NFData InstanceStatusEvent
