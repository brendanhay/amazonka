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
-- Module      : Network.AWS.EC2.Types.VolumeStatusEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusEvent where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a volume status event.
--
-- /See:/ 'newVolumeStatusEvent' smart constructor.
data VolumeStatusEvent = VolumeStatusEvent'
  { -- | The earliest start time of the event.
    notBefore :: Core.Maybe Core.ISO8601,
    -- | The type of this event.
    eventType :: Core.Maybe Core.Text,
    -- | The ID of the instance associated with the event.
    instanceId :: Core.Maybe Core.Text,
    -- | The ID of this event.
    eventId :: Core.Maybe Core.Text,
    -- | The latest end time of the event.
    notAfter :: Core.Maybe Core.ISO8601,
    -- | A description of the event.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeStatusEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notBefore', 'volumeStatusEvent_notBefore' - The earliest start time of the event.
--
-- 'eventType', 'volumeStatusEvent_eventType' - The type of this event.
--
-- 'instanceId', 'volumeStatusEvent_instanceId' - The ID of the instance associated with the event.
--
-- 'eventId', 'volumeStatusEvent_eventId' - The ID of this event.
--
-- 'notAfter', 'volumeStatusEvent_notAfter' - The latest end time of the event.
--
-- 'description', 'volumeStatusEvent_description' - A description of the event.
newVolumeStatusEvent ::
  VolumeStatusEvent
newVolumeStatusEvent =
  VolumeStatusEvent'
    { notBefore = Core.Nothing,
      eventType = Core.Nothing,
      instanceId = Core.Nothing,
      eventId = Core.Nothing,
      notAfter = Core.Nothing,
      description = Core.Nothing
    }

-- | The earliest start time of the event.
volumeStatusEvent_notBefore :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.UTCTime)
volumeStatusEvent_notBefore = Lens.lens (\VolumeStatusEvent' {notBefore} -> notBefore) (\s@VolumeStatusEvent' {} a -> s {notBefore = a} :: VolumeStatusEvent) Core.. Lens.mapping Core._Time

-- | The type of this event.
volumeStatusEvent_eventType :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.Text)
volumeStatusEvent_eventType = Lens.lens (\VolumeStatusEvent' {eventType} -> eventType) (\s@VolumeStatusEvent' {} a -> s {eventType = a} :: VolumeStatusEvent)

-- | The ID of the instance associated with the event.
volumeStatusEvent_instanceId :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.Text)
volumeStatusEvent_instanceId = Lens.lens (\VolumeStatusEvent' {instanceId} -> instanceId) (\s@VolumeStatusEvent' {} a -> s {instanceId = a} :: VolumeStatusEvent)

-- | The ID of this event.
volumeStatusEvent_eventId :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.Text)
volumeStatusEvent_eventId = Lens.lens (\VolumeStatusEvent' {eventId} -> eventId) (\s@VolumeStatusEvent' {} a -> s {eventId = a} :: VolumeStatusEvent)

-- | The latest end time of the event.
volumeStatusEvent_notAfter :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.UTCTime)
volumeStatusEvent_notAfter = Lens.lens (\VolumeStatusEvent' {notAfter} -> notAfter) (\s@VolumeStatusEvent' {} a -> s {notAfter = a} :: VolumeStatusEvent) Core.. Lens.mapping Core._Time

-- | A description of the event.
volumeStatusEvent_description :: Lens.Lens' VolumeStatusEvent (Core.Maybe Core.Text)
volumeStatusEvent_description = Lens.lens (\VolumeStatusEvent' {description} -> description) (\s@VolumeStatusEvent' {} a -> s {description = a} :: VolumeStatusEvent)

instance Core.FromXML VolumeStatusEvent where
  parseXML x =
    VolumeStatusEvent'
      Core.<$> (x Core..@? "notBefore")
      Core.<*> (x Core..@? "eventType")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "eventId")
      Core.<*> (x Core..@? "notAfter")
      Core.<*> (x Core..@? "description")

instance Core.Hashable VolumeStatusEvent

instance Core.NFData VolumeStatusEvent
